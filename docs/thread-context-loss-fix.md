# Fixing thread context loss

## Summary

The thread context loss problem documented in `thread-context-loss.md` can be completely eliminated without forking Shake and without importing any Shake-internal modules. The fix is mechanical: every wrapped function that calls a potentially-suspending Shake function saves the current node ID before the call and re-establishes it on the (possibly different) thread after the call returns.

## Background

### The current approach

We maintain a `TVar (Map ThreadId Int)` that maps OS thread IDs to telemetry node IDs. Rule wrappers call `setThreadNode` when a rule starts executing, and action wrappers call `getThreadNode` when recording edges.

### Why it breaks

Shake's `Action` monad uses continuation-passing style. When an action suspends (waiting for a dependency, a batch fence, or a parallel child), the continuation is captured and later enqueued onto the thread pool via `addPool`. Whichever thread picks it up has no entry in our `ThreadId → NodeId` map. The original thread may already be running a different rule.

This affects `batch` (always), `need`/`apply` (when the dependency isn't ready), and `parallel` (parent after children complete).

## The fix: re-establish context after suspending calls

### Core insight

We already wrap every public Shake function that can trigger suspension (`need`, `askOracle`, `parallel`, `doesFileExist`, etc.). At the point where our wrapper runs, thread context is either valid (set by the rule wrapper or re-established by a previous wrapped call) or already lost (Nothing). If it's valid, we can **save** the node ID before calling the real Shake function and **re-set** it after the call returns — regardless of which thread we're now on.

### The pattern

Before the fix:

```haskell
need :: [FilePath] -> Action ()
need files = do
    state <- getTelemetryState
    mNode <- Shake.liftIO $ getThreadNode state
    case mNode of
        Nothing -> pure ()  -- context lost, skip edge recording
        Just nid -> mapM_ (\f -> Shake.liftIO $ recordEdge state nid (T.pack f) FileNode) files
    Shake.need files
    -- After this point we may be on a different thread.
    -- The next wrapped call will find Nothing. Edges silently dropped.
```

After the fix:

```haskell
need :: [FilePath] -> Action ()
need files = do
    state <- getTelemetryState
    mNode <- Shake.liftIO $ getThreadNode state
    case mNode of
        Nothing -> Shake.need files  -- already lost, nothing to save
        Just nid -> do
            mapM_ (\f -> Shake.liftIO $ recordEdge state nid (T.pack f) FileNode) files
            Shake.need files
            -- Re-establish context on whatever thread we're on now.
            Shake.liftIO $ setThreadNode state nid
```

The `setThreadNode` after `Shake.need` writes the saved `nid` into the `ThreadId → NodeId` map for the current thread. If the thread didn't change (fast path, dependency already built), this is a harmless no-op overwrite. If the thread changed (suspension occurred), this establishes context on the new thread so subsequent wrapped calls find it.

### Why this is safe

**No races on `ThreadId` context.** `setThreadNode` always uses `myThreadId` — no thread can modify another thread's entry. Between our save (before the Shake call) and our restore (after), the original thread T1 may be reused by a different rule that overwrites T1's context — this is fine, T1's old context is stale. When our continuation resumes on T2, T2 is dedicated to this continuation; any prior context on T2 is stale and safe to overwrite.

**No risk at the first call.** The rule wrapper (`wrapFileAction`, `wrapAction`, etc.) calls `setThreadNode` before the user's action body begins. The first wrapped call in the action body will always find valid context.

**No cost when nothing suspends.** If `Shake.need` returns immediately (dependency cached/already built), the thread doesn't change. The `setThreadNode` writes the same value — effectively a no-op.

### Which functions need the re-establish pattern

Every wrapped function that eventually calls Shake's `apply`/`apply1` or `actionFenceRequeue` can suspend. This includes:

| Wrapped function | Suspends via |
|------------------|-------------|
| `need`, `needed`, `orderOnly` | `apply` (when deps not ready) |
| `askOracle`, `askOracles`, `askOracleWith` | `apply1` |
| `doesFileExist`, `doesDirectoryExist` | `apply1` |
| `getDirectoryContents`, `getDirectoryFiles`, `getDirectoryDirs` | `apply1` |
| `getEnv`, `getEnvWithDefault` | `apply1` |
| `parallel`, `forP`, `par` | `actionFenceRequeue` |

All of these are already wrapped in `Actions.hs` and `Parallel.hs`. The change is adding `setThreadNode state nid` after each call to the underlying Shake function.

### Parallel: re-establish parent context

The `parallel` wrapper already propagates the parent's node ID to child actions. After `Shake.parallel` returns (the parent resumes, possibly on a new thread), the parent's context must be re-established:

```haskell
parallel :: [Action a] -> Action [a]
parallel acts = do
    state <- getTelemetryState
    mNode <- Shake.liftIO $ getThreadNode state
    let wrappedActs = case mNode of
            Nothing -> acts
            Just nid -> map (propagateContext state nid) acts
    results <- Shake.parallel wrappedActs
    -- Parent may have resumed on a different thread. Re-establish.
    case mNode of
        Just nid -> Shake.liftIO $ setThreadNode state nid
        Nothing  -> pure ()
    pure results
```

### Batch: wrap the `one` callback

Batch is the trickiest case. Shake's `batch` suspends the rule's action at the fence between the per-item phase (`one`) and the collect phase (`many`). The suspension happens inside Shake's batch internals, after our `wrappedAct` has already returned — so adding `setThreadNode` inside `wrappedAct` doesn't help.

The fix: intercept the `rules` parameter to wrap the `one` callback that Shake provides:

```haskell
batch :: Int -> ((a -> Action ()) -> Rules ()) -> (a -> Action b) -> ([b] -> Action ()) -> Rules ()
batch maxBatch rules act collect = Shake.batch maxBatch wrappedRules wrappedAct wrappedCollect
  where
    wrappedRules realOne = rules $ \a -> do
        -- Save context before calling realOne.
        -- realOne runs wrappedAct, then suspends at the batch fence.
        -- After the fence fires, the continuation resumes on a new thread.
        state <- getTelemetryState
        mNode <- Shake.liftIO $ getThreadNode state
        realOne a
        -- Re-establish context on the (possibly new) resume thread.
        case mNode of
            Just nid -> Shake.liftIO $ setThreadNode state nid
            Nothing  -> pure ()

    wrappedAct a = do
        state <- getTelemetryState
        nid <- Shake.liftIO $ registerNode state "batch-item" BatchNode
        Shake.liftIO $ setThreadNode state nid
        result <- act a
        Shake.liftIO $ finishNode state nid
        pure result

    wrappedCollect bs = do
        state <- getTelemetryState
        nid <- Shake.liftIO $ registerNode state "batch-collect" BatchNode
        Shake.liftIO $ setThreadNode state nid
        collect bs
        Shake.liftIO $ finishNode state nid
```

The saved `mNode` is the file rule's node ID (set by the `%>` wrapper). After the batch suspension, this is what we want — the file rule's action body is continuing, and subsequent dependency calls should be attributed to that rule.

### Execution trace

Here's the complete flow for a rule body with two `need` calls, where the first suspends:

```
Thread T1:
  wrapFileAction:
    registerNode "output.o" → nid=5
    setThreadNode 5          → map: {T1 → 5}

  act out:
    need ["a.o"]:
      getThreadNode           → Just 5  ✓
      recordEdge 5 → "a.o"
      Shake.need ["a.o"]      → SUSPENDS (a.o not built yet)
      --- T1 returns to pool, picks up other work ---
      --- T1's context may be overwritten by another rule ---

Thread T2 (continuation resumes):
      setThreadNode 5          → map: {..., T2 → 5}

    need ["b.o"]:
      getThreadNode            → Just 5  ✓  (re-established!)
      recordEdge 5 → "b.o"
      Shake.need ["b.o"]       → returns immediately (b.o cached)
      setThreadNode 5          → map: {..., T2 → 5}  (no-op)

  wrapFileAction:
    finishNode 5
```

Both edges are recorded. No context loss.

## What this does NOT require

- **No Shake fork.** The fix uses only Shake's public API.
- **No internal module imports.** `getCurrentKey`, `Key`, `Local`, `Stack` — none of these are needed.
- **No new data structures.** The existing `TelemetryState` with `tsThreadContext :: TVar (Map ThreadId Int)` is sufficient.
- **No API changes.** The wrapped functions keep the same signatures.

## Performance cost

Each wrapped function call adds one extra `setThreadNode` call (an `atomically $ modifyTVar'` on the thread context map). This is the same cost as the existing `getThreadNode` call (a `readTVarIO`). Both are sub-microsecond operations. The overhead is negligible relative to any real work the build rules perform.

## What this does NOT fix

**Cached/skipped rules** still produce placeholder nodes with no timing data, and their transitive dependencies are still absent from the graph. When Shake skips a rule, its body never executes, so no `need` calls are intercepted. This limitation is inherent to the wrapping approach and unrelated to thread context.

## Scope of change

| File | Change |
|------|--------|
| `Wrap/Actions.hs` | Add `setThreadNode` after each `Shake.*` call in the `Just nid` branch |
| `Wrap/Parallel.hs` | Add `setThreadNode` after `Shake.parallel`/`Shake.forP`/`Shake.par` |
| `Wrap/Rules.hs` | Wrap `one` callback in `batch` with context save/restore |

No changes needed to `State.hs`, `Graph.hs`, `Entry.hs`, or `shake-telemetry.cabal`.

Estimated diff: ~40 lines changed across 3 files.

## Alternative considered: `getCurrentKey` from Shake internals

Shake's `Action` monad carries the current rule's `Key` in its `Local` state, accessible via `getCurrentKey :: Action (Maybe Key)` (defined in `Development.Shake.Internal.Core.Action`). Since `Local` is wrapped in an `IORef` that travels with continuations, `getCurrentKey` works correctly across thread pool requeuing.

However, both `getCurrentKey` and `Key` are in Shake's `other-modules` (not `exposed-modules` in `shake.cabal`), meaning they **cannot be imported by downstream packages**. Using this approach would require either:

1. A Shake fork/patch to expose these modules (even a one-line cabal change is still a fork to maintain)
2. A PR to Shake upstream to expose `getCurrentKey` (uncertain and slow)

The re-establish approach described in this document achieves the same result using only the public API, making it strictly preferable.
