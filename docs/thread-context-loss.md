# Thread Context Loss in shake-telemetry

## What we track

shake-telemetry intercepts Shake's dependency-creating functions (`need`, `askOracle`, `doesFileExist`, etc.) to build a dependency graph of the build. Each node in the graph represents a rule execution (a file being built, a phony target running, an oracle being queried) and each edge represents a dependency between them.

To record an edge, two pieces of information are needed:

1. **The target** -- what is being depended on (e.g. `"src/Main.hs"` passed to `need`)
2. **The source** -- which rule is currently executing and requesting that dependency

The target is straightforward: it comes from the function arguments. The source is the hard part.

## How we track the source

Shake runs rules in parallel across OS threads drawn from a thread pool. When a rule starts executing, our wrapper (`%>`, `phony`, `addOracle`, etc.) does three things:

```
registerNode   -- create a graph node for this rule
setThreadNode  -- associate the current thread with that node
run the action -- execute the user's rule body
finishNode     -- stamp the end time
```

`setThreadNode` writes into a `TVar (Map ThreadId Int)` -- a mapping from OS thread IDs to graph node IDs. Later, when the rule body calls a dependency function like `need`, our wrapper calls `getThreadNode` to look up the current thread's node ID, giving us the source end of the edge.

This works when the action body runs on a single thread from start to finish. Unfortunately, Shake does not guarantee this.

## How it gets lost

### The continuation mechanism

Shake's `Action` monad is built on continuation-passing style internally. The key primitive is `captureRAW`:

```haskell
captureRAW :: ((Either SomeException a -> IO ()) -> IO ()) -> Action a
```

This pauses the current action, captures its continuation (the remaining computation), and hands it to a callback. The continuation can later be invoked to resume the action.

### Fences and requeuing

When an action needs to wait for an asynchronous result (another action to finish, a batch to collect), Shake uses a `Fence` -- essentially a one-shot callback register. The waiting action calls `actionFenceRequeue`:

```
actionFenceRequeue fence:
  1. Test if the fence already has a result (fast path: return immediately)
  2. If not ready:
     a. captureRAW captures the continuation
     b. waitFence registers a callback on the fence
     c. When the fence fires, the callback calls addPool to enqueue
        the continuation onto the thread pool
  3. The current thread returns to the pool to do other work
```

Step 2c is where context is lost. The continuation is placed onto a work queue. Whichever thread picks it up next will resume the action -- but that thread has no entry in our `ThreadId -> NodeId` map. The original thread's mapping is stale (it may already be running a different rule), and the new thread was never told which rule it is continuing.

### Where suspension happens

There are two code paths in Shake that trigger this:

**1. `apply` (used by `need`, `askOracle`, etc.)**

When a rule calls `need ["foo.o"]`, Shake's `apply` calls `applyKeyValue` internally. This checks whether the dependency is already built:

```haskell
-- Build.hs (simplified)
applyKeyValue callStack ks = do
  wait <- ... -- check database for results
  case wait of
    Now vs -> pure vs                          -- fast path: already built
    _      -> do
      vs <- Action $ captureRAW $ \continue ->
        ... addPool PoolResume globalPool $     -- requeue on pool
              continue x
      pure vs
```

The `Now` fast path returns immediately on the same thread -- no context loss. But if the dependency hasn't been built yet, the action suspends via `captureRAW` and resumes on a different pool thread when the dependency completes.

This means sequential dependency calls within a single rule can lose context:

```haskell
"output" %> \out -> do
  need ["a.o"]    -- edge recorded on thread T1
                  -- if a.o wasn't ready, we suspended and resumed on T2
  need ["b.o"]    -- T2 has no context → edge silently dropped
```

Whether this happens depends on build order and caching. If `a.o` was already built (incremental build, or built earlier in this run), the `Now` fast path keeps us on T1. If `a.o` had to be built and we had to wait, we lose context.

**2. `actionFenceRequeue` (used by `batch`, `parallel`, etc.)**

Higher-level operations like `batch` and `parallel` use fences explicitly. Unlike `apply`, these always suspend -- there is no "already ready" fast path for `batch` (it must wait for the batch to fill).

### Concrete example: `batch`

The `batch` function is the most reliable trigger. Shake's `batch` collects multiple rule executions into groups and processes them together:

```
Rule A calls `one` (produces item)  -- runs on thread T1
Rule B calls `one` (produces item)  -- runs on thread T2
Rule C calls `one` (produces item)  -- runs on thread T3
                                       ↓
                          batch threshold reached
                                       ↓
           `many` (collect) runs on a pool thread  -- thread T4
                                       ↓
                        fences are signaled with results
                                       ↓
Rule A resumes via addPool             -- might run on thread T5
Rule B resumes via addPool             -- might run on thread T6
Rule C resumes via addPool             -- might run on thread T7
```

After `one` completes, each rule's action is suspended waiting on a fence. When `many` finishes and signals the fences, the continuations are enqueued onto the thread pool. Threads T5, T6, T7 are arbitrary pool workers -- they were never associated with rules A, B, or C.

If the resumed continuation calls `need` or any other dependency function, our `getThreadNode` lookup on the new thread finds nothing.

### Other triggers

- **`parallel`/`forP`/`par`** -- child actions are queued with `addPoolWait`, and the parent action is suspended with `actionFenceRequeue` until all children complete. The parent resumes on a different thread. Our wrappers handle the child-thread case by propagating the parent's node ID, but they cannot prevent the parent from resuming on a different thread after the children finish.
- **`need` / `askOracle` / any `apply`-based call** -- as described above, when the dependency isn't ready yet. This is conditional on build order and caching.
- **Any internal Shake operation** that calls `captureRAW` + `addPool`.

## The fix

Previously, `getThreadNode` would crash the entire build:

```haskell
getThreadNode :: TelemetryState -> IO Int
getThreadNode state = do
  tid <- myThreadId
  ctx <- readTVarIO (tsThreadContext state)
  case Map.lookup tid ctx of
    Just nid -> pure nid
    Nothing  -> error $ "no thread context for " ++ show tid
```

Now it returns `Maybe`:

```haskell
getThreadNode :: TelemetryState -> IO (Maybe Int)
getThreadNode state = do
  tid <- myThreadId
  ctx <- readTVarIO (tsThreadContext state)
  pure $ Map.lookup tid ctx
```

All callers (in `Actions.hs` and `Parallel.hs`) handle `Nothing` by skipping edge recording. The underlying Shake function always runs regardless -- the build is never affected.

## What gets lost

When an action resumes on a new thread, any dependency calls (`need`, `askOracle`, etc.) made after the resumption will not have edges recorded in the graph. The nodes for the depended-on files still exist (created by their own rules), but the edge from the requesting rule to those nodes is silently dropped.

The severity depends on the trigger:

- **`batch`**: always loses context after the `one` phase. Any `need` in the rule body after the batch suspension point is lost. This is the most predictable case.
- **`need` / `apply`**: conditional. On a clean build with high parallelism (`-j N`), dependencies are more likely to not be ready yet, causing suspension. On incremental builds where most targets are cached, the `Now` fast path keeps the thread, and edges are preserved. This means the graph can vary between clean and incremental builds.
- **`parallel`**: the parent loses context after children complete, but the parent typically doesn't call `need` after `parallel` returns -- it processes the results.

In practice, rules tend to declare their dependencies up front (`need` at the top of the action body), then do computation. This pattern naturally puts most edge-recording calls before any suspension point.

## Why not fix the root cause?

Thread-local context is fundamentally at odds with Shake's continuation-based execution model. Alternative approaches and their tradeoffs:

| Approach | Problem |
|----------|---------|
| Store node ID in Shake's `Local` state | Requires forking Shake or using `unsafePerformIO` to access internals |
| Use Haskell's implicit parameters | `Action` monad doesn't support implicit parameter propagation across `captureRAW` |
| Wrap `captureRAW` | Not exported; internal to Shake |
| Store context in a thread-local `IORef` via `unsafePerformIO` | Same problem -- the new thread doesn't inherit the `IORef` |

The `Map ThreadId Int` approach is the best available without modifying Shake itself. Accepting `Maybe` and losing some edges is the pragmatic tradeoff: the telemetry is best-effort, and an incomplete graph is far better than a crashed build.
