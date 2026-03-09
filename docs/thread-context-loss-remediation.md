# Thread Context Loss: Non-Fork Remediation Options

## Purpose

This document evaluates whether `shake-telemetry` can fully or partially fix thread-context loss **without forking Shake**, and proposes practical implementation options.

It complements `docs/thread-context-loss.md` by focusing on remediation strategy.

## Problem recap

`shake-telemetry` currently attributes dependency edges using a thread-local map:

- `ThreadId -> NodeId` in telemetry state
- wrappers call `setThreadNode` at rule entry
- dependency wrappers (`need`, `askOracle`, etc.) call `getThreadNode` to find edge source

This fails when Shake suspends an `Action` continuation and resumes it on a different pool thread.

## Why context is lost (source evidence)

Shake continuation resume paths enqueue continuation work on the pool:

- `actionFenceRequeueBy` uses `captureRAW` + `addPool`:
  - `../shake/src/Development/Shake/Internal/Core/Pool.hs:60`
- `apply` blocking path (used by `need`/`askOracle`/directory/env wrappers) also requeues:
  - `../shake/src/Development/Shake/Internal/Core/Build.hs:176`
- `parallel`/`forP`/`par` wait for child fences:
  - `../shake/src/Development/Shake/Internal/Core/Action.hs:496`
- `batch` waits with per-item fences:
  - `../shake/src/Development/Shake/Internal/Core/Action.hs:552`

So `ThreadId` is not a stable identity for an `Action` continuation.

## Can we fix this perfectly without forking Shake?

### Short answer

Not with the current wrapper-only thread-map approach.

### Why

The stable per-action context exists internally as Shake `Local`, but public API does not expose it:

- internal helpers: `getCurrentKey`, `getLocal`
  - `../shake/src/Development/Shake/Internal/Core/Action.hs:611`
- internal modules are not exposed in Shake package exports:
  - exposed modules list: `../shake/shake.cabal:132`
  - internals listed as non-exposed `other-modules`: `../shake/shake.cabal:143`

Without access to internal `Local`/continuation identity, wrappers cannot reliably track source across arbitrary requeues.

## What is still possible without forking

## Option A: Rebind thread context after wrapped calls

### Idea

In each wrapped action:

1. read current node (`mNode <- getThreadNode`)
2. call underlying Shake function
3. if `mNode` existed, call `setThreadNode` again before returning

This restores context on the thread that resumed the continuation.

### Where to apply

- dependency wrappers:
  - `src/Development/Shake/Telemetry/Wrap/Actions.hs`
- parallel wrappers:
  - `src/Development/Shake/Telemetry/Wrap/Parallel.hs`
- additional APIs that can requeue and are currently re-exported unchanged:
  - `withResource` (uses `actionFenceRequeueBy`)
    - `../shake/src/Development/Shake/Internal/Resource.hs:47`
  - `unsafeExtraThread` (uses `actionAlwaysRequeue`)
    - `../shake/src/Development/Shake/Internal/Core/Action.hs:487`
  - `reschedule` (uses `actionAlwaysRequeuePriority`)
    - `../shake/src/Development/Shake/Internal/Core/Action.hs:607`

### Expected impact

- likely fixes most missing-edge cases in current failing tests
- low complexity and backward-compatible

### Limits

- still heuristic and wrapper-dependent
- if user code enters unwrapped suspension/requeue paths and then calls wrapped dependency APIs, loss can still occur
- still tied to thread-local identity, which is inherently weaker than action-local identity

## Option B: Use Shake profile dependencies as ground truth edges

### Key insight

Shake already records exact dependencies in rule results:

- dependencies persisted as `depends = flattenDepends localDepends`
  - `../shake/src/Development/Shake/Internal/Core/Build.hs:224`
- profile JSON generation emits these dependencies as `prfDepends`
  - `../shake/src/Development/Shake/Internal/Profile.hs:82`

This dependency graph is action-local, not thread-local.

### Practical validation done locally

Using `shakeReport=[".../report.json"]`, profile JSON included complete edges in cases where wrapper thread-context tests fail:

- blocked sequential `need` chain: `main.need` had all deps in one entry (`[[0,1,2,3]]`)
- `parallel` + post-`parallel need`: `main.par` had both child deps and post-wait dep (`[[0,1,2],[3]]`)
- `batch`: both batched outputs depended on shared dep (`a.bat -> shared.dep`, `b.bat -> shared.dep`)
- oracle/env example: profile contained `OracleQ (...)` and `getEnv HOME` nodes and dependencies

### Integration model

Hybrid approach:

1. keep wrappers for telemetry metadata (node typing, labels, timing enrichment where needed)
2. derive edges primarily from Shake profile JSON (`prfDepends`)
3. merge by canonical label mapping
4. fallback to wrapper edges only when profile data unavailable

### Benefits

- removes root cause of missing edges in continuation requeue scenarios
- no fork required
- uses officially documented profile output (`--report=...json`)

### Tradeoffs

- profile schema is stable in practice but not a strong typed API contract
- mapping from profile labels to telemetry node types needs rules (e.g., file/oracle/env/directory patterns)
- potential label collisions need deterministic resolution

## Option C: Depend on Shake internal modules directly (without forking)

Technically possible only via unsupported packaging tricks (vendoring/internal exposure overrides). This is fragile across Shake releases and effectively similar to maintaining a fork-level compatibility burden.

Not recommended.

## Recommended path

1. **Implement Option A immediately** (cheap, likely high win).
2. **Prototype Option B in parallel** (ground-truth edges).
3. Move to **hybrid default**: profile-derived edges as source of truth, wrapper edges as fallback/augmentation.

This gives near-term improvements without waiting for larger refactors, while providing a robust long-term non-fork strategy.

## Suggested implementation plan

1. Add a helper `withPreservedContext :: Action a -> Action a` in wrapper layer.
2. Apply helper to all wrapped dependency and parallel APIs.
3. Add wrappers for `withResource`, `unsafeExtraThread`, `reschedule` to preserve parent context after return.
4. Re-run failing thread-context tests and capture pass/fail delta.
5. Add profile parser module for Shake report JSON (`prfName`, `prfDepends`, timing fields).
6. Add merge step: profile edges override wrapper edge set.
7. Add deterministic node-type classification from profile labels.
8. Add integration tests that compare wrapper-only vs profile-merged edge completeness.

## Test strategy

- Keep current failing tests as regression spec for wrapper-only behavior.
- Add new tests for post-mitigation expectations:
  - blocked `need` sequences preserve all edges
  - post-`parallel`/`forP`/`par` dependencies preserved
  - `withResource` and `unsafeExtraThread` scenarios preserve attribution after return
- Add profile parity tests:
  - telemetry edge set equals (or is superset of) profile edge set for supported node classes.

## Decision summary

- **Full fix with only thread-local wrappers:** no.
- **Substantial improvement without fork:** yes (Option A).
- **Robust non-fork solution:** yes (Option B/hybrid using Shake profile dependencies).

