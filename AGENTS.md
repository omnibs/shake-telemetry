# Agent Instructions

This file contains instructions for LLM agents working on the shake-telemetry project.

## Key documents

- `PROBLEM.md` -- Problem statement and constraints
- `ARCHITECTURE.md` -- Design decisions and module structure
- `ROADMAP.md` -- Phased implementation plan with tasks

## Building

Enter the devenv shell, then use cabal:

```sh
devenv shell
cabal build
```

## Testing

```sh
devenv shell
cabal test
```

Tests use `tasty` as the runner with `hedgehog` for property tests.

<!-- TODO: Add specific test module names as they are created -->
<!-- TODO: Add instructions for running a subset of tests -->

## Project structure

```
src/Development/Shake/
  Telemetry.hs                  -- Drop-in replacement for Development.Shake
  Telemetry/
    Graph.hs                    -- BuildGraph, Node, Edge, NodeType data types
    State.hs                    -- TelemetryState, thread context, node registration
    CriticalPath.hs             -- Critical path algorithm
    Json.hs                     -- Aeson instances, JSON output
    Mermaid.hs                  -- Mermaid chart generation
    Wrap/
      Entry.hs                  -- Wrapped entry points (shake, shakeArgs, ...)
      Rules.hs                  -- Wrapped rule definers (%>, phony, addOracle, ...)
      Actions.hs                -- Wrapped dependency creators (need, askOracle, ...)
      Parallel.hs               -- Wrapped parallelism (parallel, forP, par)
      Reexports.hs              -- Everything re-exported unchanged from Shake
test/
  Main.hs                       -- Tasty test runner
```

## Conventions

- Language: GHC2021, GHC 9.8.x
- Strict fields (`!`) on all data types
- STM for concurrent state; use strict TVar operations to avoid space leaks
- Test framework: `tasty` + `tasty-hedgehog` + `hedgehog`
- No global mutable state; telemetry state is threaded via Shake's `shakeExtra`

<!-- TODO: Add linting/formatting instructions if a tool is adopted -->
<!-- TODO: Add commit message conventions if desired -->
