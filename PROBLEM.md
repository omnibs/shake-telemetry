# Problem statement

I have a Haskell Shake Build System that takes too long to run.

It has thousands of lines of code, and is very hard to analyze.

It uses oracles, batching, all sorts of fancy stuff.

I want to make it faster, but I don't how to be effective at it.

I think I need to be able to visualize the critical path of the build, but Shake doesn't make it easy to do that.

## Limitations

### Shake itself

Shake has a report it produces from its builds, but the report only contains rule names and execution times -- it doesn't list the dependencies between the rules it executed.

I need to understand the dependencies so it's possible to calculate the critical path of the build.

### Interventions

I don't want to fork Shake, and I'm skeptical the author would accept a PR that adds the necessary information to the report, and I don't want to maintain a fork of Shake.

I think the best course of action is to build a wrapper library that exposes the same API as Shake, but also logs the dependencies between rules as they are executed.

## Goals

### Wrapper library

The wrapper library wraps the full `Development.Shake` API as a drop-in replacement. Users switch by changing their import from `Development.Shake` to the telemetry module -- no other code changes required.

All observable dependency mechanisms should be tracked: `need` calls, oracle queries, and any other source of dependencies. Oracle queries and batched rules should be represented as distinct node types in the graph (e.g., `type: "file"`, `type: "oracle"`, `type: "batch"`) so they can be visually and programmatically distinguished.

### Build outputs

Every build produces a graph file in JSON format that lists the rules executed, their dependencies, and their execution times.

Every build also produces a Mermaid chart of the critical path through the build graph.

### Critical path analysis

The project includes critical path computation: given the dependency graph with timing data, calculate and annotate the critical path. This analysis is part of the library output, not a separate tool.

### Long-term goal

A CLI graph explorer that can read the JSON graph file and allow me to explore it interactively with full information. This is out of scope for now.

### Normative

The wrapper library should be extensively tested.

## Constraints

### Performance

The telemetry overhead should be acceptable -- under 5% of total build time. Shake runs rules in parallel, so the logging mechanism must be thread-safe and low-contention (e.g., using STM, MVar, or atomic operations).

### Scale

A typical build produces 500-5,000 rules and dependency edges. The JSON format doesn't need to support streaming, but should handle this scale comfortably.

### Tooling

- GHC 9.8.x
- Nix + Cabal
- Target: a Haskell library (cabal package)
