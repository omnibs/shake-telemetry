# Phase 5: Shake API Wrapping Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement all Shake API wrappers so that `Development.Shake.Telemetry` can be used as a drop-in replacement for `Development.Shake`, recording telemetry for every build operation.

**Architecture:** Each wrapper retrieves `TelemetryState` from Shake's `shakeExtra` mechanism. Rule definers wrap the user's action body with `registerNode`/`setThreadNode`/`finishNode`. Dependency creators call `getThreadNode`/`recordEdge` before delegating. Parallelism wrappers propagate the parent thread's node ID to child threads. Re-exports pass through ~110 unchanged Shake exports. Entry points set up and tear down telemetry state.

**Tech Stack:** `shake` (the build system), `Development.Shake.Telemetry.State` (our telemetry state), `text` for labels, `containers` for maps.

**Key Shake API facts:**
- `Located` = `Partial` = `HasCallStack` (from GHC.Stack / `extra` package)
- `getShakeExtra :: Typeable a => Action (Maybe a)` retrieves from `shakeExtra`
- `getShakeExtraRules :: Typeable a => Rules (Maybe a)` retrieves from `shakeExtra` in Rules
- `addShakeExtra :: Typeable a => a -> HashMap TypeRep Dynamic -> HashMap TypeRep Dynamic`
- `shakeWithDatabase` is only in `Development.Shake.Database`, NOT in `Development.Shake`

---

### Task 1: Re-exports (`Wrap/Reexports.hs`)

**Files:**
- Modify: `src/Development/Shake/Telemetry/Wrap/Reexports.hs`

This is the longest file but purely mechanical. We import everything from `Development.Shake` and re-export only the things we do NOT wrap. The wrapped functions will come from other `Wrap.*` modules.

**Step 1: Implement Reexports.hs**

The module must re-export everything from `Development.Shake` EXCEPT:
- Entry points: `shake`, `shakeArgs`, `shakeArgsWith`, `shakeArgsOptionsWith` (wrapped in Entry.hs)
- Rule definers: `(%>)`, `(|%>)`, `(?>)`, `(&%>)`, `(&?>)`, `phony`, `(~>)`, `phonys`, `addOracle`, `addOracleCache`, `addOracleHash`, `batch` (wrapped in Rules.hs)
- Dependency creators: `need`, `needed`, `want`, `orderOnly`, `orderOnlyAction`, `askOracle`, `askOracles`, `askOracleWith`, `doesFileExist`, `doesDirectoryExist`, `getDirectoryContents`, `getDirectoryFiles`, `getDirectoryDirs`, `getEnv`, `getEnvWithDefault` (wrapped in Actions.hs)
- Parallelism: `parallel`, `forP`, `par` (wrapped in Parallel.hs)

Use an explicit export list with qualified import from `Development.Shake`. Re-export all types, data constructors, classes, and non-wrapped functions.

```haskell
module Development.Shake.Telemetry.Wrap.Reexports
  ( -- * Core types
    Rules
  , Action
  , ShakeOptions (..)
  , shakeOptions
  , Rebuild (..)
  , Lint (..)
  , Change (..)
  , Verbosity (..)
  , Progress (..)
  , Resource
  , ShakeException (..)
  , ShakeValue
  , RuleResult

  -- * Rule modifiers
  , action
  , alternatives
  , priority
  , versioned
  , withoutActions

  -- * File operations
  , copyFile'
  , copyFileChanged
  , readFile'
  , readFileLines
  , writeFile'
  , writeFileLines
  , writeFileChanged
  , removeFiles
  , removeFilesAfter
  , withTempFile
  , withTempDir
  , withTempFileWithin
  , withTempDirWithin
  , getDirectoryFilesIO

  -- * Tracking
  , trackRead
  , trackWrite
  , trackAllow
  , needHasChanged
  , resultHasChanged

  -- * Command execution
  , cmd
  , cmd_
  , command
  , command_
  , CmdOption (..)
  , CmdResult
  , CmdString
  , Stdout (..)
  , StdoutTrim (..)
  , Stderr (..)
  , Stdouterr (..)
  , Exit (..)
  , Process (..)
  , CmdTime (..)
  , CmdLine (..)
  , FSATrace (..)

  -- * Resources
  , newResource
  , newResourceIO
  , withResource
  , withResources
  , newThrottle
  , newThrottleIO
  , unsafeExtraThread

  -- * Caching
  , newCache
  , newCacheIO
  , historyDisable
  , produces
  , reschedule
  , deprioritize

  -- * Verbosity
  , getVerbosity
  , putVerbose
  , putInfo
  , putWarn
  , putError
  , withVerbosity
  , quietly
  , putLoud
  , putNormal
  , putQuiet

  -- * Progress
  , progressSimple
  , progressDisplay
  , progressTitlebar
  , progressProgram
  , getProgress

  -- * Targets
  , getTargets
  , addTarget
  , withTargetDocs
  , withoutTargets
  , addHelpSuffix

  -- * Options
  , getShakeOptions
  , getShakeOptionsRules
  , getHashedShakeVersion
  , getShakeExtra
  , getShakeExtraRules
  , addShakeExtra
  , shakeOptDescrs

  -- * Exception handling
  , actionOnException
  , actionFinally
  , actionBracket
  , actionCatch
  , actionRetry
  , runAfter

  -- * Misc
  , traced
  , unit
  , alwaysRerun

  -- * Environment (getEnvError only — getEnv/getEnvWithDefault wrapped in Actions)
  , getEnvError

  -- * FilePattern (re-exported from Development.Shake.FilePath)
  , FilePattern
  , (?==)
  , (<//>)
  , filePattern

  -- * Re-exported classes (from Development.Shake.Classes)
  , Typeable
  , Hashable
  , Binary
  , NFData

  -- * Re-exported from standard libraries
  , OptDescr (..)
  , ArgDescr (..)
  , ExitCode (..)
  , ProcessHandle
  ) where

import Development.Shake
```

Note: By importing the whole `Development.Shake` module but having an explicit export list, we only re-export what's listed. This avoids name clashes with our wrapped versions.

**Step 2: Verify it compiles**

Run: `devenv shell -- cabal build`
Expected: Compiles cleanly. Some exports may need adjustment if names don't match exactly.

**Step 3: Commit**

```
git add src/Development/Shake/Telemetry/Wrap/Reexports.hs
git commit -m "Implement Shake re-exports (unchanged API surface)"
```

---

### Task 2: Wrap rule definers (`Wrap/Rules.hs`)

**Files:**
- Modify: `src/Development/Shake/Telemetry/Wrap/Rules.hs`

**Step 1: Implement Rules.hs**

All rule definers follow the same pattern:
1. Get `TelemetryState` from `shakeExtra` (via `getShakeExtraRules` for Rules-level, or `getShakeExtra` for Action-level wrapping of the body).
2. Wrap the user's action body to call `registerNode`, `setThreadNode`, run the body, then `finishNode`.

Key types from Shake:
```
(%>)  :: Located => FilePattern -> (FilePath -> Action ()) -> Rules ()
(|%>) :: Located => [FilePattern] -> (FilePath -> Action ()) -> Rules ()
(?>)  :: Located => (FilePath -> Bool) -> (FilePath -> Action ()) -> Rules ()
(&%>) :: Located => [FilePattern] -> ([FilePath] -> Action ()) -> Rules ()
(&?>) :: Located => (FilePath -> Maybe [FilePath]) -> ([FilePath] -> Action ()) -> Rules ()
phony  :: Located => String -> Action () -> Rules ()
(~>)   :: Located => String -> Action () -> Rules ()
phonys :: Located => (String -> Maybe (Action ())) -> Rules ()
addOracle      :: (RuleResult q ~ a, ShakeValue q, ShakeValue a, Located) => (q -> Action a) -> Rules (q -> Action a)
addOracleCache :: (RuleResult q ~ a, ShakeValue q, ShakeValue a, Located) => (q -> Action a) -> Rules (q -> Action a)
addOracleHash  :: (RuleResult q ~ a, ShakeValue q, ShakeValue a, Located) => (q -> Action a) -> Rules (q -> Action a)
batch :: Int -> ((a -> Action ()) -> Rules ()) -> (a -> Action b) -> ([b] -> Action ()) -> Rules ()
```

For oracle wrappers, the returned query function doesn't need wrapping here -- oracle dependency recording happens in `askOracle` (Actions.hs). The `addOracle*` wrappers only need to wrap the oracle *handler* body to register the oracle node when it executes.

For `phonys`, the user provides `(String -> Maybe (Action ()))`. We wrap each returned `Action ()` similarly to `phony`.

For `batch`, the wrapping is more complex. The `batch` function's signature is:
```haskell
batch :: Int -> ((a -> Action ()) -> Rules ()) -> (a -> Action b) -> ([b] -> Action ()) -> Rules ()
```
We wrap the third argument (the per-item action) to register a BatchNode.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Development.Shake.Telemetry.Wrap.Rules
  ( (%>)
  , (|%>)
  , (?>)
  , (&%>)
  , (&?>)
  , phony
  , (~>)
  , phonys
  , addOracle
  , addOracleCache
  , addOracleHash
  , batch
  ) where

import Data.Text qualified as T
import Development.Shake (Action, FilePattern, Located, RuleResult, Rules, ShakeValue)
import Development.Shake qualified as Shake
import Development.Shake.Telemetry.Graph (NodeType (..))
import Development.Shake.Telemetry.State (TelemetryState, finishNode, registerNode, setThreadNode)

-- | Retrieve TelemetryState from shakeExtra inside an Action.
getTelemetryState :: Action TelemetryState
getTelemetryState = do
  mstate <- Shake.getShakeExtra
  case mstate of
    Just s -> pure s
    Nothing -> error "shake-telemetry: TelemetryState not found in shakeExtra"

-- | Wrap a (FilePath -> Action ()) body with telemetry.
wrapFileAction :: NodeType -> (FilePath -> Action ()) -> FilePath -> Action ()
wrapFileAction ntype act out = do
  state <- getTelemetryState
  nid <- Shake.liftIO $ registerNode state (T.pack out) ntype
  Shake.liftIO $ setThreadNode state nid
  act out
  Shake.liftIO $ finishNode state nid

-- | Wrap a ([FilePath] -> Action ()) body with telemetry (for batch/multi rules).
wrapBatchAction :: NodeType -> ([FilePath] -> Action ()) -> [FilePath] -> Action ()
wrapBatchAction ntype act outs = do
  state <- getTelemetryState
  let label = T.intercalate ", " (map T.pack outs)
  nid <- Shake.liftIO $ registerNode state label ntype
  Shake.liftIO $ setThreadNode state nid
  act outs
  Shake.liftIO $ finishNode state nid

-- | Wrap a plain Action () with telemetry (for phony).
wrapAction :: NodeType -> String -> Action () -> Action ()
wrapAction ntype name act = do
  state <- getTelemetryState
  nid <- Shake.liftIO $ registerNode state (T.pack name) ntype
  Shake.liftIO $ setThreadNode state nid
  act
  Shake.liftIO $ finishNode state nid

-- File rules

(%>) :: Located => FilePattern -> (FilePath -> Action ()) -> Rules ()
pat %> act = pat Shake.%> wrapFileAction FileNode act

(|%>) :: Located => [FilePattern] -> (FilePath -> Action ()) -> Rules ()
pats |%> act = pats Shake.|%> wrapFileAction FileNode act

(?>) :: Located => (FilePath -> Bool) -> (FilePath -> Action ()) -> Rules ()
test ?> act = test Shake.?> wrapFileAction FileNode act

-- Batch file rules

(&%>) :: Located => [FilePattern] -> ([FilePath] -> Action ()) -> Rules ()
pats &%> act = pats Shake.&%> wrapBatchAction BatchNode act

(&?>) :: Located => (FilePath -> Maybe [FilePath]) -> ([FilePath] -> Action ()) -> Rules ()
test &?> act = test Shake.&?> wrapBatchAction BatchNode act

-- Phony rules

phony :: Located => String -> Action () -> Rules ()
phony name act = Shake.phony name (wrapAction PhonyNode name act)

(~>) :: Located => String -> Action () -> Rules ()
name ~> act = name Shake.~> wrapAction PhonyNode name act

phonys :: Located => (String -> Maybe (Action ())) -> Rules ()
phonys test = Shake.phonys $ \name ->
  case test name of
    Nothing -> Nothing
    Just act -> Just (wrapAction PhonyNode name act)

-- Oracle rules

addOracle :: (RuleResult q ~ a, ShakeValue q, ShakeValue a, Located) => (q -> Action a) -> Rules (q -> Action a)
addOracle handler = Shake.addOracle $ \q -> do
  state <- getTelemetryState
  let label = T.pack (show q)
  nid <- Shake.liftIO $ registerNode state label OracleNode
  Shake.liftIO $ setThreadNode state nid
  result <- handler q
  Shake.liftIO $ finishNode state nid
  pure result

addOracleCache :: (RuleResult q ~ a, ShakeValue q, ShakeValue a, Located) => (q -> Action a) -> Rules (q -> Action a)
addOracleCache handler = Shake.addOracleCache $ \q -> do
  state <- getTelemetryState
  let label = T.pack (show q)
  nid <- Shake.liftIO $ registerNode state label OracleNode
  Shake.liftIO $ setThreadNode state nid
  result <- handler q
  Shake.liftIO $ finishNode state nid
  pure result

addOracleHash :: (RuleResult q ~ a, ShakeValue q, ShakeValue a, Located) => (q -> Action a) -> Rules (q -> Action a)
addOracleHash handler = Shake.addOracleHash $ \q -> do
  state <- getTelemetryState
  let label = T.pack (show q)
  nid <- Shake.liftIO $ registerNode state label OracleNode
  Shake.liftIO $ setThreadNode state nid
  result <- handler q
  Shake.liftIO $ finishNode state nid
  pure result

-- Batch

batch :: Int -> ((a -> Action ()) -> Rules ()) -> (a -> Action b) -> ([b] -> Action ()) -> Rules ()
batch maxBatch rules act collect = Shake.batch maxBatch rules wrappedAct collect
  where
    wrappedAct a = do
      state <- getTelemetryState
      nid <- Shake.liftIO $ registerNode state "batch-item" BatchNode
      Shake.liftIO $ setThreadNode state nid
      result <- act a
      Shake.liftIO $ finishNode state nid
      pure result
```

**Step 2: Verify it compiles**

Run: `devenv shell -- cabal build`
Expected: Compiles. May need adjustments for `Show` constraint on oracle `q` type (already guaranteed by `ShakeValue` which requires `Show`).

**Step 3: Commit**

```
git add src/Development/Shake/Telemetry/Wrap/Rules.hs
git commit -m "Implement telemetry wrappers for rule definers"
```

---

### Task 3: Wrap dependency creators (`Wrap/Actions.hs`)

**Files:**
- Modify: `src/Development/Shake/Telemetry/Wrap/Actions.hs`

**Step 1: Implement Actions.hs**

All dependency creators follow the same pattern:
1. Get `TelemetryState` from `shakeExtra`.
2. Get the current thread's node ID via `getThreadNode`.
3. Record an edge from the current node to the dependency target.
4. Delegate to the real Shake function.

Key types:
```
need              :: Partial => [FilePath] -> Action ()
needed            :: Partial => [FilePath] -> Action ()
want              :: Partial => [FilePath] -> Rules ()
orderOnly         :: [FilePath] -> Action ()
orderOnlyAction   :: Action a -> Action a
askOracle         :: (RuleResult q ~ a, ShakeValue q, ShakeValue a) => q -> Action a
askOracles        :: (RuleResult q ~ a, ShakeValue q, ShakeValue a) => [q] -> Action [a]
askOracleWith     :: (RuleResult q ~ a, ShakeValue q, ShakeValue a) => q -> a -> Action a
doesFileExist     :: FilePath -> Action Bool
doesDirectoryExist :: FilePath -> Action Bool
getDirectoryContents :: FilePath -> Action [FilePath]
getDirectoryFiles :: FilePath -> [FilePattern] -> Action [FilePath]
getDirectoryDirs  :: FilePath -> Action [FilePath]
getEnv            :: String -> Action (Maybe String)
getEnvWithDefault :: String -> String -> Action String
```

`want` runs in `Rules`, not `Action`, so we can't use `getThreadNode` there. `want` is essentially `action . need`. We wrap it to record edges from a synthetic "want" action node. However, to keep it simple, we can wrap `want` by recording each wanted file as a target node and delegating. Since there's no "current thread" context in Rules, we'll just delegate without edge recording for `want` — the edges will be captured when the rules actually execute and call `need`.

Actually, the simplest correct approach: `want` just calls `Shake.want` and the targets get recorded when the actual rules fire. No edge recording needed in `want` itself.

`orderOnlyAction` wraps an `Action a` — it doesn't create dependencies to specific files, so we just delegate.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Development.Shake.Telemetry.Wrap.Actions
  ( need
  , needed
  , want
  , orderOnly
  , orderOnlyAction
  , askOracle
  , askOracles
  , askOracleWith
  , doesFileExist
  , doesDirectoryExist
  , getDirectoryContents
  , getDirectoryFiles
  , getDirectoryDirs
  , getEnv
  , getEnvWithDefault
  ) where

import Data.Text qualified as T
import Development.Shake (Action, FilePattern, Partial, RuleResult, Rules, ShakeValue)
import Development.Shake qualified as Shake
import Development.Shake.Telemetry.Graph (NodeType (..))
import Development.Shake.Telemetry.State (TelemetryState, getThreadNode, recordEdge)

-- | Retrieve TelemetryState from shakeExtra inside an Action.
getTelemetryState :: Action TelemetryState
getTelemetryState = do
  mstate <- Shake.getShakeExtra
  case mstate of
    Just s -> pure s
    Nothing -> error "shake-telemetry: TelemetryState not found in shakeExtra"

-- | Record edges from the current thread's node to each target.
recordEdges :: TelemetryState -> NodeType -> [String] -> Action ()
recordEdges state ntype targets = do
  currentNode <- Shake.liftIO $ getThreadNode state
  mapM_ (\t -> Shake.liftIO $ recordEdge state currentNode (T.pack t) ntype) targets

-- Core file dependencies

need :: Partial => [FilePath] -> Action ()
need files = do
  state <- getTelemetryState
  recordEdges state FileNode files
  Shake.need files

needed :: Partial => [FilePath] -> Action ()
needed files = do
  state <- getTelemetryState
  recordEdges state FileNode files
  Shake.needed files

want :: Partial => [FilePath] -> Rules ()
want = Shake.want

orderOnly :: [FilePath] -> Action ()
orderOnly files = do
  state <- getTelemetryState
  recordEdges state FileNode files
  Shake.orderOnly files

orderOnlyAction :: Action a -> Action a
orderOnlyAction = Shake.orderOnlyAction

-- Oracle dependencies

askOracle :: (RuleResult q ~ a, ShakeValue q, ShakeValue a) => q -> Action a
askOracle q = do
  state <- getTelemetryState
  currentNode <- Shake.liftIO $ getThreadNode state
  Shake.liftIO $ recordEdge state currentNode (T.pack (show q)) OracleNode
  Shake.askOracle q

askOracles :: (RuleResult q ~ a, ShakeValue q, ShakeValue a) => [q] -> Action [a]
askOracles qs = do
  state <- getTelemetryState
  currentNode <- Shake.liftIO $ getThreadNode state
  mapM_ (\q -> Shake.liftIO $ recordEdge state currentNode (T.pack (show q)) OracleNode) qs
  Shake.askOracles qs

askOracleWith :: (RuleResult q ~ a, ShakeValue q, ShakeValue a) => q -> a -> Action a
askOracleWith q a = do
  state <- getTelemetryState
  currentNode <- Shake.liftIO $ getThreadNode state
  Shake.liftIO $ recordEdge state currentNode (T.pack (show q)) OracleNode
  Shake.askOracleWith q a

-- Implicit file dependencies

doesFileExist :: FilePath -> Action Bool
doesFileExist path = do
  state <- getTelemetryState
  currentNode <- Shake.liftIO $ getThreadNode state
  Shake.liftIO $ recordEdge state currentNode (T.pack path) DirectoryNode
  Shake.doesFileExist path

doesDirectoryExist :: FilePath -> Action Bool
doesDirectoryExist path = do
  state <- getTelemetryState
  currentNode <- Shake.liftIO $ getThreadNode state
  Shake.liftIO $ recordEdge state currentNode (T.pack path) DirectoryNode
  Shake.doesDirectoryExist path

getDirectoryContents :: FilePath -> Action [FilePath]
getDirectoryContents path = do
  state <- getTelemetryState
  currentNode <- Shake.liftIO $ getThreadNode state
  Shake.liftIO $ recordEdge state currentNode (T.pack path) DirectoryNode
  Shake.getDirectoryContents path

getDirectoryFiles :: FilePath -> [FilePattern] -> Action [FilePath]
getDirectoryFiles dir pats = do
  state <- getTelemetryState
  currentNode <- Shake.liftIO $ getThreadNode state
  Shake.liftIO $ recordEdge state currentNode (T.pack dir) DirectoryNode
  Shake.getDirectoryFiles dir pats

getDirectoryDirs :: FilePath -> Action [FilePath]
getDirectoryDirs path = do
  state <- getTelemetryState
  currentNode <- Shake.liftIO $ getThreadNode state
  Shake.liftIO $ recordEdge state currentNode (T.pack path) DirectoryNode
  Shake.getDirectoryDirs path

-- Environment dependencies

getEnv :: String -> Action (Maybe String)
getEnv var = do
  state <- getTelemetryState
  currentNode <- Shake.liftIO $ getThreadNode state
  Shake.liftIO $ recordEdge state currentNode (T.pack var) EnvNode
  Shake.getEnv var

getEnvWithDefault :: String -> String -> Action String
getEnvWithDefault var def = do
  state <- getTelemetryState
  currentNode <- Shake.liftIO $ getThreadNode state
  Shake.liftIO $ recordEdge state currentNode (T.pack var) EnvNode
  Shake.getEnvWithDefault var def
```

**Step 2: Verify it compiles**

Run: `devenv shell -- cabal build`

**Step 3: Commit**

```
git add src/Development/Shake/Telemetry/Wrap/Actions.hs
git commit -m "Implement telemetry wrappers for dependency creators"
```

---

### Task 4: Wrap parallelism (`Wrap/Parallel.hs`)

**Files:**
- Modify: `src/Development/Shake/Telemetry/Wrap/Parallel.hs`

**Step 1: Implement Parallel.hs**

Key types:
```
parallel :: [Action a] -> Action [a]
forP     :: [a] -> (a -> Action b) -> Action [b]
par      :: Action a -> Action b -> Action (a, b)
```

Each wrapper captures the parent thread's node ID and injects `setThreadNode` at the start of each child action.

```haskell
module Development.Shake.Telemetry.Wrap.Parallel
  ( parallel
  , forP
  , par
  ) where

import Development.Shake (Action)
import Development.Shake qualified as Shake
import Development.Shake.Telemetry.State (TelemetryState, getThreadNode, setThreadNode)

-- | Retrieve TelemetryState from shakeExtra inside an Action.
getTelemetryState :: Action TelemetryState
getTelemetryState = do
  mstate <- Shake.getShakeExtra
  case mstate of
    Just s -> pure s
    Nothing -> error "shake-telemetry: TelemetryState not found in shakeExtra"

parallel :: [Action a] -> Action [a]
parallel actions = do
  state <- getTelemetryState
  parentNode <- Shake.liftIO $ getThreadNode state
  Shake.parallel
    [ do Shake.liftIO $ setThreadNode state parentNode
         a
    | a <- actions
    ]

forP :: [a] -> (a -> Action b) -> Action [b]
forP xs f = do
  state <- getTelemetryState
  parentNode <- Shake.liftIO $ getThreadNode state
  Shake.forP xs $ \x -> do
    Shake.liftIO $ setThreadNode state parentNode
    f x

par :: Action a -> Action b -> Action (a, b)
par a b = do
  state <- getTelemetryState
  parentNode <- Shake.liftIO $ getThreadNode state
  Shake.par
    (Shake.liftIO (setThreadNode state parentNode) >> a)
    (Shake.liftIO (setThreadNode state parentNode) >> b)
```

**Step 2: Verify it compiles**

Run: `devenv shell -- cabal build`

**Step 3: Commit**

```
git add src/Development/Shake/Telemetry/Wrap/Parallel.hs
git commit -m "Implement telemetry wrappers for parallelism"
```

---

### Task 5: Wrap entry points (`Wrap/Entry.hs`)

**Files:**
- Modify: `src/Development/Shake/Telemetry/Wrap/Entry.hs`

**Step 1: Implement Entry.hs**

Entry points create `TelemetryState`, inject into `shakeExtra`, delegate, then post-process. Output paths are derived from `shakeFiles` in `ShakeOptions` (default `.shake`), writing to `<shakeFiles>/telemetry/build-graph.json` and `<shakeFiles>/telemetry/critical-path.mmd`.

Key types:
```
shake                :: ShakeOptions -> Rules () -> IO ()
shakeArgs            :: ShakeOptions -> Rules () -> IO ()
shakeArgsWith        :: ShakeOptions -> [OptDescr (Either String a)] -> ([a] -> [String] -> IO (Maybe (Rules ()))) -> IO ()
shakeArgsOptionsWith :: ShakeOptions -> [OptDescr (Either String a)] -> (ShakeOptions -> [a] -> [String] -> IO (Maybe (ShakeOptions, Rules ()))) -> IO ()
```

Note: `shakeWithDatabase` is from `Development.Shake.Database` (not re-exported by `Development.Shake`), so we do NOT wrap it in this phase.

For `shakeArgsWith` and `shakeArgsOptionsWith`, the user provides callbacks that return `Maybe (Rules ())` or `Maybe (ShakeOptions, Rules ())`. We need to inject telemetry into the `ShakeOptions` these functions use. For `shakeArgsWith`, the options are fixed upfront. For `shakeArgsOptionsWith`, the user's callback returns modified options — we need to wrap that callback.

```haskell
module Development.Shake.Telemetry.Wrap.Entry
  ( shake
  , shakeArgs
  , shakeArgsWith
  , shakeArgsOptionsWith
  ) where

import Development.Shake (Rules, ShakeOptions (..))
import Development.Shake qualified as Shake
import System.Console.GetOpt (OptDescr)

import Development.Shake.Telemetry.CriticalPath (computeCriticalPath)
import Development.Shake.Telemetry.Json (writeJsonGraph)
import Development.Shake.Telemetry.Mermaid (writeMermaidChart)
import Development.Shake.Telemetry.State (TelemetryState, freezeGraph, newTelemetryState)

-- | Inject TelemetryState into ShakeOptions.
injectTelemetry :: TelemetryState -> ShakeOptions -> ShakeOptions
injectTelemetry state opts =
  opts {shakeExtra = Shake.addShakeExtra state (shakeExtra opts)}

-- | Output paths derived from shakeFiles.
telemetryJsonPath :: ShakeOptions -> FilePath
telemetryJsonPath opts = shakeFiles opts ++ "/telemetry/build-graph.json"

telemetryMermaidPath :: ShakeOptions -> FilePath
telemetryMermaidPath opts = shakeFiles opts ++ "/telemetry/critical-path.mmd"

-- | Post-build: freeze graph, compute critical path, write outputs.
finalizeTelemetry :: ShakeOptions -> TelemetryState -> IO ()
finalizeTelemetry opts state = do
  graph <- freezeGraph state
  let analyzed = computeCriticalPath graph
  writeJsonGraph (telemetryJsonPath opts) analyzed
  writeMermaidChart (telemetryMermaidPath opts) analyzed

shake :: ShakeOptions -> Rules () -> IO ()
shake opts rules = do
  state <- newTelemetryState
  let opts' = injectTelemetry state opts
  Shake.shake opts' rules
  finalizeTelemetry opts' state

shakeArgs :: ShakeOptions -> Rules () -> IO ()
shakeArgs opts rules = do
  state <- newTelemetryState
  let opts' = injectTelemetry state opts
  Shake.shakeArgs opts' rules
  finalizeTelemetry opts' state

shakeArgsWith :: ShakeOptions -> [OptDescr (Either String a)] -> ([a] -> [String] -> IO (Maybe (Rules ()))) -> IO ()
shakeArgsWith opts flags act = do
  state <- newTelemetryState
  let opts' = injectTelemetry state opts
  Shake.shakeArgsWith opts' flags act
  finalizeTelemetry opts' state

shakeArgsOptionsWith :: ShakeOptions -> [OptDescr (Either String a)] -> (ShakeOptions -> [a] -> [String] -> IO (Maybe (ShakeOptions, Rules ()))) -> IO ()
shakeArgsOptionsWith opts flags act = do
  state <- newTelemetryState
  let opts' = injectTelemetry state opts
  Shake.shakeArgsOptionsWith opts' flags $ \o a s -> do
    result <- act o a s
    case result of
      Nothing -> pure Nothing
      Just (userOpts, rules) -> pure $ Just (injectTelemetry state userOpts, rules)
  finalizeTelemetry opts' state
```

**Step 2: Verify it compiles**

Run: `devenv shell -- cabal build`

**Step 3: Commit**

```
git add src/Development/Shake/Telemetry/Wrap/Entry.hs
git commit -m "Implement telemetry entry point wrappers"
```

---

### Task 6: Top-level module (`Telemetry.hs`)

**Files:**
- Modify: `src/Development/Shake/Telemetry.hs`

**Step 1: Implement Telemetry.hs**

This module re-exports everything, combining wrapped functions from `Wrap.*` modules with unchanged re-exports from `Wrap.Reexports`. The export list should be explicit to avoid conflicts.

```haskell
module Development.Shake.Telemetry
  ( -- Re-export everything from Reexports (unchanged Shake API)
    module Development.Shake.Telemetry.Wrap.Reexports

    -- Wrapped entry points
  , shake
  , shakeArgs
  , shakeArgsWith
  , shakeArgsOptionsWith

    -- Wrapped rule definers
  , (%>)
  , (|%>)
  , (?>)
  , (&%>)
  , (&?>)
  , phony
  , (~>)
  , phonys
  , addOracle
  , addOracleCache
  , addOracleHash
  , batch

    -- Wrapped dependency creators
  , need
  , needed
  , want
  , orderOnly
  , orderOnlyAction
  , askOracle
  , askOracles
  , askOracleWith
  , doesFileExist
  , doesDirectoryExist
  , getDirectoryContents
  , getDirectoryFiles
  , getDirectoryDirs
  , getEnv
  , getEnvWithDefault

    -- Wrapped parallelism
  , parallel
  , forP
  , par
  ) where

import Development.Shake.Telemetry.Wrap.Actions
import Development.Shake.Telemetry.Wrap.Entry
import Development.Shake.Telemetry.Wrap.Parallel
import Development.Shake.Telemetry.Wrap.Reexports
import Development.Shake.Telemetry.Wrap.Rules
```

**Step 2: Verify it compiles**

Run: `devenv shell -- cabal build`
Expected: Compiles cleanly.

**Step 3: Commit**

```
git add src/Development/Shake/Telemetry.hs
git commit -m "Wire up top-level Telemetry module with all exports"
```

---

### Task 7: Write tests for rule wrappers and dependency creators

**Files:**
- Create: `test/Test/Telemetry/WrapTest.hs`
- Modify: `test/Main.hs` (add import + test group)
- Modify: `shake-telemetry.cabal` (add test module)

These tests run small in-process Shake builds via `Development.Shake.Telemetry` and verify the telemetry graph is correct.

**Step 1: Write the test file**

The tests create a `TelemetryState`, inject it into `ShakeOptions`, run a small Shake build using our wrapped functions, then freeze the graph and assert on node/edge counts and types.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Telemetry.WrapTest (wrapTests) where

import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Data.Vector qualified as Vector
import Test.Tasty
import Test.Tasty.HUnit

import Development.Shake (ShakeOptions (..), shakeOptions)
import Development.Shake qualified as Shake
import Development.Shake.Telemetry.Graph
import Development.Shake.Telemetry.State
import Development.Shake.Telemetry.Wrap.Actions qualified as WA
import Development.Shake.Telemetry.Wrap.Rules qualified as WR

-- Helper: run a small Shake build with telemetry and return the frozen graph.
withTelemetry :: (TelemetryState -> Shake.Rules ()) -> IO BuildGraph
withTelemetry rules = do
  state <- newTelemetryState
  let opts = shakeOptions
        { shakeExtra = Shake.addShakeExtra state (shakeExtra shakeOptions)
        , shakeFiles = "/dev/null"
        , shakeVerbosity = Shake.Silent
        }
  Shake.shake opts (rules state)
  freezeGraph state

-- Count nodes of a given type.
countNodesOfType :: NodeType -> BuildGraph -> Int
countNodesOfType nt graph =
  length [n | n <- IntMap.elems (graphNodes graph), nodeType n == nt]

-- Count edges.
countEdges :: BuildGraph -> Int
countEdges graph = Vector.length (graphEdges graph)

wrapTests :: TestTree
wrapTests =
  testGroup
    "Wrap"
    [ testCase "file rule (%>) records FileNode" $ do
        graph <- withTelemetry $ \_state -> do
          "*.txt" WR.%> \out -> do
            Shake.writeFile' out "hello"
          Shake.want ["test.txt"]
        assertBool "has FileNode" (countNodesOfType FileNode graph > 0)

    , testCase "phony rule records PhonyNode" $ do
        graph <- withTelemetry $ \_state -> do
          WR.phony "clean" $ pure ()
          Shake.action $ Shake.need ["clean"]
        assertBool "has PhonyNode" (countNodesOfType PhonyNode graph > 0)

    , testCase "need records edges" $ do
        graph <- withTelemetry $ \_state -> do
          "*.a" WR.%> \out -> do
            Shake.writeFile' out "a"
          "*.b" WR.%> \out -> do
            WA.need [out -<.> "a"]
            Shake.writeFile' out "b"
          Shake.want ["test.b"]
        assertBool "has edges" (countEdges graph > 0)
    ]
```

Note: These tests may need adjustment once we see how they interact with Shake's actual execution. The test sandbox needs a temp directory for `shakeFiles`. We'll refine as we go.

**Step 2: Wire into Main.hs and cabal**

Add `import Test.Telemetry.WrapTest (wrapTests)` and `wrapTests` to the test group. Add `Test.Telemetry.WrapTest` to `other-modules` in cabal. Add `shake` and `filepath` to test `build-depends`.

**Step 3: Run tests**

Run: `devenv shell -- cabal test`
Expected: Tests pass (or we fix issues iteratively).

**Step 4: Commit**

```
git add test/Test/Telemetry/WrapTest.hs test/Main.hs shake-telemetry.cabal
git commit -m "Add integration tests for Shake API wrappers"
```

---

### Task 8: Final verification and mark Phase 5 done

**Step 1: Run full test suite**

Run: `devenv shell -- cabal test`
Expected: All tests pass.

**Step 2: Mark phase done**

Edit `ROADMAP.md`: change `## Phase 5: Shake API wrapping` to `## Phase 5: Shake API wrapping [DONE]`.

**Step 3: Commit**

```
git add ROADMAP.md
git commit -m "Mark Phase 5 as done"
```
