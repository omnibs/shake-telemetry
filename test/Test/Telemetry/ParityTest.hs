{-# LANGUAGE OverloadedStrings #-}

-- | Verifies that Development.Shake.Telemetry exports the same API
-- surface as Development.Shake by running GHCi's :browse on both
-- modules and comparing the exported names. If Shake adds new exports,
-- this test will fail until we add them to our module.
module Test.Telemetry.ParityTest (parityTests) where

import Data.Char (isSpace)
import Data.List (sort)
import Data.Set (Set)
import Data.Set qualified as Set
import System.Process (readProcess)
import Test.Tasty
import Test.Tasty.HUnit

-- | Extract exported names from :browse output.
--
-- GHCi :browse output has lines like:
--   Development.Shake.need :: [FilePath] -> Action ()
--   shake-0.19.9:Development.Shake.Internal.Rules.File.need :: ...
--   type shake-0.19.9:Development.Shake.Internal.Options.Change :: *
--   data shake-0.19.9:Development.Shake.Internal.Options.Change
--   class Development.Shake.Command.CmdResult a where
--   pattern Development.Shake.Chatty
--
-- Multi-line signatures have continuation lines starting with whitespace.
-- We extract the unqualified name (after the last dot) from definition lines.
extractNames :: String -> Set String
extractNames output = Set.fromList $ concatMap parseLine (lines output)
  where
    parseLine line
      | null line = []
      -- Indented lines: class methods look like "  Module.Name :: ..."
      -- Other continuation lines (type signatures, constraints) don't
      -- start with a qualified identifier containing dots.
      | isSpace (head' line)
      , let trimmed = dropWhile isSpace line
      , not (null trimmed)
      , not (isSpace (head' trimmed))
      , let word = takeWhile (\c -> c /= ' ' && c /= '\n') trimmed
      , '.' `elem` word
      , " :: " `isIn` trimmed = extractFrom trimmed
      | isSpace (head' line) = []
      -- "type role ..." -> skip (role annotations)
      | "type role " `startsWith` line = []
      -- "type family Module.Name ..." -> extract name
      | "type family " `startsWith` line =
          extractFrom (drop 12 line)
      -- "type Module.Name :: *" -> kind signature, skip (data/type decl covers it)
      | "type " `startsWith` line, " :: " `isIn` dropQual (drop 5 line) =
          []
      -- "type Module.Name = ..." -> type alias
      | "type " `startsWith` line =
          extractFrom (drop 5 line)
      -- "data Module.Name" -> data type
      | "data " `startsWith` line =
          extractFrom (drop 5 line)
      -- "class Module.Name ..." -> class
      | "class " `startsWith` line =
          extractFrom (drop 6 line)
      -- "pattern Module.Name" -> pattern synonym
      | "pattern " `startsWith` line =
          extractFrom (drop 8 line)
      -- "Module.Name :: ..." -> function/value
      | otherwise =
          extractFrom line

    extractFrom s =
      let qualified = takeWhile (\c -> c /= ' ' && c /= '\n') s
          name = unqualify qualified
      in if null name then [] else [name]

    -- Strip package prefix like "shake-0.19.9:" and module path
    -- "shake-0.19.9:Development.Shake.Internal.Options.Change" -> "Change"
    -- "Development.Shake.need" -> "need"
    -- "Development.Shake.(%>)" -> "(%>)"
    unqualify s =
      let -- Strip package prefix (e.g., "shake-0.19.9:")
          noPkg = case break (== ':') s of
            (_, ':':rest) -> rest
            _ -> s
      in lastDotComponent noPkg

    -- Get the part after the last dot, handling operators in parens
    -- "Foo.Bar.baz" -> "baz"
    -- "Foo.Bar.(%>)" -> "(%>)"
    lastDotComponent [] = []
    lastDotComponent s = go s
      where
        go xs = case break (== '.') xs of
          (before, []) -> before  -- no more dots
          (_, '.':'(':rest) -> '(' : rest  -- operator like .(%>)
          (_, '.':rest)
            | null rest -> xs
            | otherwise -> go rest
          _ -> xs

    startsWith prefix str = take (length prefix) str == prefix
    isIn needle haystack = any (startsWith needle) (tails' haystack)
    tails' [] = [[]]
    tails' xs@(_:rest) = xs : tails' rest
    head' (c:_) = c
    head' [] = ' '
    dropQual = dropWhile (/= ' ')

-- | Run GHCi :browse on a module and return its output.
browseModule :: String -> IO String
browseModule modName =
  readProcess "cabal" ["exec", "ghc", "--", "-e", ":browse " ++ modName] ""

-- | Names that Shake exports but we intentionally don't re-export.
-- Each entry should have a comment explaining why.
knownMissing :: Set String
knownMissing = Set.fromList
  [ -- Deprecated verbosity pattern synonyms. Our module re-exports the
    -- Verbosity type with all constructors. These deprecated aliases
    -- (Chatty=Verbose, Loud=Verbose, Normal=Info, Quiet=Warn) are pattern
    -- synonyms that we re-export via the Reexports module.
    -- If they show up as missing, it's a parsing issue, not a real gap.
  ]

-- | Names that :browse shows in our module but not in Shake, which we
-- intentionally export (e.g. telemetry-specific additions).
-- Note: :browse shows class names when only a method is exported, so
-- class names shared by both modules appear on both sides and cancel out.
knownExtras :: Set String
knownExtras = Set.fromList
  [ -- Our wrapped entry points / dependency creators / rule definers
    -- that replace Shake's versions show up with our module prefix in
    -- :browse but resolve to the same unqualified names, so they don't
    -- appear as extras. Only truly new names need listing here.
  ]

parityTests :: TestTree
parityTests =
  testGroup
    "API parity"
    [ testCase "Development.Shake.Telemetry has all Development.Shake exports" $ do
        shakeOutput <- browseModule "Development.Shake"
        telemetryOutput <- browseModule "Development.Shake.Telemetry"

        let shakeNames = extractNames shakeOutput
            telemetryNames = extractNames telemetryOutput
            missing = (shakeNames `Set.difference` telemetryNames) `Set.difference` knownMissing
            extra   = (telemetryNames `Set.difference` shakeNames) `Set.difference` knownExtras

        let errors = concat
              [ if Set.null missing then []
                else [ "Missing from Development.Shake.Telemetry:\n"
                     ++ unlines (map ("  - " ++) (sort (Set.toList missing)))
                     ]
              , if Set.null extra then []
                else [ "Extra in Development.Shake.Telemetry (not in Development.Shake):\n"
                     ++ unlines (map ("  + " ++) (sort (Set.toList extra)))
                     ]
              ]

        if null errors
          then pure ()
          else assertFailure $ concat errors
            ++ "Shake exports " ++ show (Set.size shakeNames) ++ " names, "
            ++ "Telemetry exports " ++ show (Set.size telemetryNames) ++ " names"
    ]
