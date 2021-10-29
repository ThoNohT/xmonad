module Keybinds (KeyMapCategory (..), KeyMapKey (..), unwrapCategories, categoryTextFormat) where

import Prelude hiding (tail)

import Data.Bifunctor (bimap)
import XMonad (X)
import Data.List (intercalate)

data KeyMapCategory = MkCat {name :: String, mappings :: [KeyMapKey]}

unwrapCategories :: [KeyMapCategory] -> [(String, X ())]
unwrapCategories = concatMap unwrapCategory
  where
    unwrapCategory MkCat {mappings = m} = concatMap unwrapKey m

data KeyMapKey
  = MkKey {binding :: String, action :: X (), keyDescription :: String}
  | forall a b.
    Show b =>
    MkRangeKey
      { keyRange :: [String], -- Needs to be finite, or things will hang.
        targetRange :: [a],
        showRange :: [b],
        keyToBinding :: String -> String,
        showToText :: String -> String,
        targetToAction :: a -> X ()
      }

unwrapKey :: KeyMapKey -> [(String, X ())]
unwrapKey MkKey {binding = b, action = a} = [(b, a)]
unwrapKey MkRangeKey {keyToBinding = ktb, targetToAction = tta, keyRange = kr, targetRange = tr} =
  zipWith (curry (bimap ktb tta)) kr tr

bindingWidth :: KeyMapKey -> Int
bindingWidth MkKey {binding = b} = length b
bindingWidth MkRangeKey {keyToBinding = ktb, keyRange = kr} = foldl max 0 $ map (ktb >> length) kr

categoryTextFormat :: [KeyMapCategory] -> String
categoryTextFormat = intercalate "\n\n" . map formatCategory
  where
    formatCategory MkCat {mappings = m, name = n} = intercalate "\n" $ n : map (formatBinding width) m
      where
        width = foldl max 0 $ map bindingWidth m

    padRight width str = if length str < width then padRight width (str ++ " ") else str

    unquote :: String -> String
    unquote ('"' : tail) =
        case reverse tail of
            ('"' : tail') -> reverse tail'
            _ -> '"' : tail
    unquote other = other

    formatBinding width MkKey {binding = b, keyDescription = d} = padRight width b ++ " : " ++ d
    formatBinding width MkRangeKey {keyToBinding = ktb, showToText = stt, keyRange = kr, showRange = sr} =
      let sr' = take (length kr) sr
       in unwords
            [ padRight width (ktb "K"),
              ": K<-[" ++ intercalate "," kr ++ "], " ++ stt ("[" ++ intercalate "," (map (unquote . show) sr') ++ "]")
            ]
