module Main (main) where

import qualified Lib
import qualified System.IO.Silently as Silently
import qualified Test.Framework as Test
import qualified Test.Framework.Providers.HUnit as Test
import Test.HUnit ((~=?))

main :: IO ()
main = do
  captured <- Silently.capture_ Lib.someFunc
  Test.defaultMain (Test.hUnitTestToTests ("someFunc\n" ~=? captured))
