module RandomStrings (generateCode, generatePostToken) where

import Models

import Numeric (showHex)
import System.Random (randomRIO)
import Control.Monad (replicateM)

_lPad0 :: Int -> String -> String
_lPad0 k s
  | length s < k = "0" ++ s
  | otherwise    = s

_generateHex8 :: IO String
_generateHex8 = do
  num <- randomRIO (0, 4294967295) :: IO Integer -- that's 16**8 - 1
  return $ _lPad0 8 $ showHex num ""

generateHex32 :: IO String
generateHex32 = do
  arr <- replicateM 4 _generateHex8
  return $ concat arr

generateCode :: IO Code
generateCode = generateHex32

generatePostToken :: IO PostToken
generatePostToken = generateHex32
