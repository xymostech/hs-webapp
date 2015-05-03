module Logging
( level1
, level2
, level3
, level4
, level5
)
where

import Debug.Trace (trace)

logLevel :: Int
logLevel = 5

doLog :: Int -> String -> a -> a
doLog level message val =
  if level <= logLevel
  then trace message val
  else val

level1 = doLog 1
level2 = doLog 2
level3 = doLog 3
level4 = doLog 4
level5 = doLog 5
