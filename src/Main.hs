module Main
  ( main,
  )
where

import CmdArgs
import qualified Data.IntMap as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Day.Day01
import qualified Day.Day02
import qualified Day.Day03
import Input (getInput)
import Options.Applicative
import Utils

funcs :: IntMap (String -> IO ())
funcs =
  Map.fromList
    [ 1 =: Day.Day01.run,
      2 =: Day.Day02.run,
      3 =: Day.Day03.run
    ]

lastDayNr :: Int
lastDayRunnner :: String -> IO ()
(lastDayNr, lastDayRunnner) = IntMap.findMax funcs

runner :: Options -> IO ()
runner Options {day, input} = do
  let func :: String -> IO ()
      func i = case day of
        LastDay ->
          lastDayRunnner i
        SpecificDay d ->
          case IntMap.lookup d funcs of
            Nothing -> do
              putStrLn $ show d <> " is not implemented."
              putStrLn $ "Currently implemented : " <> unwords (show <$> IntMap.keys funcs)
            Just dayRunner ->
              dayRunner i
  case input of
    StdIn -> do
      getContents >>= func
    File path -> do
      readFile path >>= func
    DayInput -> do
      getInput lastDayNr >>= func

main :: IO ()
main = do
  let parser = cmdParser lastDayNr
  execParser parser >>= runner
