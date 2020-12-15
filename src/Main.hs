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
import qualified Day.Day04
import qualified Day.Day05
import qualified Day.Day06
import qualified Day.Day07
import qualified Day.Day08
import qualified Day.Day09
import qualified Day.Day10
import qualified Day.Day11
import qualified Day.Day12
import qualified Day.Day13
import qualified Day.Day14
import qualified Day.Day15
import Input (getInput)
import Options.Applicative (execParser)
import Utils ((=:))

funcs :: IntMap (String -> IO ())
funcs =
  Map.fromList
    [ 1 =: Day.Day01.run,
      2 =: Day.Day02.run,
      3 =: Day.Day03.run,
      4 =: Day.Day04.run,
      5 =: Day.Day05.run,
      6 =: Day.Day06.run,
      7 =: Day.Day07.run,
      8 =: Day.Day08.run,
      9 =: Day.Day09.run,
      10 =: Day.Day10.run,
      11 =: Day.Day11.run,
      12 =: Day.Day12.run,
      13 =: Day.Day13.run,
      14 =: Day.Day14.run,
      15 =: Day.Day15.run
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
  inputFile <- case input of
    StdIn -> do
      getContents
    File path -> do
      readFile path
    Test -> do
      let path = "input/" <> show lastDayNr <> "test"
      readFile path
    DayInput -> do
      case day of
        LastDay -> getInput lastDayNr
        SpecificDay d -> getInput d
  func inputFile

main :: IO ()
main = do
  let parser = cmdParser lastDayNr
  execParser parser >>= runner
