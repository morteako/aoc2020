module Main
  ( main,
  )
where

import CmdArgs
import qualified Data.IntMap as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Day.Day1 as Day1 (run)
import Input (getInput)
import Options.Applicative
import Utils

funcs :: IntMap (String -> IO ())
funcs =
  Map.fromList
    []

runner :: Options -> IO ()
runner Options {day, input} = do
  let func :: String -> IO ()
      func i = case day of
        LastDay ->
          (snd . IntMap.findMax) funcs i
        SpecificDay d ->
          case IntMap.lookup d funcs of
            Nothing -> do
              putStrLn $ show d <> " is not implemented."
              putStrLn $ "Currently implemented : " <> unwords (map show (IntMap.keys funcs))
            Just runner ->
              runner i
  case input of
    StdIn -> do
      getContents >>= func
    File path -> do
      readFile path >>= func
    DayInput -> do
      case length funcs of
        0 -> putStrLn "No days are currently implemented :O :(("
        d -> getInput d >>= func

main :: IO ()
main = do
  let parser = cmdParser $ show $ length funcs
  execParser parser >>= runner
