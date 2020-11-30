module CmdArgs where

import qualified Data.Bifunctor as Bifunctor
import Options.Applicative as Opt
import Text.Megaparsec as Parsec
import Utils

data Options = Options
  { day :: Day,
    input :: Input
  }
  deriving (Show)

data Day = LastDay | SpecificDay Int deriving (Show)

data Input = StdIn | File String | DayInput deriving (Show)

megaparsecReader :: Parsec String String a -> ReadM a
megaparsecReader parser =
  eitherReader (Bifunctor.first show . Parsec.parse parser "")

cmdParser :: Int -> ParserInfo Options
cmdParser lastDay =
  info
    (options2 <**> helper)
    ( fullDesc
        <> progDesc ("Run a advent of code challenge. Default is to run the last implemented challenge (" <> show lastDay <> ") and fetch the corresponding input")
        <> header "aoc2020 - haskell solutions for advent of code 2020"
    )

options2 :: Parser Options
options2 =
  Options
    <$> (specificDayInput <|> pure LastDay)
    <*> (stdInput <|> fileInput <|> pure DayInput)

specificDayInput :: Parser Day
specificDayInput =
  SpecificDay . read
    <$> strOption
      ( long "day"
          <> metavar "DAY"
          <> help "Run challenge for the provided day"
      )

fileInput :: Parser Input
fileInput =
  File
    <$> strOption
      ( long "file"
          <> metavar "FILENAME"
          <> help "Read from input file"
      )

stdInput :: Parser Input
stdInput =
  flag'
    StdIn
    ( long "stdin"
        <> help "Read from stdin"
    )