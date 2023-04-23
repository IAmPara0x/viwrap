module CmdArgs
  ( CaptureArgs (..)
  , LaunchMode (..)
  , SimulateArgs (..)
  , launchArgParser
  ) where

import Options.Applicative


data SimulateArgs
  = SimulateArgs
      { captureContentsFilePath :: FilePath
      , inputOffset             :: Int
      , simulateProgram         :: String
      , simulateProgramArgs     :: [String]
      }

data CaptureArgs
  = CaptureArgs
      { outputCaptureContentsFilePath :: FilePath
      , captureProgram                :: String
      , captureProgramArgs            :: [String]
      }

data LaunchMode
  = Capture CaptureArgs
  | Simulate


launchArgParser :: Parser LaunchMode
launchArgParser =
  (flag' Capture (long "capture" <> help "launch in capture mode") <*> parseCaptureArgs)
    <|> pure Simulate


parseCaptureArgs :: Parser CaptureArgs
parseCaptureArgs =
  CaptureArgs
    <$> strOption (long "file" <> metavar "TARGET")
    <*> strOption (help "name of program" <> metavar "EXECUTABLE" <> value "python" <> short 'p')
    <*> option (many str) (help "program args" <> metavar "EXECUTABLE_ARGS" <> value mempty)
