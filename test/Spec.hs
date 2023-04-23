module Main
  ( main
  ) where

import Capture.Viwrap      qualified as Capture
import CmdArgs
import Simulate.Viwrap     qualified as Simulate

import Options.Applicative

main :: IO ()
main = do

  mode <- execParser opts

  case mode of
    Capture args -> Capture.main args
    Simulate     -> Simulate.main SimulateArgs { captureContentsFilePath = "./data/capture"
                                               , inputOffset             = 0
                                               , simulateProgram         = "python"
                                               , simulateProgramArgs     = []
                                               }
  where opts = info (launchArgParser <**> helper) (fullDesc <> header "Test executable for viwrap")
