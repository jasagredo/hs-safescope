module Main where

import HsSpeedscope.FFI ( analyze, withFakeEvents, toSpeedScope )
import Options ( Options(Options, file), opts )
import Options.Applicative ( execParser )
import GHC.RTS.Events ( readEventLogFromFile )
import Data.Aeson ( encodeFile )

main :: IO ()
main = do
  Options{ file = eventlog } <- execParser opts
  el <- either error id <$> readEventLogFromFile eventlog
  encodeFile (eventlog ++ ".json")
    . toSpeedScope
    . withFakeEvents
    . analyze
    $ el
