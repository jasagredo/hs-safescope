{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module HsSpeedscope.FFI (analyze, toSpeedScope, withFakeEvents) where

import Control.Monad.State
import Data.Aeson
import Data.Function
import qualified Data.List as L
import Data.List.Extra
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word
import GHC.RTS.Events
import HsSpeedscope.FFI.Types
import Speedscope.Schema
import Text.ParserCombinators.ReadP as TR

sortedEvents :: EventLog -> [Event]
sortedEvents (EventLog _header (Data es)) = sortEvents es

analyzeEvent :: Event -> State Analysis ()
analyzeEvent (Event time spec _cap) =
  modify
    ( (\s -> s {last_time = time})
        . ( \s ->
              case spec of
                ProgramArgs _ (pname : _args) ->
                  s {prog_name = (Just pname)}
                RtsIdentifier _ rts_ident ->
                  s {rts_version = parseIdent rts_ident}
                ProfBegin ival ->
                  s {prof_interval = Just ival}
                ProfSampleCostCentre t _ _ st ->
                  s {el_samples = Sample t (VU.toList st) : el_samples s}
                CapCreate {} ->
                  s {num_caps = num_caps s + 1}
                HeapProfCostCentre i l m lo _ ->
                  s {cost_centres = CCInfo i l m lo : cost_centres s}
                UserMessage (T.stripPrefix "START " -> Just e) ->
                  let UserEventStartStop t = parseUserEventStartStop e
                   in s {artificial_events = ArtificialEvent t V.empty 0 time time : artificial_events s}
                UserMessage (T.stripPrefix "STOP " -> Just e) ->
                  let UserEventStartStop t = parseUserEventStartStop e
                   in s {artificial_events = updateStop t time $ artificial_events s}
                UserMessage (T.stripPrefix "ANN_SCC " -> Just e) ->
                  let UserEventCCSInfo t c = parseUserEventCCSInfo (cost_centres s) e
                   in s {artificial_events = updateCCSInfo t c $ artificial_events s}
                UserMessage (T.stripPrefix "ANN_TH " -> Just e) ->
                  let UserEventThreadInfo t i = parseUserEventThreadInfo e
                   in s {artificial_events = updateThreadId t i $ artificial_events s}
                _ -> s
          )
    )
  where
    updateStop :: (EventLabel, EventSubscript) -> Timestamp -> [ArtificialEvent] -> [ArtificialEvent]
    updateStop eid time (x : xs)
      | eid == tag x = x {end = time} : xs
      | otherwise = x : updateStop eid time xs

    updateThreadId :: (EventLabel, EventSubscript) -> Int -> [ArtificialEvent] -> [ArtificialEvent]
    updateThreadId eid th (x : xs)
      | eid == tag x = x {aeThread = th} : xs
      | otherwise = x : updateThreadId eid th xs

    updateCCSInfo :: (EventLabel, EventSubscript) -> Vector Word32 -> [ArtificialEvent] -> [ArtificialEvent]
    updateCCSInfo eid cc (x : xs)
      | eid == tag x = x {ccs = cc} : xs
      | otherwise = x : updateCCSInfo eid cc xs

analyze :: EventLog -> Analysis
analyze = flip (execState . mapM_ analyzeEvent . sortedEvents) initialState

initialState :: Analysis
initialState = Analysis Nothing Nothing Nothing [] [] [] 0 0

withFakeEvents :: Analysis -> Analysis
withFakeEvents s =
  s
    { el_samples = el_samples s ++ artificial,
      artificial_events = []
    }
  where
    artificial =
      concatMap
        createFakeEventsForThread
        $ zip
          ( L.groupBy ((==) `on` aeThread)
              . L.sortBy (compare `on` aeThread)
              . artificial_events
              $ s
          )
          [fromIntegral (num_caps s) + 1 ..]

    createFakeEventsForThread :: ([ArtificialEvent], Capset) -> [Sample]
    createFakeEventsForThread (evs, cap) =
      let prof_int = fromJust $ prof_interval s

          end_time = fromIntegral $ last_time s

          idleId = head [ccId c | c <- cost_centres s, ccLabel c == "IDLE"]

          sorted = L.sortOn end evs

          nTicks x y = replicate (fromIntegral $ fromIntegral (y - x) `div` prof_int)

          createFake (x : y : xs) = let (next, l) = createFake (y : xs) in (createFakeForEv x ++ createFakeForGap x y ++ next, l)
          createFake [x] = (createFakeForEv x, x)

          createFakeForEv x = nTicks (start x) (end x) (Sample cap (reverse $ V.toList $ ccs x))
          createFakeForGap x y = nTicks (end x) (start y) (Sample cap [idleId])
       in case sorted of
            x : xs ->
              let (withoutEndTime, lastEvent) = createFake sorted
               in nTicks (end lastEvent) end_time (Sample cap [idleId])
                    ++ reverse withoutEndTime
                    ++ nTicks 0 (start x) (Sample cap [idleId])
            [] -> []

toSpeedScope :: Analysis -> Value
toSpeedScope s =
  toJSON $
    File
      { shared = Shared {frames = ccs_json},
        profiles =
          map
            ( \x@(c, _) ->
                mkProfile
                  ( (if c < fromIntegral (num_caps s) then "Capability " <> T.pack (show c) else "OSThread " <> T.pack (show $ c - fromIntegral (num_caps s)))
                      <> ": "
                      <> (last $ T.split (== '/') $ fromMaybe "" $ prog_name s)
                  )
                  (fromMaybe 1 $ prof_interval s)
                  x
            )
            caps,
        name = Just (last $ T.split (== '/') $ fromMaybe "" $ prog_name s),
        activeProfileIndex = Just 0,
        exporter = Just $ fromString $ "hs-safescope@" ++ CURRENT_PACKAGE_VERSION
      }
  where
    ccs_json = map mkFrame (cost_centres s)

    mkFrame :: CCInfo -> Frame
    mkFrame (CCInfo _n name _m file) = Frame {name = name, file = Just file, col = Nothing, line = Nothing}

    caps :: [(Capset, [[Int]])]
    caps = groupSort $ mapMaybe mkSample (reverse (el_samples s))

    mkSample :: Sample -> Maybe (Capset, [Int])
    mkSample (Sample ti ccs) = Just (ti, map (subtract 1 . fromIntegral) (reverse ccs))

mkProfile :: Text -> Word64 -> (Capset, [[Int]]) -> Profile
mkProfile pname interval (_n, samples) = SampledProfile sampledProfile
  where
    sampledProfile =
      MkSampledProfile
        { unit = Nanoseconds,
          name = pname,
          startValue = 0,
          endValue = length samples,
          weights = fromIntegral <$> sample_weights,
          samples = samples
        }
    sample_weights = replicate (length samples) interval
