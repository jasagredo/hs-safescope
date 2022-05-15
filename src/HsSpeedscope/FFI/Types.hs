{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}

module HsSpeedscope.FFI.Types where

import Control.Monad
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (isDigit, isSpace)
import Data.Either (fromRight)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Version
import Data.Version.Extra (Version)
import Data.Word (Word32, Word64)
import GHC.RTS.Events (Capset, Timestamp)
import qualified Text.ParserCombinators.ReadP as TR

type EventLabel = Text

type EventSubscript = Int

data CCInfo = CCInfo
  { ccId :: Word32,
    ccLabel :: Text,
    ccModule :: Text,
    ccLoc :: Text
  }
  deriving (Show)

data Sample = Sample Capset [Word32] deriving (Show)

{-------------------------------------------------------------------------------
  User events
-------------------------------------------------------------------------------}

data UserEventStartStop = UserEventStartStop (EventLabel, EventSubscript)

parseUserEventStartStop :: Text -> UserEventStartStop
parseUserEventStartStop s =
  case TR.decimal s of
    Left _ -> UserEventStartStop (s, 0)
    Right (eid, cs) -> UserEventStartStop (T.dropWhile isSpace cs, eid)

data UserEventCCSInfo = UserEventCCSInfo (EventLabel, EventSubscript) (Vector Word32)

parseUserEventCCSInfo :: [CCInfo] -> Text -> UserEventCCSInfo
parseUserEventCCSInfo hpcc s =
  let (eid, s') = either (const (0, s)) id $ TR.decimal s
   in UserEventCCSInfo (head (T.words s'), eid) . getCCId . T.dropWhile (not . isSpace) $ s'
  where
    oneNoOneYes (_ : x : xs) = x : oneNoOneYes xs
    oneNoOneYes _ = []

    parseCCRequest :: Text -> Maybe (Text, Text, Text)
    parseCCRequest t =
      let [ident, loc] = T.split (== ' ') t
          (lab, mod) = bimap T.reverse T.reverse $ T.span (/= '.') $ T.reverse ident
       in Just (T.init mod, lab, T.init (T.tail loc))

    getCCId =
      V.fromList
        . map (\(parseCCRequest -> Just (m, l, loc)) -> head [i | CCInfo i la mo lo <- hpcc, la == l && mo == m && lo == loc])
        . oneNoOneYes
        . T.split (== '\"')

data UserEventThreadInfo = UserEventThreadInfo (EventLabel, EventSubscript) Int

parseUserEventThreadInfo :: Text -> UserEventThreadInfo
parseUserEventThreadInfo s =
  let (eid, s') = fromRight (0, s) $ TR.decimal s
      ws = T.words s'
   in UserEventThreadInfo (head ws, eid) (either error fst $ TR.decimal $ last ws)

{-------------------------------------------------------------------------------
  Artificial events
-------------------------------------------------------------------------------}

data ArtificialEvent = ArtificialEvent
  { tag :: (EventLabel, EventSubscript),
    ccs :: Vector Word32,
    aeThread :: Int,
    start :: Timestamp,
    end :: Timestamp
  }
  deriving (Show)

{-------------------------------------------------------------------------------
  Analysis
-------------------------------------------------------------------------------}
data Analysis = Analysis
  { prog_name :: Maybe Text,
    rts_version :: Maybe (Version, Text),
    prof_interval :: Maybe Word64,
    cost_centres :: [CCInfo],
    artificial_events :: [ArtificialEvent],
    el_samples :: [Sample],
    num_caps :: Int,
    last_time :: Timestamp
  }
  deriving (Show)

parseIdent :: Text -> Maybe (Version, Text)
parseIdent s = convert $
  listToMaybe $
    flip TR.readP_to_S (T.unpack s) $ do
      void $ TR.string "GHC-"
      [v1, v2, v3] <- replicateM 3 (intP <* TR.optional (TR.char '.'))
      TR.skipSpaces
      return $ makeVersion [v1, v2, v3]
  where
    intP = do
      x <- TR.munch1 isDigit
      return $ read x

    convert x = (\(a, b) -> (a, T.pack b)) <$> x
