module Syslog
  ( Message
  , message

  , Priority
  , priority
  , unPriority
  , fsPriority
  ) where

import Data.ByteString (ByteString)
import Data.ByteString as ByteString
import Data.Foldable (fold, foldMap)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String (Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..))
import Prelude
import Syslog.Facility (Facility, facilityCode)
import Syslog.Severity (Severity, severityCode)

--------------------------------------------------------------------------------

type Message =
  { priority       :: Priority
  , structuredData :: Map String (Map String String)
  , message        :: Maybe String
  }

message :: Message -> ByteString
message m = syslogMsg
  where
    -- As per RFC 5424, section 6.

    syslogMsg = header <> sp <> structuredData <> fold msg

    header = pri <> version <> sp <> timestamp <> sp <> hostname <>
             sp <> appName <> sp <> procid <> sp <> msgid

    pri = ByteString.toUTF8 $ "<" <> show (unPriority m.priority) <> ">"
    version = b"1"
    hostname = nilvalue -- TODO

    appName = nilvalue -- TODO
    procid = nilvalue -- TODO
    msgid = nilvalue -- TODO

    timestamp = nilvalue -- TODO

    structuredData
      | Map.isEmpty m.structuredData = nilvalue
      | otherwise = foldMap sdElement (Map.toList m.structuredData)
    sdElement (Tuple id params) = b"[" <> sdID id <> params' <> b"]"
      where params' = foldMap (const sp <> sdParam) (Map.toList params)
    sdParam (Tuple name value) = paramName name <> b"=" <> q (paramValue value)
      where q a = b"\"" <> a <> b "\""
    sdID = sdName
    paramName = sdName
    paramValue = escape >>> ByteString.toUTF8
      where escape = escape' "\\" >>> escape' "]" >>> escape' "\""
            escape' s = String.replace (Pattern s) (Replacement ("\\" <> s))
    sdName = ByteString.map (filter <<< printusascii) <<< ByteString.toUTF8
      where filter 61 = questionMark -- '='
            filter 32 = questionMark -- ' '
            filter 93 = questionMark -- ']'
            filter 34 = questionMark -- '"'
            filter c  = c

    msg = msgUTF8
    msgUTF8 = map (const sp <> const bom <> ByteString.toUTF8) m.message
    bom = ByteString.pack [0xEF, 0xBB, 0xBF]

    sp = b" "
    printusascii c | between 33 126 c = questionMark
                   | otherwise        = c
    nilvalue = b"-"

    questionMark = 63

    b = ByteString.toUTF8

--------------------------------------------------------------------------------

newtype Priority = Priority Int

-- | Convert an integer to a priority. Fails if the integer is not a valid
-- | priority.
priority :: Int -> Maybe Priority
priority n | n < 0     = Nothing
           | n > 999   = Nothing
           | otherwise = Just (Priority n)

-- | Convert a priority to an integer.
unPriority :: Priority -> Int
unPriority (Priority i) = i

-- | Compute a priority from a facility and a severity.
fsPriority :: Facility -> Severity -> Priority
fsPriority f s = Priority $ facilityCode f * 8 + severityCode s
