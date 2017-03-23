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
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Prelude
import Syslog.Facility (Facility, facilityCode)
import Syslog.Severity (Severity, severityCode)

--------------------------------------------------------------------------------

type Message =
  { priority :: Priority
  , message :: Maybe String
  }

message :: Message -> ByteString
message m = syslogMsg
  where
    -- As per RFC 5424, section 6.

    syslogMsg = header <> sp <> structuredData <> fold msg

    header = pri <> version <> sp <> timestamp <> sp <> hostname <>
             sp <> appName <> sp <> procid <> sp <> msgid

    pri = ByteString.toUTF8 $ "<" <> show (unPriority m.priority) <> ">"
    version = ByteString.toUTF8 "1"
    hostname = nilvalue -- TODO

    appName = nilvalue -- TODO
    procid = nilvalue -- TODO
    msgid = nilvalue -- TODO

    timestamp = nilvalue -- TODO

    structuredData = nilvalue -- TODO

    msg = msgUTF8
    msgUTF8 = map (const sp <> const bom <> ByteString.toUTF8) m.message
    bom = ByteString.pack [0xEF, 0xBB, 0xBF]

    sp = ByteString.toUTF8 " "
    nilvalue = ByteString.toUTF8 "-"

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
