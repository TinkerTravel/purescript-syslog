module Control.Logger.Syslog
  ( Message
  , syslog
  ) where

import Control.Logger (Logger(..))
import Data.Map (Map)
import Data.Maybe (Maybe)
import Prelude
import Syslog as Syslog
import Syslog.Facility (Facility)
import Syslog.Severity (Severity)

type Message =
  { severity       :: Severity
  , structuredData :: Map String (Map String String)
  , message        :: Maybe String
  }

syslog
  :: ∀ f
   . (Syslog.Message -> f Unit)
  -> Facility
  -> Logger f Message
syslog w f = Logger \m ->
  w { priority:       Syslog.fsPriority f m.severity
    , structuredData: m.structuredData
    , message:        m.message
    }
