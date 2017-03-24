module Control.Logger.Syslog
  ( Message(..)
  , syslog
  ) where

import Control.Logger (Logger(..))
import Data.Maybe (Maybe)
import Data.StrMap (StrMap)
import Prelude
import Syslog as Syslog
import Syslog.Facility (Facility)
import Syslog.Severity (Severity)

data Message = Message Severity (StrMap (StrMap String)) (Maybe String)

syslog
  :: ∀ f
   . (Syslog.Message -> f Unit)
  -> Facility
  -> Logger f Message
syslog w f = Logger \(Message severity structuredData message) ->
  w { priority:       Syslog.fsPriority f severity
    , structuredData: structuredData
    , message:        message
    }
