module Control.Logger.Syslog
  ( syslog
  ) where

import Control.Logger (Logger(..))
import Data.Maybe (Maybe(..))
import Prelude
import Syslog (Message, fsPriority)
import Syslog.Facility (Facility)
import Syslog.Severity (Severity)
import Data.Tuple (Tuple(..))

syslog
  :: ∀ f
   . (Message -> f Unit)
  -> Facility
  -> Logger f (Tuple Severity String)
syslog w f = Logger \(Tuple s m) ->
  w {priority: fsPriority f s, message: Just m}
