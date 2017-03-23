module Syslog.Severity
  ( Severity(..)
  , severityCode
  ) where

data Severity
  = Emergency
  | Alert
  | Critical
  | Error
  | Warning
  | Notice
  | Informational
  | Debug

severityCode :: Severity -> Int
severityCode Emergency = 0
severityCode Alert = 1
severityCode Critical = 2
severityCode Error = 3
severityCode Warning = 4
severityCode Notice = 5
severityCode Informational = 6
severityCode Debug = 7
