module Syslog.TCP
  ( log
  , frame
  ) where

import Control.Monad.Aff (Aff, makeAff)
import Data.ByteString (ByteString)
import Data.ByteString as ByteString
import Node.Stream (Writable, write)
import Prelude
import Syslog (Message, message)

log :: ∀ r eff. Writable r eff -> Message -> Aff eff Unit
log s m =
  let f = frame (message m)
  in makeAff \_ onOK -> void $ write s (ByteString.unsafeThaw f) (onOK unit)

frame :: ByteString -> ByteString
frame syslogMsg = syslogFrame
  where
    -- As per RFC 6587, section 3.4.1.
    syslogFrame = msgLen <> sp <> syslogMsg
    msgLen = ByteString.toUTF8 $ show (ByteString.length syslogMsg)
    sp = ByteString.toUTF8 " "
