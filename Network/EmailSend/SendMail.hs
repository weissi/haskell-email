module Network.EmailSend.SendMail(sendMessageSendMail
                                 , SendMailBackend(..)) where

----------------------------------------
---- STDLIB
----------------------------------------
import qualified System.IO as Sio
import qualified System.Process as Spo
import qualified Data.ByteString as BS
import System.Exit (ExitCode(..))

----------------------------------------
---- SITE-PACKAGES
----------------------------------------
import Network.EmailSend (MailBackend(..), SendingReceipt)
import Data.Encoding.UTF8 (UTF8(..))
import Data.Encoding (decodeStrictByteString)

----------------------------------------
---- LOCAL
----------------------------------------

_SENDMAIL_BINARY_ = "/usr/sbin/sendmail"

data SendMailBackend = SendMailBackend String | StdSendMailBackend
instance MailBackend SendMailBackend where
    sendMessage = sendMessageSendMail

sendMessageSendMail :: SendMailBackend -> String -> String -> BS.ByteString -> IO SendingReceipt
sendMessageSendMail (SendMailBackend binary) = sendEmail binary
sendMessageSendMail StdSendMailBackend = sendEmail _SENDMAIL_BINARY_

sendEmail :: String -> String -> String -> BS.ByteString -> IO SendingReceipt
sendEmail binary from to message =
  do (inh, outh, errh, ph) <-
       Spo.runInteractiveProcess binary [to] Nothing Nothing
     Sio.hClose outh
     BS.hPutStr inh message
     Sio.hClose inh
     ret <- Spo.waitForProcess ph
     case ret of
       ExitSuccess -> return Nothing
       ExitFailure code -> return $ Just $ show code
