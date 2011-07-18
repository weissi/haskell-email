module Network.EmailSend.SMTP(SMTPBackend(..)) where

----------------------------------------
---- STDLIB
----------------------------------------

----------------------------------------
---- SITE-PACKAGES
----------------------------------------
import Data.ByteString (ByteString)
import qualified Network.HaskellNet.SMTP as SMTP
import Network.EmailSend (MailBackend(..), SendingReceipt)

----------------------------------------
---- LOCAL
----------------------------------------

data SMTPBackend = SMTPBackend String String

instance MailBackend SMTPBackend where
    sendMessage = smtpSendMessage

smtpSendMessage :: SMTPBackend -> String -> String -> ByteString ->
                   IO SendingReceipt
smtpSendMessage (SMTPBackend server hostname) from to message = (do
    c <- SMTP.connectSMTP server
    SMTP.sendCommand c $ SMTP.EHLO hostname
    SMTP.sendCommand c $ SMTP.MAIL from
    SMTP.sendCommand c $ SMTP.RCPT to
    SMTP.sendCommand c $ SMTP.DATA message
    SMTP.sendCommand c SMTP.NOOP
    SMTP.sendCommand c SMTP.QUIT
    return Nothing) `catch` (\m -> return $ Just $ show m)
