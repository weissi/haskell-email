{- vim: set encoding=utf-8 :
 -*- coding: utf-8 -*- -}
----------------------------------------
---- STDLIB
----------------------------------------

----------------------------------------
---- SITE-PACKAGES
----------------------------------------
import Network.EmailSend.SendMail (SendMailBackend(..))
import Data.Email (sendSimpleEmail)
import Network.EmailSend(sendMessage, MailBackend(..), SendingReceipt, )
import Network.EmailSend.SMTP (SMTPBackend(..))
import Network.EmailSend.SendMail (SendMailBackend(..))

----------------------------------------
---- LOCAL
----------------------------------------

main = do
    putStrLn "Sending Mail..."
    let smtp = SMTPBackend "localhost" "localhost"
    --sendSimpleEmail StdSendMailBackend "dirk" "weissi" "test ääööüü" "hallo ääüüöö"
    x <- sendSimpleEmail smtp "dirk" "weissi" "test ääööüü" "hallo ääüüöö"
    case x of
      Nothing -> putStrLn "done."
      Just m -> putStrLn $ "ERROR: "++m
    putStrLn "bye bye"
