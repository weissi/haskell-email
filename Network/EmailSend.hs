module Network.EmailSend(MailBackend(..), SendingReceipt) where

import Data.ByteString (ByteString)

type SendingReceipt = Maybe String

class MailBackend a where
    sendMessage :: a -> String -> String -> ByteString -> IO SendingReceipt
