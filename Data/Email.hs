module Data.Email (sendSimpleEmail) where

----------------------------------------
---- STDLIB
----------------------------------------
import Data.Encoding (encodeStrictByteString)
import Data.Encoding.UTF8 (UTF8(..))

----------------------------------------
---- SITE-PACKAGES
----------------------------------------
import Network.EmailSend (sendMessage)
import Text.ParserCombinators.Parsec.Rfc2822 (NameAddr(..))

----------------------------------------
---- LOCAL
----------------------------------------
import Data.EmailSerializer (serializeMessage)
import Data.EmailRepr (_EMPTY_EMAIL_, Email(..), email2message, Content(..))

sendSimpleEmail backend from to subject text =
    sendMessage backend from to (
        (serializeMessage . email2message)
            _EMPTY_EMAIL_ { em_from=[(NameAddr Nothing from)]
                          , em_to=[(NameAddr Nothing to)]
                          , em_subject=subject
                          , em_optionals=[("Content-Type",
                                           "text/plain; charset=utf-8")]
                          , em_body=ContentByte
                                      (encodeStrictByteString UTF8 text)
                          })
