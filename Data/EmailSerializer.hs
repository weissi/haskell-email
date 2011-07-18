-- vim: set encoding=utf-8 :

module Data.EmailSerializer(_CRLF_, Message, encodeHeader, encodeAscii, serializeMessage) where
----------------------------------------
---- STDLIB
----------------------------------------
import Data.Encoding (encodeStrictByteString)
import System.Time (calendarTimeToString, CalendarTime)

----------------------------------------
---- SITE-PACKAGES
----------------------------------------
import Text.ParserCombinators.Parsec.Rfc2822 (GenericMessage(..), Field(..),
                                              NameAddr(..))
import qualified Data.ByteString as BS (ByteString, empty, concat)
import Data.Encoding (encodeStrictByteString)
import Data.Encoding.ASCII (ASCII(..))

----------------------------------------
---- LOCAL
----------------------------------------
import WashUtils.RFC2047 (encodeValueBS)

_EMPTY_MESSAGE_ = Message [] BS.empty
_CRLF_ = encodeAscii "\r\n"

encodeAscii ::  String -> BS.ByteString
encodeAscii s = encodeStrictByteString ASCII s

maybeEncodeValue ::  [Char] -> String -> BS.ByteString
maybeEncodeValue "Content-Type" v = encodeAscii v
maybeEncodeValue "Content-Disposition" v = encodeAscii v
maybeEncodeValue k v = encodeValueBS v

encodeHeaderBS :: String -> BS.ByteString -> BS.ByteString
encodeHeaderBS key value = BS.concat [encodeAscii (key++": "), value]

encodeHeader :: String -> String -> BS.ByteString
encodeHeader key value = encodeHeaderBS key (maybeEncodeValue key value)

type Message = GenericMessage BS.ByteString

serializeMessage :: Message -> BS.ByteString
serializeMessage (Message hs msg) = BS.concat [encodeHeaders hs
                                              ,_CRLF_
                                              ,msg
                                              ]

crlfAppend ::  BS.ByteString -> BS.ByteString -> BS.ByteString
crlfAppend l r = BS.concat [l, _CRLF_, r]

encodeHeaders :: [Field] -> BS.ByteString
encodeHeaders fs = foldr crlfAppend BS.empty $ map encodeField fs

encodeField :: Field -> BS.ByteString
encodeField (OptionalField k v) = BS.concat[encodeAscii k
                                           ,encodeAscii ": "
                                           ,maybeEncodeValue k v
                                           ]
encodeField (From ns) = encodeNameAddrs "From" ns
encodeField (Sender n) = encodeNameAddr "Sender" n
encodeField (ReturnPath r) = encodeHeader "Return-Path" r
encodeField (ReplyTo ns) = encodeNameAddrs "Reply-To" ns
encodeField (To ns) = encodeNameAddrs "To" ns
encodeField (Cc ns) = encodeNameAddrs "Cc" ns
encodeField (Bcc ns) = encodeNameAddrs "Bcc" ns
encodeField (MessageID id) = encodeHeader "Message-ID" id
encodeField (InReplyTo sl) = encodeStringList "In-Reply-To" sl
encodeField (References sl) = encodeStringList "References" sl
encodeField (Subject s) = encodeHeader "Subject" s
encodeField (Comments s) = encodeHeader "Comments" s
encodeField (Keywords dsl) = encodeDoubleStringList "Keywords" dsl
encodeField (Date ct) = encodeCalendarTime "Date" ct
encodeField (ResentDate ct) = encodeCalendarTime "Resent-Date" ct
encodeField (ResentFrom ns) = encodeNameAddrs "Resent-From" ns
encodeField (ResentSender n) = encodeNameAddr "Resent-Sender" n
encodeField (ResentTo ns) = encodeNameAddrs "Resent-To" ns
encodeField (ResentCc ns) = encodeNameAddrs "Resent-Cc" ns
encodeField (ResentBcc ns) = encodeNameAddrs "Resent-Bcc" ns
encodeField (ResentMessageID s) = encodeHeader "Resent-Message-ID" s
encodeField (ResentReplyTo ns) = encodeNameAddrs "Resent-Reply-To" ns
encodeField (Received rc) = encodeReceived "Received" rc
encodeField (ObsReceived orc) = encodeObsReceived "Obs-Received" orc

appendComma :: BS.ByteString -> BS.ByteString -> BS.ByteString
appendComma l r = BS.concat [l, encodeAscii ", ", r]

encodeNameAddrs :: String -> [NameAddr] -> BS.ByteString
encodeNameAddrs k vs = encodeHeaderBS k $
                       foldr appendComma BS.empty $ map encodeNameAddrValue vs

encodeNameAddr :: String -> NameAddr -> BS.ByteString
encodeNameAddr k v = encodeHeaderBS k $ encodeNameAddrValue v

encodeNameAddrValue :: NameAddr -> BS.ByteString
encodeNameAddrValue (NameAddr Nothing m) = encodeValueBS m
encodeNameAddrValue (NameAddr (Just n) m) = encodeValueBS (n++" <"++m++">")


encodeStringList :: String -> [String] -> BS.ByteString
encodeStringList k vs = encodeHeaderBS k $ foldr appendComma BS.empty $
                                                 map encodeValueBS vs

encodeCalendarTime :: String -> System.Time.CalendarTime -> BS.ByteString
encodeCalendarTime k v = encodeHeader k (calendarTimeToString v)

encodeReceived ::  String -> ([(String, String)], CalendarTime) -> BS.ByteString
encodeReceived k v = encodeHeader k (show v)

encodeObsReceived :: String -> [(String, String)] -> BS.ByteString
encodeObsReceived k v = encodeHeader k (show v)

encodeDoubleStringList ::  String -> [[String]] -> BS.ByteString
encodeDoubleStringList k v = encodeHeader k (show v)
