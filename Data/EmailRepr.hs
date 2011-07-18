-- vim: set encoding=utf-8 :

module Data.EmailRepr(Email(..), Message, _EMPTY_EMAIL_, encodeHeaders
                     ,Headers, Content(..), MimePart(..), email2message) where
----------------------------------------
---- STDLIB
----------------------------------------
import Data.Time.Calendar (toGregorian)
import Data.Time.Format (formatTime)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..), TimeZone(..), ZonedTime(..))

import System.Time (CalendarTime(..), Day(..), Month(..))
import System.Locale (defaultTimeLocale)

----------------------------------------
---- SITE-PACKAGES
----------------------------------------
import Text.ParserCombinators.Parsec.Rfc2822
       (Field(..), NameAddr(..), GenericMessage(..))
import qualified Data.ByteString as BS (ByteString, empty, concat)
import Data.EmailSerializer (Message, encodeHeader, _CRLF_)

----------------------------------------
---- LOCAL
----------------------------------------

data Email = Email { em_from :: [NameAddr]
                   , em_to :: [NameAddr]
                   , em_cc :: [NameAddr]
                   , em_bcc :: [NameAddr]
                   , em_subject :: String
                   , em_date :: Maybe ZonedTime
                   , em_optionals :: Headers
                   , em_body :: Content
                   } deriving Show

_EMPTY_EMAIL_ = Email [] [] [] [] "" Nothing [] (ContentByte BS.empty)
type Headers = [(String, String)]

data Content = ContentByte BS.ByteString | ContentMime MimePart deriving Show

data MimePart = MimePart
  { mp_mime_type :: Maybe String -- text/plain \
  , mp_filename :: Maybe String  -- test.txt    +--> Content-Type
  , mp_charset :: Maybe String   -- UTF-8      /
  , mp_inline :: Bool            -- True       ----> Content-Disposition
  , mp_optionals :: Headers      -- [("MIME-Version", "1.0)]
  , mp_content :: Content        -- "foo bar"
  } deriving Show

append2 ::  BS.ByteString -> BS.ByteString -> BS.ByteString
append2 a b = BS.concat [a, _CRLF_, b]

encodeHeaders :: Headers -> BS.ByteString
encodeHeaders hs = foldr append2 BS.empty $ map (uncurry encodeHeader) hs

dow2ltDow :: Int -> Day
dow2ltDow day =
            case day of
              0 -> Sunday
              1 -> Monday
              2 -> Tuesday
              3 -> Wednesday
              4 -> Thursday
              5 -> Friday
              6 -> Saturday

dtMonth2ltMonth :: Int -> Month
dtMonth2ltMonth dtMonth =
            case dtMonth of
              1 -> January
              2 -> February
              3 -> March
              4 -> April
              5 -> May
              6 -> June
              7 -> July
              8 -> August
              9 -> September
              10 -> October
              11 -> November
              12 -> December

zonedTimeToCalendarTime :: ZonedTime -> CalendarTime
zonedTimeToCalendarTime (ZonedTime (LocalTime day (TimeOfDay h min s)) (TimeZone tzMin tzDst tzName)) =
-- =
    CalendarTime y' m' d h min s' ps wd yd tzName tzSec tzDst
    where (y, m, d)  = toGregorian day
          y' = fromInteger y
          m' = dtMonth2ltMonth m
          s' = fromInteger (round s)
          tzSec = tzMin * 60
          wd = dow2ltDow (read $ formatTime defaultTimeLocale "%w" day)
          ps = 0
          yd = read $ formatTime defaultTimeLocale "%j" day


email2message :: Email -> Message
email2message (Email ef et ecc ebcc es ed eo (ContentByte eb)) = 
              Message fields' eb
              where fromField = From ef
                    toField = To et
                    ccField = Cc ecc
                    bccField = Bcc ebcc
                    subjectField = Subject es
                    optionalFields = map (uncurry OptionalField) eo
                    fields = [fromField] ++ [toField] ++ [ccField] ++ [bccField] ++
                             [subjectField] ++ optionalFields
                    fields' = case ed of
                                   Just date -> 
                                     fields ++ [dateField]
                                     where format = "%a, %d %b %Y %H:%M:%S %z"
                                           dateField = Date (zonedTimeToCalendarTime date)
                                   Nothing -> fields
