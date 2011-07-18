-- © 2003 Peter Thiemann
module WashUtils.RFC2047 where
-- decoding of header fields
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as BsChar8
import Data.Char
import qualified Data.Encoding as DataEnc
import Data.Encoding.ASCII (ASCII(..))
import Data.Encoding (encodeStrictByteString)
import Data.List

import qualified WashUtils.Base64 as Base64
import qualified WashUtils.QuotedPrintable as QuotedPrintable
import qualified WashUtils.Base64 as Base64
import WashUtils.Hex

import Text.ParserCombinators.Parsec

lineString =
  do initial <- many (noneOf "\n\r=")
     rest <- option "" (do xs <- try encoded_words <|> string "="
			   ys <- lineString
			   return (xs ++ ys))
     return (initial ++ rest)

especials = "()<>@,;:\\\"/[]?.="
tokenchar = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" \\ especials
p_token = many1 (oneOf tokenchar)
p_encoded_text = many1 $ oneOf "!\"#$%&'()*+,-./0123456789:;<=>@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
allchar = "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\f\r\SO\SI\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM\SUB\ESC\FS\GS\RS\US !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\DEL"

-- supress linear white space between adjacent encoded_word
encoded_words =
  do ew <- encoded_word
     ws <- many space
     option (ew++ws) (encoded_words >>= \ews -> return (ew++ews))

encoded_word =
  do string "=?"
     charset <- p_token
     char '?'
     encoding <- p_token
     char '?'
     encoded_text <- p_encoded_text
     string "?="
     return $ decode charset (map toUpper encoding) encoded_text

decodeCharset charset text =
--encodeCharset charset text =
    DataEnc.decodeStrictByteString encoding (BsChar8.pack text)
        where encoding = DataEnc.encodingFromString (map toUpper charset)

decode charset "B" encoded_text =
    decodeCharset charset (Base64.decode' encoded_text)
decode charset "Q" encoded_text =
    decodeCharset charset (decode_quoted encoded_text)
decode charset encoding encoded_text =
  error ("Unknown encoding: " ++ encoding)

decode_quoted [] = []
decode_quoted ('=':upper:lower:xs) =
  chr (16 * hexDigitVal upper + hexDigitVal lower) : decode_quoted xs
--decode_quoted ('_':xs) = ' ' : decode_quoted xs
decode_quoted (x:xs) =
  x : decode_quoted xs

-- --------------------------------------------------------------------
-- RFC 2047: encoding of header fields

encodeCharset ::  String -> String -> [Char]
encodeCharset charset text =
    BsChar8.unpack (DataEnc.encodeStrictByteString encoder text)
           where encoder = DataEnc.encodingFromString charset

encodeWord ::  String -> [Char]
encodeWord w =
  "=?" ++ charset ++ "?" ++ encoding ++ "?" ++ encoded ++ "?="
  where encoding = "B"
	charset  = "UTF-8"
        --encoded = QuotedPrintable.encode' converted
        encoded = Base64.encode' converted
        converted = encodeCharset charset w

encodeValue ::  [Char] -> [Char]
encodeValue v =
  case span (not . flip elem "()<>@.!,?") v of
    ([], []) -> []
    (word, []) -> maybeEncode word
    (word, x:rest) -> maybeEncode word ++ x : encodeValue rest

maybeEncode ::  [Char] -> [Char]
maybeEncode word | all p word = word
                 | otherwise = encodeWord word
  where p x = x /= '?' && let ox = ord x in ox >= 33 && ox <= 126

encodeAscii = encodeStrictByteString ASCII

encodeCharsetBS ::  String -> String -> Bs.ByteString
encodeCharsetBS x = encodeAscii .  encodeCharset x

encodeWordBS ::  String -> Bs.ByteString
encodeWordBS = encodeAscii . encodeWord

encodeValueBS ::  [Char] -> Bs.ByteString
encodeValueBS = encodeAscii . encodeValue

maybeEncodeBS ::  [Char] -> Bs.ByteString
maybeEncodeBS = encodeAscii . maybeEncode
