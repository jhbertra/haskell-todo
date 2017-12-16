module Http
  ( Request
  , parseRequest
  ) where

import Data.List
import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)


type CfStringParser a = Parsec.Parsec String () a


data RequestLine = RequestLine String String String deriving Show
data HeaderField = HeaderField String String deriving Show
data Request = Request RequestLine [HeaderField] String deriving Show


-- HTTP Primatives


space :: CfStringParser String 
space  = Parsec.string " " <?> "A Single Space"


crlf :: CfStringParser String
crlf = Parsec.string "\r\n" <?> "CRLF"


ows :: CfStringParser String
ows = (Parsec.many $ Parsec.oneOf [' ', '\t']) <?> "Optional Whitespace"


rws :: CfStringParser String
rws = (Parsec.many1 $ Parsec.oneOf [' ', '\t']) <?> "Required Whitespace"


tchar :: CfStringParser Char
tchar = (Parsec.oneOf ['!', '#', '$', '%', '&', '\'', '*', '+', '-', '.', '^', '_', '`', '|', '~']
  <|> Parsec.letter
  <|> Parsec.digit)
  <?> "A Legal Token Character"


token :: CfStringParser String
token = (Parsec.many1 tchar) <?> "A Token"


vchar :: CfStringParser Char
vchar = Parsec.oneOf ['\0033' .. '\0126']


-- Request Line


method :: CfStringParser String
method = (Parsec.many1 Parsec.letter) <?> "An HTTP Verb"


requestTarget :: CfStringParser String
requestTarget = (Parsec.many1 $ Parsec.noneOf [' ']) <?> "A Request Target"


httpVersion :: CfStringParser String
httpVersion = (Parsec.many1 $ Parsec.noneOf [' ', '\r', '\n']) <?> "An HTTP Version"


requestLine :: CfStringParser RequestLine
requestLine = (RequestLine 
  <$> (method <* space)
  <*> (requestTarget <* space)
  <*> (httpVersion <* crlf))
  <?> "A Request Line"


fieldName :: CfStringParser String
fieldName = token <?> "A Header Field Name"


fieldValue :: CfStringParser String
fieldValue = ((:) 
  <$> vchar 
  <*> (Parsec.many $ Parsec.oneOf[' ', '\t'] <|> vchar))
  <?> "A Field Value"


header :: CfStringParser HeaderField
header = (HeaderField 
  <$> (fieldName <* Parsec.char ':')
  <*> (ows *> fieldValue <* ows))
  <?> "A Header"


body :: CfStringParser String
body = (Parsec.many Parsec.anyChar) <?> "A Body"


request :: CfStringParser Request
request = (Request 
  <$> requestLine
  <*> (Parsec.many $ header <* crlf)
  <*> (crlf *> body))
  <?> "An HTTP Request"


parseRequest :: String -> Either Parsec.ParseError Request
parseRequest = Parsec.parse request "(HttpRequest)"