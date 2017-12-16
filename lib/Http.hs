module Http
  ( Request
  , parseRequest
  ) where


import Control.Applicative
import Data.CaseInsensitive
import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))


type CfStringParser a = Parsec.Parsec String () a

data Method
  = Get
  | Head
  | Post
  | Put
  | Delete
  | Connect
  | Options
  | Trace
  | Extension String
  deriving Show


data RequestLine = RequestLine Method String String deriving Show
data HeaderField = HeaderField String String deriving Show
data Request = Request RequestLine [HeaderField] String deriving Show


-- HTTP Primatives


space :: CfStringParser String 
space  = Parsec.string " " <?> "A Single Space"


crlf :: CfStringParser String
crlf = Parsec.string "\r\n" <?> "CRLF"


ows :: CfStringParser String
ows = (Parsec.many $ Parsec.oneOf [' ', '\t']) <?> "Optional Whitespace"


tchar :: CfStringParser Char
tchar = 
  ( Parsec.oneOf ['!', '#', '$', '%', '&', '\'', '*', '+', '-', '.', '^', '_', '`', '|', '~']
    <|> Parsec.letter
    <|> Parsec.digit
  ) <?> "A Legal Token Character"


token :: CfStringParser String
token = (Parsec.many1 tchar) <?> "A Token"


vchar :: CfStringParser Char
vchar = Parsec.oneOf ['\0033' .. '\0126']


-- Request Line


stringToMethod :: String -> Method
stringToMethod s
  | s ^== "GET" = Get
  | s ^== "HEAD" = Head
  | s ^== "POST" = Post
  | s ^== "PUT" = Put
  | s ^== "DELETE" = Delete
  | s ^== "CONNECT" = Connect
  | s ^== "OPTIONS" = Options
  | s ^== "TRACE" = Trace
  | otherwise = Extension s


method :: CfStringParser Method
method = fmap stringToMethod ((Parsec.many1 Parsec.letter) <?> "An HTTP Verb")


requestTarget :: CfStringParser String
requestTarget = (Parsec.many1 $ Parsec.noneOf [' ']) <?> "A Request Target"


httpVersion :: CfStringParser String
httpVersion = (Parsec.many1 $ Parsec.noneOf [' ', '\r', '\n']) <?> "An HTTP Version"


requestLine :: CfStringParser RequestLine
requestLine = 
  ( RequestLine 
    <$> (method <* space)
    <*> (requestTarget <* space)
    <*> (httpVersion <* crlf)
  ) <?> "A Request Line"


-- Headers


fieldName :: CfStringParser String
fieldName = token <?> "A Header Field Name"


fieldValue :: CfStringParser String
fieldValue = 
  ( (:) 
    <$> vchar 
    <*> (Parsec.many $ Parsec.oneOf[' ', '\t'] <|> vchar)
  ) <?> "A Field Value"


header :: CfStringParser HeaderField
header = 
  ( HeaderField 
    <$> (fieldName <* Parsec.char ':')
    <*> (ows *> fieldValue <* ows)
  ) <?> "A Header"


-- Top Level


body :: CfStringParser String
body = (Parsec.many Parsec.anyChar) <?> "A Body"


request :: CfStringParser Request
request = 
  ( Request 
    <$> requestLine
    <*> (Parsec.many $ header <* crlf)
    <*> (crlf *> body)
  ) <?> "An HTTP Request"


parseRequest :: String -> Either Parsec.ParseError Request
parseRequest = Parsec.parse request "(HttpRequest)"