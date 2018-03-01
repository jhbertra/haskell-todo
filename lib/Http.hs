module Http
  ( Request
  , parseRequest
  ) where


import Control.Applicative
import Data.CaseInsensitive
import Data.Char
import Data.List
import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))


type CfStringParser a = Parsec.Parsec String () a
type PathSegment = String
type Scheme = String
type Query = String
type UserInfo = String
type Host = String
type Port = Int

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

newtype Path = Path [PathSegment] deriving Show
data Authority = Authority (Maybe UserInfo) Host (Maybe Port) deriving Show
data AbsoluteUri = AbsoluteUri Scheme Authority Path (Maybe Query) deriving Show

data RequestTarget
  = OriginForm Path (Maybe String)
  | AbsoluteForm AbsoluteUri
  | AuthorityForm Authority
  | AsteriskForm
  deriving Show

data Version = Version Int Int deriving Show
data RequestLine = RequestLine Method RequestTarget Version deriving Show
data HeaderField = HeaderField String String deriving Show
data Request = Request RequestLine [HeaderField] (Maybe String) deriving Show


-- Basic


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


idigit :: CfStringParser Int
idigit = fmap digitToInt Parsec.digit


integer :: CfStringParser Int
integer = fmap read (Parsec.many1 Parsec.digit)


pseq :: [CfStringParser a] -> CfStringParser [a]
pseq (x:xs) = (:) <$> x <*> pseq xs


unreserved :: CfStringParser Char
unreserved = Parsec.oneOf ['-', '.', '_', '~'] <|> Parsec.letter <|> Parsec.digit


pctEncoded :: CfStringParser String
pctEncoded = pseq [Parsec.char '%', Parsec.hexDigit, Parsec.hexDigit]


subDelims :: CfStringParser Char
subDelims = Parsec.oneOf ['!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=']


pchar :: CfStringParser String
pchar = 
  ((:[]) <$> (unreserved <|> subDelims <|> Parsec.oneOf [':', '@']) <?> "A Legal Path Character")
  <|> (pctEncoded <?> "A percent-encoded character")


pconcat :: CfStringParser [String] -> CfStringParser String
pconcat = liftA $ intercalate ""


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


absolutePath :: CfStringParser Path
absolutePath =
  ( Path
    <$> (Parsec.many1 $ Parsec.string "/" *> (pconcat $ Parsec.many pchar))
  ) <?> "An absolute path"


query :: CfStringParser String
query = (pconcat $ Parsec.many $ pchar <|> Parsec.string "/" <|> Parsec.string "?") <?> "A query"


originForm :: CfStringParser RequestTarget
originForm = OriginForm <$> absolutePath <*> (Parsec.optionMaybe $ Parsec.string "?" *> query)


scheme :: CfStringParser String
scheme = 
  ( (++) 
    <$> ((:[]) <$> Parsec.letter)
    <*> (Parsec.many $ Parsec.letter <|> Parsec.digit <|> Parsec.oneOf ['+', '-', '.'])
  ) <?> "A Scheme"


userInfo :: CfStringParser UserInfo
userInfo =
  pconcat $ Parsec.many $
    ((:[]) <$> (unreserved <|> subDelims <|> Parsec.oneOf [':']) <?> "A userinfo Character")
    <|> (pctEncoded <?> "A percent-encoded character")


ipLiteral :: CfStringParser String
ipLiteral = Parsec.string "[" *> (ipV6Address <|> ipVFuture) <* Parsec.string "]"


host :: CfStringParser Host
host = ipLiteral <|> ipv4Address <|> regName


authority :: CfStringParser Authority
authority =
  ( Authority
    <$> (Parsec.optionMaybe $ userInfo <* Parsec.string "@")
    <*> host
    <*> (Parsec.optionMaybe $ Parsec.string ":" *> integer)
  )


absoluteUri :: CfStringParser AbsoluteUri
absoluteUri =
  AbsoluteUri
  <$> (scheme <* Parsec.string ":")
  <*> authority
  <*> hierPartPath
  <*> (Parsec.optionMaybe $ Parsec.string "?" *> query)

  
absoluteForm :: CfStringParser RequestTarget
absoluteForm = AbsoluteForm <$> absouteUri


authorityForm :: CfStringParser RequestTarget
authorityForm = (AuthorityForm <$> (Parsec.many1 Parsec.letter)) <?> "An authority-form target"


asteriskForm :: CfStringParser RequestTarget
asteriskForm = AsteriskForm <$ Parsec.char '*'


requestTarget :: CfStringParser RequestTarget
requestTarget =
  originForm
  <|> absoluteForm
  <|> authorityForm
  <|> asteriskForm


httpVersion :: CfStringParser Version
httpVersion = 
  ( Version 
    <$> (Parsec.string "HTTP/" *> idigit <* (Parsec.char '.'))
    <*> idigit
  ) <?> "An HTTP Version of the form HTTP/<major-version>.<minor-version>"


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
fieldValue = (Parsec.many $ Parsec.oneOf[' ', '\t'] <|> vchar) <?> "A Field Value"


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
    <*> (crlf *> (Parsec.optionMaybe body))
  ) <?> "An HTTP Request"


parseRequest :: String -> Either Parsec.ParseError Request
parseRequest = Parsec.parse request "(HttpRequest)"