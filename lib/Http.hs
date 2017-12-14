module Http
  ( Request
  , parseRequest
  ) where

import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)

data Request = Request String [String] String deriving Show


requestLine :: Parsec.Parsec String () String
requestLine = Parsec.many1 <$> Parsec.noneOf $ ['\r','\n']


header :: Parsec.Parsec String () String
header = Parsec.many1 <$> Parsec.noneOf $ ['\r','\n']


crlf :: Parsec.Parsec String () String
crlf = Parsec.string "\r\n" <?> "CRLF"


body :: Parsec.Parsec String () String
body = Parsec.many Parsec.anyChar


request :: Parsec.Parsec String () Request
request = Request 
  <$> ((requestLine <?> "Request Line") <* crlf) 
  <*> (Parsec.many ((header <?> "Request Header") <* crlf))
  <*> (crlf *> body <?> "Request Body")


parseRequest :: String -> Either Parsec.ParseError Request
parseRequest = Parsec.parse request "(HttpRequest)"