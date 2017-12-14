module Http
  ( Request
  , parseRequest
  ) where

import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)


data RequestLine = RequestLine String String String deriving Show


data Request = Request RequestLine [String] String deriving Show


space :: Parsec.Parsec String () String 
space  = Parsec.string " " <?> "A Single Space"


crlf :: Parsec.Parsec String () String
crlf = Parsec.string "\r\n" <?> "CRLF"


method :: Parsec.Parsec String () String
method = Parsec.many1 Parsec.letter


requestTarget :: Parsec.Parsec String () String
requestTarget = Parsec.many1 $ Parsec.noneOf [' ']


httpVersion :: Parsec.Parsec String () String
httpVersion = Parsec.many1 $ Parsec.noneOf [' ', '\r', '\n']


requestLine :: Parsec.Parsec String () RequestLine
requestLine = RequestLine 
  <$> (method <* space)
  <*> (requestTarget <* space)
  <*> (httpVersion <* crlf)


header :: Parsec.Parsec String () String
header = Parsec.many1 $ Parsec.noneOf ['\r','\n']


body :: Parsec.Parsec String () String
body = Parsec.many Parsec.anyChar


request :: Parsec.Parsec String () Request
request = Request 
  <$> (requestLine <?> "Request Line")
  <*> (Parsec.many ((header <?> "Request Header") <* crlf))
  <*> (crlf *> body <?> "Request Body")


parseRequest :: String -> Either Parsec.ParseError Request
parseRequest = Parsec.parse request "(HttpRequest)"