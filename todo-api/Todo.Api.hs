module Todo.Api where

  data Request = ListRequest deriving Show

  newtype Response = ListResponse [String] deriving Show

  handleRequest :: Request -> Response
  handleRequest ListRequest = ListResponse ["foo", "bar", "squid"]