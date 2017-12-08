module Todo.Api where

  data Id 
    = Uid Int 
    | Name String deriving Show

  data Request 
    = ListRequest 
    | GetRequest Id deriving Show

  data Response 
    = ListResponse [String]
    | GetResponse String deriving Show

  newtype Error = TodoNotFound Id deriving Show

  handleRequest :: Request -> Either Error Response
  handleRequest ListRequest = Right $ ListResponse ["foo", "bar", "squid"]
  handleRequest (GetRequest id) =
    do
      { todo <- case id of  Uid i | i == 42 -> Right "foo"
                            Name n | n == "foo" -> Right "foo"
                            _ -> Left $ TodoNotFound id
      ; return $ GetResponse todo
      }
