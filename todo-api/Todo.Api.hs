module Todo.Api where

  data Id 
    = Uid Int 
    | Name String deriving Show

  data Request 
    = ListRequest 
    | GetRequest Id deriving Show

  data Response 
    = ListResponse [String]
    | GetResponse (Maybe String) deriving Show

  handleRequest :: Request -> Response
  handleRequest ListRequest = ListResponse ["foo", "bar", "squid"]
  handleRequest (GetRequest id) =
    GetResponse $
      case id of
        Uid i -> if i == 42 then Just "foo" else Nothing
        Name n -> if n == "foo" then Just "foo" else Nothing
