module Todo.Api where


  data Todo = Todo
    { uid :: Int
    , name :: String
    , description :: String
    , completed :: Bool
    } deriving Show


  data Id
    = Uid Int
    | Name String deriving Show


  data Request
    = ListRequest
    | GetRequest Id deriving Show


  data Response
    = ListResponse [Todo]
    | GetResponse Todo deriving Show


  newtype Error = TodoNotFound Id deriving Show


  handleRequest :: Request -> Either Error Response
  handleRequest ListRequest =
      Right $
        ListResponse
          [ Todo
            { uid = 1
            , name = "foo"
            , description = "Something about foo"
            , completed = False
            }
          ]

  handleRequest (GetRequest id) =
    do
      { todo <-
        if case id of
          Uid i | i == 42 -> True
          Name n | n == "foo" -> True
          _ -> False
        then
          Right Todo
            { uid = 42
            , name = "foo"
            , description = "Something about foo"
            , completed = False
            }
        else
          Left $ TodoNotFound id
      ; return $ GetResponse todo
      }
