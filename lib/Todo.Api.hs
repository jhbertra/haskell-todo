module Todo.Api where


  data TodoDto = TodoDto
    { name' :: String
    , description' :: String
    } deriving Show


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
    | GetRequest Id
    | CreateRequest TodoDto deriving Show


  data Response
    = ListResponse [Todo]
    | GetResponse Todo
    | CreateResponse Todo deriving Show


  data Error 
    = TodoNotFound Id
    | InvalidName String
    | DuplicateName String deriving Show


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

  handleRequest (CreateRequest TodoDto {name' = name, description' = description})
    | name == "" = Left $ InvalidName name
    | name == "foo" = Left $ DuplicateName name
    | otherwise =
        Right $
          CreateResponse Todo
            { uid = 42
            , name = name
            , description = description
            , completed = False
            }