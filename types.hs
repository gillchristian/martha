Server.effectfulHandler
  :: Server.Method
     -> String
     -> (Server.Request -> IO Server.Response)
     -> Server.Handler

Server.statefulHandler
  :: Server.Method
     -> String
     -> (state -> Server.Request -> (state, Server.Response))
     -> Server.StatefulHandler state

-- ---

Server.handlersWithState
  :: state -> [Server.StatefulHandler state] -> Server.Handler
