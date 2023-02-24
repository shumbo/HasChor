{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Choreography (runChoreography)
import Choreography.Choreo
import Choreography.Location
import Choreography.Network.Http
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.IORef
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy
import GHC.IORef (IORef (IORef))
import GHC.TypeLits (KnownSymbol)
import System.Environment

client :: Proxy "client"
client = Proxy

server :: Proxy "server"
server = Proxy

type State = Map String String

data Request = Put String String | Get String deriving (Show, Read)

type Response = Maybe String

readRequest :: IO Request
readRequest = do
  putStrLn "Command?"
  line <- getLine
  case parseRequest line of
    Just t -> return t
    Nothing -> putStrLn "Invalid command" >> readRequest
  where
    parseRequest :: String -> Maybe Request
    parseRequest s =
      let l = words s
       in case l of
            ["GET", k] -> Just (Get k)
            ["PUT", k, v] -> Just (Put k v)
            _ -> Nothing

kvs :: (Request @ "client", IORef State @ "server") -> Choreo IO (Response @ "client", IORef State @ "server")
kvs (request, stateRef) = do
  request' <- (client, request) ~> server
  response <-
    server `locally` \unwrap -> case unwrap request' of
      Put key value -> do
        modifyIORef (unwrap stateRef) (Map.insert key value)
        return (Just value)
      Get key -> do
        state <- readIORef (unwrap stateRef)
        return (Map.lookup key state)
  response' <- (server, response) ~> client
  return (response', stateRef)

mainChoreo :: Choreo IO ()
mainChoreo = do
  stateRef <- server `locally` \_ -> newIORef (Map.empty :: State)
  loop stateRef
  where
    loop :: IORef State @ "server" -> Choreo IO ()
    loop stateRef = do
      request <- client `locally` \_ -> readRequest
      (response, _) <- kvs (request, stateRef)
      client `locally` \unwrap -> do putStrLn (show (unwrap response))
      loop stateRef

main :: IO ()
main = do
  runChoreo mainChoreo
