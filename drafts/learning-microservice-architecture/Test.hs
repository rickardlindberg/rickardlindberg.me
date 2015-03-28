{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import Data.ByteString.Char8 (ByteString(), pack, unpack, append)
import Data.Time
import System.ZMQ3.Monadic

-- First, let's define the two endpoints for publising and subscribing to
-- messages on the bus.

pubUrl = "inproc://publish"
subUrl = "inproc://subscribe"

-- The bus itself is implemented with a proxy between XPub and XSub sockets.

bus :: ZMQ z () -> [ZMQ z ()] -> ZMQ z ()
bus main nodes = do
    busPub <- socket XPub
    busSub <- socket XSub
    bind busPub subUrl
    bind busSub pubUrl
    async $ proxy busSub busPub Nothing
    mapM_ async nodes
    main

-- Let's define two functions that makes it easier to get Pub and Sub sockets
-- for use on the bus.

getPublisher :: ZMQ z (Socket z Pub)
getPublisher = do
    s <- socket Pub
    connect s pubUrl
    return s

getSubscriber :: ByteString -> ZMQ z (Socket z Sub)
getSubscriber topic = do
    s <- socket Sub
    connect s subUrl
    subscribe s topic
    return s

zmqLog :: String -> ZMQ z ()
zmqLog message = do
    t <- liftIO getCurrentTime
    let x = show t ++ ": " ++ message
    liftIO $ putStrLn x

-- The code bellow is specific to the problem being solved. It uses the bus
-- framework above.

main :: IO ()
main = runZMQ $ bus repl [logger, singleNumbers]

repl :: ZMQ z ()
repl = do
    pub <- getPublisher
    sub <- getSubscriber "conclusion"
    zmqLog "repl online"
    forever $ do
        query <- liftIO getLine
        send pub [] ("query " `append` pack query)

logger :: ZMQ z ()
logger = do
    sub <- getSubscriber ""
    zmqLog "logger online"
    forever $ do
        message <- receive sub
        zmqLog $ "logger: " ++ unpack message

singleNumbers :: ZMQ z ()
singleNumbers = do
    pub <- getPublisher
    sub <- getSubscriber "query"
    zmqLog "single numbers online"
    forever $ do
        query <- receive sub
        send pub [] ("conclusion " `append` pack (show $ parseNum $ unwords $ tail $ words $ unpack query))
    where
        parseNum :: String -> Int
        parseNum = read

publisher :: ZMQ z ()
publisher = do
    pub <- getPublisher
    forever $ do
        send pub [] "hello"
        liftIO $ threadDelay (1 * 1000 * 1000)
