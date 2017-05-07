---
title: Learning Microservice Architecture
---

I find Fred George's talks about microservice architecture interesting. He
describes a different way to structure systems: a different architecture that
I've not come across before. What he describes also sounds similar to the actor
model. But the talks only give an overview. How do you implement microservice
architecture in practice?

In this article I will explore microservice architecture by writing small but
complete examples. I'm learning as I go along.

Framework
---------

In order to test my ideas, I will build a small framework. I want to implement
a message bus where anyone can publish and subscribe to messages. That way each
microservice does not have to know about each other. They just need to know
how to connect to the bus.

I'm not sure if you always implement microservice architecture with a message
bus, but from Fred's presentations, I got the feeling that's how he did it.

My implementation will be in Haskell and use ZeroMQ for communication. First up
are necessary imports:

\begin{code}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import Data.ByteString.Char8 (ByteString(), pack, unpack, append)
import Data.Time
import System.Environment
import System.ZMQ3.Monadic
\end{code}

Next, let's define the two endpoints for publishing and subscribing to messages
on the bus.

\begin{code}
pubUrl = "inproc://publish"
subUrl = "inproc://subscribe"
\end{code}

The bus itself is implemented with a proxy between XPub and XSub sockets. When
the XPub and XSub sockets have been bound to their endpoint, the proxy starts
in the background along with any nodes that should be on the bus. The main node
is run in the foreground and should act as the entry point for the bus.

\begin{code}
bus :: ZMQ z () -> [ZMQ z ()] -> ZMQ z ()
bus main nodes = do
    busPub <- socket XPub
    busSub <- socket XSub
    bind busPub subUrl
    bind busSub pubUrl
    async $ proxy busSub busPub Nothing
    mapM_ async nodes
    main
\end{code}

Let's define two functions that make it easier to get Pub and Sub sockets for
use on the bus.

\begin{code}
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
\end{code}

And a function to make logging to the console easier:

\begin{code}
zmqLog :: String -> ZMQ z ()
zmqLog message = do
    t <- liftIO getCurrentTime
    let x = show t ++ ": " ++ message
    liftIO $ putStrLn x
\end{code}

Example usage
-------------

\begin{code}
examplePublisher :: ZMQ z ()
examplePublisher = do
    pub <- getPublisher
    forever $ do
        send pub [] "hello Rickard"
        send pub [] "another message"
        liftIO $ threadDelay (1 * 1000 * 1000)

exampleSubscriber :: ZMQ z ()
exampleSubscriber = do
    sub <- getSubscriber "hello"
    forever $ do
        message <- receive sub
        zmqLog $ unpack message

exampleMain :: IO ()
exampleMain = runZMQ $ bus (forever $ return ()) [examplePublisher, exampleSubscriber]
\end{code}

Kata
----

First, I need a problem to solve. Is microservice architecture only
suited for some kinds of problems? It seems like you want to solve small
problems, like the string calculator kata, within a single microservice. But
let's try to break it up.  How can we partition the string calculator into
multiple microservices?

"" -> 0
"5" -> 5
"2,3" -> 5
"1,2,3" -> 6

Possible actors:

- add two numbers
- split string on numbers

The code bellow is specific to the problem being solved. It uses the bus
framework above.

\begin{code}
kataMain :: IO ()
kataMain = runZMQ $ bus repl [logger, singleNumbers]

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
\end{code}

Putting it together
-------------------

\begin{code}
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["example"] -> exampleMain
        ["kata"] -> kataMain
\end{code}
