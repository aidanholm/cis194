{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

{-Exercise 1-}

parseMessageType :: String -> MessageType

parseMessageType s
    | ch == 'I' = Info
    | ch == 'W' = Warning
    | ch == 'E' = Error (read $ words s !! 1 :: Int)
    | otherwise    = error "Bad log message prefix character!"
    where ch = s !! 0

parseMessageTimestamp :: String -> TimeStamp

parseMessageTimestamp s = read (f $ (i . parseMessageType) s)
    where f n = words s !! n
          i (Error _) = 2
          i _ = 1

parseMessageContent :: String -> String

parseMessageContent s = unwords . drop ((i . parseMessageType) s) $ words s
    where i (Error _) = 3
          i _ = 2

parseMessage :: String -> LogMessage

parseMessage s = LogMessage mType mTime mCont
    where mType = parseMessageType s
          mTime = parseMessageTimestamp s
          mCont = parseMessageContent s

parse :: String -> [LogMessage]

parse = map parseMessage . lines

{-Exercise 2-}

insert :: LogMessage -> MessageTree -> MessageTree

insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert (LogMessage oType oTime oContents) (Node left (LogMessage sType sTime sContents) right)
    | oTime < sTime = Node (insert other left) self right
    | oTime > sTime = Node left self (insert other right)
    | otherwise = error "Cannot have duplicate timestamps!"
    where other = LogMessage oType oTime oContents
          self  = LogMessage sType sTime sContents
insert (LogMessage _ _ _) (Node _ (Unknown _) _) = error "Trees cannot contain (Unknown _)"

{-Exercise 3-}

build :: [LogMessage] -> MessageTree

build [] = Leaf
build (x:xs) = insert x $ build xs

{-Exercise 4-}

inOrder :: MessageTree -> [LogMessage]

inOrder Leaf = []
inOrder (Node left message right) = (inOrder left) ++ message : (inOrder right)

{-Exercise 5-}

whatWentWrong :: [LogMessage] -> [String]

whatWentWrong = (map content) . inOrder . build . filter severe 
    where severe (LogMessage (Error n) _ _) = n > 50
          severe _ = False
          content (LogMessage _ _ c) = c
          content (Unknown _) = ""
