{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage

parseMessage str = let xs = words str in
    case xs of
        ("I":time:msg) -> LogMessage Info (read time::Int) (unwords msg)
        ("W":time:msg) -> LogMessage Warning (read time::Int) (unwords msg)
        ("E":num:time:msg) -> LogMessage (Error (read num::Int)) (read time::Int) (unwords msg)
        _ -> Unknown (unwords xs)


parse :: String -> [LogMessage]

parse str = map parseMessage (lines str)

insert :: LogMessage -> MessageTree -> MessageTree

insert lmsg@LogMessage{} Leaf = Node Leaf lmsg Leaf
insert lmsg1@(LogMessage _ ts1 _) (Node left lmsg2@(LogMessage _ ts2 _) right)
    | ts1 > ts2 = Node left lmsg2 (insert lmsg1 right)
    | otherwise = Node (insert lmsg1 left) lmsg2 right
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]

inOrder Leaf = []
inOrder (Node left lmsg right) = inOrder left ++ [lmsg] ++ inOrder right



severe :: Int -> LogMessage -> Bool
severe minLvl (LogMessage (Error lvl) _ _)
  | lvl > minLvl  = True
  | otherwise = False
severe _ _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = extractMessage (filter (severe 50) xs)

extractMessage :: [LogMessage] -> [String]
extractMessage (LogMessage _ _ msg : msgs) = msg : extractMessage msgs
extractMessage _ = []
