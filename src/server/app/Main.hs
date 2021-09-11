module Main where

import Data.Char
import Network.HTTP
import Text.HTML.TagSoup


openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

haskellLastModifiedDateTime :: IO ()
haskellLastModifiedDateTime = do
    src <- openURL "http://wiki.haskell.org/Haskell"
    let lastModifiedDateTime = fromFooter $ parseTags src
    putStrLn $ "wiki.haskell.org was last modified on " ++ lastModifiedDateTime
    where fromFooter = unwords . drop 6 . words . innerText . take 2 . dropWhile (~/= "<li id=lastmod>")

bsParse :: [Tag String] -> String
bsParse html = do
    "parse bs program"

parse :: IO ()
parse = do
    html <- readFile "BS11.html"
    putStrLn $ bsParse $ parseTags html
    putStrLn "finish"

main :: IO ()
main = do
    parse



    