module Main where

import Data.Char
import Network.HTTP
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match (tagOpen)

data Broadcaster = BS String | TOKYO_MX String deriving Show

data ProgramInfo = ProgramInfo {
    broadcaster :: String,
    title :: String,
    category :: String,
    description :: String,
    url :: String, 
    date :: String,
    time :: String
} deriving Show


openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

bsParseCore :: [Tag String] -> [Tag String]
bsParseCore html = do
    let week_broadcaster = dropWhile (~/= tag_start_broadcaster)  html
    week_broadcaster
    where 
        tag_start_broadcaster :: Tag String
        tag_start_broadcaster = TagOpen "div" [("class", "broadcaster_box currentDate theday")]
        tag_broadcaster :: Tag String
        tag_broadcaster = TagOpen "div" [("class", "broadcaster_box")]

bsSearchAnime :: String -> ProgramInfo 
bsSearchAnime anime = do
    ProgramInfo {broadcaster="", title="", category="", description="", url="", date="", time=""}

bsListAllProgram :: [Tag String] -> [ProgramInfo]
bsListAllProgram html = do
    [ProgramInfo {broadcaster="", title="", category="", description="", url="", date="", time=""}]


parse :: IO ()
parse = do
    html <- readFile "BS11.html"
    writeFile "temp.txt" $ innerText $ bsParseCore $ parseTags html
    putStrLn "finish"

main :: IO ()
main = do
    parse



    