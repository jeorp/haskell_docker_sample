module Main where

import Data.Char
import Network.HTTP
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match (tagOpen)


data Broadcaster = BS | TOKYO_MX | NHK deriving Show

data ProgramInfo = ProgramInfo {
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


bsListAllProgram :: [Tag String] -> [ProgramInfo]
bsListAllProgram html = do

    [ProgramInfo {title="", category="", description="", url="", date="", time=""}]
    where
        tag_section_day :: Tag String
        tag_section_day = TagOpen "div" [("data-date-programs", "一日の番組群")]
        tag_section_program :: Tag String
        tag_section_program = TagOpen "div" [("data-program", "番組枠")]



mxParseCore :: [Tag String] -> [Tag String]
mxParseCore html = do
    html
    where
        tag_start_table :: Tag String
        tag_start_table = TagOpen "tr" [("id","t500")]

mxListAllProgram :: [Tag String] -> [ProgramInfo]
mxListAllProgram html = do
    [ProgramInfo {title="", category="", description="", url="", date="", time=""}]
    where
        tag_section :: Tag String
        tag_section = TagOpen "td" [("class", "program_set tb_set_mx1")]

mxIsAnime :: String -> Bool 
mxIsAnime url = do
    True
    where
        tag_genre :: Tag String 
        tag_genre = TagOpen "div" [("class","genre")]

nhkParseCore :: [Tag String] -> [Tag String]
nhkParseCore html = do
    html
    where
        tag_start_table :: Tag String 
        tag_start_table = TagOpen "th" [("class", "time time-7")]

nhkListAllProgram :: [Tag String] -> [ProgramInfo]
nhkListAllProgram html = do
    [ProgramInfo {title="", category="", description="", url="", date="", time=""}]
    where
        tag_section :: Tag String 
        tag_section = TagOpen "td" [("","")]



searchAnime :: String -> [ProgramInfo] -> [ProgramInfo] 
searchAnime anime list = do
    [ProgramInfo {title="", category="", description="", url="", date="", time=""}]

searchDate :: String -> [ProgramInfo] -> [ProgramInfo]
searchDate date list = do
    [ProgramInfo {title="", category="", description="", url="", date="", time=""}]


parse :: IO ()
parse = do
    html <- readFile "BS11.html"
    writeFile "temp.txt" $ innerText $ bsParseCore $ parseTags html
    putStrLn "finish"

main :: IO ()
main = do
    parse



    