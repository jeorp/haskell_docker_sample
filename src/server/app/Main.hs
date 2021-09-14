{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char
import qualified Data.ByteString.Char8 as C
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T 
import Network.HTTP
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match (tagOpen)
import Data.Bool (bool)
import Text.Show.Unicode


data Broadcaster = BS | TOKYO_MX | NHK deriving Show

data ProgramInfo = ProgramInfo {
    title :: String,
    category :: Bool,
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

    [ProgramInfo {title="", category=True, description="", url="", date="", time=""}]
    where
        tag_section_day :: Tag String
        tag_section_day = TagOpen "div" [("data-date-programs", "一日の番組群")]
        tag_section_program :: Tag String
        tag_section_program = TagOpen "div" [("data-program", "番組枠")]



mxParseCore :: [Tag String] -> [Tag String]
mxParseCore html = do
    takeWhile (~/= tag_truncate) $ dropWhile (~/= tag_start_table)  html
    where
        tag_start_table :: Tag String
        tag_start_table = TagOpen "tr" [("id","t500")]
        tag_truncate :: Tag String 
        tag_truncate = TagClose "tbody"

mxListAllProgram :: [Tag String] -> [ProgramInfo]
mxListAllProgram html = do
    let program = map (f . g . h . i) $ partitions (~== tag_section) html
    program 

    where
        tag_section :: Tag String
        tag_section = TagOpen "td" [("class", "program_set tb_set_mx1")]
        i :: [Tag String] -> [Tag String]
        i tags = takeWhile (~/= tag_close) tags
            where
                tag_close :: Tag String 
                tag_close = TagClose "td"
        h :: [Tag String] -> (String, [Tag String])
        h tags =  do
            let time = fromTagText $ tags !! 3
            let program_part = dropWhile (\tag -> (~/= tag1) tag && (~/= tag2) tag) tags 
            (show time, program_part)
            where
                tag1 :: Tag String 
                tag1 = TagOpen "div" [("class", "title")]
                tag2 :: Tag String 
                tag2 = TagOpen "div" [("class", "title title_long")]
        g :: (String, [Tag String]) -> (String, String, String,[Tag String])
        g (time, tags) = do
            let size = length tags
            let url_ = fromAttrib "href" $ tags !! 1
            let title = ushow $ fromTagText $ tags !! 3
            (title, time, "https://s.mxtv.jp/bangumi/" ++url_, dropWhile (~/= tag_about) tags)
            where
                tag_about :: Tag String 
                tag_about = TagOpen "div" [("class", "about")]
        f :: (String, String, String, [Tag String]) -> ProgramInfo
        f (title, time, url, tags) = do
            let about = if not (null tags)
                            then fromTagText $ tags !! 1
                            else ""
            --let is_animme = mxIsAnime tags
            ProgramInfo {title=title, category=True, description=ushow about, url=url, date="", time=time}

mxIsAnime :: [Tag String] -> Bool 
mxIsAnime html = do
    let genre = innerText $ dropWhile (~/= tag_genre) html
    genre == "アニメ"
    where
        tag_genre :: Tag String 
        tag_genre = TagOpen "div" [("class","genre")]

nhkParseCore :: [Tag String] -> [Tag String]
nhkParseCore html = do
    dropWhile (~/= tag_start_table)  html
    where
        tag_start_table :: Tag String 
        tag_start_table = TagOpen "th" [("class", "time time-7")]

nhkListAllProgram :: [Tag String] -> [ProgramInfo]
nhkListAllProgram html = do
    [ProgramInfo {title="", category=True, description="", url="", date="", time=""}]
    where
        tag_section :: Tag String 
        tag_section = TagOpen "td" [("","")]



searchAnime :: String -> [ProgramInfo] -> [ProgramInfo] 
searchAnime anime list = do
    [ProgramInfo {title="", category=True, description="", url="", date="", time=""}]

searchDate :: String -> [ProgramInfo] -> [ProgramInfo]
searchDate date list = do
    [ProgramInfo {title="", category=True, description="", url="", date="", time=""}]


parse :: IO ()
parse = do
    html <- readFile "TOKYOMX_20210915.html"
    writeFile "temp.txt" $  ushow  $ mxListAllProgram $ mxParseCore $ parseTags html
    putStrLn "finish" 

main :: IO ()
main = do
    parse



    