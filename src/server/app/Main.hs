{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char
import Network.HTTP
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match (tagOpen)
import Text.Show.Unicode


data Broadcaster = BS11 | TOKYO_MX | NHK deriving Show

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
    let week_broadcaster =  takeWhile (~/= tag_truncate) . dropWhile (~/= tag_start_broadcaster)
    week_broadcaster html
    where 
        tag_start_broadcaster :: Tag String
        tag_start_broadcaster = TagOpen "div" [("class", "broadcaster_box currentDate theday")]
        tag_truncate :: Tag String
        tag_truncate = TagClose "section"


bsListAllProgram :: [Tag String] -> [ProgramInfo]
bsListAllProgram html = do
    let programs_each_days = map i $ partitions (~== tag_section_day) html
    let programs_each = map (partitions (~== tag_section)) programs_each_days

    [ProgramInfo {title="", category=True, description="", url="", date="", time=""}]
    where
        tag_section_day :: Tag String
        tag_section_day = TagOpen "ul" [("","")]
        tag_section :: Tag String 
        tag_section = TagClose "li"
        i :: [Tag String] -> [Tag String]
        i = takeWhile (~/= tag_trun)
            where 
                tag_trun :: Tag String 
                tag_trun = TagClose "ul"

bsListDay :: (Int, [[Tag String]]) -> [ProgramInfo]
bsListDay (index,tags_) = do
    let filter_tags = filter (\ts -> not $ null ts && fromAttrib "class" (head ts) /= "dummy") $ map (tail . takeWhile (~/= tag_trun)) tags_
    map (h. i) filter_tags
    where
        tag_trun :: Tag String 
        tag_trun = TagClose "li"
        i :: [Tag String] -> (String, String, String, [Tag String])
        i program_tags = do
            let url_ = fromAttrib "href" $ program_tags !! 1
            let time = fromTagText $ program_tags !! 3
            let genre = fromTagText $ program_tags !! 6
            ("https://www.bs11.jp/program/"++url_,time, genre, dropWhile (~/= tag_trun) program_tags)
            where
                tag_trun :: Tag String 
                tag_trun = TagOpen "span" [("class", "title")]
        h :: (String, String, String, [Tag String]) -> ProgramInfo
        h (url,time,genre,program_tags) = do
            let title = fromTagText $ program_tags !! 1
            let description = fromTagText $ program_tags !! 4
            ProgramInfo {title=title, category=True, description=description, url=url, date=show index, time=time}

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
        i = takeWhile (~/= tag_close) 
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
            (tail title, time, "https://s.mxtv.jp/bangumi/" ++url_, dropWhile (~/= tag_about) tags)
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
nhkParseCore = do
    takeWhile (~/= tag_end_table) . dropWhile (~/= tag_start_table) 
    where
        tag_start_table :: Tag String 
        tag_start_table = TagOpen "div" [("class", "inner")]
        tag_end_table :: Tag String
        tag_end_table = TagClose "table"

nhkListAllProgram :: [Tag String] -> [ProgramInfo]
nhkListAllProgram html = do
    let programs_ = partitions (\tag -> isTagOpenName "div" tag && fromAttrib "class" tag == "phead") html
    let programs = map  (takeWhile (~/= tag_trun)) programs_
    map (h. i) programs
    where
        tag_trun :: Tag String 
        tag_trun = TagClose "td"
        i :: [Tag String] -> (String, [Tag String])
        i program_tags = do
            let time = fromTagText $ program_tags !! 3
            let tags_trun = dropWhile (\tag -> isTagOpenName "a" tag && fromAttrib "class" tag == "to-dtl") program_tags
            (time, tags_trun)
        h :: (String, [Tag String]) -> ProgramInfo
        h (time, tag_program) = do 
            let url_ = fromAttrib "href" $ head tag_program
            let title = fromTagText $ tag_program !! 1
            let description = fromTagText $ tag_program !! 8
            ProgramInfo {title=title, category=True, description=description, url=url_, date="", time=time}

nhkIsAnime :: String -> Bool 
nhkIsAnime title = take 3 title == "アニメ"



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



    