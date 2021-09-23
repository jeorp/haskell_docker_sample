{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

main :: IO ()
main = runSqlite "a.db" $ do
    --runMigration migrateAll

    insert $ Person "John Doe" $ Just 30
    --janeId <- insert $ Person "Jane Doe" Nothing

    --insert $ BlogPost "My fr1st p0st" johnId
    --insert $ BlogPost "One more for good measure" johnId

    oneJohnPost <- selectList [PersonName ==. "John Doe"] []
    liftIO $ print (oneJohnPost :: [Entity Person])

    --john <- get johnId
    --liftIO $ print (john :: Maybe Person)

    --delete janeId
    --deleteWhere [BlogPostAuthorId ==. johnId]