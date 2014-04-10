{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database where

import Data.Text (Text)
import Database.Persist.TH
import Database.Persist.MongoDB hiding (master)
import Language.Haskell.TH.Syntax

let mongoSettings =
        (mkPersistSettings (ConT ''MongoBackend)) {
            mpsGeneric = False
         }
 in share [mkPersist mongoSettings] [persistLowerCase|
Graph
    config Text
    deriving Show
|]
