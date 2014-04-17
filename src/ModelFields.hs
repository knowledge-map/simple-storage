{-# LANGUAGE TemplateHaskell #-}
module ModelFields where

import Database.Persist.TH

data Lock = Locked | Unlocked
    deriving (Eq, Show, Read)
derivePersistField "Lock"

lockFromBool :: Bool -> Lock
lockFromBool True = Locked
lockFromBool False = Unlocked
