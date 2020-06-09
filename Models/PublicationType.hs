{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.PublicationType where

import Database.Persist.TH

data PublicationType = Journal | Conference | Editor | Book | BookPart | ReferenceWork | Artifact | Other
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "PublicationType"
