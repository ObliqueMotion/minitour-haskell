module Position
    ( Position(..)
    , rowOf
    , colOf
    , coordString
    ) where

data Position = Position { col :: Int
                         , row :: Int
                         } deriving (Show)

rowOf :: Position -> Int
rowOf Position { row = row } = row

colOf :: Position -> Int
colOf Position { col = col } = col

coordString :: Position -> String
coordString Position { col = col
                     , row = row
                     } = "(" ++ show col ++ ", " ++ show row ++ ")"