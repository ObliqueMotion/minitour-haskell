module Position
    ( Position(..)
    , rowOf
    , colOf
    , coordString
    , position
    ) where

data Position = Position { col :: Int
                         , row :: Int
                         } deriving (Show)

position :: Int -> Int -> Position
position col row = Position { col = col, row = row }

rowOf :: Position -> Int
rowOf Position { row = row } = row

colOf :: Position -> Int
colOf Position { col = col } = col

coordString :: Position -> String
coordString Position { col = col
                     , row = row
                     } = "(" ++ show col ++ ", " ++ show row ++ ")"