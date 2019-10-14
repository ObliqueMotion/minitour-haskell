module Position
    ( Position(..)
    , rowOf
    , colOf
    , incRow
    , incCol
    , zeroCol
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

incRow :: Int -> Position -> Position
incRow n Position { col = col
                  , row = row
                  } = position col (row+n)

incCol :: Int -> Position -> Position
incCol n Position { col = col 
                  , row = row
                  } = position (col+n) row

zeroCol :: Position -> Position
zeroCol Position { row = row } = position 0 row

coordString :: Position -> String
coordString Position { col = col
                     , row = row
                     } = "(" ++ show col ++ ", " ++ show row ++ ")"