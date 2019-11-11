module Position
    ( Position(..)
    , rowOf
    , colOf
    , coordString
    , position
    , (+>)
    , start
    ) where

data Position = Position { row :: Int
                         , col :: Int
                         } deriving (Show, Eq)

start :: Position
start = position 1 1

position :: Int -> Int -> Position
position row col = Position { row = row, col = col }

rowOf :: Position -> Int
rowOf Position { row = row } = row

colOf :: Position -> Int
colOf Position { col = col } = col

(+>) :: Position -> Char -> Position
p +> c 
   | c == '\n' = nextRow p
   | otherwise = nextCol p

nextRow :: Position -> Position
nextRow Position { row = row } = position (row+1) 1

nextCol :: Position -> Position
nextCol Position { col = col 
                 , row = row
                 } = position row (col+1)

coordString :: Position -> String
coordString Position { col = col
                     , row = row
                     } = "(" ++ show row ++ ", " ++ show col ++ ")"