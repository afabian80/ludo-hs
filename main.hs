{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import Color

type Position = Int
data Token = Token Color Position

instance Show Token where
        show (Token color pos) = show color ++ "-" ++ show pos

instance Eq Token where
        (Token ca posa) == (Token cb posb) = ca == cb &&  posa == posb

instance Ord Token where
        (Token _ posa) `compare` (Token _ posb) = posa `compare` posb

getColor :: Token -> Color
getColor (Token color _) = color

getPosition :: Token -> Position
getPosition (Token _ position) = position

randomNumbers :: [Int]
randomNumbers = [1, 6, 5, 1, 1, 1, 5, 1, 6, 5, 2, 2, 2, 5, 2, 5, 4, 4, 2, 1, 4, 3, 5, 4, 6, 2, 2, 2, 2, 4, 4, 5, 6, 1, 5, 2, 6, 1, 6, 5, 2, 5, 6, 6, 5, 6, 2, 2, 6, 2, 3, 2, 2, 1, 1, 6, 5, 5, 1, 6, 4, 6, 6, 5, 6, 5, 2, 2, 4, 6, 3, 3, 5, 6, 3, 4, 1, 4, 1, 1, 6, 3, 3, 1, 6, 1, 4, 2, 4, 5, 6, 2, 4, 5, 6, 6, 4, 3, 4, 6]

tokens :: [Token]
tokens = [Token color 0 | color <- concatMap (replicate 4) [Red, Green, Blue, Yellow]]

main :: IO ()
main = do
        reportTokens tokens
        let color = Red
        reportColor color
        let (dice, nextColor, randoms) = roll color randomNumbers
        reportDice dice
        --let pieceToMove = chooseBestPieceFor dice player pieces
        let tokenToMove = farthestToken color tokens
        reportMove tokenToMove dice nextColor
        let steppedPieces = move tokenToMove dice False tokens
        putStrLn "done"

move :: Token -> Int -> Bool -> [Token] -> [Token]
move _ _ _ [] = []
move piece step hasMoved (p:ps)
        | p == piece && not hasMoved = moveSingle p step : move piece step True ps
        | otherwise = p : move piece step hasMoved ps

reportDice :: Int -> IO ()
reportDice dice =
        putStrLn $ "Dice is " ++ show dice

reportTokens :: [Token] -> IO ()
reportTokens pcs =
        putStrLn $ "Board is " ++ show pcs

reportMove :: Token -> Int -> Color -> IO ()
reportMove piece dice nextColor =
        putStrLn $ show piece ++
        " is about to move " ++
        show dice ++ " steps, " ++
        "next color is " ++
        show nextColor

roll :: Color -> [Int] -> (Int, Color, [Int])
roll color randoms = (number, nextColorFor number color, tail randoms)
        where number = head randoms

-- chooseBestPieceFor :: Int -> Player -> [Piece] -> Piece
-- chooseBestPieceFor dice player ps = farthestPiece player ps

farthestToken :: Color -> [Token] -> Token
farthestToken color ts = farthest ownTokens
        where
                ownTokens = filter (not . isEnemy color) ts

farthest :: [Token] -> Token
farthest = maximum

moveSingle :: Token -> Int -> Token
moveSingle (Token color position) step
        | position == 0 = Token color home
        | otherwise = Token color (position + step)
        where
                home = case color of
                        Red    -> 1
                        Green  -> 40
                        Blue   -> 14
                        Yellow -> 27

-- move :: [Piece] -> Player -> Int -> [Piece]
-- move [] _ _ = []
-- move (p:ps) player step
--         | getPlayer p == player = moveSingle p step : move ps player step
--         | otherwise = p : move ps player step

tokensOnField :: [Token] -> Int -> [Token]
tokensOnField ts n = filter (\piece -> getPosition piece == n) ts

isEnemy :: Color -> Token -> Bool
isEnemy c1 (Token c2 _) = c1 /= c2

enemies :: Color -> [Token] -> [Token]
enemies color = filter (isEnemy color)

-- collide :: [Piece] -> Player -> Int -> Bool
-- collide ts c n =
