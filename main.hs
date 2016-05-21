{-# OPTIONS_GHC -fno-warn-unused-binds #-}

data Color = Red | Green | Blue | Yellow deriving (Eq, Enum)
type Position = Int
data Piece = Piece Color Position

instance Show Color where
        show Red    = "R"
        show Green  = "G"
        show Blue   = "B"
        show Yellow = "Y"

instance Show Piece where
        show (Piece color pos) = show color ++ "-" ++ show pos

instance Eq Piece where
        (Piece ca posa) == (Piece cb posb) = ca == cb &&  posa == posb

instance Ord Piece where
        (Piece _ posa) `compare` (Piece _ posb) = posa `compare` posb

getColor :: Piece -> Color
getColor (Piece color _) = color

getPosition :: Piece -> Position
getPosition (Piece _ position) = position

randomNumbers :: [Int]
randomNumbers = [1, 6, 5, 1, 1, 1, 5, 1, 6, 5, 2, 2, 2, 5, 2, 5, 4, 4, 2, 1, 4, 3, 5, 4, 6, 2, 2, 2, 2, 4, 4, 5, 6, 1, 5, 2, 6, 1, 6, 5, 2, 5, 6, 6, 5, 6, 2, 2, 6, 2, 3, 2, 2, 1, 1, 6, 5, 5, 1, 6, 4, 6, 6, 5, 6, 5, 2, 2, 4, 6, 3, 3, 5, 6, 3, 4, 1, 4, 1, 1, 6, 3, 3, 1, 6, 1, 4, 2, 4, 5, 6, 2, 4, 5, 6, 6, 4, 3, 4, 6]

pieces :: [Piece]
pieces = [Piece color 0 | color <- concatMap (replicate 4) [Red, Green, Blue, Yellow]]

main :: IO ()
main = do
        reportPieces pieces
        let color = Red
        reportColor color
        let (dice, nextColor, randoms) = roll color randomNumbers
        reportDice dice
        --let pieceToMove = chooseBestPieceFor dice player pieces
        let pieceToMove = farthestPiece color pieces
        reportMove pieceToMove dice nextColor
        let steppedPieces = move pieceToMove dice False pieces
        putStrLn "done"

move :: Piece -> Int -> Bool -> [Piece] -> [Piece]
move _ _ _ [] = []
move piece step hasMoved (p:ps)
        | p == piece && not hasMoved = moveSingle p step : move piece step True ps
        | otherwise = p : move piece step hasMoved ps

reportDice :: Int -> IO ()
reportDice dice =
        putStrLn $ "Dice is " ++ show dice

reportColor :: Color -> IO ()
reportColor color =
        putStrLn $ "Current color is " ++ show color

reportPieces :: [Piece] -> IO ()
reportPieces pcs =
        putStrLn $ "Board is " ++ show pcs

reportMove :: Piece -> Int -> Color -> IO ()
reportMove piece dice nextColor =
        putStrLn $ show piece ++
        " is about to move " ++
        show dice ++ " steps, " ++
        "next color is " ++
        show nextColor

roll :: Color -> [Int] -> (Int, Color, [Int])
roll color randoms = (number, nextColorFor number color, tail randoms)
        where number = head randoms

nextColorFor :: Int -> Color -> Color
nextColorFor 6 color = color
nextColorFor _ Yellow = Red
nextColorFor _ color = succ color

-- chooseBestPieceFor :: Int -> Player -> [Piece] -> Piece
-- chooseBestPieceFor dice player ps = farthestPiece player ps

farthestPiece :: Color -> [Piece] -> Piece
farthestPiece color ps = farthest ownPieces
        where
                ownPieces = filter (not . isEnemy color) ps

farthest :: [Piece] -> Piece
farthest = maximum

moveSingle :: Piece -> Int -> Piece
moveSingle (Piece color position) step
        | position == 0 = Piece color home
        | otherwise = Piece color (position + step)
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

piecesOnField :: [Piece] -> Int -> [Piece]
piecesOnField ps n = filter (\piece -> getPosition piece == n) ps

isEnemy :: Color -> Piece -> Bool
isEnemy c1 (Piece c2 _) = c1 /= c2

enemies :: Color -> [Piece] -> [Piece]
enemies color = filter (isEnemy color)

-- collide :: [Piece] -> Player -> Int -> Bool
-- collide ts c n =
