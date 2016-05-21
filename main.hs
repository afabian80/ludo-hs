{-# OPTIONS_GHC -fno-warn-unused-binds #-}

data Player = Red | Green | Blue | Yellow deriving (Eq, Enum)
type Position = Int
data Piece = Piece Player Position

instance Show Player where
        show Red    = "R"
        show Green  = "G"
        show Blue   = "B"
        show Yellow = "Y"

instance Show Piece where
        show (Piece player pos) = show player ++ "-" ++ show pos

instance Eq Piece where
        (Piece pla posa) == (Piece plb posb) = pla == plb &&  posa == posb

instance Ord Piece where
        (Piece _ posa) `compare` (Piece _ posb) = posa `compare` posb

getPlayer :: Piece -> Player
getPlayer (Piece player _) = player

getPosition :: Piece -> Position
getPosition (Piece _ position) = position

randomNumbers :: [Int]
randomNumbers = [1, 6, 5, 1, 1, 1, 5, 1, 6, 5, 2, 2, 2, 5, 2, 5, 4, 4, 2, 1, 4, 3, 5, 4, 6, 2, 2, 2, 2, 4, 4, 5, 6, 1, 5, 2, 6, 1, 6, 5, 2, 5, 6, 6, 5, 6, 2, 2, 6, 2, 3, 2, 2, 1, 1, 6, 5, 5, 1, 6, 4, 6, 6, 5, 6, 5, 2, 2, 4, 6, 3, 3, 5, 6, 3, 4, 1, 4, 1, 1, 6, 3, 3, 1, 6, 1, 4, 2, 4, 5, 6, 2, 4, 5, 6, 6, 4, 3, 4, 6]

pieces :: [Piece]
pieces = [Piece player 0 | player <- concatMap (replicate 4) [Red, Green, Blue, Yellow]]

main :: IO ()
main = do
        reportPieces pieces
        let player = Red
        reportPlayer player
        let (dice, nextPlayer, randoms) = roll player randomNumbers
        reportDice dice
        --let pieceToMove = chooseBestPieceFor dice player pieces
        let pieceToMove = farthestPiece player pieces
        reportMove pieceToMove dice nextPlayer
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

reportPlayer :: Player -> IO ()
reportPlayer player =
        putStrLn $ "Current player is " ++ show player

reportPieces :: [Piece] -> IO ()
reportPieces pcs =
        putStrLn $ "Board is " ++ show pcs

reportMove :: Piece -> Int -> Player -> IO ()
reportMove piece dice nextPlayer =
        putStrLn $ show piece ++
        " is about to move " ++
        show dice ++ " steps, " ++
        "next player is " ++
        show nextPlayer

roll :: Player -> [Int] -> (Int, Player, [Int])
roll player randoms = (number, nextPlayerFor number player, tail randoms)
        where number = head randoms

nextPlayerFor :: Int -> Player -> Player
nextPlayerFor 6 player = player
nextPlayerFor _ Yellow = Red
nextPlayerFor _ player = succ player

-- chooseBestPieceFor :: Int -> Player -> [Piece] -> Piece
-- chooseBestPieceFor dice player ps = farthestPiece player ps

farthestPiece :: Player -> [Piece] -> Piece
farthestPiece player ps = farthest ownPieces
        where
                ownPieces = filter (not . isEnemy player) ps

farthest :: [Piece] -> Piece
farthest = maximum

moveSingle :: Piece -> Int -> Piece
moveSingle (Piece player position) step
        | position == 0 = Piece player home
        | otherwise = Piece player (position + step)
        where
                home = case player of
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

isEnemy :: Player -> Piece -> Bool
isEnemy player1 (Piece player2 _) = player1 /= player2

enemies :: Player -> [Piece] -> [Piece]
enemies player = filter (isEnemy player)

-- collide :: [Piece] -> Player -> Int -> Bool
-- collide ts c n =
