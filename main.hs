data Player = Red | Green | Blue | Yellow deriving (Show, Eq, Enum)
type Position = Int
data Piece = Piece Player Position deriving (Show)

randomNumbers :: [Int]
randomNumbers = [1, 6, 5, 1, 1, 1, 5, 1, 6, 5, 2, 2, 2, 5, 2, 5, 4, 4, 2, 1, 4, 3, 5, 4, 6, 2, 2, 2, 2, 4, 4, 5, 6, 1, 5, 2, 6, 1, 6, 5, 2, 5, 6, 6, 5, 6, 2, 2, 6, 2, 3, 2, 2, 1, 1, 6, 5, 5, 1, 6, 4, 6, 6, 5, 6, 5, 2, 2, 4, 6, 3, 3, 5, 6, 3, 4, 1, 4, 1, 1, 6, 3, 3, 1, 6, 1, 4, 2, 4, 5, 6, 2, 4, 5, 6, 6, 4, 3, 4, 6]

pieces :: [Piece]
pieces = [Piece player 0 | player <- concatMap (replicate 4) [Red, Green, Blue, Yellow]]

main :: IO ()
main = do
        print pieces
        let player = Red
        let (dice, nextPlayer, randoms) = roll player randomNumbers
        --let pieceToMove = chooseBestPieceFor dice nextPlayer pieces
        putStrLn "done"

roll :: Player -> [Int] -> (Int, Player, [Int])
roll player randoms = (dice, nextPlayerFor dice player, tail randoms)
        where dice = head randoms

nextPlayerFor :: Int -> Player -> Player
nextPlayerFor 6 player = player
nextPlayerFor _ Yellow = Red
nextPlayerFor _ player = succ player

getPlayer :: Piece -> Player
getPlayer (Piece player _) = player

getPosition :: Piece -> Position
getPosition (Piece _ position) = position

moveSingle :: Piece -> Int -> Piece
moveSingle (Piece player position) step = Piece player (position + step)

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
