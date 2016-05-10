main :: IO ()
main = do
        --let ts = tokens
        let allTokens = move tokens Red 3
        putStrLn "All tokens:"
        print allTokens
        putStrLn ""

        let tokensAtHome = tokensOnField allTokens 0
        putStrLn "Tokens at home:"
        print tokensAtHome
        putStrLn ""

        let redEnemies = enemies Red allTokens
        putStrLn "Enemies of Red:"
        print redEnemies

data Color = Red | Green | Blue | Yellow deriving (Show, Eq)
type Position = Int
data Token = Token Color Position deriving (Show)

getColor :: Token -> Color
getColor (Token c _) = c

getPosition :: Token -> Position
getPosition (Token _ p) = p

tokens :: [Token]
tokens = [Token c 0 | c <- concatMap (replicate 4) [Red, Green, Blue, Yellow]]

moveSingle :: Token -> Int -> Token
moveSingle (Token c p) n = Token c (p + n)

move :: [Token] -> Color -> Int -> [Token]
move [] _ _ = []
move (x:xs) c n
        | getColor x == c = moveSingle x n : move xs c n
        | otherwise = x : move xs c n

tokensOnField :: [Token] -> Int -> [Token]
tokensOnField xs n = filter (\t -> getPosition t == n) xs

isEnemy :: Color -> Token -> Bool
isEnemy c1 (Token c2 _) = c1 /= c2

enemies :: Color -> [Token] -> [Token]
enemies c = filter (isEnemy c)

-- isHitting :: [Token] -> Color -> Int -> Bool
-- isHitting ts c n =
