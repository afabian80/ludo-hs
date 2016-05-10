main :: IO ()
main = print "Hello"

data Color = Red | Green | Blue | Yellow deriving (Show, Eq)
type Position = Int
data Token = Token Color Position deriving (Show)

getColor :: Token -> Color
getColor (Token c _) = c

getPosition :: Token -> Position
getPosition (Token _ p) = p

tokens :: [Token]
tokens = map (\c-> Token c 0) [Red, Green, Blue, Yellow]

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
enemies c ts = filter (isEnemy c) ts

-- isHitting :: [Token] -> Color -> Int -> Bool
-- isHitting ts c n =
