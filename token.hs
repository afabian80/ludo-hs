module Token where

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

move :: Token -> Int -> Bool -> [Token] -> [Token]
move _ _ _ [] = []
move piece step hasMoved (p:ps)
        | p == piece && not hasMoved = moveSingle p step : move piece step True ps
        | otherwise = p : move piece step hasMoved ps

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

farthestToken :: Color -> [Token] -> Token
farthestToken color ts = farthest ownTokens
        where
                ownTokens = filter (not . isEnemy color) ts

farthest :: [Token] -> Token
farthest = maximum

tokensOnField :: [Token] -> Int -> [Token]
tokensOnField ts n = filter (\piece -> getPosition piece == n) ts

isEnemy :: Color -> Token -> Bool
isEnemy c1 (Token c2 _) = c1 /= c2

enemies :: Color -> [Token] -> [Token]
enemies color = filter (isEnemy color)
