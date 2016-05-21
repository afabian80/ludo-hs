{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import Color
import Token

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

reportDice :: Int -> IO ()
reportDice dice =
        putStrLn $ "Dice is " ++ show dice

roll :: Color -> [Int] -> (Int, Color, [Int])
roll color randoms = (number, nextColorFor number color, tail randoms)
        where number = head randoms
