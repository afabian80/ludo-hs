module Color where

data Color = Red | Green | Blue | Yellow deriving (Eq, Enum)

instance Show Color where
        show Red    = "R"
        show Green  = "G"
        show Blue   = "B"
        show Yellow = "Y"

reportColor :: Color -> IO ()
reportColor color =
        putStrLn $ "Current color is " ++ show color

nextColorFor :: Int -> Color -> Color
nextColorFor 6 color = color
nextColorFor _ Yellow = Red
nextColorFor _ color = succ color
