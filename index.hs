import System.IO
import Data.List

data Piece = Empty | Nought | Cross
  deriving (Eq)

instance Show Piece where
  show Empty = " "
  show Nought = "O"
  show Cross = "X"

showBoard :: [Piece] -> String
showBoard [] = ""
showBoard (x:xs)
  | (length xs) `mod` 3 == 2 && (length xs) /= 8 = "\n" ++ "| " ++ (show x) ++ " |" ++ showBoard xs
  | otherwise = "| " ++ (show x) ++ " |" ++ showBoard xs

verifyIsFree :: Integer -> [Piece] -> Maybe Integer
verifyIsFree x b
  | x < 0 = Nothing
  | x >= 8 = Nothing
  | b !! (fromIntegral x) == Empty = Just x
  | otherwise = Nothing

makeMove :: Integer -> Piece -> [Piece] -> Maybe [Piece]
makeMove x p b = 
  case verifyIsFree x b of
    Nothing -> Nothing
    Just i -> Just ((take (fromIntegral i) b) ++ [p] ++ (drop (fromIntegral (i+1)) b))

nextTurn :: Piece -> Piece
nextTurn Cross = Nought
nextTurn Nought = Cross

play :: Piece -> [Piece] -> IO()
play turn board = do
  putStrLn $ showBoard board
  input <- getLine
  let index = read input :: Integer
  case makeMove index turn board of
    Nothing -> do
      putStrLn "Invalid move"
      play turn board
    Just newBoard -> do
      play (nextTurn turn) newBoard

main :: IO ()
main = do
  play Cross (replicate 9 Empty)

