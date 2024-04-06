import System.IO
import Data.List
import Data.Bits

data Piece = Empty | Nought | Cross
  deriving (Eq)

instance Show Piece where
  show Empty = " "
  show Nought = "O"
  show Cross = "X"

isValid :: Int -> Bool
isValid x
  | x < 0 || x > 8 = False
  | otherwise = True

showBoard :: [Piece] -> String
showBoard [] = ""
showBoard (x:xs)
  | (length xs) `mod` 3 == 2 && (length xs) /= 8 = "\n" ++ "| " ++ (show x) ++ " |" ++ showBoard xs
  | otherwise = "| " ++ (show x) ++ " |" ++ showBoard xs

verifyIsFree :: [Piece] -> Int -> Bool
verifyIsFree b x
  | b !! x == Empty = True
  | otherwise = False

makeMoveUnsafe :: Int -> Piece -> [Piece] -> [Piece]
makeMoveUnsafe x p b = (take x b) ++ [p] ++ (drop (x+1) b)

makeMove :: Int -> Piece -> [Piece] -> Maybe [Piece]
makeMove x p b = 
  case verifyIsFree b x of
    False -> Nothing
    True -> Just (makeMoveUnsafe x p b)

nextTurn :: Piece -> Piece
nextTurn Cross = Nought
nextTurn Nought = Cross

-- This can probably be simplified with foldl or something similar
binaryRepresentation :: Piece -> [Piece] -> Int -> Int
binaryRepresentation p b 0 = fromEnum (b !! 8 == p)
binaryRepresentation p b i = (.|.) (shift (fromEnum (b !! (8 - i) == p)) i) (binaryRepresentation p b (i - 1))

hasWon :: Piece -> [Piece] -> Bool
hasWon p b = any (\x -> (.&.) x (binaryRepresentation p b 8) == x) [0b111000000, 0b000111000, 0b000000111, 0b100100100, 0b010010010, 0b001001001, 0b100010001, 0b001010100]

isFull :: [Piece] -> Bool
isFull b = (foldl1 (||) $ map (verifyIsFree b) [0..8]) == False

eval :: Piece -> Int
eval Cross = 1
eval Nought = -1

orderTuples :: (Int, Int) -> (Int, Int) -> Ordering
orderTuples x y
  | snd x < snd y = LT
  | snd x > snd y = GT
  | snd x == snd y = EQ

minimax :: [Piece] -> Piece -> (Int, Int)
minimax b p
  | hasWon p b == True = (-1, eval p)
  | isFull b == True = (-1, 0)
  | otherwise = do
    if p == Cross then
      maximumBy orderTuples [(x, snd $ minimax (makeMoveUnsafe x p b) (nextTurn p)) | x <- [0..8], verifyIsFree b x == True]
    else 
      minimumBy orderTuples [(x, snd $ minimax (makeMoveUnsafe x p b) (nextTurn p)) | x <- [0..8], verifyIsFree b x == True]

play :: Piece -> [Piece] -> IO()
play turn board = do
  putStrLn $ showBoard board

  input <- getLine
  let index = read input :: Int

  if isValid index == False then do
    putStrLn "That move is invalid. Please enter a move between 0 and 8"
    play turn board
  else
    case makeMove index turn board of
      Nothing -> do
        putStrLn "Invalid move"
        play turn board
      Just newBoard -> do
        if hasWon turn newBoard then do
          putStrLn $ showBoard newBoard
          end turn
        else
          if isFull newBoard then do
            putStrLn $ showBoard newBoard
            end Empty
          else do
            let ai = fst $ minimax newBoard (nextTurn turn)
            play turn (makeMoveUnsafe ai (nextTurn turn) newBoard)

end :: Piece -> IO ()
end Empty = putStrLn "It was a draw!"
end p = putStrLn $ show p ++ " has won!"

main :: IO ()
main = play Cross (replicate 9 Empty)

