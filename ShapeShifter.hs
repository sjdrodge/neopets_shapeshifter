{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module ShapeShifter ( GameBoard
                    , GameShape
                    , GameState(..)
                    , GamePlan
                    , ppGameBoard
                    , ppGamePlan
                    , ppGameState
                    , solve
                    , flips
                    , checksum
                    ) where

import Control.Monad
import Data.Aeson.Types (FromJSON (parseJSON), genericParseJSON, defaultOptions, fieldLabelModifier)
import Data.List
import Data.Ord
import GHC.Generics (Generic)
import Text.Regex (mkRegex, subRegex)
import qualified Data.Vector.Unboxed as U

type BoardIndex = (Int, Int)

type BoardOffset = Int

type BoardEntry = Int

type Modularity = Int

data GameBoard = GameBoard { dimensions :: BoardIndex
                           , boardData :: U.Vector BoardEntry
                           } deriving (Generic, Show)

type GameShape = GameBoard

data GameState = GameState { modularity :: Modularity
                           , board :: GameBoard
                           , shapes :: [GameShape]
                           } deriving (Generic, Show)

type GamePlan = [(GameShape, BoardIndex)]

instance FromJSON GameBoard where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = \str -> subRegex (mkRegex "^boardD") str "d" }

instance FromJSON GameState

rcIndex :: GameBoard -> Int -> Int -> BoardEntry
rcIndex b r c = boardData b U.! (numCols b * r + c)

ppGameBoard :: GameBoard -> String
ppGameBoard b = unlines ("":[ unwords [ show ( rcIndex b i j )
                                | j <- [0 .. (numCols b - 1)]
                                ]
                                | i <- [0 .. (numRows b - 1)]
                            ])

ppGamePlan :: GamePlan -> String
ppGamePlan = unwords . map f
    where f (sh, (n,m)) = ppGameBoard sh ++ "-\n" ++ show (n + 1, m + 1) ++ "\n"

ppGameState :: GameState -> String
ppGameState st = ppGameBoard b ++ "% " ++ show (modularity st)
                                where b = board st

numRows :: GameBoard -> Int
numRows = fst . dimensions

numCols :: GameBoard -> Int
numCols = snd . dimensions

boardSize :: GameBoard -> BoardIndex
boardSize b = (numRows b, numCols b)

possibleIndices :: GameBoard -> GameShape -> [BoardIndex]
possibleIndices b sh = do
    r <- [0 .. (numRows b - numRows sh)]
    c <- [0 .. (numCols b - numCols sh)]
    return (r, c)

applyShape :: Modularity -> GameBoard -> GameShape -> BoardIndex -> GameBoard
applyShape m b s i = b {boardData = U.accum f (boardData b) (shapeIndicesList b s i)}
                         where f x y   = (x + y) `rem` m


shapeIndicesList :: GameBoard -> GameShape -> BoardIndex -> [(BoardOffset, BoardEntry)]
shapeIndicesList b s i = do
    r <- [0 .. (numRows s - 1)]
    c <- [0 .. (numCols s - 1)]
    let (r0, c0) = i
    guard (rcIndex s r c == 1)
    return ( numCols b * (r0 + r) + (c0 + c), 1 )

possiblePlans :: Modularity -> GameBoard -> GameShape -> [(BoardIndex, GameBoard)]
possiblePlans m b s = do
    i <- possibleIndices b s
    let b' = applyShape m b s i
    return (i, b')

isSolved :: GameBoard -> Bool
isSolved = U.all (0==) . boardData

mass :: GameShape -> Int
mass = U.sum . boardData

distance :: Modularity -> GameBoard -> Int
distance m = U.foldr f 0 . boardData
    where f x z = ((m - x) `rem` m) + z

distanceFromMass :: Modularity -> GameBoard -> [GameShape] -> Int
distanceFromMass m b xs = (sum . map mass ) xs - distance m b

pruneAndSort :: Modularity -> [GameShape] -> [(BoardIndex, GameBoard)] -> [(BoardIndex, GameBoard)]
pruneAndSort m ss = sortBy (comparing h) . filter ((0<=) . h)
    where h (_, b) = distanceFromMass m b ss

shapeShifter' :: Modularity -> [GameShape] -> (BoardIndex, GameBoard) -> Maybe GamePlan
shapeShifter' m (s:ss) (i, b) = do
    ret <- shapeShifter m b ss
    return $ (s, i) : ret

shapeShifter :: Modularity -> GameBoard -> [GameShape] -> Maybe GamePlan
shapeShifter _ b ss | null ss = if isSolved b then Just [] else Nothing
shapeShifter m b (s:ss) = msum . map (shapeShifter' m (s:ss)) . pruneAndSort m ss . possiblePlans m b $ s

solve :: GameState -> Maybe GamePlan
solve st = shapeShifter (modularity st) (board st) . sortBy g . sortBy f $ shapes st
    where f x y = comparing mass y x --larger first
          g = comparing (h . boardSize)
          h (r, c) = (rmax - r + 1) * (cmax - c + 1)
          (rmax, cmax) = boardSize . board $ st

flipsAndChecksum :: GameState -> (Int, Int)
flipsAndChecksum st = ( (sum . map mass . shapes) st - distance (modularity st) (board st) )
                      `quotRem` modularity st

flips :: GameState -> Int
flips = fst . flipsAndChecksum

checksum :: GameState -> Int
checksum = snd . flipsAndChecksum
