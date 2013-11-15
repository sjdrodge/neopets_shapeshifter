{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}
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

import Control.Applicative
import Control.Monad
import Data.Aeson ((.=), (.:))
import Data.List
import Data.List.Split
import Data.Ord
import qualified Data.Aeson as J
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

data GameBoard = GameBoard { dimensions :: BoardIndex
                           , boardData :: V.Vector (U.Vector Int)
                           }

type GameShape = GameBoard

type Modularity = Int

data GameState = GameState { modularity :: Modularity
                           , board :: GameBoard
                           , shapes :: [GameShape]
                           }

type GamePlan = [(GameShape, BoardIndex)]

newtype BoardIndex = BoardIndex (Int,Int)
    deriving (Eq, Ord, Show)

ppGameBoard :: GameBoard -> String
ppGameBoard b = unlines ("":[ unwords [ show ( boardData b V.! i U.! j )
                                | j <- [0 .. (numCols b - 1)]
                                ]
                                | i <- [0 .. (numRows b - 1)]
                            ])

ppGamePlan :: GamePlan -> String
ppGamePlan = unwords . map f
    where f (sh, BoardIndex (n,m)) = ppGameBoard sh ++ "-\n" ++ show (n + 1, m + 1) ++ "\n"

ppGameState :: GameState -> String
ppGameState st = ppGameBoard b ++ "% " ++ show (modularity st)
                                where b = board st

instance Num BoardIndex where
    (+) (BoardIndex (a, b)) (BoardIndex (c, d)) = BoardIndex (a+c, b+d)
    (*) (BoardIndex (a, b)) (BoardIndex (c, d)) = BoardIndex (a*c, b*d)
    negate (BoardIndex (a, b)) = BoardIndex (-a, -b)
    abs (BoardIndex (a, b)) = BoardIndex (abs a, abs b)
    signum (BoardIndex (a, b)) = BoardIndex (signum a, signum b)
    fromInteger x = BoardIndex (fromIntegral x, fromIntegral x)

instance J.ToJSON BoardIndex where
    toJSON (BoardIndex (x,y)) = (J.Array . V.fromList . map (J.toJSON . (+1)) ) [x,y]

instance J.ToJSON GameBoard where
    toJSON b = J.object [ "dimensions" .= (numRows b, numCols b)
                        , "data" .= (J.Array . V.fromList . concatMap (map J.toJSON) . map U.toList . V.toList) (boardData b)
                        ]

instance J.FromJSON GameBoard where
    parseJSON (J.Object b) = GameBoard
                             <$> ( BoardIndex <$> b .: "dimensions" )
                             <*> fmap (V.fromList . map U.fromList)
                                 ( chunksOf
                                   <$> ( ( (\(BoardIndex(_, y)) -> y + 1) . snd . listToBoardIndexRange ) <$> b .: "dimensions" )
                                   <*> ( b .: "data" )
                                 )

instance J.ToJSON GameState where
    toJSON (GameState { modularity = m, board = b, shapes = shs }) = J.object [ "modularity" .= m
                                                                              , "board" .= b
                                                                              , "shapes" .= shs
                                                                              ]

instance J.FromJSON GameState where
    parseJSON (J.Object b) = GameState
                             <$> b .: "modularity"
                             <*> b .: "board"
                             <*> ( V.toList <$> b .: "shapes" )

listToBoardIndexRange :: [Int] -> (BoardIndex, BoardIndex)
listToBoardIndexRange (x:y:[]) = (BoardIndex (0, 0), BoardIndex (x-1, y-1))

numRows :: GameBoard -> Int
numRows = (\ (BoardIndex (r,_)) -> r) . dimensions

numCols :: GameBoard -> Int
numCols = (\ (BoardIndex (_,c)) -> c) . dimensions

boardSize :: GameBoard -> BoardIndex
boardSize b = BoardIndex (numRows b, numCols b)

possibleIndices :: GameBoard -> GameShape -> [BoardIndex]
possibleIndices b sh = do
    r <- [0 .. (numRows b - numRows sh)]
    c <- [0 .. (numCols b - numCols sh)]
    return $ BoardIndex (r, c)

applyShape :: Modularity -> GameBoard -> GameShape -> BoardIndex -> GameBoard
applyShape m b s i = b {boardData = V.fromList . map U.fromList . chunksOf (numCols b) . V.toList $ flatvec}
                         where f x y   = (x + y) `rem` m
                               flatvec = V.accum f (V.concatMap V.convert $ boardData b) (shapeIndicesList b s i)


shapeIndicesList :: GameBoard -> GameShape -> BoardIndex -> [(Int, Int)]
shapeIndicesList b s i = do
    r <- [0 .. (numRows s - 1)]
    c <- [0 .. (numCols s - 1)]
    let BoardIndex (r0, c0) = i
    guard (boardData s V.! r U.! c == 1)
    return ( numCols b * (r0 + r) + (c0 + c), 1 )

possiblePlans :: Modularity -> GameBoard -> GameShape -> [(BoardIndex, GameBoard)]
possiblePlans m b s = do
    i <- possibleIndices b s
    let b' = applyShape m b s i
    return (i, b')

isSolved :: GameBoard -> Bool
isSolved = V.all (U.all (0==)) . boardData

mass :: GameShape -> Int
mass = V.sum . V.map U.sum . boardData

distance :: Modularity -> GameBoard -> Int
distance m = V.sum . V.map (U.foldr f 0) . boardData
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
          h (BoardIndex (r, c)) = (rmax - r + 1) * (cmax - c + 1)
          BoardIndex (rmax, cmax) = boardSize . board $ st

flipsAndChecksum :: GameState -> (Int, Int)
flipsAndChecksum st = ( (sum . map mass . shapes) st - distance (modularity st) (board st) )
                      `quotRem` modularity st

flips :: GameState -> Int
flips = fst . flipsAndChecksum

checksum :: GameState -> Int
checksum = snd . flipsAndChecksum
