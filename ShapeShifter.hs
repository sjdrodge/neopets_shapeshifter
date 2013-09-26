{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances #-}
module ShapeShifter ( GameBoard
                    , GameShape
                    , GameState(..)
                    , GamePlan(..)
                    , applyShape
                    , ppGameBoard
                    , ppGamePlan
                    , ppGameState
                    , shapeShifter
                    ) where

import Control.Applicative
import Control.Monad
import Data.Aeson((.=), (.:))
import Data.Array.IArray
import Data.Data
import Data.List
import Data.Maybe
import Data.Ord
import Data.Typeable()
import qualified Data.Aeson as J
import qualified Data.Vector as V

type GameBoard = Array BoardIndex Int

type GameShape = GameBoard

data GameState = GameState { modularity :: Int
                           , board :: GameBoard
                           , shapes :: [GameShape]
                           } deriving (Data, Typeable, Show)

data GamePlan = GamePlan (GameState, [BoardIndex])
    deriving (Data, Typeable, Show)

newtype BoardIndex = BoardIndex (Int,Int)
    deriving (Eq, Ord, Ix, Data, Typeable, Show)

ppGameBoard :: GameBoard -> String
ppGameBoard b = unlines ("":[ unwords [ show ( b ! BoardIndex (j, i) )
                                | i <- [0..((snd . f . snd . bounds) b)] 
                                ] 
                                | j <- [0..((fst . f . snd . bounds) b)]
                            ]) where f (BoardIndex (x,y)) = (x,y)

ppGamePlan :: GamePlan -> String
ppGamePlan (GamePlan (st, ixs)) = ppGameState st ++ "\n" ++ (unwords $ map f $ zip (shapes st) ixs)
    where f (sh, BoardIndex (n,m)) = ppGameBoard sh ++ "-\n" ++ show (n + 1, m + 1) ++ "\n"

ppGameState :: GameState -> String
ppGameState st = ppGameBoard b ++ "% " ++ show (modularity st)
                                where b = board st

instance Num BoardIndex where
    (+) (BoardIndex (a, b)) (BoardIndex (c, d)) = BoardIndex (a+c, b+d)
    (*) (BoardIndex (a, b)) (BoardIndex (c, d)) = BoardIndex (a*c, b*d)
    negate (BoardIndex (a, b)) = BoardIndex ((-a), (-b))
    abs (BoardIndex (a, b)) = BoardIndex (abs a, abs b)
    signum (BoardIndex (a, b)) = BoardIndex (signum a, signum b)
    fromInteger x = BoardIndex ( fromIntegral x, fromIntegral x)

instance J.ToJSON BoardIndex where
    toJSON (BoardIndex (x,y)) = (J.Array . V.fromList . map (J.toJSON . (+1)) ) [x,y]

instance J.ToJSON GameBoard where
    toJSON b = J.object [ "dimensions" .= (snd . bounds) b
                        , "data" .= (J.Array . V.fromList . map J.toJSON . elems) b
                        ]

instance J.FromJSON GameBoard where
    parseJSON (J.Object b) = listArray
                             <$> ( listToBoardIndexRange <$> b .: "dimensions" )
                             <*> ( V.toList <$> b .: "data" )

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

possibleIndices :: GameBoard -> GameShape -> [BoardIndex]
possibleIndices b sh = range (mn, mx)
                              where f = (snd . bounds)
                                    mn = (fst . bounds) b
                                    mx = f b - f sh

-- Warning: partial function (fails when shapes is empty) 
possibleStates :: GameState -> [(GameState, BoardIndex)]
possibleStates (GameState { modularity = m, board = b, shapes = (sh:shs) }) = do
    i <- possibleIndices b sh
    nb <- return (applyShape m b sh i)
    return (GameState { modularity = m, board = nb, shapes = shs }, i)

isSolved :: GameBoard -> Bool
isSolved = not . isJust . find (0/=) . elems

applyShape :: Int -> GameBoard -> GameShape -> BoardIndex -> GameBoard
applyShape m b s i = accum f b [ (i + j, s ! j) | j <- range (bounds s) ]
                         where f x y = ( (x + y) `mod` m )

score :: GameState -> Int
score (GameState m b shs)  = foldr f 0 (elems b)
    where f x a = ((m - x) + a) `mod` m

heuristic :: (GameState, BoardIndex) -> (GameState, BoardIndex) -> Ordering
heuristic (st, i) (st', i') = (comparing score) st st'

shapeShifter :: GameState -> Maybe GamePlan
shapeShifter st | null (shapes st) = if isSolved (board st) then Just ( GamePlan (st,[]) ) else Nothing
shapeShifter st = join . find isJust $ map f (sortBy heuristic (possibleStates st))
                                           where f (st', i) = do
                                                 GamePlan (_, ixs) <- shapeShifter st'
                                                 return $ GamePlan (st, (i:ixs))
