{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module ShapeShifter ( GameBoard
                    , GameShape
                    , GameState(..)
                    , GamePlan(..)
                    , ppGameBoard
                    , ppGamePlan
                    , ppGameState
                    , solve
                    ) where

import Control.Applicative
import Control.Monad
import Data.Aeson((.=), (.:))
import Data.Array.IArray
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Aeson as J
import qualified Data.Vector as V

type GameBoard = Array BoardIndex Int

type GameShape = GameBoard

type Modularity = Int

data GameState = GameState { modularity :: Modularity
                           , board :: GameBoard
                           , shapes :: [GameShape]
                           } deriving (Show)

data GamePlan = GamePlan { gboard :: GameBoard
                         , shapesToLocs :: [(GameShape, BoardIndex)]
                         } deriving (Show)

newtype BoardIndex = BoardIndex (Int,Int)
    deriving (Eq, Ord, Ix, Show)

ppGameBoard :: GameBoard -> String
ppGameBoard b = unlines ("":[ unwords [ show ( b ! BoardIndex (j, i) )
                                | i <- [0..((snd . f . snd . bounds) b)]
                                ]
                                | j <- [0..((fst . f . snd . bounds) b)]
                            ]) where f (BoardIndex (x,y)) = (x,y)

ppGamePlan :: GamePlan -> String
ppGamePlan (GamePlan {gboard = b, shapesToLocs = stl}) = ppGameBoard b ++ "----\n" ++ (unwords . map f $ stl)
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
    fromInteger x = BoardIndex (fromIntegral x, fromIntegral x)

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

applyShape :: Modularity -> GameBoard -> GameShape -> BoardIndex -> GameBoard
applyShape m b s i = accum f b [ (i + j, s ! j) | j <- range (bounds s) ]
                         where f x y = ( (x + y) `mod` m )

possiblePlans :: Modularity -> GameBoard -> GameShape -> [GamePlan]
possiblePlans m b s = do
    i <- possibleIndices b s
    let b' = (applyShape m b s i)
    return $ GamePlan {gboard = b', shapesToLocs = [(s,i)]}

isSolved :: GameBoard -> Bool
isSolved = not . isJust . find (0/=) . elems

mass :: GameShape -> Int
mass = sum . elems

distance :: Modularity -> GameBoard -> Int
distance m b  = foldr f 0 (elems b)
    where f x z = ((m - x) `mod` m) + z

distanceFromMass :: Modularity -> GameBoard -> [GameShape] -> Int
distanceFromMass m b xs = (sum . map mass ) xs - distance m b

shapeShifter :: Modularity -> GameBoard -> [GameShape] -> Maybe GamePlan
shapeShifter _ b ss | null ss = if isSolved b then Just ( GamePlan {gboard = b, shapesToLocs = []} ) else Nothing
shapeShifter m b (s:ss) = join . find isJust . map f . sortBy (comparing g) . filter ((0<=) . g) . possiblePlans m b $ s
                                           where g (GamePlan {gboard = b'}) = distanceFromMass m b' ss
                                                 f (GamePlan {gboard = b', shapesToLocs = [stl']}) = do
                                                    (GamePlan {shapesToLocs = stl}) <- shapeShifter m b' ss
                                                    return $ GamePlan { gboard = b, shapesToLocs = stl':stl}

solve :: GameState -> Maybe GamePlan
solve st = shapeShifter (modularity st) (board st) $ sortBy f (shapes st)
    where f x y = compare (g y) (g x)
          g = snd . bounds
