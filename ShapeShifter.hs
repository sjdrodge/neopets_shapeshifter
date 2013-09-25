module ShapeShifter ( GameBoard
                    , GameShape
                    , GameState(..)
                    , GamePlan(..)
                    , applyShape
                    , shapeShifter
                    ) where

import Control.Monad
import Data.Array.IArray
import Data.List
import Data.Maybe

type GameBoard = Array BoardIndex Int

type GameShape = GameBoard

data GameState = GameState { modularity :: Int
                           , gameBoard :: GameBoard
                           }

data GamePlan = GamePlan [(GameShape, BoardIndex)]

newtype BoardIndex = BoardIndex (Int,Int)
    deriving (Eq, Ord, Show, Ix)

ppGameBoard :: GameBoard -> String
ppGameBoard b = unlines ("":[ unwords [ show ( b ! BoardIndex (j, i) )
                                | i <- [0..((snd . f . snd . bounds) b)] 
                                ] 
                                | j <- [0..((fst . f . snd . bounds) b)]
                            ]) where f (BoardIndex (x,y)) = (x,y)

instance Show GameState where
    show state = ppGameBoard b ++ "% " ++ show (modularity state)
                                 where b = gameBoard state

instance Show GamePlan where
    show (GamePlan xs) = concatMap ( \(s,i) -> "(" ++ ppGameBoard s ++ ", " ++ show i ++ ")" ) xs

instance Num BoardIndex where
    (+) (BoardIndex (a, b)) (BoardIndex (c, d)) = BoardIndex (a+c, b+d)
    (*) (BoardIndex (a, b)) (BoardIndex (c, d)) = BoardIndex (a*c, b*d)
    negate (BoardIndex (a, b)) = BoardIndex ((-a), (-b))
    abs (BoardIndex (a, b)) = BoardIndex (abs a, abs b)
    signum (BoardIndex (a, b)) = BoardIndex (signum a, signum b)
    fromInteger x = BoardIndex ( fromIntegral x, fromIntegral x)

possibleIndices :: GameBoard -> GameShape -> [BoardIndex]
possibleIndices board shape = range (mn, mx)
                              where f = (snd . bounds)
                                    mn = (fst . bounds) board
                                    mx = f board - f shape

possibleStates :: GameState -> GameShape -> [GameState]
possibleStates state shape = [ GameState { modularity = modularity state
                                         , gameBoard = (applyShape m b shape i)
                                         } | i <- possibleIndices b shape 
                             ] where b = gameBoard state
                                     m = modularity state

isSolved :: GameBoard -> Bool
isSolved = not . isJust . find (0/=) . elems

applyShape :: Int -> GameBoard -> GameShape -> BoardIndex -> GameBoard
applyShape m b s i = accum f b [ (i + j, s ! j) | j <- range (bounds s) ]
                         where f x y = ( (x + y) `mod` m )

shapeShifter :: GameState -> [GameShape] -> Maybe GamePlan
shapeShifter state [] = if isSolved (gameBoard state) then Just ( GamePlan [] ) else Nothing
shapeShifter state (shape:shapes) = g . join . find isJust
                                  $ zipWith f (possibleIndices (gameBoard state) shape)
                                  $ map (`shapeShifter` shapes) (possibleStates state shape)
                                  where f _ Nothing = Nothing
                                        f i (Just p) = Just (i,p)
                                        g Nothing = Nothing
                                        g (Just (i, GamePlan p)) = Just ( GamePlan ((shape,i):p) )
