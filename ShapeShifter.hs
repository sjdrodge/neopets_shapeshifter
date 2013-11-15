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
import Data.Int (Int8)
import GHC.Generics (Generic)
import Text.Regex (mkRegex, subRegex)
import qualified Data.Vector.Unboxed as U

type BoardIndex = (Int, Int)

type BoardOffset = Int

type Modularity = Int8

type BoardEntry = Modularity

data GameBoard = GameBoard { dimensions :: BoardIndex
                           , boardData :: U.Vector BoardEntry
                           } deriving (Generic, Show)

type GameShape = GameBoard

data GameState = GameState { modularity :: Modularity
                           , board :: GameBoard
                           , shapes :: [GameShape]
                           } deriving (Generic, Show)

type GamePlan = [(GameShape, BoardIndex)]

type Delta = U.Vector (BoardOffset, BoardEntry)

data GameShape_ = GameShape_ { sDims   :: BoardIndex
                             , sMass   :: Int
                             , sDeltas :: [(BoardIndex, Delta)]
                             } deriving (Eq)

data GameBoard_ = GameBoard_ { bDims :: BoardIndex
                             , bDist :: Int
                             , bData :: U.Vector BoardEntry
                             , bMod  :: Modularity
                             }

type GamePlan_ = [(GameShape_, BoardIndex)]

instance FromJSON GameBoard where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = \str -> subRegex (mkRegex "^boardD") str "d" }

instance FromJSON GameState

mkGameShapes_ :: GameState -> [GameShape_]
mkGameShapes_ st = map (\ sh -> GameShape_ { sDims   = dimensions sh
                                           , sMass   = mass sh
                                           , sDeltas = map (flip (,) =<< (U.fromList . shapeIndicesList (board st) sh))
                                                           (possibleIndices (board st) sh)
                                           }
                       ) (shapes st)

mkGameBoard_ :: GameState -> GameBoard_
mkGameBoard_ st = GameBoard_ { bDims = dimensions (board st)
                             , bDist = distance (modularity st) (board st)
                             , bData = boardData (board st)
                             , bMod  = modularity st
                             }

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

shapeIndicesList :: GameBoard -> GameShape -> BoardIndex -> [(BoardOffset, BoardEntry)]
shapeIndicesList b s i = do
    r <- [0 .. (numRows s - 1)]
    c <- [0 .. (numCols s - 1)]
    let (r0, c0) = i
    guard ( rcIndex s r c == 1 )
    return ( numCols b * (r0 + r) + (c0 + c), 1 )

possibleIndices :: GameBoard -> GameShape -> [BoardIndex]
possibleIndices b sh = do
    r <- [0 .. (numRows b - numRows sh)]
    c <- [0 .. (numCols b - numCols sh)]
    return (r, c)

applyDelta :: Modularity -> GameBoard_ -> Delta -> GameBoard_
applyDelta m b d = b { bDist = bDist b + distDelta
                     , bData = bData'
                     } where f x y     = (x + y) `rem` m
                             bData'    = U.accumulate f (bData b) d
                             distDelta = U.foldr (\(i,_) z -> fromIntegral ((bData' U.! i) - (bData b U.! i)) + z) 0 d

possiblePlans :: Modularity -> GameBoard_ -> GameShape_ -> [(BoardIndex, GameBoard_)]
possiblePlans m b s = do
    (i, d) <- sDeltas s
    let b' = applyDelta m b d
    return (i, b')

mass :: GameShape -> Int
mass = U.sum . U.map fromIntegral . boardData

distance :: Modularity -> GameBoard -> Int
distance m = U.sum . U.map fromIntegral . U.map f . boardData
    where f x = (m - x) `rem` m

distanceFromMass :: Modularity -> GameBoard_ -> [GameShape_] -> Int
distanceFromMass _ b xs = (sum . map sMass ) xs - bDist b

pruneAndSort :: Modularity -> [GameShape_] -> [(BoardIndex, GameBoard_)] -> [(BoardIndex, GameBoard_)]
pruneAndSort m ss = sortBy (comparing h) . filter ((0<=) . h)
    where h (_, b) = distanceFromMass m b ss

shapeShifter' :: Modularity -> [GameShape_] -> (BoardIndex, GameBoard_) -> Maybe GamePlan_
shapeShifter' m (s:ss) (i, b) = do
    ret <- shapeShifter m b ss
    return $ (s, i) : ret

shapeShifter :: Modularity -> GameBoard_ -> [GameShape_] -> Maybe GamePlan_
shapeShifter _ b ss | null ss = if bDist b == 0 then Just [] else Nothing
shapeShifter m b (s:ss) = msum . map (shapeShifter' m (s:ss)) . pruneAndSort m ss . possiblePlans m b $ s

solve' :: GameState -> Maybe GamePlan_
solve' st = shapeShifter (modularity st) (mkGameBoard_ st) . sortBy g . sortBy f $ mkGameShapes_ st
    where f x y = comparing sMass y x --larger first
          g = comparing (h . sDims)
          h (r, c) = (rmax - r + 1) * (cmax - c + 1)
          (rmax, cmax) = boardSize . board $ st

solve :: GameState -> Maybe GamePlan
solve st = do
    (shs, ixs) <- liftM unzip (solve' st)
    let shapeTable = zip (mkGameShapes_ st) (shapes st)
    shs' <- mapM (`lookup` shapeTable) shs
    return $ zip shs' ixs

flipsAndChecksum :: GameState -> (Int, Int)
flipsAndChecksum st = ( (sum . map mass . shapes) st - distance (modularity st) (board st) )
                      `quotRem` fromIntegral (modularity st)

flips :: GameState -> Int
flips = fst . flipsAndChecksum

checksum :: GameState -> Int
checksum = snd . flipsAndChecksum
