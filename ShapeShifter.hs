{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module ShapeShifter ( GameBoard
                    , GameShape
                    , GameState (..)
                    , GamePlan
                    , ppGameBoard
                    , ppGamePlan
                    , ppGameState
                    , solve
                    , flips
                    , checksum
                    ) where

import Control.Arrow (second)
import Control.DeepSeq (($!!), NFData)
import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Data.Aeson.Types
import Data.Functor
import Data.Int (Int8)
import Data.List
import Data.Ord
import Data.STRef
import GHC.Generics (Generic)
import Text.Regex (mkRegex, subRegex)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

type BoardIndex = (Int, Int)

type BoardOffset = Int

type Modularity = Int8

type BoardEntry = Modularity

data GameBoard = GameBoard { dimensions :: BoardIndex
                           , boardData  :: U.Vector BoardEntry
                           } deriving (Generic, Show)

type GameShape = GameBoard

data GameState = GameState { modularity :: Modularity
                           , board      :: GameBoard
                           , shapes     :: [GameShape]
                           } deriving (Generic, Show)

data ShapeIndex = ShapeIndex { shape :: GameShape
                             , index :: BoardIndex
                             } deriving (Generic, Show)

type GamePlan = [ShapeIndex]

type Delta = U.Vector BoardOffset

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

instance NFData GameBoard_
instance NFData GameShape_

gameBoardJSONOptions :: Options
gameBoardJSONOptions = defaultOptions
    { fieldLabelModifier = \str -> subRegex (mkRegex "^boardD") str "d" }

instance FromJSON GameBoard where
    parseJSON = genericParseJSON gameBoardJSONOptions

instance ToJSON GameBoard where
    toJSON = genericToJSON gameBoardJSONOptions

instance FromJSON GameState
instance ToJSON GameState

instance FromJSON ShapeIndex
instance ToJSON ShapeIndex

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
ppGameBoard b = unlines ("" : [ unwords [ show ( rcIndex b i j )
                                  | j <- [0 .. (numCols b - 1)]
                                  ]
                                  | i <- [0 .. (numRows b - 1)]
                              ])

ppGamePlan :: GamePlan -> String
ppGamePlan = unwords . map f
    where f si   = ppGameBoard s ++ "-\n" ++ show (r + 1, c + 1) ++ "\n"
              where s      = shape si
                    (r, c) = index si

ppGameState :: GameState -> String
ppGameState st = ppGameBoard b ++ "% " ++ show (modularity st)
                                where b = board st

numRows :: GameBoard -> Int
numRows = fst . dimensions

numCols :: GameBoard -> Int
numCols = snd . dimensions

shapeIndicesList :: GameBoard -> GameShape -> BoardIndex -> [BoardOffset]
shapeIndicesList b s i = do
    r <- [0 .. (numRows s - 1)]
    c <- [0 .. (numCols s - 1)]
    let (r0, c0) = i
    guard ( rcIndex s r c == 1 )
    return $!! numCols b * (r0 + r) + (c0 + c)

possibleIndices :: GameBoard -> GameShape -> [BoardIndex]
possibleIndices b sh = do
    r <- [0 .. (numRows b - numRows sh)]
    c <- [0 .. (numCols b - numCols sh)]
    return (r, c)

applyDelta :: Modularity -> GameBoard_ -> Delta -> GameBoard_
applyDelta m b d = runST $ do
    dist' <- newSTRef (bDist b)
    nref <- newSTRef 0
    vec' <- U.thaw (bData b)

    U.forM_ d $ \i -> do
        x <- UM.read vec' i
        let x' = x + 1
        UM.write vec' i x'
        let doUpdate | x' == 1   = modifySTRef nref (+ 1)
                     | x' == m   = UM.write vec' i 0
                     | otherwise = return ()
        doUpdate

    n <- readSTRef nref
    modifySTRef dist' (+ (n * fromIntegral m - U.length d))

    dist'' <- readSTRef dist'
    vec'' <- U.unsafeFreeze vec'
    return $!! b {bData = vec'', bDist = dist''}

possiblePlans :: Modularity -> GameBoard_ -> GameShape_ -> [(BoardIndex, GameBoard_)]
possiblePlans m b s = second (applyDelta m b) <$> sDeltas s

mass :: GameShape -> Int
mass = U.sum . U.map fromIntegral . boardData

distance :: Modularity -> GameBoard -> Int
distance m = U.sum . U.map fromIntegral . U.map f . boardData
    where f x = (m - x) `rem` m

distanceFromMass :: Modularity -> GameBoard_ -> [GameShape_] -> Int
distanceFromMass _ b xs = (sum . map sMass ) xs - bDist b

pruneAndSort :: Modularity -> [GameShape_] -> [(BoardIndex, GameBoard_)] -> [(BoardIndex, GameBoard_)]
pruneAndSort m ss = sortBy (comparing h) . filter ((0 <=) . h)
    where h (_, b) = distanceFromMass m b ss

search :: Modularity -> GameBoard_ -> [GameShape_] -> Maybe GamePlan_
search _ b []     = if bDist b == 0 then Just [] else Nothing
search m b (s:ss) = msum . map f . pruneAndSort m ss . possiblePlans m b $ s
    where f (i, b') = ((s, i) :) <$> search m b' ss

degreesOfFreedom :: GameBoard_ -> GameShape_ -> Int
degreesOfFreedom b s = (rmax - r + 1) * (cmax - c + 1)
    where (r, c) = sDims s
          (rmax, cmax) = bDims b

sortShapes_ :: GameBoard_ -> [GameShape_] -> [GameShape_]
sortShapes_ b ss = sortBy (comparing (degreesOfFreedom b)) . sortBy (flip $ comparing sMass) $ ss

solve :: GameState -> Maybe GamePlan
solve st = do
    let board' = mkGameBoard_ st
    let shapes' = sortShapes_ board' (mkGameShapes_ st)
    gameplan <- search (modularity st) board' shapes'
    ixs <- evalStateT (mapM shapeToPlanIndex (mkGameShapes_ st)) gameplan
    return $ zipWith ShapeIndex (shapes st) ixs

shapeToPlanIndex :: GameShape_ -> StateT GamePlan_ Maybe BoardIndex
shapeToPlanIndex sh = do
    plan <- get
    ix <- lift (sh `lookup` plan)
    put $ delete (sh, ix) plan
    return ix

flipsAndChecksum :: GameState -> (Int, Int)
flipsAndChecksum st = ( (sum . map mass . shapes) st - distance (modularity st) (board st) )
                      `quotRem` fromIntegral (modularity st)

flips :: GameState -> Int
flips = fst . flipsAndChecksum

checksum :: GameState -> Int
checksum = snd . flipsAndChecksum
