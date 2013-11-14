import Control.Monad
import Data.Maybe
import ShapeShifter
import System.Environment
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
    [filepath] <- getArgs
    bstr <- B.readFile filepath
    let st = fromMaybe (error "Error - Malformed JSON.") $ getGameState bstr
    when (checksum st /= 0) $ error "Unsolvable puzzle - checksum failed."
    printStats st
    putStr $ showPlan (solve st)

printStats :: GameState -> IO ()
printStats st = do
    putStrLn $ showNumShapes st
    putStrLn $ showFlips st

showFlips :: GameState -> String
showFlips = ("flips: "++) . show . flips

showNumShapes :: GameState -> String
showNumShapes = ("shapes: "++) . show . length . shapes

showPlan :: Maybe GamePlan -> String
showPlan = maybe "No solution found." ppGamePlan

getGameState :: B.ByteString -> Maybe GameState
getGameState = J.decode
