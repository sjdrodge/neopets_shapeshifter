import ShapeShifter
import System.Environment
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
    [depthstr, filepath] <- getArgs
    let depth = read depthstr
    bstr <- B.readFile filepath
    let st = maybe (error "Error - Malformed JSON.") id $ getGameState bstr
    if checksum st /= 0 then error "Unsolvable puzzle - checksum failed." else return ()
    printStats st
    putStr $ showPlan (solve depth st)

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
