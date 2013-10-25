import ShapeShifter
import System.Environment
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
    [filepath] <- getArgs
    bstr <- B.readFile filepath
    let st = maybe (error "Error - Malformed JSON.") id $ getGameState bstr
    if checksum st /= 0 then error "Unsolvable puzzle - checksum failed." else return ()
    putStrLn $ showFlips st
    putStr $ showPlan (solve st)

showFlips :: GameState -> String
showFlips = ("flips: "++) . show . flips

showPlan :: Maybe GamePlan -> String
showPlan = maybe "No solution found." ppGamePlan

getGameState :: B.ByteString -> Maybe GameState
getGameState = J.decode
