import ShapeShifter
import System.Environment
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
    [filepath] <- getArgs
    bstr <- B.readFile filepath
    let st = getGameState bstr
    putStrLn $ showFlips st
    putStr $ showPlan (solve =<< st)

showFlips :: Maybe GameState -> String
showFlips = maybe "Malformed JSON." (("flips: "++) . show . flips )

showPlan :: Maybe GamePlan -> String
showPlan = maybe "No solution found." ppGamePlan

getGameState :: B.ByteString -> Maybe GameState
getGameState = J.decode
