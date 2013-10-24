import ShapeShifter
import System.Environment
import Control.Monad
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
    [filepath] <- getArgs
    bstr <- B.readFile filepath
    let st = getGameState bstr
    maybePutStr . liftM ( ("flips: "++) . (++"\n") . show . flips ) $ st
    maybePutStr . liftM ppGamePlan $ solve =<< st

maybePutStr :: Maybe String -> IO ()
maybePutStr Nothing = putStr "No solution found."
maybePutStr (Just str) = putStr str

getGameState :: B.ByteString -> Maybe GameState
getGameState = J.decode
