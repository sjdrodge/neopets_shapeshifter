import ShapeShifter
import System.Environment
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
    [filepath] <- getArgs
    bstr <- B.readFile filepath
    mstr <- return (maybeSolve bstr)
    maybePutStr mstr

maybePutStr :: Maybe String -> IO ()
maybePutStr Nothing = putStr ""
maybePutStr (Just str) = putStr str

maybeSolve :: B.ByteString -> Maybe String
maybeSolve str = do
    st <- J.decode str :: Maybe GameState
    p <- shapeShifter st
    return (ppGamePlan p)
