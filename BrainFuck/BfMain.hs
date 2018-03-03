import Bf
import System.Environment

main :: IO ()
main = (head <$> getArgs) >>= readFile >>= evalProgram
