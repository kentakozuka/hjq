module Main where
import System.Environment
import System.Exit
import Data.Hjq
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
    args <- getArgs
    -- ファイル、標準入力で処理を分ける
    case args of

        -- 引数が2つの場合
        (query : file : []) -> do
            json <- B.readFile file
            -- pack :: String -> Text
            printResult $ hjq json (T.pack query)

        -- 引数が1つの場合
        (query : []) -> do
            -- 標準入力からJSONを取得
            json <- B.getContents
            printResult $ hjq json (T.pack query)

        -- それ以外
        _ -> do putStrLn $ "Invalid arguments error. : " ++ show args
                exitWith $ ExitFailure 1

-- 標準出力
printResult :: Either T.Text B.ByteString -> IO ()
printResult (Right s) = B.putStrLn s
printResult (Left s) = do
    T.putStrLn s
    exitWith $ ExitFailure 1
