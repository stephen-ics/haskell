module Main where

import Eval
import Parsec

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO ()
process line = do
    let res = Parser.parseExpr line
    case res of
        Left err -> print err
        Right ex -> case Eval.eval ex of
            Nothing -> putStrLn "Cannot evaluated"
            Just result -> putStrLn $ ppexpr result

main :: IO ()
main = runInputT defaultSettings loop
    where
    loop = do
        minput <- getInputLine "Repl> "
        case minput of
            Nothing -> outputStrLn "Goodbye."
            Just input -> (liftIO $ process input) >> loop
