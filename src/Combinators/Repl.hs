{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

----------------------------------------------------------------------------
-- |
-- Module      : Combinators.Repl
-- Description : A REPL for Combinator reduction
--
----------------------------------------------------------------------------
module Combinators.Repl ( main ) where

import Combinators.Abstraction
import Combinators.Calculus
import Combinators.Lambda
import Combinators.Parser
import Combinators.Pretty

import Data.Char ( isSpace )
import Data.Function ( (&) )
import qualified Data.Map.Strict as Map

import System.IO ( stdout, hFlush )

main :: IO ()
main = do
    putStrLn "S/K/I Reduction Machine\n" >> mainAux

getInputLine :: String -> IO String
getInputLine prompt = do
    putStr prompt
    hFlush stdout >> getLine

mainAux :: IO ()
mainAux = do
    input <- getInputLine "Expr> "
    case input of
        (':' : rest) -> runCommand rest
        _            -> mainAux

runCommand :: String -> IO ()
runCommand = getCommand commands
  where
    getCommand options input =
        let (command, exprs) = break isSpace input in
            case options Map.!? command of
                Nothing  -> (putStrLn "Invalid command!") >> mainAux
                Just cmd -> cmd $ trimFront exprs

commands :: Map.Map String (String -> IO ())
commands = [ ("t",    compCmd)
           , ("h",    helpCmd)
           , ("l",    lambCmd)
           , ("c",    combCmd)
           , ("step", stepCmd)
           , ("whn",  whnmCmd)
           , ("norm", normCmd)
           , ("q",    quitCmd)
           ] & Map.fromList

quitCmd :: String -> IO ()
quitCmd _ = putStrLn "Leaving S/K/I" >> return ()

lambCmd :: String -> IO ()
lambCmd str = case parseLambda str of
    Nothing   -> mainAux
    Just expr -> putStr "Expr> " >> pprLInf expr >> mainAux

combCmd :: String -> IO ()
combCmd str = case parseCombinator str of
    Nothing   -> mainAux
    Just expr -> putStr "Expr> " >> pprInf expr >> mainAux

parseLambda :: String -> Maybe (LExpr String)
parseLambda str = case runParser parseLExpr str of
    Right (e,_) -> Just e
    Left _      -> Nothing -- Improve this to use the parser error

parseCombinator :: String -> Maybe (Expr String)
parseCombinator str = case runParser parseExpr str of
    Right (e,_) -> Just e
    Left _      -> Nothing

-- Note: make pretty printing of compilations better
compCmd :: String -> IO ()
compCmd str = case parseLambda str of
    Just expr -> putStrLn "" >> pprLInf expr >> putStrLn "===" >> (pprInf $ compile expr) >> putStrLn "" >> mainAux
    Nothing   -> putStrLn "Invalid lambda expression" >> mainAux

displayRelation :: (Expr String -> Expr String) -> String -> IO ()
displayRelation rel str = case parseCombinator str of
    Just expr -> putStrLn "" >> pprInf expr >> putStrLn "--->" >> (pprInf $ rel expr) >> putStrLn "" >> mainAux
    Nothing   -> putStrLn "Invalid combinator expression" >> mainAux

stepCmd :: String -> IO ()
stepCmd = displayRelation whc

whnmCmd :: String -> IO ()
whnmCmd = displayRelation whNorm

normCmd :: String -> IO ()
normCmd = displayRelation norm

-- Note: implement verbose normalisation, i.e., where we show each step in the reduction

helpCmd :: String -> IO ()
helpCmd _ = displayOptions

displayOptions :: IO ()
displayOptions = do
    putStrLn  " Commands in the SKI REPL:"
    pprNested ":q                Quits the REPL"
    pprNested ":t <expr>         Compiles a lambda expression into a combinator expression"
    pprNested ":l <expr>         Parses a lambda expression and pretty prints the AST"
    pprNested ":c <expr>         Parses a combinator expression and pretty prints the AST"
    pprNested ":step <expr>      Perform weak head contraction on a combinator expression"
    pprNested ":whn <expr>       Weak head normalisation"
    pprNested ":norm <expr>      Normalisation"
    pprNested ":h                Displays the command options"
    mainAux

-- | From Data.String.Utils, need to fix import
trimFront :: String -> String
trimFront str = case str of
    []     -> []
    (x:xs) -> if elem x " \t\n\r" then trimFront xs else str


