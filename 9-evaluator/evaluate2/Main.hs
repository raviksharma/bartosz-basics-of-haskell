module Main where

import qualified Data.Map as M
import Lexer (tokenize)
import Parser (parse)
import Evaluator (evaluate)

main = do
   loop (M.fromList [("pi", pi), ("e", exp 1.0)])

loop symTab = do
   str <- getLine
   if null str
   then
      return ()
   else
      let toks = tokenize str
          tree = parse toks
          (val, symTab') = evaluate tree symTab
      in do
          print val
          loop symTab'
