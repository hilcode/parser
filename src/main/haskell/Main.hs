module Main where

import Hilcode.Prelude
import Hilcode.RegExp

main ∷ IO ()
main
  = do print $ match program ""
       print $ match program "a"
       print $ match program "aa"
       print $ match program "aaa"
       print $ match program "a5"
       print $ match program "aa5"
       print $ match program "aaab"
       print $ match program "aaa123Hello, World!456"
       print program
  where
      regexp ∷ RegExp Char String
      regexp = repeat <| repeat ((atom 'a' <> atom 'a') <|> (atom 'a' <> atom 'a' <> atom 'a') <|> range '0' '9' <|> atoms "Hello, world!")

      -- regexp = repeat <| repeat <| repeat nothing <|> repeat (atom 'a') <|> repeat (atom 'a' <> atom 'a') <|> repeat (atom 'a' <> atom 'a' <> atom 'a') <|> repeat (atom 'a' <> atom 'a' <> atom 'a' <> atom 'a')
      program ∷ Lexer Char String
      program = makeLexer [TokenDefinition regexp id]
