module Main where

import Control.Effects
import Control.Effects.Parser

import Data.Char (isDigit, isSpace)
import Control.Applicative


testParser1 :: Maybe [String]
testParser1 = run $
   with (parse "a list of words  ") $ \p ->
     parseMany p $ do
       noBacktrack p $ parseMany  p $ itemIf p isSpace
       noBacktrack p $ parseMany1 p $ itemIf p (not . isSpace)


testParser2 :: IO ()
testParser2 = runBase $ do
   input <- base getLine
   maybeResult <- with (parse input) $ \p ->
      let expr    = sum
          sum     = leftAssoc product [('+', (+)), ('-', (-))]
          product = leftAssoc unary   [('*', (*)), ('/', div)]
          unary   = oneOf p [char '-' >> negate <$> unary, atom]
          atom    = oneOf p [number, char '(' *> expr <* char ')']
          number  = read <$> digits <* spaces
          digits  = noBacktrack p $ parseMany1 p $ itemIf p isDigit
          spaces  = noBacktrack p $ parseMany  p $ itemIf p isSpace
          char ch = itemIf p (==ch) <* spaces
          leftAssoc unit opPairs = do
             let ops = map (\(ch, op) -> char ch >> flip op <$> unit) opPairs
             foldl (\a op -> op a) <$> unit <*> parseMany p (oneOf p ops)
      in spaces *> expr <* parseEnd p
   case maybeResult of
      Nothing -> base $ putStrLn $ "Syntax error in expression"
      Just n  -> base $ putStrLn $ "Result: " ++ show (n :: Integer)
