{-# LANGUAGE FlexibleContexts #-}
module Control.Effects.Parser (
   parse,
   oneOf,
   item,
   itemIf,
   lookahead,
   noMatch,
   noBacktrack,
   parseFail,
   parseEnd,
   parseOpt,
   parseMany,
   parseMany1,
   parseOpt',
   parseMany',
   parseMany1'
) where

import Control.Applicative
import Control.Effects
import Control.Monad

-- A parser is a function from a list of items to a parsed result.
-- The result culminates with "return (Just a)" on success. On
-- failure the result is the second argument, which is defaults 
-- to "return Nothing", but can be modified to include effects
-- such as backtracking.
newtype Parser c m a = Parser ([c] -> m (Maybe a) -> m (Maybe a))

-- Generates a Handler to parse input from the given list of items.
parse :: Monad m => [c] -> Handler (Parser c m a) (Maybe a) m a
parse input = Handler
   { ret = \a -> return . Parser $ \cs f -> return (Just a)
   , fin = \(Parser r) -> r input (return Nothing)
   }

-- Internal helper primitive to change the input for the remainder of the parser.
putInput :: AutoLift (Parser c m a) m n => Effect (Parser c m a) m -> [c] -> n ()
putInput p cs' = operation p $ \k -> return . Parser $ \cs f -> do
  Parser r <- k ()
  r cs' f

-- Internal helper primitive to change the action on failure for the remainder of the parser.
putFailAction :: AutoLift (Parser c m a) m n => Effect (Parser c m a) m -> m (Maybe a) -> n ()
putFailAction p f' = operation p $ \k -> return . Parser $ \cs f -> do
  Parser r <- k ()
  r cs f'

-- Tries each action from the given list in turn, stopping with the first one
-- which successfully parses the input. A parse failure in the action or the
-- continuation results in backtracking to the original location in the input,
-- and reverts the state of any more deeply nested effects.
oneOf :: AutoLift (Parser c m a) m n => Effect (Parser c m a) m -> [n b] -> n b
oneOf p []     = parseFail p
oneOf p (a:as) = join $ operation p $ \k -> return . Parser $ \cs f -> do
   Parser r <- k a
   r cs $ do
      Parser r' <- k (oneOf p as)
      r' cs f

-- Returns and consumes the next item from the input.
-- Fails if there is no more input to be consumed.
item :: AutoLift (Parser c m a) m n => Effect (Parser c m a) m -> n c
item p = operation p $ \k -> return . Parser $ \cs f ->
   case cs of
      []    -> f
      (h:t) -> do { Parser r <- k h; r t f }

-- Returns and consumes the next item from the input, provided it satisfies a predicate.
-- Fails if there is no more input, or if the predicate function evaluates to False.
itemIf :: AutoLift (Parser c m a) m n => Effect (Parser c m a) m -> (c -> Bool) -> n c
itemIf p f = do { x <- item p; if f x then return x else parseFail p }

-- Zero-width positive lookahead assertion. Runs the given action against the
-- current input without consuming it. A parse failure in the action causes the
-- lookahead to fail; otherwise, the result of the action is returned.
lookahead :: AutoLift (Parser c m a) m n => Effect (Parser c m a) m -> n b -> n b
lookahead p a = join $ operation p $ \k -> return . Parser $ \cs f -> do
   Parser r <- k (a <* putInput p cs)
   r cs f

-- Zero-width negative lookahead assertion. Runs the given action against the
-- current input without consuming it. A parse failure in the action causes the
-- lookahead to succeed; otherwise, the lookahead fails.
noMatch :: AutoLift (Parser c m a) m n => Effect (Parser c m a) m -> n b -> n ()
noMatch p a = join $ operation p $ \k -> return . Parser $ \cs f -> do
   Parser r <- k (a >> putFailAction p f >> parseFail p)
   r cs $ do
      Parser r' <- k (return ())
      r' cs f

-- Prevents backtracking into the middle of the given action. This can be used to
-- create actions which always consume as much or as little of the input as possible,
-- even if that would cause a later match to fail. Note that backtracking is still
-- possible within the action, or across the entire noBacktrack action.
-- Examples:
--   noBacktrack p $ oneOf [return (), item p >> return ()]  ==> never consumes any input
--   noBacktrack p $ parseMany p $ itemIf p (=='a')          ==> always consumes all the 'a's
noBacktrack :: AutoLift (Parser c m a) m n => Effect (Parser c m a) m -> n b -> n b
noBacktrack p a = join $ operation p $ \k -> return . Parser $ \cs f -> do
   Parser r <- k (a <* putFailAction p f)
   r cs f

-- A parser which always fails.
parseFail :: AutoLift (Parser c m a) m n => Effect (Parser c m a) m -> n b
parseFail p = operation p $ \k -> return . Parser $ \cs f -> f

-- A parser which succeeds only when there is no more input.
parseEnd :: AutoLift (Parser c m a) m n => Effect (Parser c m a) m -> n ()
parseEnd p = noMatch p (item p)

-- Try the given parser; returns Just the result on success, or Nothing otherwise.
parseOpt :: AutoLift (Parser c m a) m n => Effect (Parser c m a) m -> n b -> n (Maybe b)
parseOpt p f = oneOf p [Just <$> f, return Nothing]

-- Apply the given parser zero or more times, and return a list of the results.
parseMany :: AutoLift (Parser c m a) m n => Effect (Parser c m a) m -> n b -> n [b]
parseMany p f = oneOf p [parseMany1 p f, return []]

-- Like parseMany, but fails if there isn't at least one match.
parseMany1 :: AutoLift (Parser c m a) m n => Effect (Parser c m a) m -> n b -> n [b]
parseMany1 p f = (:) <$> f <*> parseMany p f

-- A non-greedy version of parseOpt which prefers to match Nothing.
parseOpt' :: AutoLift (Parser c m a) m n => Effect (Parser c m a) m -> n b -> n (Maybe b)
parseOpt' p f = oneOf p [return Nothing, Just <$> f]

-- A non-greedy version of parseMany which matches as few times as possible. 
parseMany' :: AutoLift (Parser c m a) m n => Effect (Parser c m a) m -> n b -> n [b]
parseMany' p f = oneOf p [return [], parseMany1' p f]

-- A non-greedy version of parseMany1 which matches as few times as possible (but at least once).
parseMany1' :: AutoLift (Parser c m a) m n => Effect (Parser c m a) m -> n b -> n [b]
parseMany1' p f = (:) <$> f <*> parseMany' p f
