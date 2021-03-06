Control.Effects.Parser
======================

`Control.Effects.Parser` is a parsing effect for the `Control.Effects` library.

`Control.Effects` is a Haskell library for programming with effects, like in the the [Eff language][Eff] by Andrej Bauer and Matija Pretnar. Effects can be used instead of monad transformers, they are designed to be easier to use and to define.

Installation
------------

    cabal install effects-parser

Using the Parser effect
-----------------------

Here's an example how to parse a list of words using `Control.Effects.Parser`.

    import Control.Effects
    import Control.Effects.Parser
    import Data.Char

    testParser1 :: Maybe [String]
    testParser1 = run $
       with (parse "a list of words  ") $ \p ->
         parseMany p $ do
           noBacktrack p $ parseMany  p $ itemIf p isSpace
           noBacktrack p $ parseMany1 p $ itemIf p (not . isSpace)

For more examples see [examples.hs](https://github.com/nybble41/effects-parser/blob/master/examples.hs).

[Eff]: http://math.andrej.com/category/programming/eff/?category_name=programming/eff
