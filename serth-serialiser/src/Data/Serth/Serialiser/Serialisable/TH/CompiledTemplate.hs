module Data.Serth.Serialiser.Serialisable.TH.CompiledTemplate (
    CompiledTemplate,
    compileTemplate,
    optimiseTemplate,
) where

import Data.ByteString.Char8 (unpack)
import Data.Serth.Serialiser.FieldName
import Data.Serth.Serialiser.Template

-- | A serialisation template that can be used by the compiler
--
-- If an item is 'Left', it represents a hole in the template that will be filled at runtime.
-- (The first element of the tuple helps building the context of the fields nesting)
--
-- If an item is 'Right', it represents a static portion of the template
-- (We use 'String's here instead of 'Data.ByteString.ByteString's because 'Language.Haskell.TH.StringL' takes a String)
type CompiledTemplate = [Either (FieldName, TemplateHole) String]

-- | Turns a 'Template' into a 'CompiledTemplate'
compileTemplate :: Template format -> CompiledTemplate
compileTemplate = optimiseTemplate . reifyTemplate
  where
    -- Traverses the template tree, turns it into a list
    reifyTemplate :: Template format -> CompiledTemplate
    reifyTemplate = \case
        Token c -> [Right [c]]
        Tokens s -> [Right $ unpack s]
        Hole fname hole -> [Left (fname, hole)]
        And left right -> compileTemplate left <> compileTemplate right
        Empty -> []

-- _Optimising_ by collating adjacent static portions of the template
optimiseTemplate :: CompiledTemplate -> CompiledTemplate
optimiseTemplate = \case
    [] -> []
    [a] -> [a]
    (Right a : Right b : c) -> case optimiseTemplate (Right b : c) of
        (Right b' : c') -> Right (a <> b') : c'
        c' -> Right a : c'
    (a : b) -> a : optimiseTemplate b
