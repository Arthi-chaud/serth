{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Futadata () where

import Data.Aeson hiding (SumEncoding (..), defaultOptions, sumEncoding)
import Data.Serth.Serialiser
import Data.Serth.Serialiser.Format.JSON
import Data.Serth.Serialiser.Serialisable.TH
import Data.Text
import Data.Version
import Text.Pandoc.Definition

instance {-# OVERLAPPING #-} Serialisable JSON Text where
    {-# INLINE builder #-}
    builder _ = fromEncoding . toEncoding

instance {-# OVERLAPPING #-} Serialisable JSON Double where
    {-# INLINE builder #-}
    builder _ = fromEncoding . toEncoding

$( genSerialisablesWithOptions @JSON
    [ ''Pandoc
    , ''Meta
    , ''MetaValue
    , ''Citation
    , ''Block
    , ''Inline
    , ''MathType
    , ''QuoteType
    , ''ListNumberStyle
    , ''ListNumberDelim
    , ''Text.Pandoc.Definition.Format
    , ''Caption
    , ''RowHeadColumns
    , ''Alignment
    , ''ColWidth
    , ''Row
    , ''TableHead
    , ''TableBody
    , ''TableFoot
    , ''Cell
    , ''RowSpan
    , ''ColSpan
    , ''CitationMode
    , ''Version
    ]
    defaultOptions{sumEncoding = TaggedObject "t" "c"}
 )
