module JSON (benchmark) where

import Criterion.Main
import qualified JSON.Nested
import qualified JSON.Simple

benchmark :: Benchmark
benchmark =
    bgroup
        "json"
        [JSON.Simple.benchmark, JSON.Nested.benchmark]
