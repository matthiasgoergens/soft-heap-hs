{-# LANGUAGE OverloadedLists #-}
module Fixed where

import Data.Vector qualified as V

import BatchClass qualified as BC
import GHC.Exts qualified as G

newtype Layer b a = Layer (V.Vector (b a))

f :: Layer b a
f = Layer []

singleton :: BC.Batch b => a -> Layer b a
singleton = Layer . pure . pure

newtype Layers b a = Layers (V.Vector (Layer b a))

-- fromList :: BC.Batch b => [a] -> Layer b a
-- fromList = Layer . V.fromList . fmap pure

data State b a = Uninit | Val (b a) | Empty
