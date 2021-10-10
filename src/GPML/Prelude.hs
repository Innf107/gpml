module GPML.Prelude (
    module Export
,   _head1
,   unsafeTail
) where

import Relude as Export hiding (
        Reader
    ,   ask
    ,   asks
    ,   local
    ,   runReader

    ,   State
    ,   put
    ,   get
    ,   gets
    ,   modify
    ,   modify'
    ,   evalState
    ,   runState
    ,   execState

    ,   fromException

    ,   mapMaybe
    ,   catMaybes
    ,   filter
    ,   ordNub
    ,   hashNub
    )
import Relude.Extra as Export

import Polysemy as Export
import Polysemy.State as Export
import Polysemy.Reader as Export
import Polysemy.Error as Export

import Witherable as Export

import Control.Lens as L 
import Control.Lens as Export (makeLenses)

import Data.List.NonEmpty as Export ((<|))

_head1 :: Traversable1 t => L.Lens' (t a) a
_head1 = L.head1

unsafeTail :: NonEmpty a -> NonEmpty a
unsafeTail (_ :| (y : ys)) = y :| ys
unsafeTail _ = error "unsafeTail: single element list"

