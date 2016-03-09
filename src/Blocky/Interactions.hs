{-# LANGUAGE FlexibleContexts #-}
module Blocky.Interactions where

import           Blocky.Common
import           Control.Lens       (Lens', each, filtered, folded, makeLenses,
                                     mapped, over, toListOf, view, _1, _2, _3,
                                     _4)
import           Control.Monad      (forM_, guard, when)
import           Control.Monad.Fix
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Traversable
import           Graphics.UI.WX     hiding (Attr, Event, on)
import qualified Graphics.UI.WX     as WX
import           Reactive.Banana
import           Reactive.Banana.WX
import           System.IO
import           System.IO.Unsafe

type VisualId = Int
type ZIndex = Int
type DragState = Bool
type Visual = (VisualId, ZIndex, Rect, DragState)

hittest :: (a -> Point)
        -> VisualId
        -> Behavior [Visual]
        -> Event a
        -> Event a
hittest pointOf oid objs evt = snd <$> filterE flt ((,) <$> objs <@> evt)
    where
        flt (objs,x) = any ((== oid) . view _1)
                     . take 1
                     . sortOn (view _2)
                     . filter ((`rectContains` pointOf x) . view _3)
                     . traceNoLF "."
                     $ objs

draggable :: Event (a, Vector) -> Event (Visual -> Visual)
draggable = fmap $ \(_,delta) v ->
    if view _4 v then over _3 (`rectMove` delta) v else v

startDrag :: Event a -> Event (Visual -> Visual)
startDrag = fmap $ \_ -> over _4 (const True)

stopDrag :: Event a -> Event (Visual -> Visual)
stopDrag = fmap $ \_ -> over _4 (const False)

bringToFront :: Event a -> Event (Visual -> Visual)
bringToFront = fmap $ \_ -> over _2 (const 0)

sendBackwards :: Event a -> Event (Visual -> Visual)
sendBackwards = fmap $ \_ -> over _2 (+1)
