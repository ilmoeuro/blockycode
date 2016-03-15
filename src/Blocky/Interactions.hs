{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module Blocky.Interactions
    ( Visual()
    , visual
    , hittest
    , getBounds
    , getVisualId
    , getZIndex
    , draggable
    , startDrag
    , stopDrag
    , bringToFront
    , sendBackwards
    , snap
    ) where

import           Blocky.Common
import           Control.Lens       (makeLenses, over, view, to, _1)
import           Data.List
import           Graphics.UI.WX     hiding (Attr, Event, on)
import           Reactive.Banana

data Visual = Visual
    { _visualId    :: Int
    , _zIndex      :: Int
    , _bounds      :: Rect
    , _dragging    :: Bool
    , _snapVectors :: [(Vector, Int)]
    }

makeLenses ''Visual

visual :: Int -> Int -> Rect -> Bool -> [(Vector, Int)] -> Visual
visual = Visual

hittest :: (a -> Point)
        -> Int
        -> Behavior [Visual]
        -> Event a
        -> Event a
hittest pointOf oid objs evt = snd <$> filterE flt ((,) <$> objs <@> evt)
    where
        flt (obs,x) = any ((== oid) . view visualId)
                    . take 1
                    . sortOn (view zIndex)
                    . filter ((`rectContains` pointOf x) . view bounds)
                    . traceNoLF "."
                    $ obs

getBounds :: Visual -> Rect
getBounds = view bounds

getVisualId :: Visual -> Int
getVisualId = view visualId

getZIndex :: Visual -> Int
getZIndex = view zIndex

draggable :: Event (a, Vector) -> Event (Visual -> Visual)
draggable = fmap moveIfDragged where
    moveIfDragged (_,d) v | view dragging v = over bounds (`rectMove` d) v
    moveIfDragged _     v                   = v

startDrag :: Event a -> Event (Visual -> Visual)
startDrag = fmap $ \_ -> over dragging (const True)

stopDrag :: Event a -> Event (Visual -> Visual)
stopDrag = fmap $ \_ -> over dragging (const False)

bringToFront :: Event a -> Event (Visual -> Visual)
bringToFront = fmap $ \_ -> over zIndex (const 0)

sendBackwards :: Event a -> Event (Visual -> Visual)
sendBackwards = fmap $ \_ -> over zIndex (+1)

snap :: Behavior [Visual] -> Event a -> Event (Visual -> Visual)
snap bvisuals evt = snap' <$> bvisuals <@ evt where
    minDelta = 25
    snap' visuals v = moveDelta (deltas visuals v) v
    moveDelta [d] v = over bounds (`rectMove` d) v
    moveDelta _   v = v
    snapPoints    v = map
                        (over
                         _1
                         (\sv -> pointMove sv (view (bounds . to rectTopLeft) v)))
                    $ view snapVectors v
    deltas visuals v = take 1
                     . sortOn vecLength
                     . filter (\x -> vecLength x < minDelta)
                     $ [ p' `vecBetween` p
                       | visual' <- visuals
                       , (p, angle) <- snapPoints visual'
                       , (p', angle') <- snapPoints v
                       , (angle + 180) `rem` 360 == angle'
                       ]
