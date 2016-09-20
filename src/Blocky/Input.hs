module Blocky.Input
    ( clicks
    , releases
    , drags
    , Hidden(..)
    ) where

import           Control.Monad.Fix
import           Graphics.UI.WX           hiding (Attr, Event, on, click)
import           Reactive.Banana
import           Blocky.Common

data Hidden = Exposed

clicks :: Event EventMouse -> Event Point
clicks = fmap mousePos . filterE click where
    click (MouseLeftDown _ _) = True
    click _                   = False

releases :: Event EventMouse -> Event Point
releases = fmap mousePos . filterE release where
    release (MouseLeftUp _ _)   = True
    release _                   = False

data IsDragging = Dragging | NotDragging deriving (Eq)
instance Monoid IsDragging where
    mempty = NotDragging
    mappend _ x = x

drags :: (MonadMoment m, MonadFix m) => Event EventMouse -> m (Event (Point, Vector))
drags evt =  onlyDragging
         <$> unaccumE
                (\(_,p) (_,p') -> p `vecBetween` p')
                (pure (pt 0 0))
                (pure getPos <@> evt)
    where
        onlyDragging :: Event ((IsDragging,Point),Vector) -> Event (Point,Vector)
        onlyDragging e =  (\((_,p),v) -> (p,v))
                      <$> filterE (\((d,_),_) -> d == Dragging) e
        getPos :: EventMouse -> (IsDragging, Point)
        getPos (MouseLeftDrag p _)      = (Dragging, p)
        getPos (MouseMiddleDrag p _)    = (Dragging, p)
        getPos (MouseRightDrag p _)     = (Dragging, p)
        getPos x                        = (NotDragging, mousePos x)
