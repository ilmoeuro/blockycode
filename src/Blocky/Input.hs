module Blocky.Input where
import           Control.Monad            (forM_, guard, when)
import           Control.Monad.Fix
import           Graphics.UI.WX           hiding (Attr, Event, on)
import qualified Graphics.UI.WX           as WX
import           Reactive.Banana
import           Reactive.Banana.WX
import           Blocky.Common

data IsDragging = Dragging | NotDragging deriving (Eq)
instance Monoid IsDragging where
    mempty = NotDragging
    mappend = flip const

clicks :: Event EventMouse -> Event Point
clicks = fmap mousePos . filterE click where
    click (MouseLeftDown _ _) = True
    click _                   = False

releases :: Event EventMouse -> Event Point
releases = fmap mousePos . filterE release where
    release (MouseLeftUp _ _)   = True
    release _                   = False

drags :: (MonadMoment m, MonadFix m) => Event EventMouse -> m (Event (Point, Vector))
drags evt =  onlyDragging
         <$> unaccumE
                (\(_,p) (i,p') -> p `vecBetween` p')
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
