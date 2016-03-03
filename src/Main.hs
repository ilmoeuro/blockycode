{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
import           Control.Lens       (Lens', each, filtered, folded, mapped,
                                     over, toListOf, view, makeLenses)
import           Control.Monad      (forM_, guard, when)
import           Control.Monad.Fix
import           Data.List
import           Data.Maybe
import           Data.Traversable
import           Debug.Trace
import           Graphics.UI.WX     hiding (Attr, Event)
import qualified Graphics.UI.WX     as WX
import           Reactive.Banana
import           Reactive.Banana.WX
import           System.IO

unaccumE :: forall a b m.  (MonadMoment m, MonadFix m)
         => (a -> a -> b)
         -> a
         -> Event a
         -> m (Event (a,b))
unaccumE f init evt = mdo
    let result :: Event (a,b)
        result = (\(x,_) x' -> (x', x `f` x')) <$> state <@> evt
    state <- stepper (init,undefined) result
    return result

(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip (<$>)

data IsDragging = Dragging | NotDragging deriving (Eq)
instance Monoid IsDragging where
    mempty = NotDragging
    mappend = flip const

clicks :: Event EventMouse -> Event Point
clicks = fmap mousePos . filterE click where
    click (MouseLeftDown _ _) = True
    click _                   = False

drags :: (MonadMoment m, MonadFix m) => Event EventMouse -> m (Event (Point, Vector))
drags evt =  onlyDragging
         <$> unaccumE
                (\(_,p) (i,p') -> p `vecBetween` p')
                (pure (pt 0 0))
                (pure getPos <@> evt)
    where
        onlyDragging :: Event ((IsDragging,Point),Vector) -> Event (Point,Vector)
        onlyDragging e =  (\((_,p),v) -> (p,v))
                      <$> (filterE (\((d,_),_) -> d == Dragging) e)
        getPos :: EventMouse -> (IsDragging, Point)
        getPos (MouseLeftDrag p _)      = (Dragging, p)
        getPos (MouseMiddleDrag p _)    = (Dragging, p)
        getPos (MouseRightDrag p _)     = (Dragging, p)
        getPos x                        = (NotDragging, mousePos x)

hittest :: [Rect] -> Rect -> Point -> Bool
hittest rs r pt = (== [r])
                . take 1
                . reverse
                . filter (`rectContains` pt)
                $ rs

draggable :: Lens' a Rect -> Event (Point, Vector) -> Event ([a] -> [a])
draggable lrect = fmap $ \(point,delta) objs ->
    over 
        ( each
        . lrect
        . filtered (\rect -> hittest
            (toListOf (folded.lrect) objs)
            rect
            point))
        (`rectMove` delta)
        objs

bringToFront :: Lens' a Rect -> Event Point -> Event ([a] -> [a])
bringToFront lrect = fmap $ \p objs ->
      uncurry (flip (++))
    . partition (\r -> hittest
        (toListOf (folded.lrect) objs)
        (view lrect r)
        p)
    $ objs
        
height, width :: Int
height   = 400
width    = 400

data Box = Box { _bounds :: Rect, _col :: Color }
makeLenses ''Box

main :: IO ()
main = start $ do
    ff <- frame [ text       := "It's functional programming time"
                , bgcolor    := white
                ]

    pp <- panel ff [ ]
    set ff [ layout  := fill . minsize (sz width height) $ widget pp ]
    
    -- event network
    let networkDescription :: MomentIO ()
        networkDescription = mdo
            emouse <-
                event1 pp mouse

            edrag <-
                drags emouse

            let eclick = clicks emouse
                initBoxes = [Box (Rect 50 50 50 50) blue
                            ,Box (Rect 120 50 50 50) red
                            ,Box (Rect 50 120 50 50) white
                            ,Box (Rect 120 120 50 50) black
                            ]

            bboxes <-
                accumB initBoxes $ unions
                    [draggable bounds edrag
                    ,bringToFront bounds eclick
                    ]

            let drawBox :: Box -> DC a -> b -> IO ()
                drawBox (Box rect c) dc _ =
                    drawRect dc rect [color := c]
                drawBoxes :: [Box] -> DC a -> b -> IO ()
                drawBoxes boxes dc v = forM_ boxes $ \b -> drawBox b dc v
        
            -- animate the sprite
            sink pp [on paint :== drawBoxes <$> bboxes]
            reactimate $ repaint pp <$ emouse
            reactimate $ print <$> edrag
    
    network <- compile networkDescription    
    actuate network
