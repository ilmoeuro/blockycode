{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
import           Control.Lens             (Lens', each, filtered, folded,
                                           makeLenses, mapped, over, toListOf,
                                           view, _1, _2, _3)
import           Data.Ord
import           Data.Function
import           Control.Monad            (forM_, guard, when)
import           Control.Monad.Fix
import           Data.List
import           Data.Maybe
import           Data.Traversable
import           Debug.Trace
import           Graphics.UI.WX           hiding (Attr, Event, on)
import qualified Graphics.UI.WX           as WX
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
                      <$> filterE (\((d,_),_) -> d == Dragging) e
        getPos :: EventMouse -> (IsDragging, Point)
        getPos (MouseLeftDrag p _)      = (Dragging, p)
        getPos (MouseMiddleDrag p _)    = (Dragging, p)
        getPos (MouseRightDrag p _)     = (Dragging, p)
        getPos x                        = (NotDragging, mousePos x)

type VisualId = Int
type ZIndex = Int
type Visual = (VisualId, ZIndex, Rect)

hittest' :: ([Visual] -> [Visual])
         -> (a -> Point)
         -> VisualId
         -> Behavior [Visual]
         -> Event a
         -> Event a
hittest' f pointOf oid objs evt = snd <$> filterE flt ((,) <$> objs <@> evt)
    where
        flt (objs,x) = any ((== oid) . view _1)
                     . f
                     . sortBy (comparing (view _2))
                     . filter ((`rectContains` pointOf x) . view _3)
                     $ objs

hittestFront :: (a -> Point) -> VisualId -> Behavior [Visual] -> Event a -> Event a
hittestFront = hittest' (take 1)

hittestBack :: (a -> Point) -> VisualId -> Behavior [Visual] -> Event a -> Event a
hittestBack = hittest' (drop 1)

draggable :: Event (a, Vector) -> Event (Visual -> Visual)
draggable = fmap $ \(_,delta) -> over _3 (`rectMove` delta)

bringToFront :: Event a -> Event (Visual -> Visual)
bringToFront = fmap $ \_ (oid,_,r) -> (oid, 0, r)

sendBackwards :: Event a -> Event (Visual -> Visual)
sendBackwards = fmap $ \_ (oid,z,r) -> (oid,z+1,r)
        
height, width :: Int
height   = 400
width    = 400

data Box = Box { _visual :: Visual, _col :: Color }
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
                initBoxes = [Box (1,0,Rect 50 50 50 50) blue
                            ,Box (2,1,Rect 50 120 50 50) red
                            ,Box (3,2,Rect 120 50 50 50) green
                            ,Box (4,3,Rect 120 120 50 50) grey
                            ]

            (bboxes :: Behavior [Box]) <- mfix $ \bboxes' ->
                fmap sequenceA <$> forM initBoxes $ \box ->
                    accumB box (unions
                        [    over visual 
                         <$> (draggable
                          .  hittestFront
                                (\(p,v) -> pointMove (vecNegate v) p)
                                (view (visual._1) box) 
                                (toListOf (each.visual) <$> bboxes')
                          $  edrag)
                        ,    over visual
                         <$> (bringToFront
                          .  hittestFront
                                id 
                                (view (visual._1) box) 
                                (toListOf (each.visual) <$> bboxes')
                          $  eclick)
                        ,    over visual
                         <$> sendBackwards eclick
                        ])

            let drawBox :: Box -> DC a -> b -> IO ()
                drawBox (Box (_,_,rect) c) dc _ =
                    drawRect dc rect [color := c, bgcolor := c]
                drawBoxes :: [Box] -> DC a -> b -> IO ()
                drawBoxes boxes dc v = forM_
                                        (sortBy (comparing (Down . view (visual._2))) boxes)
                                     $ \b -> drawBox b dc v
        
            -- animate the sprite
            sink pp [WX.on paint :== drawBoxes <$> bboxes]
            reactimate $ repaint pp <$ emouse
            reactimate $ print <$> edrag
    
    network <- compile networkDescription    
    actuate network
