{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-----------------------------------------------------------------------------
    reactive-banana
    
    Example: Actuate and pause an event network acting as a counter
------------------------------------------------------------------------------}
import           Data.Maybe
import           Data.List
import           Data.Traversable
import           Control.Monad      (when, guard, forM_)
import           Control.Monad.Fix
import           Graphics.UI.WX     hiding (Attr, Event)
import qualified Graphics.UI.WX     as WX
import           Reactive.Banana
import           Reactive.Banana.WX
import           System.IO
import           Debug.Trace

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

eclick :: Event EventMouse -> Event Point
eclick = fmap mousePos . filterE click where
    click (MouseLeftDown _ _) = True
    click _                   = False

edrag :: (MonadMoment m, MonadFix m)
      => Event EventMouse
      -> m (Event (Point, Vector))
edrag evt =  onlyDragging
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

hittest :: [Rect]
        -> Rect
        -> Point
        -> Bool
hittest rs r pt = (== [r])
                . take 1
                . reverse -- TODO optimize
                . filter (`rectContains` pt)
                $ rs

draggable :: Event (Point, Vector)
          -> Event ([Rect] -> [Rect])
draggable = fmap $ \(point,delta) rects -> rects <$$> \rect ->
    if hittest rects rect point then rect `rectMove` delta else rect

bringToFront :: Event Point
             -> Event ([Rect] -> [Rect])
bringToFront = fmap $ \p rs -> uncurry (flip (++))
                             . traceShowId 
                             . partition (\r -> hittest rs r p)
                             $ rs
        
height, width :: Int
height   = 400
width    = 400

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

            edrag' <-
                edrag emouse

            let eclick' = eclick emouse
                initRects = [Rect 50 50 50 50
                            ,Rect 120 50 50 50
                            ,Rect 50 120 50 50
                            ,Rect 120 120 50 50
                            ]

            brects <-
                accumB initRects $ unions
                    [draggable edrag'
                    ,bringToFront eclick'
                    ]

            let drawBox :: Rect -> DC a -> b -> IO ()
                drawBox rect dc _ =
                    drawRect dc rect []
                drawBoxes :: [Rect] -> DC a -> b -> IO ()
                drawBoxes boxes dc v = forM_ boxes $ \b -> drawBox b dc v
        
            -- animate the sprite
            sink pp [on paint :== drawBoxes <$> brects]
            reactimate $ repaint pp <$ emouse
            reactimate $ print <$> edrag'
    
    network <- compile networkDescription    
    actuate network
