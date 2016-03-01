{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-----------------------------------------------------------------------------
    reactive-banana
    
    Example: Actuate and pause an event network acting as a counter
------------------------------------------------------------------------------}
import           Control.Monad      (when, guard, forM_)
import           Control.Monad.Fix
import           Graphics.UI.WX     hiding (Attr, Event)
import qualified Graphics.UI.WX     as WX
import           Reactive.Banana
import           Reactive.Banana.WX
import           System.IO
import           Debug.Trace

insideBox :: (Point, Size) -> Point -> Bool
insideBox (Point x y, Size w h) (Point x' y') =
        insideX && insideY
    where
        insideX = x <= x' && x' < x+w
        insideY = y <= y' && y' < y+h

data FollowHold = Follow | Hold deriving (Show, Eq, Enum, Bounded)

followAndHold :: MonadMoment m => FollowHold -> Behavior a -> Event FollowHold -> m (Behavior a)
followAndHold initToggle behavior etoggle = do
    init <- valueBLater behavior
    sampled <- stepper init (behavior <@ etoggle)
    let toggle Follow = behavior
        toggle Hold = sampled
    switchB (toggle initToggle) (toggle <$> etoggle)

data DragEvent = StartDrag Point | StopDrag Point deriving (Show)

draggable :: (MonadFix m, MonadMoment m)
          => (Point,Size)
          -> Event EventMouse
          -> m (Event DragEvent, Behavior Point)
draggable (initPos,size@(Size w h)) events = mdo
        position <- do
            mousePos <- accumB initPos (pos <$> events)
            followAndHold Hold mousePos (filterJust (toggle <$> position <@> events))
        let event = dragEvent <$> position <@> events
        return (filterJust event, position)
    where
        centerVector = Vector (-w `div` 2) (-h `div` 2)
        pos (MouseLeftDrag p' _) p = pointMove centerVector p'
        pos (MouseMotion p' _)   p = pointMove centerVector p'
        pos _                    p = p
        toggle p (MouseLeftDown p' _)
            | insideBox (p,size) p'   = Just Follow
        toggle p (MouseLeftUp p' _)
            | insideBox (p,size) p'   = Just Hold
        toggle _ _                    = Nothing
        dragEvent p (MouseLeftDown p' _)
            | insideBox (p,size) p'   = Just (StartDrag p')
        dragEvent p (MouseLeftUp p' _)
            | insideBox (p,size) p'   = Just (StopDrag p')
        dragEvent _ _                 = Nothing
        
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
            let boxw = 50
                boxh = 50
                boxes = [(50,50), (120, 50), (50, 120), (120, 120)]

            emouse <-
                event1 pp mouse

            draggables <-
                mapM
                    (\(x,y) -> draggable (Point x y, Size boxw boxh) emouse)
                    boxes

            let bpositions = sequenceA . map snd $ draggables
            let edrag = foldr (unionWith const) never (map fst draggables)
            
            let drawBox :: Point -> DC a -> b -> IO ()
                drawBox Point{pointX,pointY} dc _ =
                    drawRect dc (Rect pointX pointY boxw boxh) []
                drawBoxes :: [Point] -> DC a -> b -> IO ()
                drawBoxes boxes dc v = forM_ boxes $ \b -> drawBox b dc v
        
            -- animate the sprite
            sink pp [on paint :== drawBoxes <$> bpositions]
            reactimate $ repaint pp <$ emouse
            reactimate $ print <$> edrag
    
    network <- compile networkDescription    
    actuate network
