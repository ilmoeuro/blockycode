{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
module Main where
import           Control.Lens             (Lens', each, filtered, folded,
                                           makeLenses, mapped, over, toListOf,
                                           view, _1, _2, _3, _4)
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
import           System.IO.Unsafe
import           Blocky.Common
import           Blocky.Interactions
import           Blocky.Input

height, width :: Int
height   = 400
width    = 400

data Box = Box { _visual :: Visual, _col :: Color }
makeLenses ''Box

main :: IO ()
main = start $ do

    hSetBuffering stdout NoBuffering

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
                erelease = releases emouse
                initBoxes = [Box (1,0,Rect 50 50 50 50,False) blue
                            ,Box (2,1,Rect 50 120 50 50,False) red
                            ,Box (3,2,Rect 120 50 50 50,False) green
                            ,Box (4,3,Rect 120 120 50 50,False) grey
                            ]

            (bboxes :: Behavior [Box]) <- mfix $ \bboxes' ->
                fmap sequenceA <$> forM initBoxes $ \box ->
                    let hittest' = hittest
                                    id 
                                    (view (visual._1) box) 
                                    (toListOf (each.visual) <$> bboxes')
                        eclickme = hittest' eclick
                    in accumB box . unions . map (over visual <$>) $
                        [ draggable edrag
                        , startDrag eclickme
                        , stopDrag erelease
                        , bringToFront eclickme
                        , sendBackwards eclick
                        ]

            let drawBox :: Box -> DC a -> b -> IO ()
                drawBox (Box (_,_,rect,_) c) dc _ =
                    drawRect dc rect [color := c, bgcolor := c]
                drawBoxes :: [Box] -> DC a -> b -> IO ()
                drawBoxes boxes dc v =
                    forM_
                        (sortBy (comparing (Down . view (visual._2))) boxes)
                    $ \b -> drawBox b dc v
        
            -- animate the sprite
            sink pp [WX.on paint :== drawBoxes <$> bboxes]
            reactimate $ repaint pp <$ emouse
    
    network <- compile networkDescription    
    actuate network
