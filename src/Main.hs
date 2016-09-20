{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
module Main where
import           Control.Lens             (each, makeLenses, over, toListOf, view, to)
import           Data.Ord
import           Control.Monad            (forM_)
import           Data.List
import           Data.Traversable
import           Graphics.UI.WX           hiding (Attr, Event, on)
import qualified Graphics.UI.WX           as WX
import           Reactive.Banana
import           Reactive.Banana.WX
import           System.IO
import           Blocky.Interactions
import           Blocky.Input             

height, width :: Int
height   = 400
width    = 400

data Box = Box { _boxVisual :: Visual, _boxColor :: Color }
makeLenses ''Box

main :: IO ()
main = start $ do

    hSetBuffering stdout NoBuffering

    ff <- frame [ text       := "blocky8266"
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
                initBoxes = [Box
                                (visual 1 0 (Rect 50 50 50 50) False
                                     [(vec 25 0, 0)
                                     ,(vec 50 25, 90)
                                     ,(vec 25 50, 180)
                                     ,(vec 0 25, 270)
                                     ])
                                blue
                            ,Box
                                (visual 2 0 (Rect 50 120 50 50) False
                                     [(vec 25 0, 0)
                                     ,(vec 50 25, 90)
                                     ,(vec 25 50, 180)
                                     ,(vec 0 25, 270)
                                     ])
                                red
                            ,Box
                                (visual 3 0 (Rect 120 120 50 100) False
                                     [(vec 25 0, 0)
                                     ,(vec 50 50, 90)
                                     ,(vec 25 100, 180)
                                     ,(vec 0 50, 270)
                                     ])
                                green
                            ,Box
                                (visual 4 0 (Rect 120 50 50 50) False
                                     [(vec 25 0, 0)
                                     ,(vec 50 25, 90)
                                     ,(vec 25 50, 180)
                                     ,(vec 0 25, 270)
                                     ])
                                grey
                            ]

            (bboxes :: Behavior [Box]) <- 
                fmap sequenceA <$> forM initBoxes $ \box ->
                    let hittest' = hittest
                                    id 
                                    (view (boxVisual.to getVisualId) box) 
                                    (toListOf (each.boxVisual) <$> bboxes)
                        eclickme = hittest' eclick
                        ereleaseme = hittest' erelease
                    in accumB box . unions . map (over boxVisual <$>) $
                        [ draggable edrag
                        , startDrag eclickme
                        , stopDrag erelease
                        , snap (map (view boxVisual) <$> bboxes) ereleaseme
                        , bringToFront eclickme
                        , sendBackwards eclick
                        ]

            let drawBox :: Box -> DC a -> b -> IO ()
                drawBox (Box vis c) dc _ =
                    drawRect dc (getBounds vis) [color := c, bgcolor := c]
                drawBoxes :: [Box] -> DC a -> b -> IO ()
                drawBoxes boxes dc v =
                    forM_
                        (sortBy (comparing (Down . getZIndex . view boxVisual)) boxes)
                    $ \b -> drawBox b dc v
        
            -- animate the sprite
            sink pp [WX.on paint :== drawBoxes <$> bboxes]
            reactimate $ repaint pp <$ emouse
    
    network <- compile networkDescription    
    actuate network
