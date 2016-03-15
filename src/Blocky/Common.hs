{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module Blocky.Common
    (Component
    ,traceNoLF
    ,unaccumE
    ,(<$$>)
    ) where

import           Control.Monad.Fix
import           Reactive.Banana
import           Reactive.Banana.WX
import           System.IO.Unsafe

type Component bi ei eo = (bi, ei) -> MomentIO eo

traceNoLF :: String -> a -> a
traceNoLF s x = s `seq` unsafePerformIO (putStr s) `seq` x
{-# NOINLINE traceNoLF #-}

unaccumE :: forall a b m.  (MonadMoment m, MonadFix m)
         => (a -> a -> b)
         -> a
         -> Event a
         -> m (Event (a,b))
unaccumE f initval evt = mdo
    let result :: Event (a,b)
        result = (\(x,_) x' -> (x', x `f` x')) <$> state <@> evt
    state <- stepper (initval,undefined) result
    return result

(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip (<$>)
