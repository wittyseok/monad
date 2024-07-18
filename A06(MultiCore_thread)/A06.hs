{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module A06
  (interpMachine) where

import System.IO.Unsafe
import           Control.Monad.State.Lazy
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Data.Map                      as Map
import           Data.Maybe
import           Data.Word
import Text.Printf

import           A02_Defs
import           A03_Defs
import           A04_Defs
import           A05_Defs
import           A03
import           A05

-- RegFile = RegFile (Map Reg Val)
-- Mem = Mem (Map Loc Val)
-- ShrMem = ShrMem (MVar (Map Loc (MVar Val)))

-- | TODO marker.
todo :: t
todo = error "todo"

forkOSResult :: IO a -> IO (MVar a)
forkOSResult act = do
  var <- newEmptyMVar
  forkOS $ do
    value <- act
    putMVar var value
  return var

instance CoreState (StateT RegFile (StateT ShrMem IO)) where
  regR reg = do
    RegFile regFile <- get
    regFile |> Map.lookup reg |> fromMaybe (Val 0) |> return

  regW reg val = do
    RegFile regFile <- get
    regFile |> Map.insert reg val |> RegFile |> put

  memL loc = do
    ShrMem mem <- lift get
    unsafePerformIO(loadShrMem loc (ShrMem mem))  |> return

  memS loc val = do
    ShrMem mem <- lift get
    storeShrMem loc val (ShrMem mem) |> lift |> lift

  memCas loc val1 val2 = do
    val <- memL loc
    case val of
      Nothing -> return Nothing
      Just val -> if val /= val1
        then return $ Just(False, val)
        else do
          memS loc val2
          return $ Just (True, val)

interpMachine :: StateT (Map Word32 RegFile) (StateT ShrMem IO) ()
interpMachine = StateT $ \regFiles -> StateT $ \shrMem -> do
  regFiles <- forM regFiles $ \regFile -> forkOSResult $ loop regFile shrMem
  regFiles <- forM regFiles takeMVar
  return (((), regFiles), shrMem)
  where
    loop regFile shrMem = do
      val <- loadShrMem (Loc $ Val 0x30) shrMem
      (((), regFileNew),_) <- runStateT (runStateT interpStep regFile) shrMem
      fault <- loadShrMem ctrlLoc shrMem
      if fault == Just (Val 0x10)
        then return regFileNew
        else loop regFileNew shrMem
