{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module A04
  ( interpThread
  ) where

import           Control.Monad.State.Lazy
import           Data.Bits
import           Data.Map                      as Map
import           Data.Maybe
import           Data.Word
import           Prelude                 hiding ( read )

import           S10_Composition

import           A02
import           A02_Defs
import           A03
import           A03_Defs
import           A04_Defs

-- | TODO marker.
todo :: t
todo = error "todo"

-- | Interpret a thread.
--
-- `Map Word32 RegFile` maps thread id to its state, i.e., register file.
--
-- `interpThread` is given a thread id, and you need to access the thread states and memory to execute the thread of the given id.
-- In doing so, the corresponding thread state and memory should be read and written.
--
-- If the given thread id doesn't have a thread, invoke `InvalidThreadId` fault (i.e., writing `0x22` to `0xdeadbeef`).
interpThread :: Word32 -> StateT (Map Word32 RegFile) (State Mem) ()
interpThread id = do
  thread <- get
  Mem mem <- lift get
  valid <- thread |> Map.lookup id |> return
  case valid of
    Nothing -> mem |> (Map.insert (Loc (Val 0xdeadbeef)) (Val 0x22)) |> Mem |> put |> lift
    Just regfile -> do
      let (new_regfile,Mem new_mem) = runStep regfile (Mem mem)
      new_mem |> Mem |> put |> lift
      thread |> Map.insert id new_regfile |> put
      return()




  
