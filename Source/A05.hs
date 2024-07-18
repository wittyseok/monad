module A05
  ( ShrMem (..)
  , newShrMem
  , loadShrMem
  , storeShrMem
  , casShrMem
  ) where

import           Control.Concurrent.MVar
import           Data.Map                      as Map

import           A02_Defs
import           A03_Defs
import           A05_Defs

-- | TODO marker.
todo :: t
todo = error "todo"


newShrMem :: IO ShrMem
newShrMem = do
  shrmem_init <- newMVar Map.empty
  return (ShrMem shrmem_init)

loadShrMem :: Loc -> ShrMem -> IO (Maybe Val)
loadShrMem loc (ShrMem mem) = do
  memory_map <- readMVar mem
  value <- memory_map |> Map.lookup loc |> return
  case value of
    Nothing -> return Nothing
    Just value -> do
      val <- readMVar value
      return (Just val)

storeShrMem :: Loc -> Val -> ShrMem -> IO ()
storeShrMem loc val (ShrMem mem) = do
  memory_map <- readMVar mem
  value <- memory_map |> Map.lookup loc |> return
  case value of
    Nothing -> do
      new_map <- takeMVar mem
      val <- newMVar val
      putMVar mem (Map.insert loc val new_map)
    Just value -> do
      old_val <- takeMVar value
      putMVar value val


casShrMem :: Loc -> Val -> Val -> ShrMem -> IO (Maybe (Bool, Val))
casShrMem loc val1 val2 (ShrMem mem) = do
  memory_map <- readMVar mem
  value <- memory_map |> Map.lookup loc |> return
  case value of
    Nothing -> return Nothing
    Just value -> do
      val <- takeMVar value
      if val /= val1
        then do
          putMVar value val
          return $ Just(False,val)
        else do
          putMVar value val2
          return $ Just(True,val)



        


