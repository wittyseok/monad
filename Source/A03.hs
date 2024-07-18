{-# LANGUAGE FlexibleInstances #-}

module A03
  ( interpInstr
  , interpStep
  , runInstr
  , runStep
  ) where

import           GHC.Generics                   ( Generic )
import           Generic.Random
import           Test.Tasty.QuickCheck         as QC
                                         hiding ( (.&.) )

import           Control.Monad.State.Lazy
import           Data.Bits
import           Data.List
import           Data.Map                      as Map
import           Data.Maybe
import           Data.Word
import           Prelude                 hiding ( read )

import           A02
import           A02_Defs
import           A03_Defs

-- | TODO marker.
todo :: t
todo = error "todo"

-- | Interpret an instruction.
--
-- For the detailed specification, please consult the test cases.
interpInstr :: CoreState m => Instr -> m InstrResult
interpInstr instr = case instr of
  Unary (reg,op,src1) -> do
    src1 <- interpOperand src1
    let val = interpUnaryOp op src1
    regW reg val
    return Normal
  
  Binary(reg,op,src1,src2) -> do
    src1 <- interpOperand src1
    src2 <- interpOperand src2
    let val = interpBinaryOp op src1 src2
    regW reg val
    return Normal

  Load(reg,src1) ->do
    src1 <-interpOperand src1
    val <- memL (Loc src1)
    case val of
      Nothing -> do
        ctrlFault InvalidLoad
        return Fault
      Just val -> do
        regW reg val
        return Normal

  Cas (reg ,src1, src2) ->do
    src1 <- interpOperand src1
    src2 <- interpOperand src2
    old_value <- regR reg
    val <-memCas (Loc old_value) src1 src2
    case val of
      Nothing ->do
        ctrlFault InvalidLoad
        return Fault
      Just (eq,vali) ->do
        case eq of
          True ->do
            regW reg src1
            return Normal
          False ->do
            regW reg vali
            return Normal

  Store (src1,src2) -> do
    src1 <-interpOperand src1
    src2 <- interpOperand src2
    memS (Loc src1) src2
    return Normal

  Jump src1 -> do
    src1 <- interpOperand src1
    return $ JumpTo $ Loc src1

  CondJump (cond,src1,src2) -> do
    cond <- regR cond
    src1 <- interpOperand src1
    src2 <- interpOperand src2
    return $ JumpTo $ Loc(if cond /= Val 0 then src1 else src2)

-- | Interpret a step.
--
-- You need to:
-- 1. read the `pcReg` register as `pc`.
-- 2. load memory from the `pc` location. If it's invalid, `ctrlFault InvalidLoad`.
-- 3. decode the load value as instruction. If it's invalid, `ctrlFault InvalidInstr`.
-- 4. interpret the instruction.
-- 5-1. if the result is normal, add 1 to `pcReg`.
-- 5-2. if the result is fault, return ().
-- 5-3. if the result is jumpTo, jump to the specified location.
interpStep :: CoreState m => m ()
interpStep = do
  pc <-regR pcReg
  instr <- memL (Loc pc)
  case instr of
    Nothing -> ctrlFault InvalidLoad
    Just (Val instr) -> case decodeInstr instr of
      Nothing -> ctrlFault InvalidInstr
      Just instr -> do
        result <- interpInstr instr
        case result of
          Fault -> return ()
          JumpTo (Loc loc) -> regW pcReg loc
          Normal -> do
            regW pcReg (f pc)
            where f (Val x) = Val (x+1)

-- | Execute an instruction.
runInstr :: RegFile -> Mem -> Instr -> (InstrResult, (RegFile, Mem))
runInstr regFile mem instr = runState (interpInstr instr) (regFile, mem)

-- | Execute a step.
runStep :: RegFile -> Mem -> (RegFile, Mem)
runStep regFile mem = runState interpStep (regFile, mem) |> snd
