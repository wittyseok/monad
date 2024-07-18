-- | Assignment 2: CPU core actions
module A02
  ( Reg
  , UnaryOp
  , BinaryOp
  , Operand
  , Instr
  , encodeReg
  , encodeUnaryOp
  , encodeBinaryOp
  , encodeOperand
  , encodeInstr
  , decodeReg
  , decodeUnaryOp
  , decodeBinaryOp
  , decodeOperand
  , decodeInstr
  ) where

import           GHC.Generics                   ( Generic )
import           Generic.Random
import           Test.Tasty.QuickCheck         as QC
                                         hiding ( (.&.) )

import           Data.Bits
import           Data.Word

import           A02_Defs

-- | TODO marker.
todo :: t
todo = error "todo"

decodeReg :: Word32 -> Maybe Reg
decodeReg w = if w<32 then Just (Reg w) else Nothing

decodeUnaryOp :: Word32 -> Maybe UnaryOp
decodeUnaryOp 0 = Just Move
decodeUnaryOp 1 = Just Negate
decodeUnaryOp 2 = Just Complement
decodeUnaryOp 3 = Just Not
decodeUnaryOp _ = Nothing


decodeBinaryOp :: Word32 -> Maybe BinaryOp
decodeBinaryOp 0 = Just Add
decodeBinaryOp 1 = Just Sub
decodeBinaryOp 2 = Just Mul
decodeBinaryOp 3 = Just Or
decodeBinaryOp 4 = Just And
decodeBinaryOp 5 = Just Xor
decodeBinaryOp 6 = Just Lt
decodeBinaryOp 7 = Just Gt
decodeBinaryOp 8 = Just Eq
decodeBinaryOp _ = Nothing

decodeOperand :: Word32 -> Maybe Operand
decodeOperand w
  | odd w = if w<1024 then Just (OperandVal (Val (shiftR w 1))) else Nothing
  | even w = if w<64 then Just (OperandReg (remove (decodeReg (shiftR w 1)))) else Nothing
    where remove (Just x) = x


remove::Maybe a -> a
remove (Just x) = x

clip :: Word32 -> Int ->  Word32
clip w size = shiftL w size -1

-- | Decode instruction. The 3 LSB bits are op-codes.
decodeInstr :: Word32 -> Maybe Instr
decodeInstr w = case (w.&.7) of
  0 -> do
    if decodeUnaryOp ((shiftR w 8).&.(clip 1 4)) == Nothing 
      then Nothing 
      else do 
        if decodeOperand((shiftR w 12).&.(clip 1 10)) == Nothing 
          then Nothing 
          else do 
            Just(Unary ( remove(decodeReg ((shiftR w 3).&.(clip 1 5))) ,remove(decodeUnaryOp ((shiftR w 8).&.(clip 1 4))) ,  remove(decodeOperand ((shiftR w 12).&.(clip 1 10)))))
  1 -> do 
    if decodeBinaryOp ((shiftR w 8).&.(clip 1 4)) == Nothing 
      then Nothing 
      else do 
        if decodeOperand((shiftR w 12).&.(clip 1 10)) == Nothing 
          then Nothing 
          else do 
            if decodeOperand((shiftR w 22).&.(clip 1 10)) == Nothing 
              then Nothing 
              else do
                 Just (Binary(remove(decodeReg((shiftR w 3).&.(clip 1 5))) ,remove(decodeBinaryOp((shiftR w 8).&.(clip 1 4))) ,remove(decodeOperand((shiftR w 12).&.(clip 1 10))) ,remove(decodeOperand((shiftR w 22).&.(clip 1 10)))))
  2 -> do
    if decodeOperand((shiftR w 12).&.(clip 1 10)) == Nothing 
      then Nothing 
      else do 
        Just (Load( remove(decodeReg((shiftR w 3).&.(clip 1 5))), remove(decodeOperand((shiftR w 12).&.(clip 1 10)))))
  3 -> do
    if decodeOperand((shiftR w 12).&.(clip 1 10)) == Nothing 
      then Nothing 
      else do 
        if decodeOperand((shiftR w 22).&.(clip 1 10)) == Nothing 
          then Nothing 
          else do 
            Just (Store( remove(decodeOperand ((shiftR w 12).&.(clip 1 10))), remove(decodeOperand ((shiftR w 22).&.(clip 1 10)))))
  4 -> do
    if decodeReg((shiftR w 3).&.(clip 1 5)) == Nothing 
      then Nothing 
      else do 
        if decodeOperand((shiftR w 12).&.(clip 1 10)) == Nothing 
          then Nothing 
          else do 
            if decodeOperand((shiftR w 22).&.(clip 1 10)) == Nothing 
              then Nothing 
              else do 
                Just(Cas(remove(decodeReg((shiftR w 3).&.(clip 1 5))) ,remove(decodeOperand((shiftR w 12).&.(clip 1 10))),remove(decodeOperand((shiftR w 22).&.(clip 1 10))))) 
  5 -> do 
    if decodeOperand((shiftR w 12).&.(clip 1 10)) == Nothing 
      then Nothing 
      else do 
        Just (Jump ( remove(decodeOperand((shiftR w 12).&.(clip 1 10)))))
  6 -> do
    if decodeOperand((shiftR w 12).&.(clip 1 10)) == Nothing 
      then Nothing 
      else do 
        if decodeOperand((shiftR w 22).&.(clip 1 10)) == Nothing 
          then Nothing 
          else do 
            Just (CondJump(remove (decodeReg ((shiftR w 3).&.(clip 1 5))) ,remove(decodeOperand((shiftR w 12).&.(clip 1 10))) ,remove(decodeOperand((shiftR w 22).&.(clip 1 10)))))
  _ -> Nothing
 