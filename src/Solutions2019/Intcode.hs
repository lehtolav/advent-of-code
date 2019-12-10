module Solutions2019.Intcode where

import Prelude hiding(replicate, drop, take)
--import Data.Sequence (Seq, index, update)
import qualified Data.Sequence as S
import Control.Monad.RWS.Strict
import Data.Foldable
import Control.Monad.Fail
import Control.Monad.Identity

import Debug.Trace

{- Seq = []
index = (!!)

update :: Value -> Value -> Memory -> Memory
update i n list =
    if i == 0
    then n:tail list
    else head list:update (i - 1) n (tail list)
type Seq a = [a]
-}

type Seq = S.Seq
index = S.index
update = S.update
replicate = S.replicate
drop = S.drop
take = S.take

type Value = Integer
type Memory = Seq Value
type Output = Memory
type Instruction = [Value] -> Op ()
data ProgramState = ProgramState { memory :: Memory, getCounter :: Value, getInput :: [Value], getRelativeBase :: Value }
  deriving(Show)

type Op = RWS Memory Output ProgramState

instance MonadFail Identity where
    fail desc = fix (error desc)

-- Memory puukoting

putTo :: Value -> Value -> Op ()
putTo location value = modify $ \state -> state { memory = update (fromIntegral location) value (memory state) }

putWithMode :: Value -> Value -> Value -> Op ()
putWithMode 0 location value = putTo location value
putWithMode 2 location value = relativeBase >>= \x -> putTo (location + x) value

getFrom :: Value -> Op Value
getFrom location = fmap (`index` fromIntegral location) (gets memory)

getWithMode :: Value -> Value -> Op Value
getWithMode 0 pos = getFrom pos
getWithMode 1 x = return x
getWithMode 2 pos = relativeBase >>= getFrom . (+pos)

advance :: Value -> Op ()
advance amount = modify $ \state -> state { getCounter = getCounter state + amount }

jumpTo :: Value -> Op ()
jumpTo location = modify $ \state -> state { getCounter = location }

input :: Op Value
input = do
    state <- get
    let (x:xs) = getInput state
    put $ state { getInput = xs }
    return x

output :: Value -> Op ()
output = tell . pure

counter :: Op Value
counter = gets getCounter

args :: Op Memory
args = do
    loc <- counter
    mem <- gets memory
    return (drop (fromIntegral loc + 1) mem)

nargs :: Int -> [Value] -> Op [Value]
nargs n modes = do
    as <- args
    zipWithM getWithMode modes (toList (take n as))

modifyRelativeBase :: Value -> Op ()
modifyRelativeBase amount = modify $ \state -> state { getRelativeBase = getRelativeBase state + amount }

relativeBase :: Op Value
relativeBase = gets getRelativeBase

-- Actual ops

binOp :: (Value -> Value -> Value) -> Instruction
binOp op (m1:m2:m3:_) = do
    [a1, a2, a3] <- nargs 3 [m1, m2, 1]
    putWithMode m3 a3 (a1 `op` a2)
    advance 4

fromInput :: Instruction
fromInput (m1:_) = do
    [a1] <- nargs 1 [1]
    inp <- input
    putWithMode m1 a1 inp
    advance 2

toOutput :: Instruction
toOutput modes = do
    [a1] <- nargs 1 modes
    output a1
    advance 2

jumpIf :: (Value -> Bool) -> Instruction
jumpIf predicate modes = do
    [a1, a2] <- nargs 2 modes
    if predicate a1 then jumpTo a2 else advance 3
    
ifToValue :: Bool -> Value
ifToValue True = 1
ifToValue False = 0

binBOp :: (Value -> Value -> Bool) -> Instruction
binBOp op = binOp (\x y -> ifToValue (op x y))

modRelBase :: Instruction
modRelBase modes = do
    [a1] <- nargs 1 modes
    modifyRelativeBase a1
    advance 2

opcodes :: [(Value, Instruction)]
opcodes = zip [1..] [binOp (+), binOp (*), fromInput, toOutput, jumpIf (/=0), jumpIf (==0), binBOp (<), binBOp (==), modRelBase]

runIntcode :: [Value] -> [Value] -> (ProgramState, Output)
runIntcode input memory = execRWS intcodeOp S.empty (ProgramState (S.fromList memory S.>< replicate 100000 0) 0 input 0)

toModes :: Value -> [Value]
toModes 0 = repeat 0
toModes number = r : toModes q
  where (q, r) = number `quotRem` 10

intcodeOp :: Op ()
intcodeOp = do
    loc <- counter
    inst <- getFrom loc
    let (modes, opcode) = inst `quotRem` 100
    let mop = lookup opcode opcodes
    maybe (return ()) (\op -> op (toModes modes) >> intcodeOp) mop