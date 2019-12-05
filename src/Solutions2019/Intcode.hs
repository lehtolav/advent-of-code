module Solutions2019.Intcode where

import Data.Sequence (Seq, index, update)
import qualified Data.Sequence as S
import Control.Monad.RWS
import Data.Foldable
import Control.Monad.Fail
import Control.Monad.Identity

import Debug.Trace

type Value = Int
type Memory = Seq Value
type Output = Memory
type Instruction = [Value] -> Op ()
data ProgramState = ProgramState { memory :: Memory, getCounter :: Value, getInput :: [Value] }
  deriving(Show)

type Op = RWS Memory Output ProgramState

instance MonadFail Identity where
    fail desc = fix (error desc)

-- Memory puukoting

putTo :: Value -> Value -> Op ()
putTo location value = modify $ \state -> state { memory = update location value (memory state) }

getFrom :: Value -> Op Value
getFrom location = fmap (`index` location) (gets memory)

getWithMode :: Value -> Value -> Op Value
getWithMode 0 pos = getFrom pos
getWithMode 1 x = return x

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
    return (S.drop (loc + 1) mem)

nargs :: Value -> [Value] -> Op [Value]
nargs n modes = do
    as <- args
    zipWithM getWithMode modes (toList (S.take n as))

-- Actual ops

binOp :: (Value -> Value -> Value) -> Instruction
binOp op (m1:m2:_) = do
    [a1, a2, a3] <- nargs 3 [m1, m2, 1]
    putTo a3 (a1 `op` a2)
    advance 4

fromInput :: Instruction
fromInput _ = do
    [a1] <- nargs 1 [1]
    inp <- input
    putTo a1 inp
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

opcodes :: [(Value, [Value] -> Op ())]
opcodes = zip [1..] [binOp (+), binOp (*), fromInput, toOutput, jumpIf (/=0), jumpIf (==0), binBOp (<), binBOp (==)]

runIntcode :: [Value] -> [Value] -> (ProgramState, Output)
runIntcode input memory = execRWS intcodeOp S.empty (ProgramState (S.fromList memory) 0 input)

toModes :: Int -> [Int]
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