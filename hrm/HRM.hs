import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import Control.Monad(guard,join)
import Data.Char(isSpace, isAlphaNum)
import Text.Read(readMaybe)
import System.Environment(getArgs)

data Instruction  = Input
                  | CopyFrom       Reg
                  | CopyTo         Reg
                  | Inc            Reg
                  | Dec            Reg
                  | Add            Reg
                  | Sub            Reg
                  | JumpIfZero     IIx
                  | JumpIfNegative IIx
                  | Jump           IIx
                  | Output
                    deriving Show

data Reg          = Reg Int
                  | Deref Int
                    deriving Show

type IIx          = Int

type Program      = Vector Instruction

--------------------------------------------------------------------------------

data Value        = Num  Int
                  | Char Char
                    deriving Show

data State        = State { input   :: [Value]
                          , output  :: [Value]
                          , pc      :: IIx
                          , regs    :: Vector (Maybe Value)
                          , acc     :: Maybe Value
                          } deriving Show

--------------------------------------------------------------------------------

regVal :: State -> Int -> Maybe Value
regVal s n = join (regs s Vector.!? n)

reg :: State -> Reg -> Maybe Int
reg s r = case r of
            Reg i   -> valid i
            Deref i -> do Num j <- regVal s i
                          valid j
  where
  valid i = do guard (0 <= i && i < Vector.length (regs s))
               return i


inc :: Value -> Maybe Value
inc v = case v of
          Num x   -> guard (x < maxBound) >> return (Num (succ x))
          Char a  -> guard (a < 'z') >> return (Char (succ a))

dec :: Value -> Maybe Value
dec v = case v of
          Num x  -> guard (minBound < x) >> return (Num (pred x))
          Char a -> guard ('a' < a) >> return (Char (pred a))

add :: Value -> Value -> Maybe Value
add (Num x) (Num y) = Just (Num (x + y))    --  no overflow check
add _ _             = Nothing

sub :: Value -> Value -> Maybe Value
sub (Num x) (Num y)   = Just (Num (x - y))
sub (Char x) (Char y) = Just (Num (fromEnum x - fromEnum y))
sub _ _               = Nothing



step :: Program -> State -> Maybe State
step p s =
  do instr <- p Vector.!? pc s
     case instr of

       Input ->
         case input s of
           []     -> Nothing
           v : vs -> return s { input = vs, acc = Just v, pc = pc s + 1 }

       Output ->
         do v <- acc s
            return s { acc = Nothing, output = v : output s, pc = pc s + 1 }

       CopyFrom r ->
         do v <- regVal s =<< reg s r
            return s { acc = Just v, pc = pc s + 1 }

       CopyTo r ->
         do v <- acc s
            i <- reg s r
            return s { regs = regs s Vector.// [(i,Just v)], pc = pc s + 1 }

       Inc r ->
          do i <- reg s r
             v <- regVal s i
             v1 <- inc v
             return s { regs = regs s Vector.// [(i,Just v1)], acc = Just v1
                      , pc = pc s + 1 }

       Dec r ->
          do i <- reg s r
             v <- regVal s i
             v1 <- dec v
             return s { regs = regs s Vector.// [(i,Just v1)], acc = Just v1
                      , pc = pc s + 1 }

       Add r ->
         do v1 <- acc s
            v2 <- regVal s =<< reg s r
            v3 <- add v1 v2
            return s { acc = Just v3, pc = pc s + 1 }

       Sub r ->
         do v1 <- acc s
            v2 <- regVal s =<< reg s r
            v3 <- sub v1 v2
            return s { acc = Just v3, pc = pc s + 1 }

       Jump i -> return s { pc = i }

       JumpIfZero i ->
          do Num v <- acc s
             return s { pc = if v == 0 then i else pc s + 1 }

       JumpIfNegative i ->
          do Num v <- acc s
             return s { pc = if v < 0 then i else pc s + 1 }


run :: [Maybe Value] -> [Value] -> Program -> State
run rs inp p = go s0
  where
  go s = case step p s of
           Nothing -> s
           Just s1 -> go s1

  s0 = State { input  = inp
             , output = []
             , pc     = 0
             , regs   = Vector.fromList rs
             , acc    = Nothing
             }

--------------------------------------------------------------------------------

parseInstr :: [(String,Int)] -> String -> Maybe Instruction
parseInstr labs ln =
  case words ln of
    [ "input" ]       -> Just Input
    [ "output" ]      -> Just Output
    [ "copyFrom", r ] -> regInstr CopyFrom r
    [ "copyTo", r ]   -> regInstr CopyTo r
    [ "inc", r ]      -> regInstr Inc r
    [ "dec", r ]      -> regInstr Dec r
    [ "add", r ]      -> regInstr Add r
    [ "sub", r ]      -> regInstr Sub r
    [ "jump", i ]     -> jumpInstr Jump i
    [ "jumpZero", i ] -> jumpInstr JumpIfZero i
    [ "jumpNeg", i ]  -> jumpInstr JumpIfNegative i
    _ -> Nothing
  where
  regInstr f r   = f <$> parseReg r
  jumpInstr f r  = f <$> lookup r labs


parseReg :: String -> Maybe Reg
parseReg xs = case xs of
                '[' : ns -> case reads ns of
                              [(a,"]")] -> Just (Deref a)
                              _ ->  Nothing
                ns -> Reg <$> readMaybe ns

findLabels :: [String] -> ([(String,Int)], [String])
findLabels lns = let xs = map isLabel lns
                     labs = count 0 xs
                 in (labs, [ a | Right a <- xs ])
  where
  isLabel ln = case break (== ':') ln of
                 (as,_:bs) | all isAlphaNum as && all isSpace bs -> Left as
                 _ -> Right ln

  count n (Right {} : more) = count (n + 1) more
  count n (Left s : more)  = (s,n) : count n more
  count _ [] = []

parse :: String -> Maybe Program
parse xs = Vector.fromList <$> mapM (parseInstr labs) instrLns
  where
  (labs, instrLns) = findLabels (lines xs)

main :: IO ()
main =
  do a : xs : _ <- getArgs
     txt <- readFile a
     case parse txt of
       Just p -> print (run [] (map Char xs) p)
       Nothing -> fail "Parse error"


