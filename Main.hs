{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Exception
import           Control.Monad
import           Data.Ratio
import           System.Console.Haskeline
import           Text.Read

type OpName = String
type Value = Rational
type Stack = [Value]

data Instr
  = Push Value
  | Op (Value -> Value -> Value) OpName

epsilon :: Double
epsilon = 1e-10

parseInstr :: String -> Maybe Instr
parseInstr = \case
  "+" -> Just $ Op (+) "+"
  "-" -> Just $ Op (-) "-"
  "*" -> Just $ Op (*) "*"
  "/" -> Just $ Op (/) "/"
  x   -> Push . (`approxRational` epsilon) <$> readMaybe x

data SemanticError
  = EmptyStack OpName
  | LeftoverStack Stack
  | NotEnoughArguments Value OpName
  | DivisionByZero Value
  deriving Show

exec :: [Instr] -> Either SemanticError (Maybe Value)
exec = exec' []
  where
    exec' :: Stack -> [Instr] -> Either SemanticError (Maybe Value)
    exec' ns (Push n : is) = exec' (n:ns) is
    exec' [] (Op _ name : _) = Left $ EmptyStack name
    exec' [n] (Op _ name : _) = Left $ NotEnoughArguments n name
    exec' (m:n:ns) (Op op name : is) =
      if name == "/" && m == 0
        then Left $ DivisionByZero n
        else exec' (n `op` m : ns)  is
    exec' [] [] = Right Nothing
    exec' [n] [] = Right $ Just n
    exec' s@(_:_:_) [] = Left $ LeftoverStack s

data Result = Result Value | DoNothing | Exit | SyntaxError | SemanticError SemanticError
  deriving Show

rpn :: String -> Result
rpn =
  maybe SyntaxError
    ( either SemanticError (maybe DoNothing Result)
    . exec
    )
  . traverse parseInstr
  . words

main :: IO ()
main = runInputT (Settings noCompletion Nothing True) prog
  where
    prog = handleInterrupt (outputStrLn "Exiting...") (start *> loop)
    start = outputStrLn "Reverse Polish Notation calculator (Ctrl-D to exit)"

    loop :: InputT IO ()
    loop = withInterrupt $ forever $ do
      str <- getInputLine "> "
      case maybe Exit rpn str of
        Result res -> outputStrLn $ displayResult res
        DoNothing -> pure ()
        Exit -> throw Interrupt
        SyntaxError -> outputStrLn "error: invalid syntax"
        SemanticError err -> case err of
          EmptyStack op -> outputStrLn $ "error: cannot perform operation " <> op <> " on an empty stack"
          LeftoverStack stack -> outputStrLn $ "error: excessive values left over: " <> unwords (displayResult <$> stack)
          NotEnoughArguments n op -> outputStrLn $
            "error: not enough arguments, cannot apply operation " <> op <> " to just " <> displayResult n
          DivisionByZero n -> outputStrLn $ "error: cannot divide " <> displayResult n <> " by 0"

    displayResult x = if denominator x == 1
      then show $ numerator x
      else show (fromRational x :: Double)
