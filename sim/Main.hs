module Main (main) where

import MachineState (emptyMachine, machineOps)
import qualified Progs
import Small (reduceFully)
import Term (Term (..))

infixl 1 ~

(~) :: Term -> Term -> Term
(~) = Seq

infixl 9 <=>

(<=>) :: String -> Term -> Term
(<=>) = Let

prog :: Term
prog =
  "x" <=> Literal 10
    ~ "y" <=> Literal 29
    ~ "z" <=> Literal 3

main :: IO ()
main = do
  let out = reduceFully machineOps prog emptyMachine
  print out
  putStrLn "-----------------------------"
  let out2 = reduceFully machineOps Progs.prog emptyMachine
  print out2
  putStrLn "-----------------------------"
  putStrLn "Testing booleans and comparisons:"
  let out3 = reduceFully machineOps Progs.prog3 emptyMachine
  print out3
