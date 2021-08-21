module Idris.Main

import Core.Context
import Core.Core
import Core.Env
import Core.Normalise
import Core.TT
import Core.UnifyState

import TTImp.Elab.Term

import TTImp.Parser
import TTImp.ProcessDecl
import TTImp.TTImp

import Parser.Source

import System

repl : {auto c : Ref Ctxt Defs} ->
       {auto u : Ref UST UState} ->
       Core ()
repl = do _ <- coreLift $ putStr "> "
          inp <- coreLift getLine
          let Right ttexp = runParser Nothing inp (expr "(input)" init)
              | Left err => do _ <- coreLift $ printLn err
                               repl
          (tm, ty) <- checkTerm [] ttexp Nothing
          _ <- coreLift $ putStrLn $ "Checked: " ++ show tm
          defs <- get Ctxt
          _ <- coreLift $ putStrLn $ "Type: " ++ show !(normalise defs [] !(getTerm ty))
          nf <- normalise defs [] tm
          _ <- coreLift $ putStrLn $ "Evaluated: " ++ show nf
          repl

runMain : List ImpDecl -> Core ()
runMain decls
    = do c <- newRef Ctxt !initDefs
         u <- newRef UST initUState
         _ <- traverse_ processDecl decls
         repl

main : IO ()
main = do [_, fname] <- getArgs
              | _ => putStrLn "Usage: tinyidris <filename>"
          Right decls <- parseFile fname (do p <- prog fname; _ <- eoi; pure p)
              | Left err => printLn err
          coreRun (runMain decls)
                  (\err => printLn err)
                  pure
