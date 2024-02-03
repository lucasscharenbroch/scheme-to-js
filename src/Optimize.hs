module Optimize where
import Parse
import Data.Function (on)
import Data.Maybe

optimize :: Program -> Program
optimize = optTailCalls

{- Helpers -}

visitResult :: (Expression -> a) -> Expression -> a
visitResult = undefined

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

{- Tail-Call Optimization -}

optTailCalls :: Program -> Program
optTailCalls (ProgDefinition (DefFunction fnName params body@(Body defs exprs@(_:_)))) = ProgDefinition def'
    where def' = if hasTailCalls fnName params $ last exprs
                 then DefTailRecFunction fnName params . Body defs . (init exprs ++) . (:[]) . replaceTailCalls fnName params $ last exprs
                 else DefFunction fnName params body
optTailCalls p = p

hasTailCalls :: String -> FormalArgs -> Expression -> Bool
hasTailCalls fnName params e = case e of
    (ExprProcedureCall (ExprVar s) args) -> fnName == s
    (ExprIf cond conseq alt) -> on (||) recurse conseq alt
    (ExprCond ccs) -> any ccHasTailCalls ccs
        where ccHasTailCalls cc = maybe False recurse (safeLast $ ccToExprList cc)
              ccToExprList (CondElse es) = es
              ccToExprList (CondIf _ es) = es
    (ExprLet _ b) -> bodyHasTailCalls b
    (ExprLetStar _ b) -> bodyHasTailCalls b
    (ExprLetRec _ b) -> bodyHasTailCalls b
    (ExprBegin es) -> maybe False recurse $ safeLast es
    _ -> False
    where bodyHasTailCalls (Body _ es) = maybe False recurse . safeLast $ es
          recurse = hasTailCalls fnName params

replaceTailCalls :: String -> FormalArgs -> Expression -> Expression
replaceTailCalls fnName params e = case e of
    e@(ExprProcedureCall (ExprVar s) args)
        | fnName == s -> ExprTailCall paramNames args'
        where (paramNames, isVariadic) = case params of
                  FormalArgList ss -> (ss, False)
                  FormalVarArgs ss s -> (ss ++ [s], True)
              args'
                  | not isVariadic && (length paramNames /= length args) = error $ "wrong arity of tail-call (" ++ fnName ++ ")"
                  | length paramNames > (1 + length args) = error $ "wrong arity of variadic tail-call (too few args) (" ++ fnName ++ ")"
                  | length paramNames /= length args = take (length paramNames) args ++ [ExprList $ drop (length paramNames) args]
                  | otherwise = args
    (ExprIf cond conseq alt) -> ExprIf cond (recurse conseq) (recurse alt)
    (ExprCond ccs) -> ExprCond . map replaceInCc $ ccs
        where replaceInCc (CondElse es@(_:_)) = CondElse . (init es ++) . (:[]) . recurse $ last es
              replaceInCc (CondIf pred es@(_:_)) = CondIf pred . (init es ++) . (:[]) . recurse $ last es
              replaceInCc cc = cc
    (ExprLet defs body) -> ExprLet defs $ replaceInBody body
    (ExprLetStar defs body) -> ExprLetStar defs $ replaceInBody body
    (ExprLetRec defs body) -> ExprLetRec defs $ replaceInBody body
    _ -> e
    where recurse = replaceTailCalls fnName params
          replaceInBody (Body defs es@(_:_)) = Body defs . (init es ++ ) . (:[]) . recurse $ last es
          replaceInBody b = b

