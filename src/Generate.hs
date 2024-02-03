module Generate where
import Parse
import Data.List
import Data.Char (toLower)

{- Helpers -}

parenthesize :: String -> String
parenthesize s = "(" ++ s ++ ")"

call :: String -> [String] -> String
call fn args = parenthesize fn ++ parenthesize (intercalate ", " args)

callMember :: String -> String -> [String] -> String
callMember obj member args = parenthesize obj ++ "." ++ member ++ parenthesize (intercalate ", " args)

mangleChar :: Char -> String
mangleChar '!' = "Bang"
mangleChar '$' = "Dollar"
mangleChar '%' = "Perc"
mangleChar '&' = "Amp"
mangleChar '*' = "Ast"
mangleChar '/' = "Slash"
mangleChar ':' = "Colon"
mangleChar '<' = "Lt"
mangleChar '=' = "Eq"
mangleChar '>' = "Gt"
mangleChar '?' = "Question"
mangleChar '~' = "Tilde"
mangleChar '^' = "Hat"
mangleChar '+' = "Add"
mangleChar '-' = "Sub"
mangleChar '.' = "Dot"
mangleChar c = [toLower c]

mangle :: String -> String
mangle id = "s2j_" ++ concatMap mangleChar id

definitionName :: Definition -> String
definitionName (DefSimple s _) = s
definitionName (DefFunction s _ _) = s

definitionRhs :: Definition -> Expression
definitionRhs (DefSimple _ e) = e
definitionRhs (DefFunction _ args body) = ExprLambda args body

{- SCM Constructors -}

scmNilType = "SchemeNil"
scmNumType = "SchemeNum"
scmCharType = "SchemeChar"
scmStringType = "SchemeString"
scmBoolType = "SchemeBool"
scmProcedureType = "SchemeProcedure"
scmSymbolType = "SchemeSymbol"
scmVectorType = "SchemeVector"
scmPairType = "SchemePair"

scmNil :: String
scmNil = "new " ++ scmNilType ++ "()"

scmNum :: Double -> String
scmNum n = "new " ++ scmNumType ++ "(" ++ show n ++ ")"

scmChar :: Char -> String
scmChar c = "new " ++ scmCharType ++ "(" ++ show c  ++ ")"

scmString :: String -> String
scmString s = "new " ++ scmStringType ++ "(" ++ show s ++ ")"

scmBool :: Bool -> String
scmBool b = "new " ++ scmBoolType ++ "(" ++ showBool b ++ ")"
    where showBool True = "true"
          showBool False = "false"

scmSymbol :: String -> String
scmSymbol str = "new " ++ scmSymbolType ++ "(" ++ show str ++ ")"

scmProcedure :: Int -> Bool -> String -> String
scmProcedure numArgs isVariadic f = "new " ++ scmProcedureType ++ "(" ++ intercalate ", " [show numArgs, map toLower $ show isVariadic, f] ++ ")"

scmList :: [String] -> String
scmList [] = "new " ++ scmNilType ++ "()"
scmList args = call "arr_to_list" ["[" ++ intercalate ", " args ++ "]"]

scmVector :: [String] -> String
scmVector args = "new " ++ scmVectorType ++ "([" ++ intercalate ", " args ++ "])"

scmPair :: String -> String -> String
scmPair left right = "new " ++ scmPairType ++ "({car: " ++ left ++ ", cdr: " ++  right ++ "})"

{- Generation Functions -}

gen :: Program -> String
gen (ProgExpression e) = genExpr e ++ ";"
gen (ProgDefinition d) = genDef d

genExpr :: Expression -> String
genExpr (ExprVar id) = mangle id
genExpr (ExprNumber num) = scmNum num
genExpr (ExprChar ch) = scmChar ch
genExpr (ExprString str) = scmString str
genExpr (ExprBool b) = scmBool b
genExpr (ExprQuotation datum) = genQuote datum
genExpr (ExprProcedureCall p args) = callMember (genExpr p) "call" (map genExpr args)
genExpr (ExprTailCall paramNames args) = call ("() => { " ++ intercalate "\n" [overwriteArgs, setRec, "}"]) []
    where overwriteArgs = "[" ++ intercalate ", " (map mangle paramNames) ++ "] = [" ++ intercalate ", " (map genExpr args) ++ "];"
          setRec = "rec = true;"
genExpr (ExprList exprs) = call "arr_to_list" ["[" ++ intercalate ", " (map genExpr exprs) ++ "]"]
genExpr (ExprLambda args body) = genLambda args body
-- (special forms)
genExpr (ExprIf cond conseq alt) = parenthesize (callMember cond' "truthy" []) ++ " ? " ++ conseq' ++ " : " ++ alt'
    where cond' = genExpr cond
          conseq' = genExpr conseq
          alt' = genExpr alt
genExpr (ExprAssignment id rhs) = mangle id ++ " = " ++ genExpr rhs ++ ";"
genExpr (ExprCond clauses) = intercalate "\n" (map genClause clauses ++ [scmNil])
    where genClause (CondIf cond conseq) = callMember (genExpr cond) "truthy" [] ++ " ? " ++ genSeq conseq ++ " : "
          genClause (CondElse conseq)  = "true ? " ++ genSeq conseq ++ " : "
          genSeq [] = scmNil
          genSeq seq = genBody $ Body [] seq
genExpr (ExprAnd args) = case args of
    [] -> scmBool True
    as -> foldr1 (\a b -> callMember a "and" ["() => " ++ b]) $ map genExpr as
genExpr (ExprOr args) = case args of
    [] -> scmBool True
    as -> foldr1 (\a b -> callMember a "or" ["() => " ++ b]) $ map genExpr as
genExpr (ExprLet bindings body) = call (mkLambda (map definitionName bindings) body) $ map (genExpr . definitionRhs) bindings
    where mkLambda args body = parenthesize (intercalate "," $ map mangle args) ++ " => " ++ genBody body
genExpr (ExprLetStar bindings body) = genExpr $ recurse bindings body
    where recurse [] body = ExprProcedureCall (ExprLambda (FormalArgList []) body) []
          recurse (d:ds) body = ExprProcedureCall (ExprLambda (FormalArgList [definitionName d]) (Body [] [recurse ds body])) [definitionRhs d]
genExpr (ExprLetRec outerBindings (Body innerBindings exprs)) = genBody $ Body (outerBindings ++ innerBindings) exprs
genExpr (ExprBegin exprs) = genBody $ Body [] exprs

genDef :: Definition -> String
genDef (DefSimple name val) = "let " ++ mangle name ++ " = " ++ genExpr val ++ ";\n"
genDef (DefFunction name args body) = "let " ++ mangle name ++ " = " ++ genLambda args body ++ ";\n"
genDef (DefTailRecFunction name args body) = "let " ++ mangle name ++ " = " ++ genLambdaTailRec args body ++ ";\n"

genQuote :: Datum -> String
genQuote (DatumSymbol s) = scmSymbol s
genQuote (DatumBool b) = scmBool b
genQuote (DatumNumber n) = scmNum n
genQuote (DatumChar c) = scmChar c
genQuote (DatumString s) = scmString s
genQuote (DatumPair car cdr) = scmPair (genQuote car) (genQuote cdr)
genQuote (DatumVector v) = scmVector (map genQuote v)
genQuote (DatumQuotation d) = scmPair (scmSymbol "quote") (scmList [genQuote d])
genQuote DatumNull = scmNil

genBody :: Body -> String
genBody (Body defs exprs) = call ("() => {\n" ++ defs' ++ exprs' ++ "}") []
    where defs' = concatMap genDef defs
          exprs' = concatMap ((++";\n") . genExpr) (init exprs) ++ "return " ++ genExpr (last exprs) ++ ";\n"

genBodyTailRec :: Body -> String
genBodyTailRec (Body defs exprs) = call ("() => {\n" ++ defs' ++ exprs' ++ "}") []
    where defs' = concatMap genDef defs
          exprs' = intros ++ concatMap ((++";\n") . genExpr) (init exprs) ++ "let res = " ++ genExpr (last exprs) ++ ";\n" ++ outros
          intros = "while(true) {\n let rec = false;\n"
          outros = "if(!rec) return res;\n}\n"

_genLambda :: (Body -> String) -> FormalArgs -> Body -> String
_genLambda _genBody (FormalArgList names) body = scmProcedure (length names) False f
    where f = "(" ++ intercalate ", " (map mangle names) ++ ")" ++ " => " ++ _genBody body
_genLambda _genBody (FormalVarArgs positionals list) body = scmProcedure (1 + length positionals) True f
    where f = "(" ++ intercalate ", " (map mangle $ positionals ++ [list]) ++ ")" ++ " => " ++ _genBody body

genLambda :: FormalArgs -> Body -> String
genLambda = _genLambda genBody

genLambdaTailRec :: FormalArgs -> Body -> String
genLambdaTailRec = _genLambda genBodyTailRec
