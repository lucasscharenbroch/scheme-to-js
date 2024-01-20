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

{- JS Constructors -}

jsNilType = "SchemeNil"
jsNumType = "SchemeNum"
jsCharType = "SchemeChar"
jsStringType = "SchemeString"
jsBoolType = "SchemeBool"
jsProcedureType = "SchemeProcedure"
jsSymbolType = "SchemeSymbol"
jsVectorType = "SchemeVector"
jsPairType = "SchemePair"

jsNil :: String
jsNil = "new " ++ jsNilType ++ "()"

jsNum :: Double -> String
jsNum n = "new " ++ jsNumType ++ "(" ++ show n ++ ")"

jsChar :: Char -> String
jsChar c = "new " ++ jsCharType ++ "(" ++ show c  ++ ")"

jsString :: String -> String
jsString s = "new " ++ jsStringType ++ "(" ++ show s ++ ")"

jsBool :: Bool -> String
jsBool b = "new " ++ jsBoolType ++ "(" ++ showBool b ++ ")"
    where showBool True = "true"
          showBool False = "false"

jsSymbol :: String -> String
jsSymbol str = "new " ++ jsSymbolType ++ "(" ++ show str ++ ")"

jsProcedure :: Int -> Bool -> String -> String
jsProcedure numArgs isVariadic f = "new " ++ jsProcedureType ++ "(" ++ intercalate ", " [show numArgs, map toLower $ show isVariadic, f] ++ ")"

jsList :: [String] -> String
jsList args = callMember jsPairType "from_arr" ["[" ++ intercalate ", " args ++ "]"]

jsVector :: [String] -> String
jsVector args = "new " ++ jsVectorType ++ "([" ++ intercalate ", " args ++ "])"

{- Generation Functions -}

gen :: Program -> String
gen (ProgExpression e) = genExpr e ++ ";"
gen (ProgDefinition d) = genDef d

genExpr :: Expression -> String
genExpr (ExprVar id) = mangle id
genExpr (ExprNumber num) = jsNum num
genExpr (ExprChar ch) = jsChar ch
genExpr (ExprString str) = jsString str
genExpr (ExprBool b) = jsBool b
genExpr (ExprQuotation datum) = genQuote datum
genExpr (ExprProcedureCall p args) = callMember (genExpr p) "call" (map genExpr args)
genExpr (ExprLambda args body) = genLambda args body
-- (special forms)
genExpr (ExprIf cond conseq alt) = parenthesize (callMember cond' "truthy" []) ++ " ? " ++ conseq' ++ " : " ++ alt'
    where cond' = genExpr cond
          conseq' = genExpr conseq
          alt' = genExpr alt
genExpr (ExprAssignment id rhs) = mangle id ++ " = " ++ genExpr rhs ++ ";"
genExpr (ExprCond clauses) = intercalate "\n" (map genClause clauses ++ [jsNil])
    where genClause (CondIf cond conseq) = callMember (genExpr cond) "truthy" [] ++ " ? " ++ genSeq conseq ++ " :"
          genClause (CondElse conseq)  = "true ? " ++ genSeq conseq ++ " : "
          genSeq [] = jsNil
          genSeq seq = genBody $ Body [] seq
genExpr (ExprAnd args) = case args of
    [] -> jsBool True
    as -> foldr1 (\a b -> callMember a "and" [b]) $ map genExpr as
genExpr (ExprOr args) = case args of
    [] -> jsBool True
    as -> foldr1 (\a b -> callMember a "or" [b]) $ map genExpr as
genExpr (ExprLet outerBindings (Body innerBindings exprs)) = genBody $ Body (outerBindings ++ innerBindings) exprs
genExpr (ExprBegin exprs) = genBody $ Body [] exprs

genDef :: Definition -> String
genDef (DefSimple name val) = "let " ++ mangle name ++ " = " ++ genExpr val ++ ";\n"
genDef (DefFunction name args body) = "let " ++ mangle name ++ " = " ++ genLambda args body ++ ";\n"

genQuote :: Datum -> String
genQuote (DatumSymbol s) = jsSymbol s
genQuote (DatumBool b) = jsBool b
genQuote (DatumNumber n) = jsNum n
genQuote (DatumChar c) = jsChar c
genQuote (DatumString s) = jsString s
genQuote (DatumList l) = jsList (map genQuote l)
genQuote (DatumVector v) = jsVector (map genQuote v)

genBody :: Body -> String
genBody (Body defs exprs) = "(() => {\n" ++ defs' ++ exprs' ++ "})()"
    where defs' = concatMap genDef defs
          exprs' = concatMap ((++";\n") . genExpr) (init exprs) ++ "return " ++ genExpr (last exprs) ++ ";\n"

genLambda :: FormalArgs -> Body -> String
genLambda (FormalArgList names) body = jsProcedure (length names) False f
    where f = "(" ++ intercalate ", " (map mangle names) ++ ")" ++ " => " ++ genBody body
genLambda (FormalVarArgs positionals list) body = jsProcedure (1 + length positionals) True f
    where f = "(" ++ intercalate ", " (map mangle $ positionals ++ [list]) ++ ")" ++ " => " ++ genBody body
