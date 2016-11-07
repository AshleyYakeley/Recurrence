module Data.Recurrence.SExpression.Eval(pattern IdentifierSExpression,pattern FuncSExpression,Dict,evalBinding,eval) where
{
    import Data.SExpression;
    import Data.Recurrence.Time(M,reportError);
    import Data.Recurrence.SExpression.Value;
    import Data.Recurrence.SExpression.Atom;

    pattern IdentifierSExpression :: String -> SExpression Atom;
    pattern IdentifierSExpression name = AtomSExpression (IdentifierAtom name);

    pattern FuncSExpression :: String -> [SExpression Atom] -> SExpression Atom;
    pattern FuncSExpression name args = ListSExpression ((IdentifierSExpression name):args);

    type Dict = String -> Maybe Value;

    addDictEntry :: String -> Value -> Dict -> Dict;
    addDictEntry n v _d k | k == n = Just v;
    addDictEntry _n _v d k = d k;

    evalBinding :: Dict -> SExpression Atom -> SExpression Atom -> M Dict;
    evalBinding dict (IdentifierSExpression name) valueExpr = do
    {
        value <- eval dict valueExpr;
        return $ addDictEntry name value dict;
    };
    evalBinding dict (FuncSExpression name argExprs) valueExpr = let
    {
        getID :: SExpression Atom -> M String;
        getID (IdentifierSExpression argname) = return argname;
        getID _ = reportError "bad function definition argument";

        addDictEntries :: [String] -> [Value] -> Dict -> M Dict;
        addDictEntries [] [] d = return d;
        addDictEntries (n:nn) (v:vv) d = addDictEntries nn vv $ addDictEntry n v d;
        addDictEntries [] (_:_) _ = reportError $ "too many arguments to " ++ show name;
        addDictEntries (_:_) [] _ = reportError $ "not enough arguments to " ++ show name;
    } in do
    {
        argNames <- traverse getID argExprs;
        return $ addDictEntry name (FunctionValue $ \args -> do
        {
            newdict <- addDictEntries argNames args dict;
            eval newdict valueExpr;
        }) dict;
    };
    evalBinding _dict _ _ = reportError "badly-formed name in binding";

    eval :: Dict -> SExpression Atom -> M Value;
    eval _dict (AtomSExpression (LiteralAtom value)) = return value;
    eval dict (IdentifierSExpression name) = case dict name of
    {
        Just v -> return v;
        _ -> reportError ("unknown binding: " ++ (show name));
    };
    eval dict (FuncSExpression "let" rest) = case rest of
    {
        [nameExpr,valueExpr,subjectExpr] -> do
        {
            newdict <- evalBinding dict nameExpr valueExpr;
            eval newdict subjectExpr;
        };
        _ -> reportError "badly-formed let"
    };
    eval dict (ListSExpression (funexpr:argexprs)) = do
    {
        fun <- eval dict funexpr;
        case fun of
        {
            FunctionValue f -> do
            {
                args <- traverse (eval dict) argexprs;
                f args;
            };
            _ -> reportError $ show funexpr ++ ": expected function: " ++ show fun;
        };
    };
    eval _dict _ = reportError "empty list";
}
