module Data.TimePhase.Eval(evalWithDict) where
{
    import Data.Traversable;
    import Control.Monad;
    import Data.TimePhase.SExpression;
    import Data.TimePhase.Value;
    import Data.TimePhase.Atom;
    import Data.TimePhase.Dict;
    
    eval :: (?dict :: String -> Maybe Value) => SExpression Atom -> M Value;
    eval (AtomSExpression (LiteralAtom value)) = return value;
    eval (AtomSExpression (IdentifierAtom name)) = case ?dict name of
    {
        Just v -> return v;
        _ -> reportError ("unknown binding: " ++ (show name));
    };
    eval (ListSExpression []) = reportError "empty list";
    eval (ListSExpression (AtomSExpression (IdentifierAtom "let"):rest)) = case rest of
    {
        [ListSExpression bindingPairs,subjectExpr] -> do
        {
            bindings <- traverse (\bindingPair -> case bindingPair of
            {
                ListSExpression [AtomSExpression (IdentifierAtom name),valueExpr] -> do
                {
                    value <- eval valueExpr;
                    return (name,value);
                };
                _ -> reportError "badly-formed bindings in let";
            }) bindingPairs;
            let
            {
                ?dict = \s -> mplus (findInPairs bindings s) (?dict s);
            } in eval subjectExpr;
        };
        _ -> reportError "badly-formed let"
    } where
    {
        findInPairs [] s = Nothing;
        findInPairs ((key,value):_) s | key == s = Just value;
        findInPairs (_:rest) s = findInPairs rest s;
    };
    eval (ListSExpression (funexpr:argexprs)) = do
    {
        fun <- eval funexpr;
        case fun of
        {
            FunctionValue f -> do
            {
                args <- traverse eval argexprs;
                f args;
            };
            _ -> reportError "expected function"
        };
    };
    
    evalWithDict :: SExpression Atom -> M Value;
    evalWithDict = let {?dict = dict} in eval;
}

