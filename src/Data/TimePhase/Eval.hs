module Data.TimePhase.Eval(evalWithDict) where
{
    import Data.Traversable;
    import Control.Monad;
    import Data.TimePhase.SExpression;
    import Data.TimePhase.Value;
    import Data.TimePhase.Atom;
    import Data.TimePhase.Dict;
    
    eval :: (?dict :: String -> Maybe Value) => SExpression String -> M Value;
    eval (AtomSExpression atom) = if allowedName atom then case ?dict atom of
    {
        Just v -> return v;
        _ -> reportError ("unknown binding: " ++ (show atom));
    } else case evalAtom atom of
    {
        Just v -> return v;
        _ -> reportError ("bad name: " ++ (show atom));
    };
    eval (ListSExpression []) = reportError "empty list";
    eval (ListSExpression (AtomSExpression "let":rest)) = case rest of
    {
        [ListSExpression bindingPairs,subjectExpr] -> do
        {
            bindings <- traverse (\bindingPair -> case bindingPair of
            {
                ListSExpression [AtomSExpression name,valueExpr] -> if allowedName name then do
                {
                    value <- eval valueExpr;
                    return (name,value);
                } else reportError ("bad binding name: " ++ (show name));
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
    
    evalWithDict :: SExpression String -> M Value;
    evalWithDict = let {?dict = dict} in eval;
}

