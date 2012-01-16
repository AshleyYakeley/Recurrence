module Data.TimePhase.Eval(evalWithDict) where
{
    import Data.Traversable;
    import Control.Monad;
    import Language.SExpression;
    import Data.TimePhase.Value;
    import Data.TimePhase.Dict;
    
    eval :: (?evalatom :: String -> Maybe Value) => SExpression String -> M Value;
    eval (AtomSExpression atom) = case ?evalatom atom of
    {
        Just v -> return v;
        _ -> reportError ("unknown: " ++ (show atom));
    };
    eval (ListSExpression []) = reportError "empty list";
    eval (ListSExpression (AtomSExpression "let":rest)) = case rest of
    {
        [ListSExpression bindingPairs,subjectExpr] -> do
        {
            bindings <- traverse (\bindingPair -> case bindingPair of
            {
                ListSExpression [AtomSExpression name,valueExpr] -> do
                {
                    value <- eval valueExpr;
                    return (name,value);
                };
                _ -> reportError "badly-formed bindings in let";
            }) bindingPairs;
            let
            {
                ?evalatom = \s -> mplus (findInPairs bindings s) (?evalatom s);
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
    evalWithDict = let {?evalatom = evalAtom} in eval;
}

