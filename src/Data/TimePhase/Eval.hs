module Data.TimePhase.Eval(evalWithDict) where
{
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
    eval (ListSExpression (funexpr:argexprs)) = do
    {
        fun <- eval funexpr;
        case fun of
        {
            FunctionValue f -> do
            {
                args <- mapM eval argexprs;
                f args;
            };
            _ -> reportError "expected function"
        };
    };
    
    evalWithDict :: SExpression String -> M Value;
    evalWithDict = let {?evalatom = evalAtom} in eval;
}

