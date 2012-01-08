module Data.TimePhase.Eval where
{
    import Data.SetSearch;
    import Data.Time;
    import Language.SExpression;
    
    data Phase a = IntervalsPhase (Intervals a) | PointSetPhase (PointSet a) | InversePointSetPhase (PointSet a);
    
    type TimePhase = Phase LocalTime;
    
    type M = Either String;
    reportError :: String -> M a;
    reportError = Left;
    
    data Value = PhaseValue TimePhase | FunctionValue ([Value] -> M Value);
    
    eval :: (?evalatom :: String -> M Value) => SExpression String -> M Value;
    eval (AtomSExpression atom) = ?evalatom atom;
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
            _ -> reportError "expected a function"
        };
    };
    
    class IsValues a where
    {
        toValues :: a -> [Value];
        fromValues :: [Value] -> M (a,[Value]);
    };
    
    class (IsValues a) => IsValue a where
    {
        toValue :: a -> Value;
        fromValue :: Value -> M a;
    };
    
    instance (IsValue a) => IsValues [a] where
    {
        toValues = fmap toValue;
        fromValues vals = do
        {
            as <- mapM fromValue vals;
            return (as,[]);
        };
    };
    
    defaultToValues :: (IsValue a) => a -> [Value];
    defaultToValues a = [toValue a];
    defaultFromValues :: (IsValue a) => [Value] -> M (a,[Value]);
    defaultFromValues (v:vs) = do
    {
        a <- fromValue v;
        return (a,vs);
    };
    defaultFromValues [] = reportError "expected more values";
    
    instance IsValues TimePhase where
    {
        toValues = defaultToValues;
        fromValues = defaultFromValues;
    };
    
    instance IsValue TimePhase where
    {
        toValue = PhaseValue;
        fromValue (PhaseValue x) = return x;
        fromValue _ = reportError "expected phase";
    };
    
    instance (IsValue a) => IsValues (M a) where
    {
        toValues = defaultToValues;
        fromValues = defaultFromValues;
    };
    
    class (IsValue a) => IsFunctionValue a where
    {
        toFunctionValue :: a -> [Value] -> M Value;
        fromFunctionValue :: ([Value] -> M Value) -> a;
    };
    
    instance (IsValue a) => IsValue (M a) where
    {
        toValue = FunctionValue . toFunctionValue;
        fromValue (FunctionValue f) = fromFunctionValue f;
        fromValue _ = reportError "expected function";
    };
    
    instance (IsValue a) => IsFunctionValue (M a) where
    {
        toFunctionValue ma [] = fmap toValue ma;
        toFunctionValue _ _ = reportError "expected fewer values";
        
        fromFunctionValue f = do
        {
            r <- f [];
            fromValue r;
        };
    };
    
    instance (IsValues arg,IsFunctionValue ret) => IsValues (arg -> ret) where
    {
        toValues = defaultToValues;
        fromValues = defaultFromValues;
    };
    
    instance (IsValues arg,IsFunctionValue ret) => IsValue (arg -> ret) where
    {
        toValue = FunctionValue . toFunctionValue;
        fromValue (FunctionValue f) = fromFunctionValue f;
        fromValue _ = reportError "expected function";
    };
    
    instance (IsValues arg,IsFunctionValue ret) => IsFunctionValue (arg -> ret) where
    {
        toFunctionValue ar args = do
        {
            (a,rest) <- fromValues args;
            toFunctionValue (ar a) rest;
        };
        
        fromFunctionValue f arg = fromFunctionValue (\vals -> f ((toValues arg) ++ vals));
    };
    
{-    
    intersectAll :: 
    
    dict :: String -> M Value;
    dict "intersect" = FunctionValue intersectAll;
    dict s = "unknown: " ++ (show s);    
    
     let
    {
        
        args = fmap (MkAnyExpr . eval) argexprs;
    } in case fun of
    {
        "intersect" -> return (intersectAll args);
    };
-}
}

