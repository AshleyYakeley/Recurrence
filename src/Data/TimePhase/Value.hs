module Data.TimePhase.Value where
{
    import Data.SetSearch;
    import Data.Time;

    data Phase a = IntervalsPhase (Intervals a) | PointSetPhase (PointSet a) | InversePointSetPhase (PointSet a);
    
    type TimePhase = Phase LocalTime;
    
    type M = Either String;
    reportError :: String -> M a;
    reportError = Left;
    
    data Value = PhaseValue TimePhase | FunctionValue ([Value] -> M Value);
    
    class IsValues a where
    {
        toValues :: a -> [Value];
        fromValues :: [Value] -> M (a,[Value]);
    };
    
    class (IsValues a) => IsValue a where
    {
        toValue :: a -> Value;
        fromValue :: Value -> M a;
        toFunctionValue :: a -> [Value] -> M Value;
        toFunctionValue a [] = return (toValue a);
        toFunctionValue _ _ = reportError "expected function";
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
        fromFunctionValue :: ([Value] -> M Value) -> a;
    };
    
    instance (IsValue a) => IsValue (M a) where
    {
        toValue = FunctionValue . toFunctionValue;
        fromValue (FunctionValue f) = fromFunctionValue f;
        fromValue _ = reportError "expected function";
        toFunctionValue ma [] = fmap toValue ma;
        toFunctionValue _ _ = reportError "expected fewer values";
    };
    
    instance (IsValue a) => IsFunctionValue (M a) where
    {
        fromFunctionValue f = do
        {
            r <- f [];
            fromValue r;
        };
    };
    
    instance (IsValues arg,IsValue ret) => IsValues (arg -> ret) where
    {
        toValues = defaultToValues;
        fromValues = defaultFromValues;
    };
    
    instance (IsValues arg,IsValue ret) => IsValue (arg -> ret) where
    {
        toValue = FunctionValue . toFunctionValue;
        fromValue (FunctionValue f) = fromFunctionValue f;
        fromValue _ = reportError "expected function";
        toFunctionValue ar args = do
        {
            (a,rest) <- fromValues args;
            toFunctionValue (ar a) rest;
        };
    };
    
    instance (IsValues arg,IsFunctionValue ret) => IsFunctionValue (arg -> ret) where
    {       
        fromFunctionValue f arg = fromFunctionValue (\vals -> f ((toValues arg) ++ vals));
    };
}
