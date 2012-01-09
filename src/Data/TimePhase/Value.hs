module Data.TimePhase.Value where
{
    import Data.SetSearch;
    import Data.Time;

    data Phase a = IntervalsPhase (Intervals a) | PointSetPhase (PointCoPointSet a);
    
    type T = UTCTime;

    instance DeltaSmaller T where
    {
        deltaSmaller (UTCTime d t) = Just (UTCTime (addDays (-1) d) t);
    };

    type TimePhase = Phase T;
    
    type M = Either String;
    reportError :: String -> M a;
    reportError = Left;
    
    data Value = PhaseValue TimePhase | IntegerValue Int | DurationValue NominalDiffTime | FunctionValue ([Value] -> M Value);
    
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
    
    instance (IsValue a) => IsValues (Maybe a) where
    {
        toValues (Just a) = [toValue a];
        toValues Nothing = [];
        fromValues vals = case vals of
        {
            [] -> return (Nothing,[]);
            v:vs -> do
            {
                a <- fromValue v;
                return (Just a,vs);
            };
        };
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
    
    instance IsValues (Intervals T) where
    {
        toValues = defaultToValues;
        fromValues = defaultFromValues;
    };
    
    instance IsValue (Intervals T) where
    {
        toValue = PhaseValue . IntervalsPhase;
        fromValue (PhaseValue (IntervalsPhase x)) = return x;
        fromValue _ = reportError "expected intervals";
    };
    
    instance IsValues (PointSet T) where
    {
        toValues = defaultToValues;
        fromValues = defaultFromValues;
    };
    
    instance IsValue (PointSet T) where
    {
        toValue = PhaseValue . PointSetPhase . PointPCPSet;
        fromValue (PhaseValue (PointSetPhase (PointPCPSet x))) = return x;
        fromValue _ = reportError "expected points";
    };
    
    instance IsValues NominalDiffTime where
    {
        toValues = defaultToValues;
        fromValues = defaultFromValues;
    };
    
    instance IsValue NominalDiffTime where
    {
        toValue = DurationValue;
        fromValue (DurationValue x) = return x;
        fromValue _ = reportError "expected duration";
    };
    
    instance IsValues Int where
    {
        toValues = defaultToValues;
        fromValues = defaultFromValues;
    };
    
    instance IsValue Int where
    {
        toValue = IntegerValue;
        fromValue (IntegerValue x) = return x;
        fromValue _ = reportError "expected integer";
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
