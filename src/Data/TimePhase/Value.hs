module Data.TimePhase.Value where
{
    import Data.Fixed;
    import Data.SetSearch;
    import Data.Time;
    import Data.TimePhase.Time;
    
    type M = Either String;
    reportError :: String -> M a;
    reportError = Left;
    
    data Value = PhaseValue TimePhase | IntegerValue Int | DurationValue NominalDiffTime | FunctionValue ([Value] -> M Value);
    
    class ToValues a where
    {
        toValues :: a -> [Value];
    };
    
    class FromValues a where
    {
        fromValues :: [Value] -> M (a,[Value]);
    };
    
    class (ToValues a) => ToValue a where
    {
        toValue :: a -> Value;
        toFunctionValue :: a -> [Value] -> M Value;
        toFunctionValue a [] = return (toValue a);
        toFunctionValue _ _ = reportError "expected function";
    };
    
    class (FromValues a) => FromValue a where
    {
        fromValue :: Value -> M a;
    };
    
    instance (ToValue a) => ToValues (Maybe a) where
    {
        toValues (Just a) = [toValue a];
        toValues Nothing = [];
    };
    
    instance (FromValue a) => FromValues (Maybe a) where
    {
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
    
    instance (ToValue a) => ToValues [a] where
    {
        toValues = fmap toValue;
    };
    
    instance (FromValue a) => FromValues [a] where
    {
        fromValues vals = do
        {
            as <- mapM fromValue vals;
            return (as,[]);
        };
    };
    
    defaultToValues :: (ToValue a) => a -> [Value];
    defaultToValues a = [toValue a];
    defaultFromValues :: (FromValue a) => [Value] -> M (a,[Value]);
    defaultFromValues (v:vs) = do
    {
        a <- fromValue v;
        return (a,vs);
    };
    defaultFromValues [] = reportError "expected more values";
    
    instance ToValues TimePhase where
    {
        toValues = defaultToValues;
    };
    
    instance FromValues TimePhase where
    {
        fromValues = defaultFromValues;
    };
    
    instance ToValue TimePhase where
    {
        toValue = PhaseValue;
    };
    
    instance FromValue TimePhase where
    {
        fromValue (PhaseValue x) = return x;
        fromValue _ = reportError "expected phase";
    };
  
    instance ToValues (Intervals T) where
    {
        toValues = defaultToValues;
    };
    
    instance ToValue (Intervals T) where
    {
        toValue = PhaseValue . toPhaseSet;
    };
    
    instance ToValues (PointSet T) where
    {
        toValues = defaultToValues;
    };
    
    instance ToValue (PointSet T) where
    {
        toValue = PhaseValue . toPhaseSet;
    };
    
    instance ToValues (PointSet Day) where
    {
        toValues = defaultToValues;
    };
    
    instance ToValue (PointSet Day) where
    {
        toValue = toValue . daysToTimeIntervals;
    };
   
    instance ToValues NominalDiffTime where
    {
        toValues = defaultToValues;
    };
   
    instance FromValues NominalDiffTime where
    {
        fromValues = defaultFromValues;
    };
    
    instance ToValue NominalDiffTime where
    {
        toValue = DurationValue;
    };
    
    instance FromValue NominalDiffTime where
    {
        fromValue (DurationValue x) = return x;
        fromValue _ = reportError "expected duration";
    };
    
    instance ToValues Int where
    {
        toValues = defaultToValues;
    };
    
    instance FromValues Int where
    {
        fromValues = defaultFromValues;
    };
    
    instance ToValue Int where
    {
        toValue = IntegerValue;
    };
    
    instance FromValue Int where
    {
        fromValue (IntegerValue x) = return x;
        fromValue _ = reportError "expected integer";
    };
    
    instance (ToValue a) => ToValues (M a) where
    {
        toValues = defaultToValues;
    };
    
    instance (FromValue a) => FromValues (M a) where
    {
        fromValues = defaultFromValues;
    };
    
    class (FromValue a) => FromFunctionValue a where
    {
        fromFunctionValue :: ([Value] -> M Value) -> a;
    };
    
    instance (ToValue a) => ToValue (M a) where
    {
        toValue = FunctionValue . toFunctionValue;
        toFunctionValue ma [] = fmap toValue ma;
        toFunctionValue _ _ = reportError "expected fewer values";
    };
    
    instance (FromValue a) => FromValue (M a) where
    {
        fromValue (FunctionValue f) = fromFunctionValue f;
        fromValue _ = reportError "expected function";
    };
    
    instance (FromValue a) => FromFunctionValue (M a) where
    {
        fromFunctionValue f = do
        {
            r <- f [];
            fromValue r;
        };
    };
    
    instance (FromValues arg,ToValue ret) => ToValues (arg -> ret) where
    {
        toValues = defaultToValues;
    };
    
    instance (ToValues arg,FromValue ret) => FromValues (arg -> ret) where
    {
        fromValues = defaultFromValues;
    };
    
    instance (FromValues arg,ToValue ret) => ToValue (arg -> ret) where
    {
        toValue = FunctionValue . toFunctionValue;
        toFunctionValue ar args = do
        {
            (a,rest) <- fromValues args;
            toFunctionValue (ar a) rest;
        };
    };
    
    instance (ToValues arg,FromValue ret) => FromValue (arg -> ret) where
    {
        fromValue (FunctionValue f) = fromFunctionValue f;
        fromValue _ = reportError "expected function";
    };
    
    instance (ToValues arg,FromFunctionValue ret) => FromFunctionValue (arg -> ret) where
    {       
        fromFunctionValue f arg = fromFunctionValue (\vals -> f ((toValues arg) ++ vals));
    };
}
