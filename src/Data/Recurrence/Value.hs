module Data.Recurrence.Value where
{
    import Data.Time;
    import Data.SetSearch;
    import Data.Recurrence.Time;
--    import Data.Recurrence.Day;

    data Value = TimeSetValue TimePhase | IntegerValue Int | DurationValue NominalDiffTime | FunctionValue ([Value] -> M Value);

    class ToValues a where
    {
        toValues :: a -> [Value];
        default toValues :: (ToValue a) => a -> [Value];
        toValues a = [toValue a];
    };

    class FromValues a where
    {
        fromValues :: [Value] -> M (a,[Value]);
        default fromValues :: (FromValue a) => [Value] -> M (a,[Value]);
        fromValues (v:vs) = do
        {
            a <- fromValue v;
            return (a,vs);
        };
        fromValues [] = reportError "expected more values";
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

    instance (ToValues a) => ToValues (Maybe a) where
    {
        toValues (Just a) = toValues a;
        toValues Nothing = [];
    };

    instance (FromValues a) => FromValues (Maybe a) where
    {
        fromValues vals = fmap (\(a,rest) -> (Just a,rest)) (fromValues vals);
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

    instance (FromValues a,FromValues b) => FromValues (a,b) where
    {
        fromValues list = do
        {
            (a,arest) <- fromValues list;
            (b,brest) <- fromValues arest;
            return ((a,b),brest);
        };
    };

    instance ToValues TimePhase;

    instance FromValues TimePhase;

    instance ToValue TimePhase where
    {
        toValue = TimeSetValue;
    };

    instance FromValue TimePhase where
    {
        fromValue (TimeSetValue x) = return x;
        fromValue _ = reportError "expected times";
    };


    instance FromValues (PieceSet T);

    instance FromValue (PieceSet T) where
    {
        fromValue v = do
        {
            ts <- fromValue v;
            case ts of
            {
                PeriodTimeSet x -> return $ piecePartialToSet x;
                EmptyTimeSet -> return empty;
                _ -> reportError "expected period";
            };
        };
    };

    instance Eq count => ToValues (PiecePartialFunction T count);

    instance Eq count => ToValue (PiecePartialFunction T count) where
    {
        toValue = toValue . PeriodTimeSet;
    };

    instance ToValues (PointSet T);

    instance ToValue (PointSet T) where
    {
        toValue = toValue . InstantTimeSet;
    };
{-
    instance ToValues (PointSet Day);

    instance ToValue (PointSet Day) where
    {
        toValue = toValue . daysToTimeIntervals;
    };
-}
    instance ToValues NominalDiffTime;

    instance FromValues NominalDiffTime;

    instance ToValue NominalDiffTime where
    {
        toValue = DurationValue;
    };

    instance FromValue NominalDiffTime where
    {
        fromValue (DurationValue x) = return x;
        fromValue _ = reportError "expected duration";
    };

    instance ToValues Int;

    instance FromValues Int;

    instance ToValue Int where
    {
        toValue = IntegerValue;
    };

    instance FromValue Int where
    {
        fromValue (IntegerValue x) = return x;
        fromValue _ = reportError "expected integer";
    };

    instance ToValues Integer;

    instance FromValues Integer;

    instance ToValue Integer where
    {
        toValue x = toValue (fromIntegral x :: Int);
    };

    instance FromValue Integer where
    {
        fromValue x = fmap fromIntegral (fromValue x :: M Int);
    };

    instance (ToValue a) => ToValues (M a);

    instance (FromValue a) => FromValues (M a);

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

    instance (FromValues arg,ToValue ret) => ToValues (arg -> ret);

    instance (ToValues arg,FromValue ret) => FromValues (arg -> ret);

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
