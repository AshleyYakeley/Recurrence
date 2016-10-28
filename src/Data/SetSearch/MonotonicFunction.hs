module Data.SetSearch.MonotonicFunction where
{
    import Prelude hiding (id,(.));
    import Control.Category;

    data MonotonicInjection a b = MkMonotonicInjection
    {
        miEval :: a -> Maybe b,
        miBack :: b -> (a,a)
    };

    instance Category MonotonicInjection where
    {
        id = MkMonotonicInjection Just $ \x -> (x,x);
        (MkMonotonicInjection bc cbb) . (MkMonotonicInjection ab baa) = MkMonotonicInjection (\a -> ab a >>= bc) $ \c -> let
        {
            (b0,b1) = cbb c;
            (a0,_) = baa b0;
            (_,a1) = baa b1;
        } in (a0,a1);
    };

    miInverse :: Eq a => MonotonicInjection a b -> b -> Maybe a;
    miInverse mi b = let
    {
        (a0,a1) = miBack mi b;
    } in if a0 == a1 then Just a0 else Nothing;

    ordInjection :: (Enum a,Ord b) => (a -> Maybe b) -> (b -> a) -> MonotonicInjection a b;
    ordInjection miEval ba = let
    {
        miBack b = let
        {
            a = ba b;
            mb' = miEval a;
        } in case mb' of
        {
            Nothing -> (a,a);
            Just b' -> case compare b b' of
            {
                EQ -> (a,a);
                LT -> (pred a,a);
                GT -> (a,succ a);
            };
        };
    } in MkMonotonicInjection{..};

    splitInjection :: (Enum a,Ord c) => (a -> c -> Maybe b) -> (b -> (a,c)) -> c -> MonotonicInjection a b;
    splitInjection acb bac cref = let
    {
        miEval a = acb a cref;

        miBack b = let
        {
            (a,c) = bac b;
        } in case compare c cref of
        {
            EQ -> (a,a);
            LT -> (pred a,a);
            GT -> (a,succ a);
        };
    } in MkMonotonicInjection{..};

    data MonotonicSurjection a b = MkMonotonicSurjection
    {
        msEval :: a -> b,
        msImage :: b -> (a,a) -- closed-open interval
        -- msEval (fst (msImage b)) = b
    };

    instance Category MonotonicSurjection where
    {
        id = MkMonotonicSurjection id $ \x -> (x,x);
        (MkMonotonicSurjection bc cbb) . (MkMonotonicSurjection ab baa) = MkMonotonicSurjection (bc . ab) $ \c -> let
        {
            (b0,b1) = cbb c;
            (a0,_) = baa b0;
            (a1,_) = baa b1; -- actually beginning of end (e.g., midnight of first day after)
        } in (a0,a1);
    };

    msSameImage :: MonotonicSurjection a b -> a -> (a,a);
    msSameImage MkMonotonicSurjection{..} = msImage . msEval;

    enumSurjection :: Enum b => (a -> b) -> (b -> a) -> MonotonicSurjection a b;
    enumSurjection msEval ba = let
    {
        msImage b = (ba b,ba $ succ b);
    } in MkMonotonicSurjection{..};
}
