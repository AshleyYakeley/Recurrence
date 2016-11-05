module Data.SetSearch.Set where
{
    import Control.Monad (mzero,mplus);
    import Data.Fixed;
    import Data.Time;
    import Data.SetSearch.Base;

    class (Num (Difference a)) => Affine a where
    {
        type Difference a :: *;
        addAffine :: Difference a -> a -> a;
        diffAffine :: a -> a -> Difference a;
    };

    instance Affine Day where
    {
        type Difference Day = Integer;
        addAffine = addDays;
        diffAffine = diffDays;
    };

    nominalDayLength :: NominalDiffTime;
    nominalDayLength = 86400;

    instance Affine LocalTime where
    {
        type Difference LocalTime = NominalDiffTime;
        addAffine ndt t = let
        {
            tod = (realToFrac (timeOfDayToTime (localTimeOfDay t))) + ndt;
        } in
        LocalTime
        {
            localDay = addDays (fromIntegral (div' tod nominalDayLength :: Integer)) (localDay t),
            localTimeOfDay = timeToTimeOfDay (realToFrac (mod' tod nominalDayLength))
        };
        diffAffine lt1 lt2 = let
        {
            dayDiffSeconds = (fromIntegral (diffAffine (localDay lt1) (localDay lt2) :: Integer)) * nominalDayLength;
            todDiffSeconds = (timeOfDayToTime (localTimeOfDay lt1)) - (timeOfDayToTime (localTimeOfDay lt2));
        } in dayDiffSeconds + (realToFrac todDiffSeconds);
    };

    delay :: (Affine (Base p),RemapBase p p) => Difference (Base p) -> p -> p;
    delay d = remapBase (addAffine d) (addAffine (negate d));

    advance :: (Affine (Base p),RemapBase p p) => Difference (Base p) -> p -> p;
    advance d = delay $ negate d;

    class Set (s :: *) where
    {
        empty :: s;
        union :: s -> s -> s;
        intersect :: s -> s -> s;
        diff :: s -> s -> s;
        symdiff :: s -> s -> s;
        symdiff s1 s2 = diff (union s1 s2) (intersect s1 s2);
    };

    class (Set s) => SetFull (s :: *) where
    {
        full :: s;
        invert :: s -> s;
        invert = diff full;
    };

    class (BasedOn s,Set s) => SetMember (s :: *) where
    {
        member :: s -> Base s -> Bool;
    };

    unionAll :: (Set s) => [s] -> s;
    unionAll [] = empty;
    unionAll [tp] = tp;
    unionAll (tp:tps) = union tp (unionAll tps);

    class (SetMember s) => SetSingle (s :: *) where
    {
        single :: Base s -> s;
    };

    class (SetMember s) => SetSearch s where
    {
        -- inclusive within interval
        searchSet :: s -> Base s -> Base s -> [Base s];
    };

    class (SetMember s) => SetFilter (s :: *) where
    {
        filterIntersect :: (Base s -> Bool) -> s -> s;
    };

    mixedIntersect :: (SetMember s1,SetFilter s2,Base s1 ~ Base s2) => s1 -> s2 -> s2;
    mixedIntersect s1 = filterIntersect (member s1);

    intersectAll :: (SetFull s) => [s] -> s;
    intersectAll [] = full;
    intersectAll [tp] = tp;
    intersectAll (tp:tps) = intersect tp (intersectAll tps);

    instance Set Bool where
    {
        empty = False;
        union = (||);
        intersect = (&&);
        diff s1 s2 = s1 && (not s2);
        symdiff = (/=);
    };

    instance SetFull Bool where
    {
        full = True;
        invert = not;
    };

    toMU :: Bool -> Maybe ();
    toMU False = Nothing;
    toMU True = Just ();

    instance Set (Maybe ()) where
    {
        empty = mzero;
        union = mplus;
        intersect = (>>);
        diff (Just ()) Nothing = full;
        diff _ _ = empty;
        symdiff p q | p == q = empty;
        symdiff _ _ = full;
    };

    instance SetFull (Maybe ()) where
    {
        full = return ();
        invert Nothing = full;
        invert (Just ()) = empty;
    };

    instance Set s => Set (a -> s) where
    {
        empty _ = empty;
        union s1 s2 a = union (s1 a) (s2 a);
        intersect s1 s2 a = intersect (s1 a) (s2 a);
        diff s1 s2 a = diff (s1 a) (s2 a);
        symdiff s1 s2 a = symdiff (s1 a) (s2 a);
    };

    instance SetFull s => SetFull (a -> s) where
    {
        full _ = full;
        invert s a = invert (s a);
    };

    instance SetMember (a -> Bool) where
    {
        member = id;
    };

    instance Eq a => SetSingle (a -> Bool) where
    {
        single = (==);
    };

    instance SetFilter (a -> Bool) where
    {
        filterIntersect = intersect;
    };
}
