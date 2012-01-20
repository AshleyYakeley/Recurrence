module Data.SetSearch.Set where
{
    import Data.Fixed;
    import Data.Time;

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
            localDay = addDays (fromIntegral (div' tod nominalDayLength)) (localDay t),
            localTimeOfDay = timeToTimeOfDay (realToFrac (mod' tod nominalDayLength))
        };
        diffAffine lt1 lt2 = let
        {
            dayDiffSeconds = (fromIntegral (diffAffine (localDay lt1) (localDay lt2))) * nominalDayLength;
            todDiffSeconds = (timeOfDayToTime (localTimeOfDay lt1)) - (timeOfDayToTime (localTimeOfDay lt2));
        } in dayDiffSeconds + (realToFrac todDiffSeconds);
    };

    class BasedOn (s :: *) where
    {
        type Base s :: *;
    };

    class (BasedOn p,BasedOn q) => RemapBase p q where
    {
        remapBase :: (Base p -> Base q) -> (Base q -> Base p) -> p -> q;
    };
    
    delay :: (Affine (Base p),RemapBase p p) => Difference (Base p) -> p -> p;
    delay diff = remapBase (addAffine diff) (addAffine (negate diff));
    
    class (BasedOn s) => Set (s :: *) where
    {
        empty :: s;
        member :: s -> Base s -> Bool;
        union :: s -> s -> s;
        intersect :: s -> s -> s;
        diff :: s -> s -> s;
        symdiff :: s -> s -> s;
    };
    
    unionAll :: (Set s) => [s] -> s;
    unionAll [] = empty;
    unionAll [tp] = tp;
    unionAll (tp:tps) = union tp (unionAll tps);
    
    class (Set s) => SetSingle (s :: *) where
    {
        single :: Base s -> s;
    };
    
    class (Set s) => SetSearch s where
    {
        -- strictly after, up to including limit
        firstAfterUntil :: s -> Base s -> Base s -> Maybe (Base s);
        -- strictly before, up to including limit
        lastBeforeUntil :: s -> Base s -> Base s -> Maybe (Base s);
    };
    
    class (Set s) => SetFilter (s :: *) where
    {
        filterIntersect :: (Base s -> Bool) -> s -> s;
    };
    
    mixedIntersect :: (Set s1,SetFilter s2,Base s1 ~ Base s2) => s1 -> s2 -> s2;
    mixedIntersect s1 = filterIntersect (member s1);
    
    class (Set s) => SetFull (s :: *) where
    {
        full :: s;
        invert :: s -> s;
        invert = diff full;
    };
    
    intersectAll :: (SetFull s) => [s] -> s;
    intersectAll [] = full;
    intersectAll [tp] = tp;
    intersectAll (tp:tps) = intersect tp (intersectAll tps);

    instance BasedOn (a -> b) where
    {
        type Base (a -> b) = a;
    };
    
    instance RemapBase (a -> x) (b -> x) where
    {
        remapBase _ ba ax b = ax (ba b);
    };

    instance (Eq a) => Set (a -> Bool) where
    {
        empty _ = False;
        member = id;
        union s1 s2 a = (s1 a) || (s2 a);
        intersect s1 s2 a = (s1 a) && (s2 a);
        diff s1 s2 a = (s1 a) && (not (s2 a));
        symdiff s1 s2 a = (s1 a) /= (s2 a);
    };

    instance (Eq a) => SetSingle (a -> Bool) where
    {
        single = (==);
    };

    instance (Eq a) => SetFilter (a -> Bool) where
    {
        filterIntersect = intersect;
    };

    instance (Eq a) => SetFull (a -> Bool) where
    {
        full _ = True;
        invert s a = not (s a);
    };
}
