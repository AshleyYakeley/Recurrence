module StationSet where
{
    {-
    To be correct, this must have only a finite number of members in any interval.
    -}
    data StationSet a = MkStationSet
    {
        ssMember :: a -> Bool,
        -- strictly after, up to including limit
        ssFirstAfterUntil :: a -> a -> Maybe a,
        -- strictly before, up to including limit
        ssLastBeforeUntil :: a -> a -> Maybe a
    };

    data KnownStationSet a = MkKnownStationSet
    {
        kssMember :: a -> Bool,
        -- strictly after
        kssFirstAfter :: a -> Maybe a,
        -- strictly before
        kssLastBefore :: a -> Maybe a
    };
    
    class Set1 (s :: *) where
    {
        type Base s :: *;
        empty :: s;
        member :: s -> Base s -> Bool;
        single :: Base s -> s;
        union :: s -> s -> s;
        -- strictly after, up to including limit
        firstAfterUntil :: s -> Base s -> Base s -> Maybe (Base s);
        -- strictly before, up to including limit
        lastBeforeUntil :: s -> Base s -> Base s -> Maybe (Base s);
    };
    
    class (Set1 s) => Set2 (s :: *) where
    {
        intersect :: s -> s -> s;
    };
    
    instance (Ord a) => Set1 (KnownStationSet a) where
    {
        type Base (KnownStationSet a) = a;
        empty = MkKnownStationSet
        {
            kssMember = \_ -> False,
            kssFirstAfter = \_ -> Nothing,
            kssLastBefore = \_ -> Nothing
        };
    
        member = kssMember;
    
        single t = MkKnownStationSet
        {
            kssMember = \a -> a == t,
            kssFirstAfter = \a -> if a < t then Just t else Nothing,
            kssLastBefore = \a -> if a > t then Just t else Nothing
        };

        union s1 s2 = MkKnownStationSet
        {
            kssMember = \a -> (kssMember s1 a) || (kssMember s2 a),
            kssFirstAfter = \a -> case (kssFirstAfter s1 a,kssFirstAfter s2 a) of
            {
                (mr,Nothing) -> mr;
                (Nothing,mr) -> mr;
                (Just r1,Just r2) -> Just (if r1 < r2 then r1 else r2);
            },
            kssLastBefore = \a -> case (kssLastBefore s1 a,kssLastBefore s2 a) of
            {
                (mr,Nothing) -> mr;
                (Nothing,mr) -> mr;
                (Just r1,Just r2) -> Just (if r1 > r2 then r1 else r2);
            }
        };
        firstAfterUntil s a limit = do
        {
            r <- kssFirstAfter s a;
            if r <= limit then return r else Nothing;
        };
        lastBeforeUntil s a limit = do
        {
            r <- kssLastBefore s a;
            if r >= limit then return r else Nothing;
        };
    };
    
    toStationSet :: (Set1 s) => s -> StationSet (Base s);
    toStationSet s = MkStationSet
    {
        ssMember = member s,
        ssFirstAfterUntil = firstAfterUntil s,
        ssLastBeforeUntil = lastBeforeUntil s
    };
    
    instance (Ord a) => Set1 (StationSet a) where
    {
        type Base (StationSet a) = a;
        empty = toStationSet (empty :: KnownStationSet a);
        single a = toStationSet (single a :: KnownStationSet a);
        member = ssMember;
        
        union s1 s2 = MkStationSet
        {
            ssMember = \a -> (ssMember s1 a) || (ssMember s2 a),
            ssFirstAfterUntil = \a limit -> case (ssFirstAfterUntil s1 a limit,ssFirstAfterUntil s2 a limit) of
            {
                (mr,Nothing) -> mr;
                (Nothing,mr) -> mr;
                (Just r1,Just r2) -> Just (if r1 < r2 then r1 else r2);
            },
            ssLastBeforeUntil = \a limit -> case (ssLastBeforeUntil s1 a limit,ssLastBeforeUntil s2 a limit) of
            {
                (mr,Nothing) -> mr;
                (Nothing,mr) -> mr;
                (Just r1,Just r2) -> Just (if r1 > r2 then r1 else r2);
            }
        };
    
        firstAfterUntil = ssFirstAfterUntil;
        lastBeforeUntil = ssLastBeforeUntil;
    };
    
    instance (Ord a) => Set2 (StationSet a) where
    {
        intersect s1 s2 = MkStationSet
        {
            ssMember = \a -> (ssMember s1 a) && (ssMember s2 a),
            ssFirstAfterUntil = \a' limit -> let
            {
                search a = do
                {
                    r1 <- ssFirstAfterUntil s1 a limit;
                    r2 <- ssFirstAfterUntil s2 a limit;
                    case compare r1 r2 of
                    {
                        EQ -> Just r1;
                        LT -> if ssMember s1 r2 then Just r2 else search r2;
                        GT -> if ssMember s2 r1 then Just r1 else search r1;
                    };
                };
            } in search a',
            ssLastBeforeUntil = \a' limit -> let
            {
                search a = do
                {
                    r1 <- ssLastBeforeUntil s1 a limit;
                    r2 <- ssLastBeforeUntil s2 a limit;
                    case compare r1 r2 of
                    {
                        EQ -> Just r1;
                        GT -> if ssMember s1 r2 then Just r2 else search r2;
                        LT -> if ssMember s2 r1 then Just r1 else search r1;
                    };
                };
            } in search a'
        };
    };

    --firstAfter :: KnownStationSet a -> KnownStationSet a -> KnownStationSet a;
    --firstAfter

}
