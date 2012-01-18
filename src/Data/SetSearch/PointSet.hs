module Data.SetSearch.PointSet where
{
    import Data.SetSearch.Set;

    {-
    To be correct, this must have only a finite number of members in any interval.
    -}
    data PointSet a = MkPointSet
    {
        ssMember :: a -> Bool,
        -- strictly after, up to including limit
        ssFirstAfterUntil :: a -> a -> Maybe a,
        -- strictly before, up to including limit
        ssLastBeforeUntil :: a -> a -> Maybe a
    };

    ssLastBefore :: (Ord a,?first :: a) => PointSet a -> a -> Maybe a;
    ssLastBefore ps a = ssLastBeforeUntil ps a ?first;

    -- | which one was before. ps2 takes priority over ps1
    pointSetFirstBefore :: (Ord a,?first :: a) => PointSet a -> PointSet a -> a -> Maybe (Either a a);
    pointSetFirstBefore ps1 ps2 a = 
    if member ps2 a then Just (Right a)
    else if member ps1 a then Just (Left a)
    else case ssLastBefore ps1 a of
    {
        Just r1 -> case ssFirstAfterUntil ps2 r1 a of -- to switch off, it must be strictly after the on 
        {
            Just r2 -> Just (Right r2);
            Nothing -> Just (Left r1);
        };
        Nothing -> Nothing; -- never switched on
    };
    
    -- | True if psOn switched on more recently than psOff
    ;
    pointSetOnAndOff :: (Ord a,?first :: a) => PointSet a -> PointSet a -> a -> Bool;
    pointSetOnAndOff psOn psOff a = case pointSetFirstBefore psOn psOff a of
    {
        Just (Left _) -> True;
        Just (Right _) -> False;
        Nothing -> False;
    };
    
    pointSetOnAfter :: (Ord a,?first :: a) => PointSet a -> a -> Bool;
    pointSetOnAfter psOn a = (member psOn a) ||
    case ssLastBefore psOn a of
    {
        Just _ -> True;
        Nothing -> False;
    };
    
    instance BasedOn (PointSet a) where
    {
        type Base (PointSet a) = a;
    };
    
    instance RemapBase (PointSet a) (PointSet b) where
    {
        remapBase ab ba psa = MkPointSet
        {
            ssMember = \b -> ssMember psa (ba b),
            ssFirstAfterUntil = \b blimit -> fmap ab (ssFirstAfterUntil psa (ba b) (ba blimit)),
            ssLastBeforeUntil = \b blimit -> fmap ab (ssLastBeforeUntil psa (ba b) (ba blimit))
        };
    };
    
    instance (Ord a) => Set (PointSet a) where
    {
        empty = MkPointSet
        {
            ssMember = \_ -> False,
            ssFirstAfterUntil = \_ _ -> Nothing,
            ssLastBeforeUntil = \_ _ -> Nothing
        };
        
        member = ssMember;
     
        union s1 s2 = MkPointSet
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

        intersect s1 s2 = MkPointSet
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
        
        diff s1 s2 = filterIntersect (not . (member s2)) s1;
        symdiff s1 s2 = MkPointSet
        {
            ssMember = \a -> (ssMember s1 a) /= (ssMember s2 a),
            ssFirstAfterUntil = \a limit -> case (ssFirstAfterUntil s1 a limit,ssFirstAfterUntil s2 a limit) of
            {
                (mr,Nothing) -> mr;
                (Nothing,mr) -> mr;
                (Just r1,Just r2) -> case compare r1 r2 of
                {
                    EQ -> Nothing;
                    LT -> Just r1;
                    GT -> Just r2;
                };
            },
            ssLastBeforeUntil = \a limit -> case (ssLastBeforeUntil s1 a limit,ssLastBeforeUntil s2 a limit) of
            {
                (mr,Nothing) -> mr;
                (Nothing,mr) -> mr;
                (Just r1,Just r2) -> case compare r1 r2 of
                {
                    EQ -> Nothing;
                    GT -> Just r1;
                    LT -> Just r2;
                };
            }
        };
    };
    
    instance (Ord a) => SetSingle (PointSet a) where
    {
        single t = MkPointSet
        {
            ssMember = \a -> a == t,
            ssFirstAfterUntil = \a limit -> if a < t && limit >= t then Just t else Nothing,
            ssLastBeforeUntil = \a limit -> if a > t && limit <= t then Just t else Nothing
        };
    };

    instance (Ord a) => SetSearch (PointSet a) where
    {
        firstAfterUntil = ssFirstAfterUntil;
        lastBeforeUntil = ssLastBeforeUntil;
    };
    
    instance (Ord a) => SetFilter (PointSet a) where
    {
        filterIntersect f ss = MkPointSet
        {
            ssMember = \a -> (ssMember ss a) && (f a),
            ssFirstAfterUntil = \a' limit -> let
            {
                search a = do
                {
                    r <- ssFirstAfterUntil ss a limit;
                    if f r
                     then return r
                     else search r;
                };   
            } in search a',
            ssLastBeforeUntil = \a' limit -> let
            {
                search a = do
                {
                    r <- ssLastBeforeUntil ss a limit;
                    if f r
                     then return r
                     else search r;
                };   
            } in search a'
        };
    };

    psSearch :: (Ord a,Enum t,Ord t) => (a -> t) -> (t -> Maybe a) -> PointSet a;
    psSearch back f = MkPointSet
    {
        ssMember = \day -> (Just day) == f (back day),
        ssFirstAfterUntil = \day limit -> let
        {
            yday = back day;
            ylimit = back limit;
            findN yday | yday > ylimit = Nothing;
            findN yday = case f yday of
            {
                Just found | found > limit -> Nothing;
                Just found | found > day -> Just found;
                _ -> findN (succ yday);
            };
        } in findN yday,
        ssLastBeforeUntil = \day limit -> let
        {
            yday = back day;
            ylimit = back limit;
            findN yday | yday < ylimit = Nothing;
            findN yday = case f yday of
            {
                Just found | found < limit -> Nothing;
                Just found | found < day -> Just found;
                _ -> findN (pred yday);
            };
        } in findN yday
    };
}
