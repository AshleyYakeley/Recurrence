module Data.SetSearch.PointSet where
{
    import Data.SetSearch.Set;

    {-
    To be correct, this must have only a finite number of members in any interval.
    -}
    data PointSet a = MkPointSet
    {
        pointsMember :: a -> Bool,
        -- strictly after, up to including limit
        pointsFirstAfterUntil :: a -> a -> Maybe a,
        -- strictly before, up to including limit
        pointsLastBeforeUntil :: a -> a -> Maybe a
    };

    pointsLastBefore :: (Ord a,?first :: a) => PointSet a -> a -> Maybe a;
    pointsLastBefore ps a = pointsLastBeforeUntil ps a ?first;

    pointsFirstAfter :: (Ord a,?last :: a) => PointSet a -> a -> Maybe a;
    pointsFirstAfter ps a = pointsFirstAfterUntil ps a ?last;

    -- | which one was most recent. ps2 takes priority over ps1
    pointsPrevious :: (Ord a,?first :: a) => PointSet a -> PointSet a -> a -> Maybe (Either a a);
    pointsPrevious ps1 ps2 a = 
    if member ps2 a then Just (Right a)
    else if member ps1 a then Just (Left a)
    else case pointsLastBefore ps1 a of
    {
        Just r1 -> case pointsFirstAfterUntil ps2 r1 a of -- to switch off, it must be strictly after the on 
        {
            Just r2 -> Just (Right r2);
            Nothing -> Just (Left r1);
        };
        Nothing -> Nothing; -- never switched on
    };

    -- | which one comes next. ps1 takes priority over ps2
    pointsNext :: (Ord a,?last :: a) => PointSet a -> PointSet a -> a -> Maybe (Either a a);
    pointsNext ps1 ps2 a = 
    if member ps1 a then Just (Left a)
    else if member ps2 a then Just (Right a)
    else case pointsFirstAfter ps2 a of
    {
        Just r2 -> case pointsLastBeforeUntil ps1 r2 a of
        {
            Just r1 -> Just (Left r1);
            Nothing -> Just (Right r2);
        };
        Nothing -> Nothing;
    };

    pointsLastUpTo :: PointSet a -> PointSet a -> PointSet a;
{-
    pointsLastUpTo subject delimiter = MkPointSet
    {
        pointsMember = \a -> (pointsMember subject a) && ((pointsMember delimiter a) ||
        
        )
    };
-}
    pointsLastUpTo subject delimiter = subject; -- WRONG
    
    -- | True if psOn switched on more recently than psOff
    ;
    pointsOnAndOff :: (Ord a,?first :: a) => PointSet a -> PointSet a -> a -> Bool;
    pointsOnAndOff psOn psOff a = case pointsPrevious psOn psOff a of
    {
        Just (Left _) -> True;
        Just (Right _) -> False;
        Nothing -> False;
    };
    
    pointsOnAfter :: (Ord a,?first :: a) => PointSet a -> a -> Bool;
    pointsOnAfter psOn a = (member psOn a) ||
    case pointsLastBefore psOn a of
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
            pointsMember = \b -> pointsMember psa (ba b),
            pointsFirstAfterUntil = \b blimit -> fmap ab (pointsFirstAfterUntil psa (ba b) (ba blimit)),
            pointsLastBeforeUntil = \b blimit -> fmap ab (pointsLastBeforeUntil psa (ba b) (ba blimit))
        };
    };
    
    instance (Ord a) => Set (PointSet a) where
    {
        empty = MkPointSet
        {
            pointsMember = \_ -> False,
            pointsFirstAfterUntil = \_ _ -> Nothing,
            pointsLastBeforeUntil = \_ _ -> Nothing
        };
        
        member = pointsMember;
     
        union s1 s2 = MkPointSet
        {
            pointsMember = \a -> (pointsMember s1 a) || (pointsMember s2 a),
            pointsFirstAfterUntil = \a limit -> case (pointsFirstAfterUntil s1 a limit,pointsFirstAfterUntil s2 a limit) of
            {
                (mr,Nothing) -> mr;
                (Nothing,mr) -> mr;
                (Just r1,Just r2) -> Just (if r1 < r2 then r1 else r2);
            },
            pointsLastBeforeUntil = \a limit -> case (pointsLastBeforeUntil s1 a limit,pointsLastBeforeUntil s2 a limit) of
            {
                (mr,Nothing) -> mr;
                (Nothing,mr) -> mr;
                (Just r1,Just r2) -> Just (if r1 > r2 then r1 else r2);
            }
        };

        intersect s1 s2 = MkPointSet
        {
            pointsMember = \a -> (pointsMember s1 a) && (pointsMember s2 a),
            pointsFirstAfterUntil = \a' limit -> let
            {
                search a = do
                {
                    r1 <- pointsFirstAfterUntil s1 a limit;
                    r2 <- pointsFirstAfterUntil s2 a limit;
                    case compare r1 r2 of
                    {
                        EQ -> Just r1;
                        LT -> if pointsMember s1 r2 then Just r2 else search r2;
                        GT -> if pointsMember s2 r1 then Just r1 else search r1;
                    };
                };
            } in search a',
            pointsLastBeforeUntil = \a' limit -> let
            {
                search a = do
                {
                    r1 <- pointsLastBeforeUntil s1 a limit;
                    r2 <- pointsLastBeforeUntil s2 a limit;
                    case compare r1 r2 of
                    {
                        EQ -> Just r1;
                        GT -> if pointsMember s1 r2 then Just r2 else search r2;
                        LT -> if pointsMember s2 r1 then Just r1 else search r1;
                    };
                };
            } in search a'
        };
        
        diff s1 s2 = filterIntersect (not . (member s2)) s1;
        symdiff s1 s2 = MkPointSet
        {
            pointsMember = \a -> (pointsMember s1 a) /= (pointsMember s2 a),
            pointsFirstAfterUntil = \a limit -> case (pointsFirstAfterUntil s1 a limit,pointsFirstAfterUntil s2 a limit) of
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
            pointsLastBeforeUntil = \a limit -> case (pointsLastBeforeUntil s1 a limit,pointsLastBeforeUntil s2 a limit) of
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
            pointsMember = \a -> a == t,
            pointsFirstAfterUntil = \a limit -> if a < t && limit >= t then Just t else Nothing,
            pointsLastBeforeUntil = \a limit -> if a > t && limit <= t then Just t else Nothing
        };
    };

    instance (Ord a) => SetSearch (PointSet a) where
    {
        firstAfterUntil = pointsFirstAfterUntil;
        lastBeforeUntil = pointsLastBeforeUntil;
    };
    
    instance (Ord a) => SetFilter (PointSet a) where
    {
        filterIntersect f ss = MkPointSet
        {
            pointsMember = \a -> (pointsMember ss a) && (f a),
            pointsFirstAfterUntil = \a' limit -> let
            {
                search a = do
                {
                    r <- pointsFirstAfterUntil ss a limit;
                    if f r
                     then return r
                     else search r;
                };   
            } in search a',
            pointsLastBeforeUntil = \a' limit -> let
            {
                search a = do
                {
                    r <- pointsLastBeforeUntil ss a limit;
                    if f r
                     then return r
                     else search r;
                };   
            } in search a'
        };
    };

    pointsSearch :: (Ord a,Enum t,Ord t) => (a -> t) -> (t -> Maybe a) -> PointSet a;
    pointsSearch back f = MkPointSet
    {
        pointsMember = \day -> (Just day) == f (back day),
        pointsFirstAfterUntil = \day limit -> let
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
        pointsLastBeforeUntil = \day limit -> let
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
