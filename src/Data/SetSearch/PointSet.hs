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

    
    -- | True if kpsOn switched on more recently than psOff
    ;
    onAndOff :: (Ord a,?first :: a) => PointSet a -> PointSet a -> a -> Bool;
    onAndOff kpsOn psOff a = case ssLastBefore kpsOn a of
    {
        Just r1 -> case ssFirstAfterUntil psOff r1 a of -- to switch off, it must be strictly after the on 
        {
            Just _ -> False; -- off after on
            Nothing -> True; -- no off after on
        };
        Nothing -> False; -- never switched on
    };
    
    instance BasedOn (PointSet a) where
    {
        type Base (PointSet a) = a;
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
}
