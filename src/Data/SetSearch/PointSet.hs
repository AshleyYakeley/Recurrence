module Data.SetSearch.PointSet where
{
    import Data.Monoid;
    import Control.Monad;
    import Data.SetSearch.Set;
    import Data.SetSearch.Cut;
    import Data.SetSearch.ValueSet;


    newtype PointSet a = MkPointSet
    {
        psValues :: a -> a -> ValueSet a
    };
    
    instance BasedOn (PointSet a) where
    {
        type Base (PointSet a) = a;
    };
    
    instance RemapBase (PointSet a) (PointSet b) where
    {
        remapBase ab ba (MkPointSet vf) = MkPointSet (\p q -> remapBase ab ba (vf (ba p) (ba q)));
    };

    instance (Ord a) => Set (PointSet a) where
    {
        empty = MkPointSet (\_ _ -> empty);
        member (MkPointSet vf) a = vsNonEmpty (vf a a);
        union (MkPointSet vf1) (MkPointSet vf2) = MkPointSet (\a b -> union (vf1 a b) (vf2 a b));
        intersect (MkPointSet vf1) (MkPointSet vf2) = MkPointSet (\a b -> intersect (vf1 a b) (vf2 a b));
        diff (MkPointSet vf1) (MkPointSet vf2) = MkPointSet (\a b -> diff (vf1 a b) (vf2 a b));
        symdiff (MkPointSet vf1) (MkPointSet vf2) = MkPointSet (\a b -> symdiff (vf1 a b) (vf2 a b));
    };

    instance (Ord a) => SetSingle (PointSet a) where
    {
        single a = MkPointSet (\p q -> if (a >= p) && (a <= q) then single a else empty);
    };

    instance (Ord a) => SetFilter (PointSet a) where
    {
        filterIntersect filt (MkPointSet vf) = MkPointSet (\a b -> filterIntersect filt (vf a b));
    };

    psValuesCut :: (Ord a) => PointSet a -> Cut a -> Cut a -> ValueSet a;
    psValuesCut ps (MkCut a ax) (MkCut b bx) = let
    {
        v = psValues ps a b;
        filt1 = case ax of
        {
            Before -> id;
            After -> filterIntersect ((/=) a);
        };
        filt2 = case bx of
        {
            Before -> filterIntersect ((/=) b);
            After -> id;
        };
    } in filt2 (filt1 v);

    psNonEmpty :: (Ord a) => PointSet a -> Cut a -> Cut a -> Bool;
    psNonEmpty ps p q = vsNonEmpty (psValuesCut ps p q);

    psFirstCut :: (Ord a) => PointSet a -> Cut a -> Cut a -> Maybe a;
    psFirstCut ps pc qc = vsFirst (psValuesCut ps pc qc);

    psLastCut :: (Ord a) => PointSet a -> Cut a -> Cut a -> Maybe a;
    psLastCut ps pc qc = vsLast (psValuesCut ps pc qc);

    instance (Ord a) => SetSearch (PointSet a) where
    {
        firstAfterUntil ps exnear incfar = psFirstCut ps (justAfter exnear) (justAfter incfar);
        lastBeforeUntil ps exnear incfar = psLastCut ps (justBefore incfar) (justBefore exnear);
    };

{-
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
-}

{-
    pointsLastRangeAfterBefore :: PointSet a -> a -> a -> Maybe a;
    pointsLastRangeAfterBefore ps a limit = if pointsMember ps a then Just a else pointsLastBeforeUntil ps a limit;
    
    pointsLastRangeAfterAfter :: (Eq a) => PointSet a -> a -> a -> Maybe a;
    pointsLastRangeAfterAfter ps a limit = if pointsMember ps a then Just a else do
    {
        r <- pointsLastBeforeUntil ps a limit;
        if r == limit then Nothing else Just r;
    };

    pointsLastBefore :: (Ord a,?first :: a) => PointSet a -> a -> Maybe a;
    pointsLastBefore ps a = pointsLastBeforeUntil ps a ?first;

    pointsFirstAfter :: (Ord a,?last :: a) => PointSet a -> a -> Maybe a;
    pointsFirstAfter ps a = pointsFirstAfterUntil ps a ?last;
-}
{-
    -- | which one was most recent. ps2 takes priority over ps1
    pointsPreviousBefore :: (Ord a,?first :: a) => PointSet a -> PointSet a -> a -> Maybe (Either a a);
    pointsPreviousBefore ps1 ps2 a = 
    
    
    case pointsLastBefore ps1 a of
    {
        Just r1 -> case pointsFirstAfterUntil ps2 r1 a of -- to switch off, it must be strictly after the on 
        {
            Just r2 -> Just (Right r2);
            Nothing -> Just (Left r1);
        };
        Nothing -> Nothing; -- never switched on
    };

    -- | which one was most recent. ps2 takes priority over ps1
    pointsPreviousFrom :: (Ord a,?first :: a) => PointSet a -> PointSet a -> a -> Maybe (Either a a);
    pointsPreviousFrom ps1 ps2 a = 
    if member ps2 a then Just (Right a)
    else if member ps1 a then Just (Left a)
    else pointsPreviousBefore ps1 ps2 a;

    -- | which one comes next. ps1 takes priority over ps2
    pointsNextAfter :: (Ord a,?last :: a) => PointSet a -> PointSet a -> a -> Maybe (Either a a);
    pointsNextAfter ps1 ps2 a = case pointsFirstAfter ps2 a of
    {
        Just r2 -> case pointsLastBeforeUntil ps1 r2 a of
        {
            Just r1 -> Just (Left r1);
            Nothing -> Just (Right r2);
        };
        Nothing -> Nothing;
    };
-}

{-
    pointsFirstInCuts :: (Ord a) => PointSet a -> Cut a -> Cut a -> Maybe a;
    pointsFirstInCuts ps near far = vsFirst (psValuesCut ps near far);

    pointsLastInCuts :: (Ord a) => PointSet a -> Cut a -> Cut a -> Maybe a;
    pointsLastInCuts ps near far = vsLast (psValuesCut ps near far);

    -- | which one comes next
    pointsWhichFirstInCuts :: (Ord a) => PointSet (Cut a) -> PointSet a -> Cut a -> Cut a -> Maybe (Either (Cut a) a);
    pointsWhichFirstInCuts ps1 ps2 near far = do
    {
        r2 <- pointsFirstInCuts ps2 near far;
        case pointsFirstInCuts ps1 (doubleCut r2) (justBefore near) of
        {
            Just r1 -> Just (Left r1);
            Nothing -> Just (Right r2);
        };
    };

    -- | which one comes last
    pointsWhichLastInCuts :: (Ord a) => PointSet (Cut a) -> PointSet a -> Cut a -> Cut a -> Maybe (Either (Cut a) a);
    pointsWhichLastInCuts ps1 ps2 near far = do
    {
        r2 <- pointsLastInCuts ps2 near far;
        case pointsLastInCuts ps1 (doubleCut r2) (justAfter near) of
        {
            Just r1 -> Just (Left r1);
            Nothing -> Just (Right r2);
        };
    };

    -- | which one comes next. ps1 takes priority over ps2
    pointsNextFrom :: (Ord a,?last :: a) => PointSet a -> PointSet a -> a -> Maybe (Either a a);
    pointsNextFrom ps1 ps2 a = 
    if member ps1 a then Just (Left a)
    else if member ps2 a then Just (Right a)
    else pointsNextAfter ps1 ps2 a;
-}
    psPrevious :: (Ord a,?first :: Cut a) => PointSet a -> Cut a -> Maybe a;
    psPrevious ps x = psLastCut ps ?first x;

    psNext :: (Ord a,?last :: Cut a) => PointSet a -> Cut a -> Maybe a;
    psNext ps x = psFirstCut ps x ?last;

    -- | the first subject point on or after delimiter
    pointsFirstFrom :: (Ord a,?first :: Cut a) => PointSet a -> PointSet a -> PointSet a;
    pointsFirstFrom subject delimiter = filterIntersect (\x -> case psPrevious delimiter (justBefore x) of
    {
        -- the previous delimiter
        Just d -> not (psNonEmpty subject (justBefore d) (justBefore x)); -- if no subject
        Nothing -> False;
    }) subject;

    -- | the first subject point after delimiter
    pointsCutFirstAfterPoints :: forall a. (Ord a,?first :: Cut a) => PointSet (Cut a) -> PointSet a -> PointSet (Cut a);
    pointsCutFirstAfterPoints subject delimiter = MkPointSet (\p q ->
        let
        {
            delims = psValuesCut delimiter p q :: ValueSet a;
            most = vsMapMaybe (\t -> psFirstCut subject (doubleCut t) (justAfter q)) delims :: ValueSet (Cut a);
            mfirstone = do
            {
                firstsubj <- psFirstCut subject (justBefore p) (justAfter q);
                d <- psPrevious delimiter firstsubj;
                if psNonEmpty subject (doubleCut d) (justBefore firstsubj)
                then Nothing else Just firstsubj;
            } :: Maybe (Cut a);
        } in case mfirstone of
        {
            Nothing -> most;
            Just firstone -> mappend (single firstone) most;
        }
    );

    -- | the last subject point before delimiter
    pointsLastOnOrBeforePoints :: (Ord a,?last :: a) => PointSet a -> PointSet a -> PointSet a;
    pointsLastOnOrBeforePoints subject delimiter = filterIntersect (\x -> case psFirstCut delimiter (justBefore x) (justAfter ?last) of
    {
        -- the next delimiter
        Just d -> not (psNonEmpty subject (justAfter x) (justAfter d)); -- if no subject
        Nothing -> False;
    }) subject;



    -- | the last subject point before delimiter
    pointsCutLastBeforePoints :: forall a. (Ord a,?last :: Cut a) => PointSet (Cut a) -> PointSet a -> PointSet (Cut a);
    pointsCutLastBeforePoints subject delimiter = MkPointSet (\p q ->
        let
        {
            delims = psValuesCut delimiter p q :: ValueSet a;
            most = vsMapMaybe (\t -> psLastCut subject (justBefore p) (doubleCut t)) delims :: ValueSet (Cut a);
            mlastone = do
            {
                lastsubj <- psLastCut subject (justBefore p) (justAfter q);
                d <- psNext delimiter lastsubj;
                if psNonEmpty subject (justAfter lastsubj) (doubleCut d)
                then Nothing else Just lastsubj;
            } :: Maybe (Cut a);
        } in case mlastone of
        {
            Nothing -> most;
            Just lastone -> mappend most (single lastone);
        }
    );
{-    
    pointsLastAndIncluding :: (Ord a,?last :: Cut a) => PointSet a -> PointSet a -> PointSet a;
    pointsLastAndIncluding last including = 
     union including (filterIntersect (\x -> case psNext including (justBefore x) of
    {
        Just d -> not (psNonEmpty last (justAfter x) (justAfter d));
        Nothing -> False;
    }) last);
-}    
    -- | True if psOn switched on more recently than psOff
    ;
    psOnAndOff :: (Ord a,?first :: Cut a) => PointSet a -> PointSet a -> a -> Bool;
    psOnAndOff psOn psOff a = case psPrevious psOn (justAfter a) of
    {
        Nothing -> False;
        Just ontime -> not (psNonEmpty psOff (justBefore ontime) (justAfter a));
    };

{- 
    pointsOnAfter :: (Ord a,?first :: a) => PointSet a -> a -> Bool;
    pointsOnAfter psOn a = (member psOn a) ||
    case pointsLastBefore psOn a of
    {
        Just _ -> True;
        Nothing -> False;
    };
-}
    pointsCutBefore :: (Ord a) => PointSet a -> PointSet (Cut a);
    pointsCutBefore (MkPointSet vf) = MkPointSet (\(MkCut p xp) (MkCut q _) -> fmap justBefore ((case xp of
    {
        Before -> id;
        After -> filterIntersect ((/=) p);
    }) (vf p q)));

    pointsCutAfter :: (Ord a) => PointSet a -> PointSet (Cut a);
    pointsCutAfter (MkPointSet vf) = MkPointSet (\(MkCut p _) (MkCut q xq) -> fmap justAfter ((case xq of
    {
        Before -> filterIntersect ((/=) q);
        After -> id;
    }) (vf p q)));

    pointsCutBoth :: (Ord a) => PointSet a -> PointSet (Cut a);
    pointsCutBoth points = union (pointsCutBefore points) (pointsCutAfter points);

    pointsFromCut :: (Ord a) => PointSet (Cut a) -> PointSet a;
    pointsFromCut (MkPointSet vf) = MkPointSet (\p q -> fmap (\(MkCut x _) -> x) (vf (justBefore p) (justAfter q)));

{-    pointsFromCut pointscut = MkPointSet
    {
        pointsMember = \a -> pointsMember pointscut (MkCut a False) || pointsMember pointscut (MkCut a True),
        pointsFirstAfterUntil = \a limit -> do
        {
            MkCut r _ <- pointsFirstAfterUntil pointscut (MkCut a True) (MkCut limit True);
            return r;
        },
        pointsLastBeforeUntil = \a limit -> do
        {
            MkCut r _ <- pointsLastBeforeUntil pointscut (MkCut a False) (MkCut limit False);
            return r;
        }
    };
-}

    pointsSearch :: (Enum t,Ord t,Ord a) => (a -> t) -> (t -> Maybe a) -> PointSet a;
    pointsSearch back f = MkPointSet (\p q -> let
    {
        tp = back p;
        tq = back q;
    
        forwards t | t > tq = [];
        forwards t | Just a <- f t = if a > q then [] else if a < p then (forwards (succ t)) else a:(forwards (succ t));
        forwards t = forwards (succ t);
        
        backwards t | t < tp = [];
        backwards t | Just a <- f t = if a < p then [] else if a > q then (backwards (pred t)) else a:(backwards (pred t));
        backwards t = backwards (pred t);
     } in mkValueSet (forwards tp) (backwards tq));

{-    pointsSearch back f = MkPointSet
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
-}
}
