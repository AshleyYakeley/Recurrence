module Data.SetSearch.PointSet where
{
    import Data.Monoid;
    import Control.Monad;
    import Data.SetSearch.Set;
    import Data.SetSearch.Cut;
    import Data.SetSearch.PointFunction;
    import Data.SetSearch.KnownPointSet;

    type PointSet a = PointFunction a ();

    pfSet :: PointFunction a b -> PointSet a;
    pfSet = fmap (\_ -> ());

    instance (Ord a) => Set (PointSet a) where
    {
        empty = pfNever;
        member = pfPoint;
        union s1 s2 = pfSet (pfSum s1 s2);
        intersect s1 s2 = pfSet (pfProduct s1 s2);
        diff s1 s2 = pfSet (pfDiff s1 s2);
        symdiff s1 s2 = pfSet (pfSymDiff s1 s2);
    };

    instance (Ord a) => SetSingle (PointSet a) where
    {
        single = pfSingle ();
    };

    instance (Ord a) => SetFilter (PointSet a) where
    {
        filterIntersect filt = pfFilter (\a _ -> filt a);
    };

    instance (Ord a) => SetSearch (PointSet a) where
    {
        firstAfterUntil = pfNextUntil;
        lastBeforeUntil = pfPrevUntil;
    };

    knownToPointSet :: (Ord a) => KnownPointSet a -> PointSet a;
    knownToPointSet kps = MkPointFunction
    {
        pfValue = \a -> if (kpsMember kps a) then Just () else Nothing,
        -- strictly after, up to including limit
        pfNextUntil = \a limit -> do
        {
            r <- kpsFirstAfter kps a;
            if r <= limit then Just r else Nothing;
        },
        -- strictly before, up to including limit
        pfPrevUntil = \a limit -> do
        {
            r <- kpsLastBefore kps a;
            if r >= limit then Just r else Nothing;
        }
    };

    -- | the first subject point on or after delimiter
    ;
    pointsFirstFrom :: (Ord a,?first :: Cut a) => PointSet a -> PointSet a -> PointSet a;
    pointsFirstFrom subject delimiter = filterIntersect (\x -> case pfPrev delimiter (justBefore x) of
    {
        -- the previous delimiter
        Just d -> not (pfNonEmpty subject (justBefore d) (justBefore x)); -- if no subject
        Nothing -> False;
    }) subject;
{-
    -- | the first subject point after delimiter
    ;
    pointsCutFirstAfterPoints :: forall a. (Ord a,?first :: Cut a) => PointFunction (Cut a) p -> PointFunction a q -> PointFunction (Cut a) p;
    pointsCutFirstAfterPoints subject delimiter = MkPointFunction
    {
        pfValue = \cut@(MkCut a _) -> do
        {
            p <- pfValue subject cut;
            case pfPrev subject cut of
            {

            };
        },

         :: Cut a -> Maybe (),
        pfNextUntil :: Cut a -> Cut a -> Maybe (Cut a),
        pfPrevUntil :: Cut a -> Cut a -> Maybe (Cut a)
    };

    MkPointSet (\p q ->
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
-}
    -- | the last subject point on or before delimiter, or last before ?last
    ;
    pointsLastOn :: forall a b. (Ord a,?last :: a) => PointFunction a b -> PointSet a -> PointFunction a b;
    pointsLastOn subject delimiter = let
    {
        pfNextDelimiter :: a -> a;
        pfNextDelimiter a = case pfNextUntil delimiter a ?last of
        {
            Nothing -> ?last;
            Just d -> d;
        };

        goodSubject :: a -> Maybe ();
        goodSubject a = if pfPoint delimiter a
        then Just ()
        else case pfNextUntil subject a (pfNextDelimiter a) of
        {
            Just _ -> Nothing;
            _ -> Just ();
        };
    } in MkPointFunction
    {
        pfValue = \a -> do
        {
            b <- pfValue subject a;
            goodSubject a;
            return b;
        },

        pfNextUntil = \n f -> do
        {
            s <- pfNextUntil subject n f;
            let {d = pfNextDelimiter s};
            case pfLastCut subject (justAfter d) (justAfter s) of
            {
                Nothing -> Just s;
                Just a | a <= f -> Just a;
                _ -> Nothing;
            };
        },

        pfPrevUntil = \n f -> let
        {
            find x = do
            {
                s <- pfPrevUntil subject n f;
                do
                {
                    goodSubject s;
                    return s;
                } `mplus` (do
                {
                    d <- pfPrevUntil delimiter s f;
                    do
                    {
                        pfValue subject d;
                        return d;
                    } `mplus` (find d);
                });
            };
        } in find n
    };

    -- | the last subject point before delimiter
    pointsCutLastBeforePoints :: forall a. (Ord a,?last :: Cut a) => PointFunction (Cut a) b -> PointSet a -> PointFunction (Cut a) b;
    pointsCutLastBeforePoints subject delimiter = let
    {
        goodSubject s = 
    } in MkPointFunction
    {
        pfValue = \a -> do
        {
            b <- pfValue subject a;
            goodSubject a;
            return b;
        },
        pfNextUntil = \n f -> do
        {
            d <- pfNext delimiter n;
            pfLastCut subject (doubleCut d) (justAfter n);
        },
        pfPrevUntil = \n f -> do
        {
            s <- pfPrevUntil subject n f;
            case goodSubject s of
            {
                Just () -> Just s;
                Nothing -> do
                {
                    d <- pfPrevUntil delimiter s f;
                    pfLastCut subject (doubleCut d) (justAfter f);
                };
            };
        }
    };


     MkPointSet (\p q ->
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

    -- | the first subject point after delimiter
    pointsCutFirstAfterPoints :: forall a. (Ord a,?first :: Cut a) => PointFunction (Cut a) b -> PointSet a -> PointFunction (Cut a) b;
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

    -- | True if psOn switched on more recently than psOff
    ;
    psOnAndOff :: (Ord a,?first :: Cut a) => Bool -> PointSet a -> PointSet a -> Cut a -> Bool;
    psOnAndOff False psOn psOff cut = case pfPrev psOn cut of
    {
        Nothing -> False; -- never switched on
        Just ontime -> not (pfNonEmpty psOff (justBefore ontime) cut);
    };
    psOnAndOff True psOn psOff cut = case pfPrev psOff cut of
    {
        Nothing -> True; -- never switched off
        Just offtime -> pfNonEmpty psOn (justAfter offtime) cut; -- an on after the off
    };

    psEvery :: (Enum a,Ord a) => PointSet a;
    psEvery = knownToPointSet MkKnownPointSet
    {
        kpsMember = \_ -> True,
        kpsFirstAfter = Just . succ,
        kpsLastBefore = Just . pred
    };

    pointsCutBefore :: (Ord a) => PointFunction a p -> PointFunction (Cut a) p;
    pointsCutBefore = pfProject projectBefore;

    pointsCutAfter :: (Ord a) => PointFunction a p -> PointFunction (Cut a) p;
    pointsCutAfter = pfProject projectAfter;

    pointsCutBoth :: (Ord a) => PointSet a -> PointSet (Cut a);
    pointsCutBoth points = union (pointsCutBefore points) (pointsCutAfter points);

    pointsFromCut :: (Ord a) => PointSet (Cut a) -> PointSet a;
    pointsFromCut ps = union (pfExtract projectBefore ps) (pfExtract projectAfter ps);
{-
    (MkPointSet vf) = MkPointSet (\p q -> fmap (\(MkCut x _) -> x) (vf (justBefore p) (justAfter q)));
-}
    pointsSearch :: (Enum p,Ord p,Ord a) => (a -> p) -> (p -> Maybe a) -> PointFunction a p;
    pointsSearch back f = MkPointFunction
    {
        pfValue = \a -> let
        {
            p = back a;
        } in case f p of
        {
            Just a' | a == a' -> Just p;
            _ -> Nothing;
        },
        pfNextUntil = \near far -> let
        {
            pn = back near;
            pf = back far;

            find p | p > pf = Nothing;
            find p | Just a <- f p = if a > far then Nothing else if a <= near then find (succ p) else Just a;
            find p = find (succ p);
        } in find pn,
        pfPrevUntil = \near far -> let
        {
            pn = back near;
            pf = back far;

            find p | p < pf = Nothing;
            find p | Just a <- f p = if a < far then Nothing else if a >= near then find (pred p) else Just a;
            find p = find (pred p);
        } in find pn
    };
}
