module Data.SetSearch.PointFunction where
{
    import Data.Maybe;
    import Data.Monoid;
    import Control.Monad;
    import Data.SetSearch.Set;
    import Data.SetSearch.Cut;

    data PointFunction a b = MkPointFunction
    {
        pfValue :: a -> Maybe b,
        pfNextUntil :: a -> a -> Maybe a,
        pfPrevUntil :: a -> a -> Maybe a
    };
    
    pfPoint :: PointFunction a b -> a -> Bool;
    pfPoint pf a = isJust (pfValue pf a);
    
    instance BasedOn (PointFunction a b) where
    {
        type Base (PointFunction a b) = a;
    };
    
    instance RemapBase (PointFunction a x) (PointFunction b x) where
    {
        remapBase ab ba pf = MkPointFunction
        {
            pfValue = \b -> pfValue pf (ba b),
            pfNextUntil = \b1 b2 -> fmap ab (pfNextUntil pf (ba b1) (ba b2)),
            pfPrevUntil = \b1 b2 -> fmap ab (pfPrevUntil pf (ba b1) (ba b2))
        };
    };

    instance Functor (PointFunction a) where
    {
        fmap pq pf = MkPointFunction
        {
            pfValue = \a -> fmap pq (pfValue pf a),
            pfNextUntil = pfNextUntil pf,
            pfPrevUntil = pfPrevUntil pf
        };
    };

    pfNever :: PointFunction a b;
    pfNever = MkPointFunction
    {
        pfValue = \_ -> Nothing,
        pfNextUntil = \_ _ -> Nothing,
        pfPrevUntil = \_ _ -> Nothing
    };

    pfSingle :: (Ord a) => b -> a -> PointFunction a b;
    pfSingle b a = MkPointFunction
    {
        pfValue = \a' -> if a == a' then Just b else Nothing,
        pfNextUntil = \anear afar -> if (a > anear) && (a <= afar) then Just a else Nothing,
        pfPrevUntil = \anear afar -> if (a < anear) && (a >= afar) then Just a else Nothing
    };

    pfProduct :: (Ord a) => PointFunction a p -> PointFunction a q -> PointFunction a (p,q);
    pfProduct pfp pfq = MkPointFunction
    {
        pfValue = \a -> do
        {
            p <- pfValue pfp a;
            q <- pfValue pfq a;
            return (p,q);
        },
        pfNextUntil = \anear afar -> let
        {
            f a | a >= afar = Nothing;
            f a = do
            {
                ap <- pfNextUntil pfp a afar;
                aq <- pfNextUntil pfq a afar;
                case compare ap aq of
                {
                    EQ -> return ap;
                    LT -> if pfPoint pfp aq then return aq else f aq;
                    GT -> if pfPoint pfq ap then return ap else f ap;
                };
            };
        } in f anear,
        pfPrevUntil = \anear afar ->
        let
        {
            f a | a <= afar = Nothing;
            f a = do
            {
                ap <- pfPrevUntil pfp a afar;
                aq <- pfPrevUntil pfq a afar;
                case compare ap aq of
                {
                    EQ -> return ap;
                    GT -> if pfPoint pfp aq then return aq else f aq;
                    LT -> if pfPoint pfq ap then return ap else f ap;
                };
            };
        } in f anear
    };

    pfBase :: PointFunction a p -> PointFunction a a;
    pfBase pf = MkPointFunction
    {
        pfValue = \a -> do
        {
            _ <- pfValue pf a;
            return a;
        },
        pfNextUntil = pfNextUntil pf,
        pfPrevUntil = pfPrevUntil pf
    };

    pfCollect :: (Ord a) => (a -> p -> Maybe q) -> PointFunction a p -> PointFunction a q;
    pfCollect apmq pf = MkPointFunction
    {
        pfValue = \a -> do
        {
            p <- pfValue pf a;
            apmq a p;
        },
        pfNextUntil = \anear afar -> let
        {
            f a | a >= afar = Nothing;
            f a = do
            {
                ap <- pfNextUntil pf a afar;
                case pfValue pf ap of
                {
                    Just p | Just _ <- apmq ap p -> return ap;
                    _ -> f ap;
                };
            };
        } in f anear,
        pfPrevUntil = \anear afar ->
        let
        {
            f a | a <= afar = Nothing;
            f a = do
            {
                ap <- pfPrevUntil pf a afar;
                case pfValue pf ap of
                {
                    Just p | Just _ <- apmq ap p -> return ap;
                    _ -> f ap;
                };
            };
        } in f anear
    };

    pfFilter :: (Ord a) => (a -> p -> Bool) -> PointFunction a p -> PointFunction a p;
    pfFilter apB = pfCollect (\a p -> if apB a p then Just p else Nothing);

    pfReduce :: (Ord a) => PointFunction a (Maybe p) -> PointFunction a p;
    pfReduce = pfCollect (\_ mp -> mp);

    pfSum :: (Ord a) => PointFunction a p -> PointFunction a q -> PointFunction a (Either (p,q) (Either p q));
    pfSum pfp pfq = MkPointFunction
    {
        pfValue = \a -> case (pfValue pfp a,pfValue pfq a) of
        {
            (Nothing,Nothing) -> Nothing;
            (Just p,Nothing) -> Just (Right (Left p));
            (Nothing,Just q) -> Just (Right (Right q));
            (Just p,Just q) -> Just (Left (p,q));
        },
        pfNextUntil = \anear afar -> case pfNextUntil pfp anear afar of
        {
            Just ap -> case pfNextUntil pfq anear ap of
            {
                Just aq -> Just aq;
                Nothing -> Just ap;
            };
            Nothing -> pfNextUntil pfq anear afar;
        },
        pfPrevUntil = \anear afar -> case pfPrevUntil pfp anear afar of
        {
            Just ap -> case pfPrevUntil pfq anear ap of
            {
                Just aq -> Just aq;
                Nothing -> Just ap;
            };
            Nothing -> pfPrevUntil pfq anear afar;
        }
    };

    pfDiff :: (Ord a) => PointFunction a p -> PointFunction a q -> PointFunction a p;
    pfDiff pfp pfq = pfFilter (\a _ -> not (isJust (pfValue pfq a))) pfp;
    
    pfSymDiff :: (Ord a) => PointFunction a p -> PointFunction a q -> PointFunction a (Either p q);
    pfSymDiff pfp pfq = pfCollect (\_ epqepq -> case epqepq of
    {
        Right epq -> Just epq;
        _ -> Nothing;
    }) (pfSum pfp pfq);
    
    pfFirstCut :: (Ord a) => PointFunction a b -> Cut a -> Cut a -> Maybe a;
    pfFirstCut pf (MkCut an xn) (MkCut af xf) = case xn of
    {
        Before | pfPoint pf an -> Just an;
        _ -> case (pfNextUntil pf an af,xf) of
        {
            (Just a,Before) | a == af -> Nothing;
            (ma,_) -> ma;
        };
    };
    
    pfLastCut :: (Ord a) => PointFunction a b -> Cut a -> Cut a -> Maybe a;
    pfLastCut pf (MkCut an xn) (MkCut af xf) = case xn of
    {
        After | pfPoint pf an -> Just an;
        _ -> case (pfPrevUntil pf an af,xf) of
        {
            (Just a,After) | a == af -> Nothing;
            (ma,_) -> ma;
        };
    };

    pfNonEmpty :: (Ord a) => PointFunction a b -> Cut a -> Cut a -> Bool;
    pfNonEmpty ps an af = isJust (pfFirstCut ps an af);

    pfPrev :: (Ord a,?first :: Cut a) => PointFunction a b -> Cut a -> Maybe a;
    pfPrev ps x = pfLastCut ps x ?first;

    pfNext :: (Ord a,?last :: Cut a) => PointFunction a b -> Cut a -> Maybe a;
    pfNext ps x = pfFirstCut ps x ?last;

{-
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
  
    -- | True if psOn switched on more recently than psOff
    ;
    psOnAndOff :: (Ord a,?first :: Cut a) => Bool -> PointSet a -> PointSet a -> Cut a -> Bool;
    psOnAndOff False psOn psOff cut = case psPrevious psOn cut of
    {
        Nothing -> False; -- never switched on
        Just ontime -> not (psNonEmpty psOff (justBefore ontime) cut);
    };
    psOnAndOff True psOn psOff cut = case psPrevious psOff cut of
    {
        Nothing -> True; -- never switched off
        Just offtime -> psNonEmpty psOn (justAfter offtime) cut; -- an on after the off
    };

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
-}
}
