module Data.SetSearch.PointFunction where
{
    import Data.Maybe;
    import Data.SetSearch.Base;
    import Data.SetSearch.MonotonicFunction;

    -- | f a0 a1 gives an ordered list, strictly ascending if a1 >= a0, and strictly descending if a1 <= a0.
    ;
    newtype PointFunction a b = MkPointFunction {pointsIncluding :: a -> a -> [(a,b)]};

    compareRel :: Ord a => (a,a) -> a -> a -> Ordering;
    compareRel (ref0,ref1) a0 a1 = case compare ref0 ref1 of
    {
        EQ -> EQ;
        LT -> compare a0 a1;
        GT -> compare a1 a0;
    };

    listFromTo :: (Ord t,Enum t) => t -> t -> [t];
    listFromTo t0 t1 | t1 >= t0 = [t0..t1];
    listFromTo t0 t1 = descend t0 where
    {
        descend t | t < t1 = [];
        descend t = t:(descend $ pred t);
    };

    pointEval :: PointFunction a b -> a -> Maybe b;
    pointEval (MkPointFunction f) a = case f a a of
    {
        [] -> Nothing;
        (_,b):_ -> Just b;
    };

    pointIs :: PointFunction a b -> a -> Bool;
    pointIs pf a = isJust (pointEval pf a);

    instance BasedOn (PointFunction a b) where
    {
        type Base (PointFunction a b) = a;
    };

    -- | Only works if functions are strictly monotonic.
    ;
    instance RemapBase (PointFunction a x) (PointFunction b x) where
    {
        remapBase ab ba (MkPointFunction f) = MkPointFunction $ \b1 b2 -> fmap (\(a,x) -> (ab a,x)) (f (ba b1) (ba b2));
    };

    instance Functor (PointFunction a) where
    {
        fmap pq (MkPointFunction f) = MkPointFunction $ \a1 a2 -> fmap (\(a,p) -> (a,pq p)) (f a1 a2);
    };

    pointClosestIncluding :: PointFunction a b -> a -> a -> Maybe (a,b);
    pointClosestIncluding (MkPointFunction f) a0 a1 = case f a0 a1 of
    {
        [] -> Nothing;
        ab:_ -> Just ab;
    };

    pointClosestExcluding :: Eq a => PointFunction a b -> a -> a -> Maybe (a,b);
    pointClosestExcluding (MkPointFunction f) a0 a1 = case f a0 a1 of
    {
        [] -> Nothing;
        (a,_):[] | a == a0 -> Nothing;
        (a,_):ab:_ | a == a0 -> Just ab;
        ab:_ -> Just ab;
    };

    pointNever :: PointFunction a b;
    pointNever = MkPointFunction $ \_ _ -> [];

    pointSingle :: (Ord a) => b -> a -> PointFunction a b;
    pointSingle b a = MkPointFunction $ \a1 a2 -> case (compare a a1,compare a a2) of
    {
        (LT,LT) -> [];
        (GT,GT) -> [];
        _ -> [(a,b)];
    };

    pointBoth :: (Ord a) => PointFunction a p -> PointFunction a q -> PointFunction a (p,q);
    pointBoth (MkPointFunction fp) (MkPointFunction fq) = MkPointFunction $ \a0 a1 -> let
    {
        aps = fp a0 a1;
        aqs = fq a0 a1;
        matchup pp@((pa,p):ppr) qq@((qa,q):qqr) = case compareRel (a0,a1) pa qa of
        {
            EQ -> (pa,(p,q)):(matchup ppr qqr);
            LT -> matchup ppr qq;
            GT -> matchup pp qqr;
        };
        matchup _ _ = [];
    }
    in matchup aps aqs;

    pointBase :: PointFunction a p -> PointFunction a (a,p);
    pointBase (MkPointFunction f) = MkPointFunction $ \a0 a1 -> fmap (\ap@(a,_) -> (a,ap)) (f a0 a1);

    pointCollect :: (a -> p -> Maybe q) -> PointFunction a p -> PointFunction a q;
    pointCollect apmq (MkPointFunction f) = MkPointFunction $ \a0 a1 -> mapMaybe (\(a,p) -> fmap (\q -> (a,q)) (apmq a p)) (f a0 a1);

    pointFilter :: (a -> p -> Bool) -> PointFunction a p -> PointFunction a p;
    pointFilter apB = pointCollect (\a p -> if apB a p then Just p else Nothing);

    pointReduce :: PointFunction a (Maybe p) -> PointFunction a p;
    pointReduce = pointCollect (\_ mp -> mp);

    data EitherBoth a b = EBLeft a | EBRight b | EBBoth a b;

    deriving instance (Eq a,Eq b) => Eq (EitherBoth a b);

    toEitherBoth :: Maybe a -> Maybe b -> Maybe (EitherBoth a b);
    toEitherBoth Nothing Nothing = Nothing;
    toEitherBoth (Just a) Nothing = Just (EBLeft a);
    toEitherBoth Nothing (Just b) = Just (EBRight b);
    toEitherBoth (Just a) (Just b) = Just (EBBoth a b);

    pointEitherBoth :: (Ord a) => PointFunction a p -> PointFunction a q -> PointFunction a (EitherBoth p q);
    pointEitherBoth (MkPointFunction pfp) (MkPointFunction pfq) = MkPointFunction $ \a0 a1 -> let
    {
        aps = pfp a0 a1;
        aqs = pfq a0 a1;
        merge [] [] = [];
        merge ((a,p):rest) [] = (a,EBLeft p):(merge rest []);
        merge [] ((a,q):rest) = (a,EBRight q):(merge [] rest);
        merge pp@((pa,p):restp) qq@((qa,q):restq) = case compareRel (a0,a1) pa qa of
        {
            EQ -> (pa,EBBoth p q):(merge restp restq);
            LT -> (pa,EBLeft p):(merge restp qq);
            GT -> (qa,EBRight q):(merge pp restq);
        };
    } in merge aps aqs;

    pointDiff :: PointFunction a p -> PointFunction a q -> PointFunction a p;
    pointDiff pfp pfq = pointFilter (\a _ -> not (isJust (pointEval pfq a))) pfp;

    pointSymDiff :: (Ord a) => PointFunction a p -> PointFunction a q -> PointFunction a (Either p q);
    pointSymDiff pfp pfq = pointCollect (\_ epqepq -> case epqepq of
    {
        EBLeft p -> Just (Left p);
        EBRight q -> Just (Right q);
        EBBoth _ _ -> Nothing;
    }) (pointEitherBoth pfp pfq);

    pointSetIncluding :: PointFunction a b -> a -> a -> [a];
    pointSetIncluding pf a0 a1 = fmap fst $ pointsIncluding pf a0 a1;

    pointsExcluding :: Eq a => PointFunction a p -> (a,a) -> [(a,p)];
    pointsExcluding (MkPointFunction f) (a0,a1) = filter (\(a,_) -> a /= a0 && a /= a1) $ f a0 a1;

    pointMapInjection :: (Ord a,Ord b) => MonotonicInjection a b -> PointFunction a p -> PointFunction b p;
    pointMapInjection MkMonotonicInjection{..} (MkPointFunction aalap) = MkPointFunction $ \b0 b1 -> mapMaybe (\(a,p) -> fmap (\b -> (b,p)) (miEval a)) $ if b1 >= b0
    then let
    {
        (_,a0) = miBack b0;
        (a1,_) = miBack b1;
    } in if a1 >= a0 then aalap a0 a1 else []
    else let
    {
        (a0,_) = miBack b0;
        (_,a1) = miBack b1;
    } in if a1 <= a0 then aalap a0 a1 else [];

    pointEveryEnum :: (Ord t,Enum t) => PointFunction t t;
    pointEveryEnum = MkPointFunction $ \t0 t1 -> if t1 >= t0
    then let
    {
        next t | t > t1 = [];
        next t = (t,t):(next $ succ t);
    } in next t0
    else let
    {
        next t | t < t1 = [];
        next t = (t,t):(next $ pred t);
    } in next t0;

    pointEveryInjection :: (Ord tp,Ord tq,Enum tp) => MonotonicInjection tp tq -> PointFunction tq tp;
    pointEveryInjection mi = pointMapInjection mi pointEveryEnum;
}
