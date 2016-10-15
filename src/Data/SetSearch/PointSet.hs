{-# OPTIONS -Wno-orphans #-}
module Data.SetSearch.PointSet where
{
    import Data.Maybe;
    import Data.SetSearch.Set;
    import Data.SetSearch.PointFunction;

    type PointSet a = PointFunction a ();

    pointSet :: PointFunction a b -> PointSet a;
    pointSet = fmap (\_ -> ());

    instance (Ord a) => Set (PointSet a) where
    {
        empty = pointNever;
        union s1 s2 = pointSet (pointEitherBoth s1 s2);
        intersect s1 s2 = pointSet (pointBoth s1 s2);
        diff s1 s2 = pointSet (pointDiff s1 s2);
        symdiff s1 s2 = pointSet (pointSymDiff s1 s2);
    };

    instance (Ord a) => SetMember (PointSet a) where
    {
        member = pointIs;
    };

    instance (Ord a) => SetSingle (PointSet a) where
    {
        single = pointSingle ();
    };

    instance (Ord a) => SetFilter (PointSet a) where
    {
        filterIntersect filt = pointFilter (\a _ -> filt a);
    };

    instance (Ord a) => SetSearch (PointSet a) where
    {
        searchSet (MkPointFunction f) a0 a1 = fmap (\(a,_) -> a) $ f a0 a1;
    };

    pointsSearch :: (Ord p,Enum p) => (t -> p) -> (p -> Maybe t) -> PointFunction t p;
    pointsSearch back f = MkPointFunction $ \t0 t1 -> let
    {
        p0 = back t0;
        p1 = back t1;
    } in mapMaybe (\p -> fmap (\t -> (t,p)) (f p)) $ listFromTo p0 p1;
}
