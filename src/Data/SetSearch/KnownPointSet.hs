module Data.SetSearch.KnownPointSet where
{
    import Data.SetSearch.Set;
    import Data.SetSearch.PointFunction;
    import Data.SetSearch.PointSet;

    data KnownPointSet a = MkKnownPointSet
    {
        kpsMember :: a -> Bool,
        -- strictly after
        kpsFirstAfter :: a -> Maybe a,
        -- strictly before
        kpsLastBefore :: a -> Maybe a
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
    
    instance BasedOn (KnownPointSet a) where
    {
        type Base (KnownPointSet a) = a;
    };
    
    instance RemapBase (KnownPointSet a) (KnownPointSet b) where
    {
        remapBase ab ba kpsa = MkKnownPointSet
        {
            kpsMember = \b -> kpsMember kpsa (ba b),
            kpsFirstAfter = \b -> fmap ab (kpsFirstAfter kpsa (ba b)),
            kpsLastBefore = \b -> fmap ab (kpsLastBefore kpsa (ba b))
        };
    };

    kpsEach :: (Ord a,Enum t) => (a -> t) -> (t -> a) -> KnownPointSet a;
    kpsEach back f = MkKnownPointSet
    {
        kpsMember = \day -> day == f (back day),
        kpsFirstAfter = \day -> Just (let
        {
            thisOne = f (back day);
            nextOne= f (succ (back day));
        } in if day < thisOne then thisOne else nextOne
        ),
        kpsLastBefore = \day -> Just (let
        {
            thisOne = f (back day);
            prevOne = f (pred (back day));
        } in if day > thisOne then thisOne else prevOne
        )
    };
}
