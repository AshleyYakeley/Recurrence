module Data.SetSearch.KnownPointSet where
{
    import Data.SetSearch.Set;
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
    knownToPointSet kps = MkPointSet
    {
        ssMember = kpsMember kps,
        -- strictly after, up to including limit
        ssFirstAfterUntil = \a limit -> do
        {
            r <- kpsFirstAfter kps a;
            if r <= limit then Just r else Nothing;
        },
        -- strictly before, up to including limit
        ssLastBeforeUntil = \a limit -> do
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
}
