module Data.SetSearch.KnownPointSet where
{
    import Data.SetSearch.Base;
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

    knownToPointSet :: Ord t => KnownPointSet t -> PointSet t;
    knownToPointSet MkKnownPointSet{..} = MkPointFunction $ \t0 t1 -> let
    {
        ascend Nothing = [];
        ascend (Just t) | t > t1 = [];
        ascend (Just t) = (t,()):(ascend' t);

        ascend' t = ascend $ kpsFirstAfter t;

        descend Nothing = [];
        descend (Just t) | t < t1 = [];
        descend (Just t) = (t,()):(descend' t);

        descend' t = descend $ kpsLastBefore t;
    } in if t1 > t0
        then (if kpsMember t0 then ascend (Just t0) else ascend' t0)
        else (if kpsMember t0 then descend (Just t0) else descend' t0);
}
