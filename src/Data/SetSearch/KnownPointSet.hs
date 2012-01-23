module Data.SetSearch.KnownPointSet where
{
    import Data.SetSearch.Set;
    import Data.SetSearch.ValueSet;
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
    knownToPointSet kps = MkPointSet (\p q -> let
    {
        forwardsRest a = case kpsFirstAfter kps a of
        {
            Just x | x <= q -> x:(forwardsRest x);
            _ -> [];
        };

        forwards a | a > q = [];
        forwards a | kpsMember kps a = a:(forwardsRest a);
        forwards a = forwardsRest a;

        backwardsRest a = case kpsLastBefore kps a of
        {
            Just x | x >= p -> x:(backwardsRest x);
            _ -> [];
        };

        backwards a | a < p = [];
        backwards a | kpsMember kps a = a:(backwardsRest a);
        backwards a = backwardsRest a;
    } in MkValueSet (forwards p) (backwards q));
    
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
