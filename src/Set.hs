module Set where
{
    class Set1 (s :: *) where
    {
        type Base s :: *;
        empty :: s;
        member :: s -> Base s -> Bool;
        single :: Base s -> s;
        union :: s -> s -> s;
    };
    
    class (Set1 s) => SetSearch s where
    {
        -- strictly after, up to including limit
        firstAfterUntil :: s -> Base s -> Base s -> Maybe (Base s);
        -- strictly before, up to including limit
        lastBeforeUntil :: s -> Base s -> Base s -> Maybe (Base s);
    };
    
    class (Set1 s) => Set2 (s :: *) where
    {
        intersect :: s -> s -> s;
        fIntersect :: (Base s -> Bool) -> s -> s;
        intersect s = fIntersect (member s);
    };

    instance (Eq a) => Set1 (a -> Bool) where
    {
        type Base (a -> Bool) = a;
        empty _ = False;
        member = id;
        single = (==);
        union s1 s2 a = (s1 a) || (s2 a);
    };

    instance (Eq a) => Set2 (a -> Bool) where
    {
        intersect s1 s2 a = (s1 a) && (s2 a);
        fIntersect = intersect;
    };

}
