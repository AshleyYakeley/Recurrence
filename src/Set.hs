module Set where
{
    class Set1 (s :: *) where
    {
        type Base s :: *;
        empty :: s;
        member :: s -> Base s -> Bool;
        union :: s -> s -> s;
        intersect :: s -> s -> s;
        diff :: s -> s -> s;
    };
    
    class (Set1 s) => SetSingle (s :: *) where
    {
        single :: Base s -> s;
    };
    
    class (Set1 s) => SetSearch s where
    {
        -- strictly after, up to including limit
        firstAfterUntil :: s -> Base s -> Base s -> Maybe (Base s);
        -- strictly before, up to including limit
        lastBeforeUntil :: s -> Base s -> Base s -> Maybe (Base s);
    };
    
    class (Set1 s) => SetFilter (s :: *) where
    {
        filterIntersect :: (Base s -> Bool) -> s -> s;
    };
    
    class (Set1 s) => SetFull (s :: *) where
    {
        full :: s;
        invert :: s -> s;
        invert = diff full;
    };

    instance (Eq a) => Set1 (a -> Bool) where
    {
        type Base (a -> Bool) = a;
        empty _ = False;
        member = id;
        union s1 s2 a = (s1 a) || (s2 a);
        intersect s1 s2 a = (s1 a) && (s2 a);
        diff s1 s2 a = (s1 a) && (not (s2 a));
    };

    instance (Eq a) => SetSingle (a -> Bool) where
    {
        single = (==);
    };

    instance (Eq a) => SetFilter (a -> Bool) where
    {
        filterIntersect = intersect;
    };

    instance (Eq a) => SetFull (a -> Bool) where
    {
        full _ = True;
        invert s a = not (s a);
    };
}
