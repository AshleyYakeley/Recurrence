module Data.SetSearch.Cut where
{ 
    -- False means "just before", True means "just after".
    data Cut a = MkCut a Bool;
    
    instance (Eq a) => Eq (Cut a) where
    {
        (MkCut a1 x1) == (MkCut a2 x2) = (a1 == a2) && (x1 == x2);
    };
    
    instance (Ord a) => Ord (Cut a) where
    {
        compare (MkCut a1 x1) (MkCut a2 x2) = let
        {
            ac = compare a1 a2;
        } in if ac == EQ then (case (x1, x2) of
        {
            (False,True) -> LT;
            (True,False) -> GT;
            _ -> EQ;
        }) else ac;
    };
}
