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

    data Start a = Ongoing | Starts (Cut a);

    instance (Eq a) => Eq (Start a) where
    {
        Ongoing == Ongoing = True;
        (Starts a1) == (Starts a2) = a1 == a2;
        _ == _ = False;
    };

    instance (Ord a) => Ord (Start a) where
    {
        compare Ongoing Ongoing = EQ;
        compare Ongoing (Starts _) = LT;
        compare (Starts _) Ongoing = GT;
        compare (Starts a1) (Starts a2) = compare a1 a2;
    };

    instance (Show a) => Show (Start a) where
    {
        show Ongoing = "ongoing";
        show (Starts (MkCut a False)) = show a;
        show (Starts (MkCut a True)) = "just after " ++ (show a); 
    };
    
    data End a = Whenever | Ends (Cut a);

    instance (Eq a) => Eq (End a) where
    {
        Whenever == Whenever = True;
        (Ends a1) == (Ends a2) = a1 == a2;
        _ == _ = False;
    };

    instance (Ord a) => Ord (End a) where
    {
        compare Whenever Whenever = EQ;
        compare Whenever (Ends _) = GT;
        compare (Ends _) Whenever = LT;
        compare (Ends a1) (Ends a2) = compare a1 a2;
    };
    
    instance (Show a) => Show (End a) where
    {
        show Whenever = "whenever";
        show (Ends (MkCut a False)) = show a;
        show (Ends (MkCut a True)) = "including " ++ (show a);
    };

    data Interval a = MkInterval (Start a) (End a);
}
