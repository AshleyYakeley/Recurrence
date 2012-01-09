module Data.SetSearch.PointCoPointSet where
{
    import Data.SetSearch.Set;
    import Data.SetSearch.PointSet;

    -- | False is a PointSet, True is a CoPointSet
    data PointCoPointSet a = MkPointCoPointSet Bool (PointSet a);
    
    instance (Ord a) => Set (PointCoPointSet a) where
    {
        type Base (PointCoPointSet a) = a;
        empty = MkPointCoPointSet False empty;
        member (MkPointCoPointSet b set) a = (member set a) /= b;
        union (MkPointCoPointSet False set1) (MkPointCoPointSet False set2) = MkPointCoPointSet False (union set1 set2);
        union (MkPointCoPointSet False set1) (MkPointCoPointSet True coset2) = MkPointCoPointSet True (diff coset2 set1);
        union (MkPointCoPointSet True coset1) (MkPointCoPointSet False set2) = MkPointCoPointSet True (diff coset1 set2);
        union (MkPointCoPointSet True coset1) (MkPointCoPointSet True coset2) = MkPointCoPointSet True (intersect coset1 coset2);
        intersect (MkPointCoPointSet False set1) (MkPointCoPointSet False set2) = MkPointCoPointSet False (intersect set1 set2);
        intersect (MkPointCoPointSet False set1) (MkPointCoPointSet True coset2) = MkPointCoPointSet False (diff set1 coset2);
        intersect (MkPointCoPointSet True coset1) (MkPointCoPointSet False set2) = MkPointCoPointSet False (diff set2 coset1);
        intersect (MkPointCoPointSet True coset1) (MkPointCoPointSet True coset2) = MkPointCoPointSet True (union coset1 coset2);
        diff (MkPointCoPointSet False set1) (MkPointCoPointSet False set2) = MkPointCoPointSet False (diff set1 set2);
        diff (MkPointCoPointSet False set1) (MkPointCoPointSet True coset2) = MkPointCoPointSet False (intersect set1 coset2);
        diff (MkPointCoPointSet True coset1) (MkPointCoPointSet False set2) = MkPointCoPointSet True (union coset1 set2);
        diff (MkPointCoPointSet True coset1) (MkPointCoPointSet True coset2) = MkPointCoPointSet False (diff coset2 coset1);
    };
    
    instance (Ord a) => SetSingle (PointCoPointSet a) where
    {
        single a = MkPointCoPointSet False (single a);
    };
    
    instance (Ord a) => SetFull (PointCoPointSet a) where
    {
        full = MkPointCoPointSet True empty;
        invert (MkPointCoPointSet b set) = MkPointCoPointSet (not b) set;
    };
}
