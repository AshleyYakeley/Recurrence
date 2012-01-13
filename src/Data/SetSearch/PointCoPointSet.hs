module Data.SetSearch.PointCoPointSet where
{
    import Data.SetSearch.Set;
    import Data.SetSearch.PointSet;

    data PointCoPointSet a = PointPCPSet (PointSet a) | CoPointPCPSet (PointSet a);
    
    instance (Ord a) => Set (PointCoPointSet a) where
    {
        type Base (PointCoPointSet a) = a;
        empty = PointPCPSet empty;
        member (PointPCPSet set) a = member set a;
        member (CoPointPCPSet set) a = not (member set a);
        union (PointPCPSet set1) (PointPCPSet set2) = PointPCPSet (union set1 set2);
        union (PointPCPSet set1) (CoPointPCPSet coset2) = CoPointPCPSet (diff coset2 set1);
        union (CoPointPCPSet coset1) (PointPCPSet set2) = CoPointPCPSet (diff coset1 set2);
        union (CoPointPCPSet coset1) (CoPointPCPSet coset2) = CoPointPCPSet (intersect coset1 coset2);
        intersect (PointPCPSet set1) (PointPCPSet set2) = PointPCPSet (intersect set1 set2);
        intersect (PointPCPSet set1) (CoPointPCPSet coset2) = PointPCPSet (diff set1 coset2);
        intersect (CoPointPCPSet coset1) (PointPCPSet set2) = PointPCPSet (diff set2 coset1);
        intersect (CoPointPCPSet coset1) (CoPointPCPSet coset2) = CoPointPCPSet (union coset1 coset2);
        diff (PointPCPSet set1) (PointPCPSet set2) = PointPCPSet (diff set1 set2);
        diff (PointPCPSet set1) (CoPointPCPSet coset2) = PointPCPSet (intersect set1 coset2);
        diff (CoPointPCPSet coset1) (PointPCPSet set2) = CoPointPCPSet (union coset1 set2);
        diff (CoPointPCPSet coset1) (CoPointPCPSet coset2) = PointPCPSet (diff coset2 coset1);
        symdiff (PointPCPSet set1) (PointPCPSet set2) = PointPCPSet (symdiff set1 set2);
        symdiff (PointPCPSet set1) (CoPointPCPSet coset2) = CoPointPCPSet (symdiff set1 coset2);
        symdiff (CoPointPCPSet coset1) (PointPCPSet set2) = CoPointPCPSet (symdiff coset1 set2);
        symdiff (CoPointPCPSet coset1) (CoPointPCPSet coset2) = PointPCPSet (symdiff coset2 coset1);
    };
    
    instance (Ord a) => SetSingle (PointCoPointSet a) where
    {
        single a = PointPCPSet (single a);
    };
    
    instance (Ord a) => SetFull (PointCoPointSet a) where
    {
        full = CoPointPCPSet empty;
        invert (PointPCPSet set) = CoPointPCPSet set;
        invert (CoPointPCPSet set) = PointPCPSet set;
    };
}
