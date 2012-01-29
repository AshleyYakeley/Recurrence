module Data.SetSearch.ValueSet where
{
    import Data.Maybe;
    import Data.List;
    import Data.SetSearch.Set;
    
    data Marked a = MkMarked Bool a;
    
    retMarked :: a -> Marked a;
    retMarked = MkMarked True;
    
    getMarked :: Marked a -> Maybe a;
    getMarked (MkMarked True a) = Just a;
    getMarked _ = Nothing;
    
    mapMarked :: (Bool -> Bool) -> Marked a -> Marked a;
    mapMarked calc (MkMarked t a) = MkMarked (calc t) a;
    
    filtMarked :: (a -> Bool) -> Marked a -> Marked a;
    filtMarked f (MkMarked t a) = MkMarked (t && f a) a;
    
    instance Functor Marked where
    {
        fmap ab (MkMarked t a) = MkMarked t (ab a);
    };
    
    -- Only the ones marked True count. The others are to keep pace in long lists.
    data ValueSet a = MkValueSet
    {
        vsPossibleForwards :: [Marked a],
        vsPossibleBackwards :: [Marked a]
    };

    mkValueSet :: [a] -> [a] -> ValueSet a;
    mkValueSet ff bb = MkValueSet (fmap retMarked ff) (fmap retMarked bb);

    vsForwards :: ValueSet a -> [a];
    vsForwards vs = mapMaybe getMarked (vsPossibleForwards vs);

    vsBackwards :: ValueSet a -> [a];
    vsBackwards vs = mapMaybe getMarked (vsPossibleBackwards vs);

    vsNonEmpty :: ValueSet a -> Bool;
    vsNonEmpty vs = case vsForwards vs of
    {
        _:_ -> True;
        _ -> False;
    };

    vsFirst :: ValueSet a -> Maybe a;
    vsFirst vs = case vsForwards vs of
    {
        a:_ -> Just a;
        _ -> Nothing;
    };
    
    vsLast :: ValueSet a -> Maybe a;
    vsLast vs = case vsBackwards vs of
    {
        a:_ -> Just a;
        _ -> Nothing;
    };

    instance Functor ValueSet where
    {
        fmap ab (MkValueSet l1 l2) = MkValueSet (fmap (fmap ab) l1) (fmap (fmap ab) l2);
    };
    
    instance BasedOn (ValueSet a) where
    {
        type Base (ValueSet a) = a;
    };

    instance RemapBase (ValueSet a) (ValueSet b) where
    {
        remapBase ab _ x = fmap ab x;
    };
    
    vsCombine :: (Ord a) => (Bool -> Bool -> Bool) -> ValueSet a -> ValueSet a -> ValueSet a;
    vsCombine calc (MkValueSet f1 b1) (MkValueSet f2 b2) = MkValueSet (cf f1 f2) (cb b1 b2) where
    {
        cf [] l = fmap (mapMarked (\t -> calc False t)) l;
        cf l [] = fmap (mapMarked (\t -> calc t False)) l;
        cf aa@((MkMarked ta a):as) bb@((MkMarked tb b):bs) = case compare a b of
        {
            LT -> (MkMarked (calc ta False) a):(cf as bb);
            GT -> (MkMarked (calc False tb) b):(cf aa bs);
            EQ -> (MkMarked (calc ta tb) a):(cf as bs);
        };
        cb [] l = l;
        cb l [] = l;
        cb aa@((MkMarked ta a):as) bb@((MkMarked tb b):bs) = case compare a b of
        {
            GT -> (MkMarked (calc ta False) a):(cb as bb);
            LT -> (MkMarked (calc False tb) b):(cb aa bs);
            EQ -> (MkMarked (calc ta tb) a):(cb as bs);
        };
    }; 
    
    instance (Ord a) => Set (ValueSet a) where
    {
        empty = MkValueSet [] [];
        member vs a = elem a (vsForwards vs);
        union = vsCombine (||);
        intersect = vsCombine (&&);
        diff = vsCombine (\a b -> a && not b);
        symdiff = vsCombine (/=);
    };
    
    instance (Ord a) => SetSingle (ValueSet a) where
    {
        single a = MkValueSet [retMarked a] [retMarked a];
    };
    
    instance (Ord a) => SetFilter (ValueSet a) where
    {
        filterIntersect filt (MkValueSet f b) = let
        {
            filtList = fmap (filtMarked filt);
        } in MkValueSet (filtList f) (filtList b);
    };
}
