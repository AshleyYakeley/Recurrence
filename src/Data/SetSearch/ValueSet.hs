module Data.SetSearch.ValueSet where
{
    import Data.List;
    import Data.SetSearch.Set;
    
    data ValueSet a = MkValueSet
    {
        vsForwards :: [a],
        vsBackwards :: [a]
    };

    vsNonEmpty :: ValueSet a -> Bool;
    vsNonEmpty (MkValueSet (_:_) _) = True;
    vsNonEmpty _ = False;

    vsFirst :: ValueSet a -> Maybe a;
    vsFirst (MkValueSet (a:_) _) = Just a;
    vsFirst _ = Nothing;

    vsLast :: ValueSet a -> Maybe a;
    vsLast (MkValueSet _ (a:_)) = Just a;
    vsLast _ = Nothing;

    instance Functor ValueSet where
    {
        fmap ab (MkValueSet l1 l2) = MkValueSet (fmap ab l1) (fmap ab l2);
    };
    
    instance BasedOn (ValueSet a) where
    {
        type Base (ValueSet a) = a;
    };

    instance RemapBase (ValueSet a) (ValueSet b) where
    {
        remapBase ab _ x = fmap ab x;
    };
    
    instance (Ord a) => Set (ValueSet a) where
    {
        empty = MkValueSet [] [];
        member (MkValueSet list _) a = elem a list;
        union (MkValueSet f1 b1) (MkValueSet f2 b2) = MkValueSet (cf f1 f2) (cb b1 b2) where
        {
            cf [] l = l;
            cf l [] = l;
            cf aa@(a:as) bb@(b:bs) = case compare a b of
            {
                LT -> a:(cf as bb);
                GT -> b:(cf aa bs);
                EQ -> a:(cf as bs);
            };
            cb [] l = l;
            cb l [] = l;
            cb aa@(a:as) bb@(b:bs) = case compare a b of
            {
                GT -> a:(cb as bb);
                LT -> b:(cb aa bs);
                EQ -> a:(cb as bs);
            };
        };
        intersect (MkValueSet f1 b1) (MkValueSet f2 b2) = MkValueSet (cf f1 f2) (cb b1 b2) where
        {
            cf [] _ = [];
            cf _ [] = [];
            cf aa@(a:as) bb@(b:bs) = case compare a b of
            {
                LT -> cf as bb;
                GT -> cf aa bs;
                EQ -> a:(cf as bs);
            };
            cb [] _ = [];
            cb _ [] = [];
            cb aa@(a:as) bb@(b:bs) = case compare a b of
            {
                GT -> cb as bb;
                LT -> cb aa bs;
                EQ -> a:(cb as bs);
            };
        };
        diff (MkValueSet f1 b1) (MkValueSet f2 b2) = MkValueSet (cf f1 f2) (cb b1 b2) where
        {
            cf [] _ = [];
            cf l [] = l;
            cf aa@(a:as) bb@(b:bs) = case compare a b of
            {
                LT -> a:(cf as bb);
                GT -> cf aa bs;
                EQ -> cf as bs;
            };
            cb [] _ = [];
            cb l [] = l;
            cb aa@(a:as) bb@(b:bs) = case compare a b of
            {
                GT -> a:(cb as bb);
                LT -> cb aa bs;
                EQ -> cb as bs;
            };
        };
        symdiff (MkValueSet f1 b1) (MkValueSet f2 b2) = MkValueSet (cf f1 f2) (cb b1 b2) where
        {
            cf [] l = l;
            cf l [] = l;
            cf aa@(a:as) bb@(b:bs) = case compare a b of
            {
                LT -> a:(cf as bb);
                GT -> b:(cf aa bs);
                EQ -> cf as bs;
            };
            cb [] l = l;
            cb l [] = l;
            cb aa@(a:as) bb@(b:bs) = case compare a b of
            {
                GT -> a:(cb as bb);
                LT -> b:(cb aa bs);
                EQ -> cb as bs;
            };
        };
    };
    
    instance (Ord a) => SetSingle (ValueSet a) where
    {
        single a = MkValueSet [a] [a];
    };
    
    instance (Ord a) => SetFilter (ValueSet a) where
    {
        filterIntersect filt (MkValueSet f b) = MkValueSet (filter filt f) (filter filt b);
    };
}
