module Data.SetSearch.MonotonicInjection where
{
    import Prelude hiding (id,(.));
    import Control.Category;

    data MonotonicInjection a b = MkMonotonicInjection
    {
        miEval :: a -> b,
        miBack :: b -> (a,a)
    };

    instance Category MonotonicInjection where
    {
        id = MkMonotonicInjection id $ \x -> (x,x);
        (MkMonotonicInjection bc cbb) . (MkMonotonicInjection ab baa) = MkMonotonicInjection (bc . ab) $ \c -> let
        {
            (b0,b1) = cbb c;
            (a0,_) = baa b0;
            (_,a1) = baa b1;
        } in (a0,a1);
    }
}
