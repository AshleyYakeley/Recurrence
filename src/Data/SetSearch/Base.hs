module Data.SetSearch.Base where
{
    class BasedOn (s :: *) where
    {
        type Base s :: *;
    };

    class (BasedOn p,BasedOn q) => RemapBase p q where
    {
        remapBase :: (Base p -> Base q) -> (Base q -> Base p) -> p -> q;
    };

    instance BasedOn (a -> b) where
    {
        type Base (a -> b) = a;
    };

    instance RemapBase (a -> x) (b -> x) where
    {
        remapBase _ ba ax b = ax (ba b);
    };
}
