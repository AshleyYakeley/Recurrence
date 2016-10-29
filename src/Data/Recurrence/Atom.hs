module Data.Recurrence.Atom where
{
    import Data.Recurrence.Value;

    data Atom = IdentifierAtom String | LiteralAtom Value;
}
