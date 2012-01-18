module Data.TimePhase.Atom where
{
    import Data.TimePhase.Value;
    
    data Atom = IdentifierAtom String | LiteralAtom Value;
}

