module Data.TimePhase.Atom where
{
    import Data.Char;
    import Data.Time;
    import Text.ParserCombinators.ReadPrec;
    import Data.TimePhase.Value;
    
    allowedName :: String -> Bool;
    allowedName ('_':_) = True;
    allowedName (c:_) | isLetter c = True;
    allowedName _ = False;
    
    evalAtom :: String -> Maybe Value;
    evalAtom "1h" = Just (toValue (3600 :: NominalDiffTime)); -- temp
    evalAtom atom = Nothing;
}

