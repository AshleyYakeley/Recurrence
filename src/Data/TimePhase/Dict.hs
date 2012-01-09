module Data.TimePhase.Dict (evalAtom) where
{
    import Data.TimePhase.Value;
    import Data.SetSearch;
    import Data.Time;
    
    never :: TimePhase;
    never = PointSetPhase empty;
    
    intersectAll :: [TimePhase] -> TimePhase;
    intersectAll _ = never;
           
    dict :: String -> Maybe Value;
    dict "never" = Just (toValue never);
    dict "intersect" = Just (toValue intersectAll);
    dict s = Nothing;
    
    evalAtom :: String -> Maybe Value;
    evalAtom = dict;
}
