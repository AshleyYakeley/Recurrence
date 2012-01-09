module Data.TimePhase.Dict (evalAtom) where
{
    import Data.TimePhase.Value;
    import Data.SetSearch;
    import Data.Time;
    
    never :: TimePhase;
    never = PointSetPhase empty;
    
    always :: TimePhase;
    always = IntervalsPhase full;
    
    intersectPair :: TimePhase -> TimePhase -> TimePhase;
    intersectPair (PointSetPhase p1) (PointSetPhase p2) = PointSetPhase (intersect p1 p2);
    intersectPair (IntervalsPhase p1) (IntervalsPhase p2) = IntervalsPhase (intersect p1 p2);
    intersectPair _ _ = error "NYI";
    
    intersectAll :: [TimePhase] -> TimePhase;
    intersectAll [] = always;
    intersectAll [tp] = tp;
    intersectAll (tp:tps) = intersectPair tp (intersectAll tps);
           
    dict :: String -> Maybe Value;
    dict "never" = Just (toValue never);
    dict "intersect" = Just (toValue intersectAll);
    dict s = Nothing;
    
    evalAtom :: String -> Maybe Value;
    evalAtom = dict;
}
