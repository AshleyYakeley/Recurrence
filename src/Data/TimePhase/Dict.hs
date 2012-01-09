module Data.TimePhase.Dict (evalAtom) where
{
    import Data.TimePhase.Value;
    import Data.SetSearch;
    import Data.Time;
    
    never :: TimePhase;
    never = PointSetPhase empty;
    
    always :: TimePhase;
    always = IntervalsPhase full;
    
    intersectPair :: TimePhase -> TimePhase -> M TimePhase;
    intersectPair (PointSetPhase p1) (PointSetPhase p2) = return (PointSetPhase (intersect p1 p2));
    intersectPair (IntervalsPhase p1) (IntervalsPhase p2) = return (IntervalsPhase (intersect p1 p2));
    intersectPair (IntervalsPhase ints) (PointSetPhase (PointPCPSet set)) = return (PointSetPhase (PointPCPSet (intervalsIntersect ints set)));
    intersectPair (PointSetPhase (PointPCPSet set)) (IntervalsPhase ints) = return (PointSetPhase (PointPCPSet (intervalsIntersect ints set)));
    intersectPair _ _ = reportError "cannot make point deletions from intervals";
    
    intersectAll :: [TimePhase] -> M TimePhase;
    intersectAll [] = return always;
    intersectAll [tp] = return tp;
    intersectAll (tp:tps) = do
    {
        rest <- intersectAll tps;
        intersectPair tp rest;
    };
           
    dict :: String -> Maybe Value;
    dict "never" = Just (toValue never);
    dict "intersect" = Just (toValue intersectAll);
    dict s = Nothing;
    
    evalAtom :: String -> Maybe Value;
    evalAtom = dict;
}
