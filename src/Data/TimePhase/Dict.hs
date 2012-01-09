module Data.TimePhase.Dict (evalAtom) where
{
    import Data.TimePhase.Value;
    import Data.SetSearch;
    import Data.Time;
    
    never :: TimePhase;
    never = IntervalsPhase empty;
    
    always :: TimePhase;
    always = IntervalsPhase full;
    
    intersectPair :: TimePhase -> TimePhase -> M TimePhase;
    intersectPair (PointSetPhase p1) (PointSetPhase p2) = return (PointSetPhase (intersect p1 p2));
    intersectPair (IntervalsPhase p1) (IntervalsPhase p2) = return (IntervalsPhase (intersect p1 p2));
    intersectPair (IntervalsPhase ints) (PointSetPhase (PointPCPSet set)) = return (PointSetPhase (PointPCPSet (intervalsIntersect set ints)));
    intersectPair (PointSetPhase (PointPCPSet set)) (IntervalsPhase ints) = return (PointSetPhase (PointPCPSet (intervalsIntersect set ints)));
    intersectPair _ _ = reportError "cannot make point deletions from intervals";
    
    intersectAll :: [TimePhase] -> M TimePhase;
    intersectAll [] = return always;
    intersectAll [tp] = return tp;
    intersectAll (tp:tps) = do
    {
        rest <- intersectAll tps;
        intersectPair tp rest;
    };
    
    unionPair :: TimePhase -> TimePhase -> M TimePhase;
    unionPair (PointSetPhase p1) (PointSetPhase p2) = return (PointSetPhase (union p1 p2));
    unionPair (IntervalsPhase p1) (IntervalsPhase p2) = return (IntervalsPhase (union p1 p2));
    unionPair (IntervalsPhase ints) (PointSetPhase (CoPointPCPSet set)) = return (PointSetPhase (CoPointPCPSet (intervalsDiff set ints)));
    unionPair (PointSetPhase (CoPointPCPSet set)) (IntervalsPhase ints) = return (PointSetPhase (CoPointPCPSet (intervalsDiff set ints)));
    unionPair _ _ = reportError "cannot make point additions to intervals";
    
    unionAll :: [TimePhase] -> M TimePhase;
    unionAll [] = return always;
    unionAll [tp] = return tp;
    unionAll (tp:tps) = do
    {
        rest <- unionAll tps;
        unionPair tp rest;
    };

    dict :: String -> Maybe Value;
    dict "never" = Just (toValue never);
    dict "always" = Just (toValue always);
    dict "intersect" = Just (toValue intersectAll);
    dict "union" = Just (toValue unionAll);
    dict s = Nothing;
    
    evalAtom :: String -> Maybe Value;
    evalAtom = dict;
}
