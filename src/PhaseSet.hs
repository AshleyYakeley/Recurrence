module PhaseSet where
{
    import StationSet;
    
    data Phase a = MkPhase (KnownStationSet a) (StationSet a);

    -- S+S+E-E-S+E
    phaseMember :: (Ord a) => Phase a -> a -> Bool;
    phaseMember (MkPhase s1 s2) t = case kssLastBefore s1 t of
    {
        Nothing -> False; -- never started
        Just start -> case ssLastBeforeUntil s2 t start of
        {
            Just _ -> False; -- ended strictly before t on or after start (but shouldn't be on)
            Nothing -> True; -- didn't end after start
        };
    };
}
