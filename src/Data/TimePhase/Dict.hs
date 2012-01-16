module Data.TimePhase.Dict (evalAtom) where
{
    import Data.Fixed;
    import Data.Time;
    import Data.TimePhase.Value;
    import Data.SetSearch;

    startOf :: TimePhase -> PointSet T;
    startOf ps = union (psExceptions ps) (intervalsStartOf (psIntervals ps));

    endOf :: TimePhase -> PointSet T;
    endOf ps = union (psExceptions ps) (intervalsEndOf (psIntervals ps));

    midnights :: PointSet T;
    midnights = MkPointSet
    {
        ssMember = \t -> (localTimeOfDay t == midnight),
        ssFirstAfterUntil = \t limit -> let
        {
            t' = LocalTime
            {
                localDay = addDays 1 (localDay t),
                localTimeOfDay = midnight
            };
        } in if t' <= limit then Just t' else Nothing,
        ssLastBeforeUntil = \t limit -> let
        {
            day = localDay t;
            t' = LocalTime
            {
                localDay = if localTimeOfDay t > midnight then day else addDays (-1) day,
                localTimeOfDay = midnight
            };
        } in if t' >= limit then Just t' else Nothing
    };
    
    theDay :: StepFunction T Day;
    theDay = MkStepFunction
    {
        sfValue = localDay,
        sfPossibleChanges = midnights
    };
    
    weekDay :: Integer -> Intervals T;
    weekDay i = fmap (\day -> mod' (toModifiedJulianDay day) 7 == i) theDay;
    
    delay :: NominalDiffTime -> TimePhase -> TimePhase;
    delay dt = remapBase (addLocalTime dt) (addLocalTime (negate dt));
    
    dict :: String -> Maybe Value;

    dict "never" = Just (toValue (empty :: TimePhase));
    dict "always" = Just (toValue (full :: TimePhase));
    dict "not" = Just (toValue (invert :: TimePhase -> TimePhase));
    dict "when" = Just (toValue (intersectAll :: [TimePhase] -> TimePhase));
    dict "and" = Just (toValue (unionAll :: [TimePhase] -> TimePhase));
    dict "start" = Just (toValue startOf);
    dict "end" = Just (toValue endOf);
    dict "delay" = Just (toValue delay);
    dict "1h" = Just (toValue (3600 :: NominalDiffTime)); -- temp

    dict "midnight" = Just (toValue midnights);

    dict "Wednesday" = Just (toValue (weekDay 0));
    dict "Thursday" = Just (toValue (weekDay 1));
    dict "Friday" = Just (toValue (weekDay 2));
    dict "Saturday" = Just (toValue (weekDay 3));
    dict "Sunday" = Just (toValue (weekDay 4));
    dict "Monday" = Just (toValue (weekDay 5));
    dict "Tuesday" = Just (toValue (weekDay 6));

    dict s = Nothing;
    
    evalAtom :: String -> Maybe Value;
    evalAtom = dict;
}
