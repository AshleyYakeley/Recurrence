module Data.TimePhase.Dict (dict) where
{
    import Data.Fixed;
    import Data.Time;
    import Data.Time.Calendar.Easter;
    import Data.SetSearch;
    import Data.TimePhase.Time;
    import Data.TimePhase.Value;

    phaseIntersectAll :: Maybe (TimePhase,[PhaseSet T]) -> TimePhase;
    phaseIntersectAll Nothing = phaseFull;
    phaseIntersectAll (Just (phase,sets)) = f sets where
    {
        f [] = phase;
        f (s:ss) = phaseIntersect (f ss) s;
    };

    phaseUnionAll :: Maybe (TimePhase,[PhaseSet T]) -> TimePhase;
    phaseUnionAll Nothing = phaseEmpty;
    phaseUnionAll (Just (phase,sets)) = f sets where
    {
        f [] = phase;
        f (s:ss) = phaseUnion (f ss) s;
    };

    dict :: String -> Maybe Value;

    dict "never" = Just (toValue (phaseEmpty :: TimePhase));
    dict "always" = Just (toValue (phaseFull :: TimePhase));
    dict "not" = Just (toValue (phaseInvert :: TimePhase -> TimePhase));
    dict "when" = Just (toValue phaseIntersectAll);
    dict "and" = Just (toValue phaseUnionAll);
    dict "start" = Just (toValue (phaseStartOf :: TimePhase -> PointSet T));
    dict "end" = Just (toValue (phaseEndOf :: TimePhase -> PointSet T));
    dict "interval" = Just (toValue fromTo);
    dict "from" = Just (toValue onAfter);
    dict "until" = Just (toValue (invert . onAfter));
    dict "nth" = Just (toValue nthIn);
    dict "of" = Just (toValue ofPhase);
    
    dict "delay" = Just (toValue (delay :: NominalDiffTime -> TimePhase -> TimePhase));

    dict "midnight" = Just (toValue newDay);
    dict "midday" = Just (toValue (timeOfDay midday));
    dict "day" = Just (toValue dayPhase);
    dict "month" = Just (toValue monthPhase);
    dict "year" = Just (toValue yearPhase);

    dict "Wednesday" = Just (toValue (weekDay 0));
    dict "Thursday" = Just (toValue (weekDay 1));
    dict "Friday" = Just (toValue (weekDay 2));
    dict "Saturday" = Just (toValue (weekDay 3));
    dict "Sunday" = Just (toValue (weekDay 4));
    dict "Monday" = Just (toValue (weekDay 5));
    dict "Tuesday" = Just (toValue (weekDay 6));

    dict "January" = Just (toValue (isMonth 1));
    dict "February" = Just (toValue (isMonth 2));
    dict "March" = Just (toValue (isMonth 3));
    dict "April" = Just (toValue (isMonth 4));
    dict "May" = Just (toValue (isMonth 5));
    dict "June" = Just (toValue (isMonth 6));
    dict "July" = Just (toValue (isMonth 7));
    dict "August" = Just (toValue (isMonth 8));
    dict "September" = Just (toValue (isMonth 9));
    dict "October" = Just (toValue (isMonth 10));
    dict "November" = Just (toValue (isMonth 11));
    dict "December" = Just (toValue (isMonth 12));

    dict "Easter" = Just (toValue (dayEachYear gregorianEaster));

    dict s = Nothing;
}
