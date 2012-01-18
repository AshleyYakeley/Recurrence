module Data.TimePhase.Dict (dict) where
{
    import Data.Fixed;
    import Data.Time;
    import Data.Time.Calendar.Easter;
    import Data.SetSearch;
    import Data.TimePhase.Time;
    import Data.TimePhase.Value;

    dict :: String -> Maybe Value;

    dict "never" = Just (toValue (empty :: TimePhase));
    dict "always" = Just (toValue (full :: TimePhase));
    dict "not" = Just (toValue (invert :: TimePhase -> TimePhase));
    dict "when" = Just (toValue (intersectAll :: [TimePhase] -> TimePhase));
    dict "and" = Just (toValue (unionAll :: [TimePhase] -> TimePhase));
    dict "start" = Just (toValue startOf);
    dict "end" = Just (toValue endOf);
    dict "interval" = Just (toValue fromTo);
    dict "from" = Just (toValue onAfter);
    dict "until" = Just (toValue (invert . onAfter));
    
    dict "delay" = Just (toValue (delay :: NominalDiffTime -> TimePhase -> TimePhase));

    dict "midnight" = Just (toValue newDay);
    dict "new-day" = Just (toValue newDay);
    dict "new-month" = Just (toValue newMonth);
    dict "new-year" = Just (toValue newYear);

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
