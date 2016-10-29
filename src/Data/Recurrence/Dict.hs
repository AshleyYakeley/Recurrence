module Data.Recurrence.Dict (dict,T) where
{
    import Data.Time;
    import Data.Time.Calendar.Easter;
    import Data.SetSearch;
    import Data.Recurrence.Time;
    import Data.Recurrence.Value;
    import Data.Recurrence.Day;
    import Data.Recurrence.Gregorian;

    phaseIntersectAll :: Maybe (TimePhase,[TimePhase]) -> TimePhase;
    phaseIntersectAll Nothing = tpAlways;
    phaseIntersectAll (Just (phase,sets)) = f sets where
    {
        f [] = phase;
        f (s:ss) = tpIntersect (f ss) s;
    };

    phaseUnionAll :: Maybe (TimePhase,[TimePhase]) -> M TimePhase;
    phaseUnionAll Nothing = return tpNever;
    phaseUnionAll (Just (phase,sets)) = f sets where
    {
        f [] = return phase;
        f (s:ss) = do
        {
            r <- f ss;
            tpUnion s r;
        };
    };

    dict :: (?now :: T) => String -> Maybe Value;

    dict "never" = Just (toValue (tpNever :: TimePhase));
    dict "always" = Just (toValue (tpAlways :: TimePhase));
    dict "not" = Just (toValue (tpInvert :: TimePhase -> M TimePhase));
    dict "between" = Just (toValue tpBetween);
    dict "except" = Just (toValue (tpDiff :: TimePhase -> TimePhase -> M TimePhase));
    dict "when" = Just (toValue phaseIntersectAll);
    dict "and" = Just (toValue phaseUnionAll);
    dict "start" = Just (toValue startOf);
    dict "end" = Just (toValue endOf);
    dict "from-until" = Just (toValue fromUntil);
    dict "through" = Just (toValue fromTo);
    dict "from" = Just (toValue onAfter);
    dict "until" = Just (toValue (invert . onAfter));
    dict "nth" = Just (toValue nthIn);
    dict "of" = Just (toValue ofPhase);
    dict "all" = Just (toValue (id :: PieceSet T -> PieceSet T));

    dict "delay" = Just (toValue (delay :: NominalDiffTime -> TimePhase -> TimePhase));

    dict "now" = Just (toValue (single ?now :: PointSet T));

    dict "midnight" = Just (toValue (isTimeOfDay midnight));
    dict "midday" = Just (toValue (isTimeOfDay midday));
    dict "day" = Just (toValue aDay);
    dict "month" = Just (toValue aMonth);
    dict "year" = Just (toValue aYear);

    dict "Sunday" = Just (toValue (isDayOfWeek 1));
    dict "Monday" = Just (toValue (isDayOfWeek 2));
    dict "Tuesday" = Just (toValue (isDayOfWeek 3));
    dict "Wednesday" = Just (toValue (isDayOfWeek 4));
    dict "Thursday" = Just (toValue (isDayOfWeek 5));
    dict "Friday" = Just (toValue (isDayOfWeek 6));
    dict "Saturday" = Just (toValue (isDayOfWeek 7));

    dict "January" = Just (toValue (isMonthOfYear 1));
    dict "February" = Just (toValue (isMonthOfYear 2));
    dict "March" = Just (toValue (isMonthOfYear 3));
    dict "April" = Just (toValue (isMonthOfYear 4));
    dict "May" = Just (toValue (isMonthOfYear 5));
    dict "June" = Just (toValue (isMonthOfYear 6));
    dict "July" = Just (toValue (isMonthOfYear 7));
    dict "August" = Just (toValue (isMonthOfYear 8));
    dict "September" = Just (toValue (isMonthOfYear 9));
    dict "October" = Just (toValue (isMonthOfYear 10));
    dict "November" = Just (toValue (isMonthOfYear 11));
    dict "December" = Just (toValue (isMonthOfYear 12));

    dict "Easter" = Just (toValue (dayEachYear (Just . gregorianEaster)));

    dict _s = Nothing;
}
