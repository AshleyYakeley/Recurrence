module Data.TimePhase.Dict (dict,T) where
{
    import Data.Time;
    import Data.Time.Calendar.Easter;
    import Data.SetSearch;
    import Data.TimePhase.Time;
    import Data.TimePhase.Value;

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

    dict "midnight" = Just (toValue (timeOfDay midnight));
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

    dict _s = Nothing;
}
