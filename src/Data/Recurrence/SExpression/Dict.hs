module Data.Recurrence.SExpression.Dict (dict,T) where
{
    import Data.Time;
    import Data.Time.Calendar.Easter;
    import Data.SetSearch;
    import Data.Recurrence.Time;
    import Data.Recurrence.SExpression.Value;

    recIntersectAll :: Maybe (Recurrence,[Recurrence]) -> Recurrence;
    recIntersectAll Nothing = recAlways;
    recIntersectAll (Just (rc,sets)) = f sets where
    {
        f [] = rc;
        f (s:ss) = recIntersect (f ss) s;
    };

    recUnionAll :: Maybe (Recurrence,[Recurrence]) -> M Recurrence;
    recUnionAll Nothing = return recNever;
    recUnionAll (Just (rc,sets)) = f sets where
    {
        f [] = return rc;
        f (s:ss) = do
        {
            r <- f ss;
            recUnion s r;
        };
    };

    recWeekday :: PiecePartialFunction T Day;
    recWeekday = let
    {
        ff :: Day -> Maybe Day;
        ff (ModifiedJulianDay mjd) | mod mjd 7 == 3 = Nothing;
        ff (ModifiedJulianDay mjd) | mod mjd 7 == 4 = Nothing;
        ff d = Just d;
    } in fmap ff theDay;

    recWorkingWeek :: PieceSet T;
    recWorkingWeek = piecePartialToSet recWeekday;

    recWeekend :: PieceSet T;
    recWeekend = let
    {
        ff :: Day -> Maybe ();
        ff (ModifiedJulianDay mjd) | mod mjd 7 == 3 = Just ();
        ff (ModifiedJulianDay mjd) | mod mjd 7 == 4 = Just ();
        ff _ = Nothing
    } in fmap ff theDay;

    dict :: (?now :: T) => String -> Maybe Value;

    dict "never" = Just (toValue (recNever :: Recurrence));
    dict "always" = Just (toValue (recAlways :: Recurrence));
    dict "not" = Just (toValue (recInvert :: Recurrence -> M Recurrence));
    dict "between" = Just (toValue recBetween);
    dict "except" = Just (toValue (recDiff :: Recurrence -> Recurrence -> M Recurrence));
    dict "when" = Just (toValue recIntersectAll);
    dict "and" = Just (toValue recUnionAll);
    dict "start" = Just (toValue recStart);
    dict "end" = Just (toValue recEnd);
    dict "from-until" = Just (toValue recFromUntil);
    dict "from-to" = Just (toValue recFromTo);
    dict "from" = Just (toValue recFrom);
    dict "until" = Just (toValue (invert . recFrom));
    dict "nth-in" = Just (toValue recNthIn);
    dict "nth-from" = Just (toValue recNthFrom);
    dict "of" = Just (toValue recOf);
    dict "all" = Just (toValue (id :: PieceSet T -> PieceSet T));

    dict "delay" = Just (toValue (delay :: NominalDiffTime -> Recurrence -> Recurrence));
    dict "advance" = Just (toValue (advance :: NominalDiffTime -> Recurrence -> Recurrence));

    dict "now" = Just (toValue (single ?now :: PointSet T));

    dict "midnight" = Just (toValue (isTimeOfDay midnight));
    dict "midday" = Just (toValue (isTimeOfDay midday));
    dict "day" = Just (toValue aDay);
    dict "month" = Just (toValue aMonth);
    dict "year" = Just (toValue aYear);

    dict "week" = Just (toValue aWeek);
    dict "weekday" = Just (toValue recWeekday);
    dict "weekend" = Just (toValue recWeekend);
    dict "working-week" = Just (toValue recWorkingWeek);
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
