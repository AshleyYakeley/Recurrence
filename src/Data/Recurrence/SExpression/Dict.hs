module Data.Recurrence.SExpression.Dict (stdDict,T) where
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

    stdDict :: (?now :: T) => String -> Maybe Value;

    stdDict "never" = Just (toValue (recNever :: Recurrence));
    stdDict "always" = Just (toValue (recAlways :: Recurrence));
    stdDict "not" = Just (toValue (recInvert :: Recurrence -> M Recurrence));
    stdDict "between" = Just (toValue recBetween);
    stdDict "except" = Just (toValue (recDiff :: Recurrence -> Recurrence -> M Recurrence));
    stdDict "when" = Just (toValue recIntersectAll);
    stdDict "and" = Just (toValue recUnionAll);
    stdDict "start" = Just (toValue recStart);
    stdDict "end" = Just (toValue recEnd);
    stdDict "from-until" = Just (toValue recFromUntil);
    stdDict "from-to" = Just (toValue recFromTo);
    stdDict "from" = Just (toValue recFrom);
    stdDict "until" = Just (toValue (invert . recFrom));
    stdDict "nth-in" = Just (toValue recNthIn);
    stdDict "nth-from" = Just (toValue recNthFrom);
    stdDict "nth-last-in" = Just (toValue recNthLastIn);
    stdDict "nth-last-until" = Just (toValue recNthLastUntil);
    stdDict "of" = Just (toValue recOf);
    stdDict "all" = Just (toValue (id :: PieceSet T -> PieceSet T));

    stdDict "delay" = Just (toValue (delay :: NominalDiffTime -> Recurrence -> Recurrence));
    stdDict "advance" = Just (toValue (advance :: NominalDiffTime -> Recurrence -> Recurrence));

    stdDict "now" = Just (toValue (single ?now :: PointSet T));

    stdDict "midnight" = Just (toValue (isTimeOfDay midnight));
    stdDict "midday" = Just (toValue (isTimeOfDay midday));
    stdDict "day" = Just (toValue aDay);
    stdDict "month" = Just (toValue aMonth);
    stdDict "year" = Just (toValue aYear);

    stdDict "week" = Just (toValue aWeek);
    stdDict "weekday" = Just (toValue recWeekday);
    stdDict "weekend" = Just (toValue recWeekend);
    stdDict "working-week" = Just (toValue recWorkingWeek);
    stdDict "Sunday" = Just (toValue (isDayOfWeek 1));
    stdDict "Monday" = Just (toValue (isDayOfWeek 2));
    stdDict "Tuesday" = Just (toValue (isDayOfWeek 3));
    stdDict "Wednesday" = Just (toValue (isDayOfWeek 4));
    stdDict "Thursday" = Just (toValue (isDayOfWeek 5));
    stdDict "Friday" = Just (toValue (isDayOfWeek 6));
    stdDict "Saturday" = Just (toValue (isDayOfWeek 7));

    stdDict "January" = Just (toValue (isMonthOfYear 1));
    stdDict "February" = Just (toValue (isMonthOfYear 2));
    stdDict "March" = Just (toValue (isMonthOfYear 3));
    stdDict "April" = Just (toValue (isMonthOfYear 4));
    stdDict "May" = Just (toValue (isMonthOfYear 5));
    stdDict "June" = Just (toValue (isMonthOfYear 6));
    stdDict "July" = Just (toValue (isMonthOfYear 7));
    stdDict "August" = Just (toValue (isMonthOfYear 8));
    stdDict "September" = Just (toValue (isMonthOfYear 9));
    stdDict "October" = Just (toValue (isMonthOfYear 10));
    stdDict "November" = Just (toValue (isMonthOfYear 11));
    stdDict "December" = Just (toValue (isMonthOfYear 12));

    stdDict "Easter" = Just (toValue (dayEachYear (Just . gregorianEaster)));

    stdDict _s = Nothing;
}
