module Data.TimePhase.Time where
{
    import Data.Fixed;
    import Data.Time;
    import Data.SetSearch;
    
    type T = LocalTime;

    getNow :: IO T;
    getNow = fmap zonedTimeToLocalTime getZonedTime;

    firstTime :: T;
    firstTime = LocalTime
    {
        localDay = ModifiedJulianDay 0,
        localTimeOfDay = midnight
    };

    type TimePhase = PhaseSet T;

    startOf :: TimePhase -> PointSet T;
    startOf ps = union (psExceptions ps) (intervalsStartOf (psIntervals ps));

    endOf :: TimePhase -> PointSet T;
    endOf ps = union (psExceptions ps) (intervalsEndOf (psIntervals ps));

    to :: TimePhase -> TimePhase -> Intervals T;
    to pa pb = let {?first = firstTime} in
        intervalsFromTo (startOf pa) (endOf pb);

    midnights :: KnownPointSet T;
    midnights = MkKnownPointSet
    {
        kpsMember = \t -> (localTimeOfDay t == midnight),
        kpsFirstAfter = \t -> let
        {
            t' = LocalTime
            {
                localDay = addDays 1 (localDay t),
                localTimeOfDay = midnight
            };
        } in Just t',
        kpsLastBefore = \t -> let
        {
            today = localDay t;
            t' = LocalTime
            {
                localDay = if localTimeOfDay t > midnight
                 then today
                 else addDays (-1) today,
                localTimeOfDay = midnight
            };
        } in Just t'
    };

    newDay :: PointSet T;
    newDay = knownToPointSet midnights;
    
    midnightOf :: Day -> LocalTime;
    midnightOf day = LocalTime
    {
        localDay = day,
        localTimeOfDay = midnight
    };
    
    midnightsBefore :: PointSet Day -> PointSet T;
    midnightsBefore psday = MkPointSet
    {
        ssMember = \t -> (localTimeOfDay t == midnight) && (ssMember psday (localDay t)),
        ssFirstAfterUntil = \t limit -> do
        {
            day <- ssFirstAfterUntil psday (localDay t) (localDay limit);
            return (midnightOf day);
        },
        ssLastBeforeUntil = \t limit -> do
        {
            let 
            {
                today = localDay t;
                limitday = if localTimeOfDay limit == midnight then localDay limit else addDays 1 (localDay limit);
            };
            day <- if (localTimeOfDay t > midnight) && (ssMember psday today)
             then return today
             else ssLastBeforeUntil psday today limitday;
            return (midnightOf day);
        }
    };

    theDay :: StepFunction T Day;
    theDay = MkStepFunction
    {
        sfValue = localDay,
        sfPossibleChanges = knownToPointSet midnights
    };

    daysToTimeIntervals :: PointSet Day -> Intervals T;
    daysToTimeIntervals psday = MkStepFunction
    {
        sfValue = \t -> ssMember psday (localDay t),
        sfPossibleChanges = union
            (midnightsBefore psday)
            (midnightsBefore (delay 1 psday))
    };
    
    weekDay :: Integer -> Intervals T;
    weekDay i = fmap (\day -> mod' (toModifiedJulianDay day) 7 == i) theDay;
    
    monthFirsts :: PointSet Day;
    monthFirsts = knownToPointSet MkKnownPointSet
    {
        kpsMember = \day -> case toGregorian day of
        {
            (_,_,1) -> True;
            _ -> False;
        },
        kpsFirstAfter = \day -> Just (case toGregorian (addGregorianMonthsClip 1 day) of
        {
            (y,m,_) -> fromGregorian y m 1;
        }),
        kpsLastBefore = \day -> Just (case toGregorian (addDays (-1) day) of
        {
            (y,m,_) -> fromGregorian y m 1;
        })
    };

    newMonth :: PointSet T;
    newMonth = midnightsBefore monthFirsts;

    theYearAndMonth :: StepFunction T (Integer,Int);
    theYearAndMonth = MkStepFunction
    {
        sfValue = (\day -> case toGregorian day of
        {
            (y,m,_) -> (y,m);
        }) . localDay,
        sfPossibleChanges = newMonth
    };

    isMonth :: Int -> Intervals T;
    isMonth i = fmap (\(_,m) -> i == m) theYearAndMonth;
    
    yearFirsts :: PointSet Day;
    yearFirsts = knownToPointSet MkKnownPointSet
    {
        kpsMember = \day -> case toGregorian day of
        {
            (_,1,1) -> True;
            _ -> False;
        },
        kpsFirstAfter = \day -> Just (case toGregorian (addGregorianYearsClip 1 day) of
        {
            (y,_,_) -> fromGregorian y 1 1;
        }),
        kpsLastBefore = \day -> Just (case toGregorian (addDays (-1) day) of
        {
            (y,_,_) -> fromGregorian y 1 1;
        })
    };

    newYear :: PointSet T;
    newYear = midnightsBefore yearFirsts;

    theYear :: StepFunction T Integer;
    theYear = MkStepFunction
    {
        sfValue = (\day -> case toGregorian day of
        {
            (y,_,_) -> y;
        }) . localDay,
        sfPossibleChanges = newYear
    };

    isYear :: Integer -> Intervals T;
    isYear y = fmap ((==) y) theYear;

    yearOfDay :: Day -> Integer;
    yearOfDay day = case toGregorian day of
    {
        (y,_,_) -> y;
    };

    dayEachYear :: (Integer -> Day) -> PointSet Day;
    dayEachYear f = knownToPointSet MkKnownPointSet
    {
        kpsMember = \day -> day == f (yearOfDay day),
        kpsFirstAfter = \day -> Just (let
        {
            thisOne = f (yearOfDay day);
            nextOne= f ((yearOfDay day) + 1);
        } in if day < thisOne then thisOne else nextOne
        ),
        kpsLastBefore = \day -> Just (let
        {
            thisOne = f (yearOfDay day);
            prevOne = f ((yearOfDay day) - 1);
        } in if day > thisOne then thisOne else prevOne
        )
    };

    maybeDayEachYear :: (Integer -> Maybe Day) -> PointSet Day;
    maybeDayEachYear f = MkPointSet
    {
        ssMember = \day -> (Just day) == f (yearOfDay day),
        ssFirstAfterUntil = \day limit -> let
        {
            yday = yearOfDay day;
            ylimit = yearOfDay limit;
            maxOffset = ylimit - yday;
            findN i | i > maxOffset = Nothing;
            findN i = case f (yday + i) of
            {
                Just found | found > limit -> Nothing;
                Just found | found > day -> Just found;
                _ -> findN (i + 1);
            };
        } in findN 0,
        ssLastBeforeUntil = \day limit -> let
        {
            yday = yearOfDay day;
            ylimit = yearOfDay limit;
            maxOffset = yday - ylimit;
            findN i | i > maxOffset = Nothing;
            findN i = case f (yday - i) of
            {
                Just found | found < limit -> Nothing;
                Just found | found < day -> Just found;
                _ -> findN (i + 1);
            };
        } in findN 0
    };
}
