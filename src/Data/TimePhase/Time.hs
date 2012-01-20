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

    type TimePhase = Phase T;

    fromTo :: TimePhase -> TimePhase -> Intervals T;
    fromTo pa pb = let {?first = firstTime} in
        intervalsFromTo (phaseStartOf pa) (phaseEndOf pb);

    onAfter :: TimePhase -> Intervals T;
    onAfter phase = let {?first = firstTime} in
        intervalsOnAfter (phaseStartOf phase);

    nthIn :: Int -> TimePhase -> TimePhase -> TimePhase;
    nthIn n psubject pdelimiter = let {?first = firstTime} in phaseIntersect psubject
        (toPhaseSet (fmap ((==) (Just n)) (sfCountSince (phaseStartOf pdelimiter) (phaseStartOf psubject))));

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
        pointsMember = \t -> (localTimeOfDay t == midnight) && (pointsMember psday (localDay t)),
        pointsFirstAfterUntil = \t limit -> do
        {
            day <- pointsFirstAfterUntil psday (localDay t) (localDay limit);
            return (midnightOf day);
        },
        pointsLastBeforeUntil = \t limit -> do
        {
            let 
            {
                today = localDay t;
                limitday = if localTimeOfDay limit == midnight then localDay limit else addDays 1 (localDay limit);
            };
            day <- if (localTimeOfDay t > midnight) && (pointsMember psday today)
             then return today
             else pointsLastBeforeUntil psday today limitday;
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
        sfValue = \t -> pointsMember psday (localDay t),
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
    
    dayOfMonth :: Int -> PointSet Day;
    dayOfMonth dd = pointsSearch (\day -> case toGregorian day of
    {
        (y,m,d) -> y * 12 + (fromIntegral (m - 1));
    }) (\i -> let
    {
        y = div i 12;
        m = (fromIntegral (mod i 12)) + 1;
    } in fromGregorianValid y m dd);

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
    dayEachYear f = knownToPointSet (kpsEach yearOfDay f);

    maybeDayEachYear :: (Integer -> Maybe Day) -> PointSet Day;
    maybeDayEachYear = pointsSearch yearOfDay;
    
    timeOfDay :: TimeOfDay -> PointSet T;
    timeOfDay tod = knownToPointSet (kpsEach (\t -> localDay t) (\day -> LocalTime day tod));
}
