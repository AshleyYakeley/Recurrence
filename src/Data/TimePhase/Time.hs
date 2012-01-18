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
    
    midnightsBefore :: KnownPointSet Day -> KnownPointSet T;
    midnightsBefore kpsday = MkKnownPointSet
    {
        kpsMember = \t -> (localTimeOfDay t == midnight) && (kpsMember kpsday (localDay t)),
        kpsFirstAfter = \t -> do
        {
            day <- kpsFirstAfter kpsday (localDay t);
            return (midnightOf day);
        },
        kpsLastBefore = \t -> do
        {
            let {today = localDay t};
            day <- if (localTimeOfDay t > midnight) && (kpsMember kpsday today)
             then return today
             else kpsLastBefore kpsday today;
            return (midnightOf day);
        }
    };

    theDay :: StepFunction T Day;
    theDay = MkStepFunction
    {
        sfValue = localDay,
        sfPossibleChanges = knownToPointSet midnights
    };

    specialDays :: KnownPointSet Day -> Intervals T;
    specialDays kpsday = MkStepFunction
    {
        sfValue = \t -> kpsMember kpsday (localDay t),
        sfPossibleChanges = union
            (knownToPointSet (midnightsBefore kpsday))
            (knownToPointSet (midnightsBefore (delay 1 kpsday)))
    };
    
    weekDay :: Integer -> Intervals T;
    weekDay i = fmap (\day -> mod' (toModifiedJulianDay day) 7 == i) theDay;
    
    monthFirsts :: KnownPointSet Day;
    monthFirsts = MkKnownPointSet
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
    newMonth = knownToPointSet (midnightsBefore monthFirsts);

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
    
    yearFirsts :: KnownPointSet Day;
    yearFirsts = MkKnownPointSet
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
    newYear = knownToPointSet (midnightsBefore yearFirsts);

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

    dayEachYear :: (Integer -> Day) -> KnownPointSet Day;
    dayEachYear yday = MkKnownPointSet
    {
        kpsMember = \day -> day == yday (yearOfDay day),
        kpsFirstAfter = \day -> Just (let
        {
            thisOne = yday (yearOfDay day);
            nextOne= yday ((yearOfDay day) + 1);
        } in if day < thisOne then thisOne else nextOne
        ),
        kpsLastBefore = \day -> Just (let
        {
            thisOne = yday (yearOfDay day);
            prevOne = yday ((yearOfDay day) - 1);
        } in if day > thisOne then thisOne else prevOne
        )
    };
}
