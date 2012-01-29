module Data.TimePhase.Time where
{
    import Data.Fixed;
    import Data.Time;
    import Data.SetSearch;
    
    type T = LocalTime;

    getNow :: IO T;
    getNow = fmap zonedTimeToLocalTime getZonedTime;

    firstTime :: Cut T;
    firstTime = justBefore (LocalTime
    {
        localDay = ModifiedJulianDay (-10000000),
        localTimeOfDay = midnight
    });

    lastTime :: Cut T;
    lastTime = justBefore (LocalTime
    {
        localDay = ModifiedJulianDay 10000000,
        localTimeOfDay = midnight
    });

    type TimePhase = Phase T;

    fromTo :: TimePhase -> TimePhase -> Intervals T;
    fromTo pa pb = let {?first=firstTime;?last=lastTime} in
        intervalsFromTo (phaseStartOf pa) (phaseEndOf pb);

    fromUntil :: TimePhase -> TimePhase -> Intervals T;
    fromUntil pa pb = let {?first=firstTime;?last=lastTime} in
        intervalsFromTo (phaseStartOf pa) (phaseStartOf pb);

    onAfter :: TimePhase -> Intervals T;
    onAfter phase = let {?first = firstTime} in
        intervalsAfter (phaseStartOf phase);

    nthIn :: Int -> TimePhase -> TimePhase -> TimePhase;
    nthIn n psubject pdelimiter = let {?first = firstTime} in phaseIntersect psubject
        (fmap ((==) (Just n)) (sfCountSince (phaseStartOf pdelimiter) (phaseStartOf psubject)));

    ofPhase :: TimePhase -> TimePhase -> TimePhase;
    ofPhase picker phase = let 
    {
        ?first = firstTime;
        ?last = lastTime;
    } in phaseOf phase (pointsFromCut (phaseStartOf picker));

    startOf :: TimePhase -> PointSet T;
    startOf phase = pointsFromCut (phaseStartOf phase);

    endOf :: TimePhase -> PointSet T;
    endOf phase = let {?last=lastTime} in
     pointsFromCut (phaseEndOf phase);

    beforeDay :: PointSet (Cut T);
    beforeDay = pointsCutBefore (knownToPointSet (MkKnownPointSet
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
    }));

    dayPhase :: TimePhase;
    dayPhase = phaseDivideBy beforeDay;
    
    dayMidnight :: Day -> T;
    dayMidnight day = LocalTime
    {
        localDay = day,
        localTimeOfDay = midnight
    };
    
    cutBeforeDay :: Day -> Cut T;
    cutBeforeDay day = justBefore (dayMidnight day);
   
    beforeDays :: PointSet Day -> PointSet (Cut T);
    beforeDays (MkPointSet vf) = MkPointSet (\p q -> let
    {
        pday = case p of
        {
            MkCut pt Before | localTimeOfDay pt == midnight -> localDay pt;
            MkCut pt _ -> addAffine 1 (localDay pt);
        };
        qday = case q of
        {
            MkCut qt _ -> localDay qt;
        };
    } in fmap cutBeforeDay (vf pday qday));
    
{-    
    beforeDays psday = pointsCutBefore (MkPointSet
    {
        pointsMember = \t -> (localTimeOfDay t == midnight) && (pointsMember psday (localDay t)),
        pointsFirstAfterUntil = \t limit -> do
        {
            day <- pointsFirstAfterUntil psday (localDay t) (localDay limit);
            return (dayMidnight day);
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
            return (dayMidnight day);
        }
    });
-}
    theDay :: StepFunction T Day;
    theDay = MkStepFunction
    {
        sfUpwardValue = \(MkCut day _) -> localDay day,
        sfPossibleChanges = beforeDay
    };

    daysToTimeIntervals :: PointSet Day -> Intervals T;
    daysToTimeIntervals psday = let {?first=firstTime} in
     intervalsFromTo (beforeDays psday) (beforeDays (delay 1 psday));
{-    
    MkStepFunction
    {
        sfUpwardValue = \(MkCut t _) -> pointsMember psday (localDay t),
        sfPossibleChanges = union
            (beforeDays psday)
            (beforeDays (delay 1 psday))
    };
-}    
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

    beforeMonth :: PointSet (Cut T);
    beforeMonth = beforeDays monthFirsts;

    monthPhase :: TimePhase;
    monthPhase = phaseDivideBy beforeMonth;

    theYearAndMonth :: StepFunction T (Integer,Int);
    theYearAndMonth = MkStepFunction
    {
        sfUpwardValue = \(MkCut t _) -> case toGregorian (localDay t) of
        {
            (y,m,_) -> (y,m);
        },
        sfPossibleChanges = beforeMonth
    };

    isYearAndMonth :: Integer -> Int -> Intervals T;
    isYearAndMonth year month = let
    {
        first = fromGregorian year month 1;
    } in intervalsOne (cutBeforeDay first) (cutBeforeDay (addGregorianMonthsRollOver 1 first));

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

    firstDayOfYear :: Integer -> Day;
    firstDayOfYear year = fromGregorian year 1 1;

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
            (y,_,_) -> firstDayOfYear y;
        }),
        kpsLastBefore = \day -> Just (case toGregorian (addDays (-1) day) of
        {
            (y,_,_) -> firstDayOfYear y;
        })
    };

    beforeYear :: PointSet (Cut T);
    beforeYear = beforeDays yearFirsts;

    yearPhase :: TimePhase;
    yearPhase = phaseDivideBy beforeYear;

    theYear :: StepFunction T Integer;
    theYear = MkStepFunction
    {
        sfUpwardValue = \(MkCut t _) -> case toGregorian (localDay t) of
        {
            (y,_,_) -> y;
        },
        sfPossibleChanges = beforeYear
    };

    isYear :: Integer -> Intervals T;
    isYear year = intervalsOne (cutBeforeDay (firstDayOfYear year)) (cutBeforeDay (firstDayOfYear (year + 1)));
    
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
