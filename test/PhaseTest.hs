{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, ImplicitParams #-}
module Main where
{
    import Data.SetSearch;
    import Data.Time;
    import Data.TimePhase;
    import Data.TimePhase.Time;
    
    psMidnight :: PointSet T;
    psMidnight = timeOfDay midnight;
    
    midnights :: Phase T;
    midnights = toPhase psMidnight;

    check :: (Show a,Eq a) => String -> a -> a -> IO ();
    check _ expected found | expected == found = return ();
    check text expected found = putStrLn (text ++ ": expected " ++ (show expected) ++ ", found " ++ (show found));

    t0 :: T;
    t0 = dayMidnight (ModifiedJulianDay 0);

    cb0 :: Cut T;
    cb0 = justBefore t0;

    ca0 :: Cut T;
    ca0 = justAfter t0;

    t005 :: T;
    t005 = LocalTime (ModifiedJulianDay 0) (TimeOfDay 0 30 0);

    cb005 :: Cut T;
    cb005 = justBefore t005;

    t01 :: T;
    t01 = LocalTime (ModifiedJulianDay 0) (TimeOfDay 1 0 0);

    cb01 :: Cut T;
    cb01 = justBefore t01;

    ca01 :: Cut T;
    ca01 = justAfter t01;

    t05 :: T;
    t05 = LocalTime (ModifiedJulianDay 0) midday;

    t1 :: T;
    t1 = dayMidnight (ModifiedJulianDay 1);

    cb1 :: Cut T;
    cb1 = justBefore t1;

    t2 :: T;
    t2 = dayMidnight (ModifiedJulianDay 2);

    cb2 :: Cut T;
    cb2 = justBefore t2;

    tdmid :: Integer -> T;
    tdmid i = LocalTime (ModifiedJulianDay i) midnight;

    td30th :: T;
    td30th = tdmid 43;

    td31st :: T;
    td31st = tdmid 44;

    td1st :: T;
    td1st = tdmid 45;

    td2nd :: T;
    td2nd = tdmid 46;

    td31stNext :: T;
    td31stNext = tdmid 75;

    td300 :: T;
    td300 = tdmid 300;

    pointInterval p = MkInterval (Starts (justBefore p)) (Ends (justAfter p));

    jbInterval t0 t1 = MkInterval (Starts (justBefore t0)) (Ends (justBefore t1));

    oneDayInterval i = jbInterval (tdmid i) (tdmid (i + 1));

    twoDays :: Intervals T;
    twoDays = let {?first = firstTime} in intervalsFromTo (single cb0) (single cb2);

    psAM1 = timeOfDay (TimeOfDay 1 0 0);

    am1 = toPhase psAM1;

    firstHour :: Intervals T;
    firstHour = fromUntil midnights am1;

    firstHourAnd :: Intervals T;
    firstHourAnd = fromTo midnights am1;

    main :: IO ();
    main = let {?first=firstTime;?last=lastTime} in do
    {
        check "is points" True (member psMidnight t0);
        check "is points union" True (member (union psMidnight psMidnight) t0);
        check "is points union empty 1" True (member (union psMidnight empty) t0);
        check "is points union empty 2" True (member (union empty psMidnight) t0);
        check "is phase" True (member (phaseSet midnights) t0);
        check "is startof" True (member (startOf midnights) t0);
        check "is phaseStartOf" True (member (phaseStartOf midnights) cb0);

        check "firstHour 00:00 13" False (vsNonEmpty (psValues psAM1 t0 t0));
        check "firstHour 00:00 12" False (vsNonEmpty (psValuesCut psAM1 (justBefore t0) (justAfter t0)));
        check "firstHour 00:00 11" False (psNonEmpty psAM1 (justBefore t0) (justAfter t0));
        check "firstHour 00:00 10" True (not (psNonEmpty psAM1 (justBefore t0) (justAfter t0)));

        check "firstHour 00:00 9" (Just t0) (vsLast (psValues (timeOfDay midnight) ((\(MkCut x _) -> x) ?first) t0));
        check "firstHour 00:00 8" (Just t0) (vsLast (psValues psMidnight ((\(MkCut x _) -> x) ?first) t0));
        check "firstHour 00:00 7" (Just t0) (vsLast (psValuesCut psMidnight ?first ca0));
        check "firstHour 00:00 6" (Just t0) (psPrevious psMidnight ca0);
        
        check "firstHour 00:00 5" True (psOnAndOff psMidnight psAM1 t0);
        check "firstHour 00:00 4" True (let {?first = justBefore ?first} in psOnAndOff (phaseStartOf midnights) (phaseEndOf am1) cb0);
        check "firstHour 00:00 3" True (sfUpwardValue (intervalsFromTo (phaseStartOf midnights) (phaseEndOf am1)) cb0);
        check "firstHour 00:00 2" True (sfValue (intervalsFromTo (phaseStartOf midnights) (phaseEndOf am1)) t0);
        check "firstHour 00:00 1" True (member (intervalsFromTo (phaseStartOf midnights) (phaseEndOf am1)) t0);
        check "firstHour 00:00" True (member firstHour t0);
        
        check "firstHour 00:00 change before 2" True (member (sfPossibleChanges (intervalsFromTo (phaseStartOf midnights) (phaseEndOf am1))) cb0);
        check "firstHour 00:00 change before 1" True (member (sfChanges (intervalsFromTo (phaseStartOf midnights) (phaseEndOf am1))) cb0);
        check "firstHour 00:00 change before" True (member (sfChanges firstHour) cb0);
        check "firstHour 00:00 change after" False (member (sfChanges firstHour) ca0);
        check "firstHour 00:30" True (member firstHour t005);
        check "firstHour 00:30 change before" False (member (sfChanges firstHour) (justBefore t005));
        check "firstHour 00:30 change after" False (member (sfChanges firstHour) (justAfter t005));

        check "firstHour 01:00" False (member firstHour t01);
        check "firstHour 01:00 change before" True (member (sfChanges firstHour) cb01);
        check "firstHour 01:00 change after" False (member (sfChanges firstHour) ca01);

        check "firstHourAnd 01:00" True (member firstHourAnd t01);
        check "firstHourAnd 01:00 change before" False (member (sfChanges firstHourAnd) cb01);
        check "firstHourAnd 01:00 change after" True (member (sfChanges firstHourAnd) ca01);
        
        check "change end firstHour" (Just cb01) (firstAfterUntil (sfChanges firstHour) cb005 cb2);


        check "endOf1am 4" False (member (intervalsEndOf (phaseSet am1)) cb01);
        check "endOf1am 2" False (member (phaseEndOf am1) cb01);
        check "endOf1am 1" True (member (phaseEndOf am1) ca01);
        check "endOf1am" (Just ca01) (firstAfterUntil (phaseEndOf am1) cb0 cb2);
        check "endOfFirstHour" (Just cb01) (firstAfterUntil (phaseEndOf (toPhase firstHour)) cb0 cb2);
        check "endOfFirstHourAnd" (Just ca01) (firstAfterUntil (phaseEndOf (toPhase firstHourAnd)) cb0 cb2);

        check "next t0 False" (Just (pointInterval t0)) (cutNextInterval midnights (justBefore t0) (justBefore t2));
        check "next t0 True" (Just (pointInterval t1)) (cutNextInterval midnights (justAfter t0) (justBefore t2));
        check "next t05 False" (Just (pointInterval t1)) (cutNextInterval midnights (justBefore t05) (justBefore t2));
        check "next t05 True" (Just (pointInterval t1)) (cutNextInterval midnights (justAfter t05) (justBefore t2));
        check "next t1 False" (Just (pointInterval t1)) (cutNextInterval midnights (justBefore t1) (justBefore t2));
        check "next t1 True" (Just (MkInterval (Starts (justBefore t2)) Whenever)) (cutNextInterval midnights (justAfter t1) (justBefore t2));
        check "next t1 True further" (Just (pointInterval t2)) (cutNextInterval midnights (justAfter t1) (justAfter t2));
        
        check "1am" (Just (pointInterval t01)) (cutNextInterval am1 cb0 cb2);
        check "twoDays" (Just (MkInterval (Starts cb0) (Ends cb2))) (cutNextInterval (toPhase twoDays) cb0 cb2);
        check "firstHour" (Just (MkInterval (Starts cb0) (Ends cb01))) (cutNextInterval (toPhase firstHour) cb0 cb2);
        
        check "31st" (Just (justBefore td31st)) (vsFirst (psValues (beforeDays (dayOfMonth 31)) (justBefore t0) (justAfter td300)));

        check "31st int 9" False
            (let {?first = justBefore ?first} in
            let
            {
                psOn = beforeDays (dayOfMonth 31);
                psOff = beforeDays (delay 1 (dayOfMonth 31));
                a = justBefore td30th;
            } in case psPrevious psOn (justAfter a) of
            {
                Nothing -> False;
                Just ontime -> not (psNonEmpty psOff (justBefore ontime) (justAfter a));
            }
            );
        check "31st int 8" False
            (let {?first = justBefore ?first} in
                psOnAndOff
                (beforeDays (dayOfMonth 31)) 
                (beforeDays (delay 1 (dayOfMonth 31)))
                (justBefore td30th)
            );
        check "31st int 7" False
            (sfUpwardValue
                (intervalsFromTo (beforeDays (dayOfMonth 31)) (beforeDays (delay 1 (dayOfMonth 31))))
                (justBefore td30th)
            );
        check "31st int 6" True
            (sfUpwardValue
                (intervalsFromTo (beforeDays (dayOfMonth 31)) (beforeDays (delay 1 (dayOfMonth 31))))
                (justBefore td31st)
            );
        check "31st int 5" (Just (justBefore td31st)) 
            (vsFirst 
                (psValues 
                    (sfPossibleChanges
                        (intervalsFromTo (beforeDays (dayOfMonth 31)) (beforeDays (delay 1 (dayOfMonth 31))) )
                    ) 
                (justBefore t0) (justAfter td300)
            ));
        check "31st int 4" (Just (justBefore td31st)) 
            (vsFirst 
                (psValues 
                    (sfChanges
                        (intervalsFromTo (beforeDays (dayOfMonth 31)) (beforeDays (delay 1 (dayOfMonth 31))) )
                    ) 
                (justBefore t0) (justAfter td300)
            ));
        check "31st int 3" (Just (justBefore td31st)) 
            (vsFirst 
                (psValues 
                    (sfMatchUpwardChanges
                        (intervalsFromTo (beforeDays (dayOfMonth 31)) (beforeDays (delay 1 (dayOfMonth 31))) )
                        id
                    ) 
                (justBefore t0) (justAfter td300)
            ));
        check "31st int 2" (Just (justBefore td31st)) 
            (vsFirst 
                (psValues 
                    (intervalsStartOf 
                        (intervalsFromTo (beforeDays (dayOfMonth 31)) (beforeDays (delay 1 (dayOfMonth 31))) )) 
                        (justBefore t0) (justAfter td300)
            ));
        check "31st int 1" (Just (justBefore td31st)) (vsFirst (psValues (intervalsStartOf (daysToTimeIntervals (dayOfMonth 31))) (justBefore t0) (justAfter td300)));
        check "31st int" (Just (jbInterval td31st td1st)) (cutNextInterval (toPhase (daysToTimeIntervals (dayOfMonth 31))) (justBefore t0) (justAfter td300));




        check "31st next 20" []
            (vsForwards (psValues (pointsSearch (\day -> case toGregorian day of
                {
                    (y,m,d) -> y * 12 + (fromIntegral (m - 1));
                }) (\i -> let
                {
                    y = div i 12;
                    m = (fromIntegral (mod i 12)) + 1;
                } in fromGregorianValid y m 31)) (ModifiedJulianDay 74) (ModifiedJulianDay 74)));
        check "31st next 19" []
            (vsForwards (psValues (dayOfMonth 31) (ModifiedJulianDay 74) (ModifiedJulianDay 74)));
        check "31st next 18" []
            (vsForwards (psValues (delay 1 (single (ModifiedJulianDay 75))) (ModifiedJulianDay 75) (ModifiedJulianDay 75)));
        check "31st next 17" []
            (vsForwards (psValues (delay 1 (dayOfMonth 31)) (ModifiedJulianDay 75) (ModifiedJulianDay 75)));
        check "31st next 16" []
            (
                let
                {
                    psOff = pointsFromCut (beforeDays (delay 1 (dayOfMonth 31)));
                } in
                vsForwards (psValues psOff td31stNext td31stNext)
            );
        check "31st next 15" []
            (
                let
                {
                    psOff = pointsFromCut (beforeDays (delay 1 (dayOfMonth 31)));
                } in
                vsForwards (psValuesCut psOff (justBefore td31stNext) (justAfter td31stNext))
            );
        check "31st next 14" False
            (
                let
                {
                    psOff = pointsFromCut (beforeDays (delay 1 (dayOfMonth 31)));
                } in
                psNonEmpty psOff (justBefore td31stNext) (justAfter td31stNext)
            );
        check "31st next 13" True
            (
                let
                {
                    psOff = pointsFromCut (beforeDays (delay 1 (dayOfMonth 31)));
                } in
                not (psNonEmpty psOff (justBefore td31stNext) (justAfter td31stNext))
            );
        check "31st next 12" True
            (
                let
                {
                    psOff = pointsFromCut (beforeDays (delay 1 (dayOfMonth 31)));
                } in
                case Just td31stNext of
                    {
                        Nothing -> False;
                        Just ontime -> not (psNonEmpty psOff (justBefore ontime) (justAfter td31stNext));
                    }
            );
        check "31st next 11" (Just td31stNext)
            (
                let
                {
                    psOn = pointsFromCut (beforeDays (dayOfMonth 31));
                    psOff = pointsFromCut (beforeDays (delay 1 (dayOfMonth 31)));
                } in
                psPrevious psOn (justAfter td31stNext)
            );
        check "31st next 10" True
            (
                let
                {
                    psOn = pointsFromCut (beforeDays (dayOfMonth 31));
                    psOff = pointsFromCut (beforeDays (delay 1 (dayOfMonth 31)));
                } in
                case psPrevious psOn (justAfter td31stNext) of
                    {
                        Nothing -> False;
                        Just ontime -> not (psNonEmpty psOff (justBefore ontime) (justAfter td31stNext));
                    }
            );
        check "31st next 9" True
            (
                (psOnAndOff
                    (pointsFromCut (beforeDays (dayOfMonth 31))) (pointsFromCut (beforeDays (delay 1 (dayOfMonth 31))))
                )
                td31stNext
            );
        check "31st next 8" True
            (
                (let {?first = justBefore ?first} in psOnAndOff
                    (beforeDays (dayOfMonth 31)) (beforeDays (delay 1 (dayOfMonth 31)))
                )
                (justBefore td31stNext)
            );
        check "31st next 7" True
            (
                (sfUpwardValue (intervalsFromTo (beforeDays (dayOfMonth 31)) (beforeDays (delay 1 (dayOfMonth 31))) ))
                (justBefore td31stNext)
            );
        check "31st next 6" True
            (
                (sfUpwardValue (daysToTimeIntervals (dayOfMonth 31)))
                (justBefore td31stNext)
            );
        check "31st next 5" (Just (justBefore td31stNext)) 
            (vsFirst (psValues 
                (sfPossibleChanges (daysToTimeIntervals (dayOfMonth 31)))
                (justBefore td2nd) (justAfter td300)
            ));
        check "31st next 4" (Just (justBefore td31stNext)) 
            (vsFirst (psValues 
                (sfChanges (daysToTimeIntervals (dayOfMonth 31)))
                (justBefore td2nd) (justAfter td300)
            ));
        check "31st next 3" (Just (justBefore td31stNext)) 
            (vsFirst (psValues 
                (intervalsStartOf (daysToTimeIntervals (dayOfMonth 31)))
                (justBefore td2nd) (justAfter td300)
            ));
        check "31st next 2" (Just (justBefore td31stNext)) 
            (vsFirst (psValues 
                (intervalsStartOf (daysToTimeIntervals (dayOfMonth 31)))
                (justAfter td1st) (justAfter td300)
            ));
        check "31st next 1" (Just (justBefore td31stNext)) 
            (vsFirst (psValues 
                (phaseStartOf (toPhase (daysToTimeIntervals (dayOfMonth 31))))
                (justAfter td1st) (justAfter td300)
            ));
            

        check "of 9" (Just (justBefore td2nd))
            (vsFirst (psValues 
                (pointsCutFirstAfterPoints beforeDay psMidnight)
                (justAfter td1st) (justAfter td300)
            ));
        check "of 8" (Just (justBefore td1st))
            (vsFirst (psValues 
                (pointsCutLastBeforePoints beforeDay psMidnight)
                (justBefore td1st) (justAfter td300)
            ));
{-
        check "of 7" (Just (jbInterval td1st td2nd))
            (cutNextInterval
                (toPhase 
                    (intervalsFromToInclusive (pointsCutLastBeforePoints beforeDay psMidnight) (pointsCutFirstAfterPoints beforeDay psMidnight)  )
                )
                (justBefore td1st) (justAfter td300)
            );
        check "of 6" (Just (jbInterval td1st td2nd))
            (cutNextInterval
                (toPhase (intervalsOf psMidnight beforeDay))
                (justBefore td1st) (justAfter td300)
            );
-}
        check "of 5" (Just (jbInterval td1st td2nd))
            (cutNextInterval
                dayPhase
                (justBefore td1st) (justAfter td300)
            );
        check "of 4" (Just (jbInterval td1st td2nd))
            (cutNextInterval
                (phaseIntersect dayPhase (intervalsOf psMidnight beforeDay))
                (justBefore td1st) (justAfter td300)
            );
        check "of 3" (Just (jbInterval td1st td2nd))
            (cutNextInterval
                (phaseIntersect dayPhase (intervalsOf psMidnight (phaseStartOf dayPhase)))
                (justBefore td1st) (justAfter td300)
            );
        check "of 2" (Just (jbInterval td1st td2nd))
            (cutNextInterval
                (phaseOf dayPhase psMidnight)
                (justBefore td1st) (justAfter td300)
            );
        check "of 1" (Just (jbInterval td1st td2nd))
            (cutNextInterval
                (phaseOf dayPhase (pointsFromCut (phaseStartOf midnights)))
                (justBefore td1st) (justAfter td300)
            );
        check "of" (Just (jbInterval td1st td2nd))
            (cutNextInterval
                (ofPhase midnights dayPhase)
                (justBefore td1st) (justAfter td300)
            );

        check "of t 16" (Just (justBefore (tdmid 160)))
            (let
            {
                psday = maybeDayEachYear (\year -> fromGregorianValid year 4 26);
                ints = intervalsFromTo (beforeDays psday) (beforeDays (delay 1 psday));
                ps = pointsFromCut (intervalsStartOf ints);
                ints2 = intervalsOf ps beforeDay;
                pso = filterIntersect (\cut -> case cut of
                {
                    MkCut a _ -> member ints2 a;
                }) (phaseStartOf dayPhase)
            } in vsFirst (psValues pso (justBefore td1st) (justAfter td300))
            );
{-
        check "of t 15" (Just (justBefore (tdmid 160)))
            (let
            {
                psday = maybeDayEachYear (\year -> fromGregorianValid year 4 26);
                ints = intervalsFromTo (beforeDays psday) (beforeDays (delay 1 psday));
                ps = pointsFromCut (intervalsStartOf ints);
                ints2 = intervalsOf ps beforeDay;
                phi = phaseIntersect dayPhase ints2;
                pso = phaseStartOf phi;
            } in vsFirst (psValues pso (justBefore td1st) (justAfter td300))
            );
-}
{-
        check "of t 14" (Just (justBefore (tdmid 160)))
            (let
            {
                psday = maybeDayEachYear (\year -> fromGregorianValid year 4 26);
                ints = intervalsFromTo (beforeDays psday) (beforeDays (delay 1 psday));
                ps = pointsFromCut (intervalsStartOf ints);
            } in vsFirst (psValues (phaseStartOf (phaseIntersect dayPhase (intervalsOf ps beforeDay)))
                (justBefore td1st) (justAfter td300))
            );
-}
{-
        check "of t 13" (Just (justBefore (tdmid 160)))
            (let
            {
                psday = maybeDayEachYear (\year -> fromGregorianValid year 4 26);
                ints = intervalsFromTo (beforeDays psday) (beforeDays (delay 1 psday));
                ps = pointsFromCut (intervalsStartOf ints);
            } in vsFirst (psValues (phaseChanges (phaseIntersect dayPhase (intervalsOf ps beforeDay)))
                (justBefore td1st) (justAfter td300))
            );
-}
{-
        check "of t 12" (Just (justBefore (tdmid 160)))
            (let
            {
                psday = maybeDayEachYear (\year -> fromGregorianValid year 4 26);
                ints = intervalsFromTo (beforeDays psday) (beforeDays (delay 1 psday));
                ps = pointsFromCut (intervalsStartOf ints);
            } in firstAfterUntil (phaseChanges (phaseIntersect dayPhase (intervalsOf ps beforeDay)))
                (justBefore td1st) (justAfter td300)
            );
-}
{-
        check "of t 11" (Just (oneDayInterval 160))
            (let
            {
                psday = maybeDayEachYear (\year -> fromGregorianValid year 4 26);
                ints = intervalsFromTo (beforeDays psday) (beforeDays (delay 1 psday));
                ps = pointsFromCut (intervalsStartOf ints);
            } in cutNextInterval
                (phaseIntersect dayPhase (intervalsOf ps beforeDay))
                (justBefore td1st) (justAfter td300)
            );
-}
        check "of t 10" (Just (oneDayInterval 160))
            (let
            {
                psday = maybeDayEachYear (\year -> fromGregorianValid year 4 26);
                ints = intervalsFromTo (beforeDays psday) (beforeDays (delay 1 psday));
                ps = pointsFromCut (intervalsStartOf ints);
            } in cutNextInterval
                (toPhase (intervalsOf ps (phaseStartOf dayPhase)))
                (justBefore td1st) (justAfter td300)
            );
{-
        check "of t 9" (Just (oneDayInterval 160))
            (let
            {
                psday = maybeDayEachYear (\year -> fromGregorianValid year 4 26);
                ints = intervalsFromTo (beforeDays psday) (beforeDays (delay 1 psday));
                ps = pointsFromCut (intervalsStartOf ints);
            } in cutNextInterval
                (phaseIntersect dayPhase (intervalsOf ps (phaseStartOf dayPhase)))
                (justBefore td1st) (justAfter td300)
            );
-}
        check "of t 8" (Just (oneDayInterval 160))
            (let
            {
                psday = maybeDayEachYear (\year -> fromGregorianValid year 4 26);
            } in cutNextInterval
                (phaseOf dayPhase (pointsFromCut (beforeDays psday)))
                (justBefore td1st) (justAfter td300)
            );
{-
        check "of t 7" (Just (jbInterval td1st td2nd))
            (let
            {
                psday = maybeDayEachYear (\year -> fromGregorianValid year 4 26);
            } in cutNextInterval
                (phaseOf dayPhase (pointsFromCut (intervalsStartOf 
                    (intervalsFromTo (beforeDays psday) (beforeDays (delay 1 psday)))
                )))
                (justBefore td1st) (justAfter td300)
            );
-}
        check "of t 6" (Just (jbInterval td1st td2nd))
            (cutNextInterval
                (phaseOf dayPhase (pointsFromCut (phaseStartOf dayPhase)))
                (justBefore td1st) (justAfter td300)
            );
{-
        check "of t 5" (Just (jbInterval td1st td2nd))
            (cutNextInterval
                (phaseOf dayPhase (pointsFromCut (phaseStartOf (toPhase 
                    (daysToTimeIntervals (maybeDayEachYear (\year -> fromGregorianValid year 4 26)))
                )))  )
                (justBefore td1st) (justAfter td300)
            );
-}
        check "of t 4" (Just (jbInterval td1st td2nd))
            (cutNextInterval
                dayPhase
                (justBefore td1st) (justAfter td300)
            );
        check "of t 3" (Just (oneDayInterval 160))
            (cutNextInterval
                (toPhase 
                    (daysToTimeIntervals (maybeDayEachYear (\year -> fromGregorianValid year 4 26)))
                )
                (justBefore td1st) (justAfter td300)
            );
{-
        check "of t 2" (Just (jbInterval td1st td2nd))
            (cutNextInterval
                (ofPhase (toPhase 
                    (daysToTimeIntervals (maybeDayEachYear (\year -> fromGregorianValid year 4 26)))
                ) dayPhase)
                (justBefore td1st) (justAfter td300)
            );
-}
        check "of t 1" (Just (jbInterval td1st td2nd))
            (cutNextInterval
                (ofPhase (toPhase 
                    (pointsToIntervals (timeOfDay (TimeOfDay 7 0 0)))
                ) dayPhase)
                (justBefore td1st) (justAfter td300)
            );
{-
        check "of t" (Just (jbInterval td1st td2nd))
            (cutNextInterval
                (ofPhase (toPhase 
                    (intersect 
                        (daysToTimeIntervals (maybeDayEachYear (\year -> fromGregorianValid year 4 26))) 
                        (pointsToIntervals (timeOfDay (TimeOfDay 7 0 0)))
                    )
                ) dayPhase)
                (justBefore td1st) (justAfter td300)
            );
-}
    };
}
