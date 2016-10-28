{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, ImplicitParams #-}
module Main where
{
    import Data.Monoid;
    import Data.SetSearch;
    import Data.Time;
    import Data.TimePhase;
    import Data.TimePhase.Time;

    psMidnight :: PointSet T;
    psMidnight = isTimeOfDay midnight;

    midnights :: PiecePartialFunction T;
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

    td30000 :: T;
    td30000 = tdmid 30000;

    pointInterval p = MkInterval (Starts (justBefore p)) (Ends (justAfter p));

    jbInterval t0 t1 = MkInterval (Starts (justBefore t0)) (Ends (justBefore t1));

    oneDayInterval i = jbInterval (tdmid i) (tdmid (i + 1));

    twoDays :: PieceSet T;
    twoDays = let {?first = firstTime} in pieceSetFromToPoints False (single cb0) (single cb2);

    psAM1 :: PointSet T;
    psAM1 = isTimeOfDay (TimeOfDay 1 0 0);

    am1 = toPhase psAM1;

    firstHour :: PieceSet T;
    firstHour = fromUntil midnights am1;

    firstHourAnd :: PieceSet T;
    firstHourAnd = fromTo midnights am1;

    main :: IO ();
    main = let {?first=firstTime} in do
    {
        check "is points" True (member psMidnight t0);
        check "is points union" True (member (union psMidnight psMidnight) t0);
        check "is points union empty 1" True (member (union psMidnight empty) t0);
        check "is points union empty 2" True (member (union empty psMidnight) t0);
        check "is phase" True (member (piecePartialToSet midnights) t0);
        check "is startof" True (member (startOf midnights) t0);
        check "is piecePartialStarts" True (member (piecePartialStarts midnights) cb0);

        check "firstHour 00:00" True (member firstHour t0);

        check "firstHour 00:00 change before" True (member (pieceChanges firstHour) cb0);
        check "firstHour 00:00 change after" False (member (pieceChanges firstHour) ca0);
        check "firstHour 00:30" True (member firstHour t005);
        check "firstHour 00:30 change before" False (member (pieceChanges firstHour) (justBefore t005));
        check "firstHour 00:30 change after" False (member (pieceChanges firstHour) (justAfter t005));

        check "firstHour 01:00" False (member firstHour t01);
        check "firstHour 01:00 change before" True (member (pieceChanges firstHour) cb01);
        check "firstHour 01:00 change after" False (member (pieceChanges firstHour) ca01);

        check "firstHourAnd 01:00" True (member firstHourAnd t01);
        check "firstHourAnd 01:00 change before" False (member (pieceChanges firstHourAnd) cb01);
        check "firstHourAnd 01:00 change after" True (member (pieceChanges firstHourAnd) ca01);

        check "change end firstHour" (Just cb01) (firstAfterUntil (pieceChanges firstHour) cb005 cb2);

        check "endOf1am" (Just ca01) (firstAfterUntil (piecePartialEnds am1) cb0 cb2);
        check "endOfFirstHour" (Just cb01) (firstAfterUntil (piecePartialEnds (toPhase firstHour)) cb0 cb2);
        check "endOfFirstHourAnd" (Just ca01) (firstAfterUntil (piecePartialEnds (toPhase firstHourAnd)) cb0 cb2);


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

--        check "31st" (Just (justBefore td31st)) (vsFirst (psValues (beforeDays (dayOfMonth 31)) (justBefore t0) (justAfter td300)));

        check "31st int" (Just (jbInterval td31st td1st)) (cutNextInterval (toPhase (daysToTimeIntervals (dayOfMonth 31))) (justBefore t0) (justAfter td300));

        check "year end" (Just (justBefore td1st)) (firstAfterUntil (piecePartialEnds (toPhase (isSingleYear 1858))) (justBefore t0) ?last);

        check "of" (Just (jbInterval td1st td2nd))
            (cutNextInterval
                (ofPhase midnights aDay)
                (justBefore td1st) (justAfter td300)
            );
{-
        check "of each year" (Just (oneDayInterval 160))
            (cutNextInterval
                (ofPhase (toPhase
                    (intersect
                        (daysToTimeIntervals (maybeDayEachYear (\year -> fromGregorianValid year 4 26)))
                        (pointsToIntervals (isTimeOfDay (TimeOfDay 7 0 0)))
                    )
                ) aDay)
                (justBefore td1st) (justAfter td300)
            );
-}


{-
        check "of this year 7" Nothing
            (let
            {
                days = daysToTimeIntervals (single (fromGregorian 1859 4 26));
                times = pointsToIntervals (isTimeOfDay (TimeOfDay 7 0 0));
                ints = intersect times days :: PieceSet T;
                intstarts = piecePartialStarts ints :: PointSet (Cut T);
                ps = pointsFromCut intstarts :: PointSet T;
            } in psPrevious ps (justBefore td1st)
            );
-}
{-
        check "of this year 6" Nothing
            (let
            {
                days = daysToTimeIntervals (single (fromGregorian 1859 4 26));
                times = pointsToIntervals (isTimeOfDay (TimeOfDay 7 0 0));
                ints = intersect times days :: PieceSet T;
                ps = pointsFromCut (piecePartialStarts ints) :: PointSet T;
                cutsbefore = pointsCutLastBeforePoints beforeDay ps;
                cutsafter = pointsCutFirstAfterPoints beforeDay ps;
                intsof = intervalsFromToInclusive False cutsbefore cutsafter :: PieceSet T;
                intsofbefore = sfCutBefore intsof;
                starts = pointIntersectPiece beforeDay intsofbefore;
            } in vsFirst (psValues starts (justBefore td1st) (justAfter td300))
            );
        check "of this year 5" Nothing
            (let
            {
                days = daysToTimeIntervals (single (fromGregorian 1859 4 26));
                times = pointsToIntervals (isTimeOfDay (TimeOfDay 7 0 0));
                ints = intersect times days :: PieceSet T;
                ps = pointsFromCut (piecePartialStarts ints) :: PointSet T;
                intsof = intervalsOf ps beforeDay :: PieceSet T;
                intsofbefore = sfCutBefore intsof;
                starts = pointIntersectPiece beforeDay intsofbefore;
            } in vsFirst (psValues starts (justBefore td1st) (justAfter td300))
            );
        check "of this year 4" Nothing
            (let
            {
                days = daysToTimeIntervals (single (fromGregorian 1859 4 26));
                times = pointsToIntervals (isTimeOfDay (TimeOfDay 7 0 0));
                ints = intersect times days :: PieceSet T;
                ps = pointsFromCut (piecePartialStarts ints);
                intsof = intervalsOf ps (piecePartialStarts aDay);
                starts = pointIntersectPiece (piecePartialStarts aDay) (sfCutBefore intsof)
            } in vsFirst (psValues starts (justBefore td1st) (justAfter td300))
            );
        check "of this year 3" Nothing
            (let
            {
                days = daysToTimeIntervals (single (fromGregorian 1859 4 26));
                times = pointsToIntervals (isTimeOfDay (TimeOfDay 7 0 0));
                ints = intersect times days :: PieceSet T;
                result = phaseOf aDay (pointsFromCut (piecePartialStarts ints));
                starts = piecePartialStarts result;
            } in vsFirst (psValues starts (justBefore td1st) (justAfter td300))
            );
        check "of this year 2" (Just (oneDayInterval 160))
            (let
            {
                days = daysToTimeIntervals (single (fromGregorian 1859 4 26));
                times = pointsToIntervals (isTimeOfDay (TimeOfDay 7 0 0));
                ints = intersect times days :: PieceSet T;
                result = phaseOf aDay (pointsFromCut (piecePartialStarts (toPhase ints)));
            } in cutNextInterval result (justBefore td1st) (justAfter td300)
            );
        check "of this year 1" (Just (oneDayInterval 160))
            (let
            {
                days = daysToTimeIntervals (single (fromGregorian 1859 4 26));
                times = pointsToIntervals (isTimeOfDay (TimeOfDay 7 0 0));
                ints = intersect times days :: PieceSet T;
                result = ofPhase (toPhase ints) aDay;
            } in cutNextInterval result (justBefore td1st) (justAfter td300)
            );
        check "of this year" (Just (oneDayInterval 160))
            (cutNextInterval
                (ofPhase (toPhase
                    (intersect
                        (daysToTimeIntervals (single (fromGregorian 1859 4 26)))
                        (pointsToIntervals (isTimeOfDay (TimeOfDay 7 0 0)))
                    )
                ) aDay)
                (justBefore td1st) (justAfter td300)
            );
-}

{-
        check "of single" (Just (oneDayInterval 160))
            (cutNextInterval
                (ofPhase (toPhase
                    (single (LocalTime
                    {
                        localDay = fromGregorian 1859 4 26,
                        localTimeOfDay = TimeOfDay 7 0 0
                    }) :: PointSet T)
                ) aDay)
                (justBefore td1st) (justAfter td300)
            );
-}
    };
}
