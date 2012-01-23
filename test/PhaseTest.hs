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
    midnights = toPhase (timeOfDay midnight);

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

    pointInterval p = MkInterval (Starts (justBefore p)) (Ends (justAfter p));

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

        check "endOfFirstHourAnd 1" (Just ca01)
         (firstAfterUntil (pointsLastOnOrBeforePoints (intervalsEndOf firstHourAnd) (intervalsEndOf firstHourAnd)) cb0 cb2);
--        check "endOfFirstHourAnd" (Just ca01) (firstAfterUntil (phaseEndOf (toPhase firstHourAnd)) cb0 cb2);

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
    };
}
