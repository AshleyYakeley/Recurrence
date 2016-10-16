module Data.TimePhase.Time where
{
    import Control.Applicative (liftA2);
    import Data.Fixed;
    import Data.Time;
    import Data.SetSearch;

    type M = Either String;
    reportError :: String -> M a;
    reportError = Left;

    type T = LocalTime;

    getNow :: IO T;
    getNow = fmap zonedTimeToLocalTime getZonedTime;

    firstTime :: T;
    firstTime = LocalTime
    {
        localDay = ModifiedJulianDay (-10000000),
        localTimeOfDay = midnight
    };

    data TimePhase = forall count. Eq count => PeriodTimeSet (PiecePartialFunction T count) | InstantTimeSet (PointSet T) | EmptyTimeSet;

    instance BasedOn TimePhase where
    {
        type Base TimePhase = T;
    };

    instance RemapBase TimePhase TimePhase where
    {
        remapBase pq qp (PeriodTimeSet (pf :: PiecePartialFunction T count)) = PeriodTimeSet (remapBase pq qp pf :: PiecePartialFunction T count);
        remapBase pq qp (InstantTimeSet ps) = InstantTimeSet $ remapBase pq qp ps;
        remapBase _ _ EmptyTimeSet = EmptyTimeSet;
    };

    tpAlways :: TimePhase;
    tpAlways = PeriodTimeSet $ piecePartialConst ();

    tpNever :: TimePhase;
    tpNever = EmptyTimeSet;

    tpIntersectPeriod :: forall count. Eq count => TimePhase -> PiecePartialFunction T count -> TimePhase;
    tpIntersectPeriod EmptyTimeSet _ = EmptyTimeSet;
    tpIntersectPeriod (PeriodTimeSet p1) p2 = PeriodTimeSet $ liftA2 (liftA2 (,)) p1 p2;
    tpIntersectPeriod (InstantTimeSet i1) p2 = InstantTimeSet $ pointIntersectPiece (piecePartialToSet p2) i1;

    tpIntersect :: TimePhase -> TimePhase -> TimePhase;
    tpIntersect tp1 (PeriodTimeSet p2) = tpIntersectPeriod tp1 p2;
    tpIntersect _ EmptyTimeSet = EmptyTimeSet;
    tpIntersect (PeriodTimeSet p1) (InstantTimeSet i2) = InstantTimeSet $ pointIntersectPiece (piecePartialToSet p1) i2;
    tpIntersect (InstantTimeSet i1) (InstantTimeSet i2) = InstantTimeSet $ intersect i1 i2;
    tpIntersect EmptyTimeSet _ = EmptyTimeSet;

    tpUnion :: TimePhase -> TimePhase -> M TimePhase;
    tpUnion EmptyTimeSet tp = return tp;
    tpUnion tp EmptyTimeSet = return tp;
    tpUnion (PeriodTimeSet p1) (PeriodTimeSet p2) = return $ PeriodTimeSet $ liftA2 toEitherBoth p1 p2;
    tpUnion (InstantTimeSet i1) (InstantTimeSet i2) = return $ InstantTimeSet $ union i1 i2;
    tpUnion _ _ = reportError "cannot union instants and periods";

    tpInvert :: TimePhase -> M TimePhase;
    tpInvert EmptyTimeSet = return tpAlways;
    tpInvert (PeriodTimeSet pf) = return $ PeriodTimeSet $ invert $ piecePartialToSet pf;
    tpInvert (InstantTimeSet _) = reportError "cannot invert instants";

    tpDiff :: TimePhase -> TimePhase -> M TimePhase;
    tpDiff tp1 EmptyTimeSet = return tp1;
    tpDiff tp1 (PeriodTimeSet p2) = return $ tpIntersectPeriod tp1 (invert $ piecePartialToSet p2);
    tpDiff EmptyTimeSet _ = return EmptyTimeSet;
    tpDiff (InstantTimeSet i1) (InstantTimeSet i2) = return $ InstantTimeSet $ diff i1 i2;
    tpDiff (PeriodTimeSet _) (InstantTimeSet _) = reportError "cannot subtract instants from periods";

    startOf :: TimePhase -> PointSet T;
    startOf (InstantTimeSet ps) = ps;
    startOf (PeriodTimeSet phase) = pointSet $ piecePartialStarts phase;
    startOf EmptyTimeSet = empty;

    endOf :: TimePhase -> PointSet T;
    endOf (InstantTimeSet ps) = ps;
    endOf (PeriodTimeSet phase) = pointSet $ piecePartialEnds phase;
    endOf EmptyTimeSet = empty;

    fromTo :: TimePhase -> TimePhase -> PieceSet T;
    fromTo tsOn tsOff = let {?first=firstTime} in
        pieceSetFromToPoints Nothing (startOf tsOn) (endOf tsOff);

    fromUntil :: TimePhase -> TimePhase -> PieceSet T;
    fromUntil tsOn tsOff = let {?first=firstTime} in
        pieceSetFromToPoints Nothing (startOf tsOn) (startOf tsOff);

    onAfter :: TimePhase -> PieceSet T;
    onAfter ts = let {?first = firstTime} in
        pieceSetAfterPoint (startOf ts);

    nthIn :: Int -> TimePhase -> TimePhase -> TimePhase;
    nthIn n (InstantTimeSet subject) (PeriodTimeSet delimiter) = let {?first = firstTime} in InstantTimeSet $ pointCountedIn n subject delimiter;
    nthIn n (PeriodTimeSet subject) (PeriodTimeSet delimiter) = let {?first = firstTime} in PeriodTimeSet $ pieceCountedIn n subject delimiter;
    nthIn _ _ _ = EmptyTimeSet;

    nthFrom :: Int -> TimePhase -> TimePhase -> TimePhase;
    nthFrom _n EmptyTimeSet _delimiter = EmptyTimeSet;
    nthFrom n (InstantTimeSet subject) delimiter = let {?first = firstTime} in InstantTimeSet $ pointCountedOnAfter n subject (startOf delimiter);
    nthFrom n (PeriodTimeSet subject) delimiter = let {?first = firstTime} in PeriodTimeSet $ pieceCountedOnAfter n subject (startOf delimiter);

    ofPhase :: TimePhase -> TimePhase -> TimePhase;
    ofPhase picker (PeriodTimeSet phase) = let {?first = firstTime} in PeriodTimeSet $ phaseOf (piecePartialToSet phase) (startOf picker);
    ofPhase _ _ = EmptyTimeSet;

    toDayStarts :: PointFunction Day a -> PointFunction T a;
    toDayStarts (MkPointFunction ddlda) = MkPointFunction $ \lt0@(LocalTime d0 t0) lt1@(LocalTime d1 t1) -> let
    {
        range = case compare lt0 lt1 of
        {
            LT -> if t0 == midnight then Just (d0,d1) else Just (succ d0,d1);
            GT -> if t1 == midnight then Just (d0,d1) else Just (d0,pred d1);
            EQ -> if t0 == midnight then Just (d0,d1) else Nothing;
        };

        dayToT d = LocalTime d midnight;

        items = case range of
        {
            Just (d0',d1') -> ddlda d0' d1';
            Nothing -> [];
        };
    } in fmap (\(d,a) -> (dayToT d,a)) items;

    everyDay :: PointSet Day;
    everyDay = MkPointFunction $ \d0 d1 -> fmap (\d -> (d,())) $ listFromTo d0 d1;

    theDay :: PieceFunction T Day;
    theDay = MkPieceFunction
    {
        pieceEval = localDay,
        pieceJoints = toDayStarts everyDay
    };

    dayPhase :: TimePhase;
    dayPhase = PeriodTimeSet $ fmap Just theDay;

    daysToTimeIntervals :: PointSet Day -> PieceSet T;
    daysToTimeIntervals psday = let {?first=firstTime} in
     pieceSetFromToPoints Nothing (toDayStarts psday) (toDayStarts (delay 1 psday));

    weekDay :: Integer -> PieceSet T;
    weekDay i = fmap (\day -> toMU $ mod' (toModifiedJulianDay day) 7 == i) theDay;

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

    theYearAndMonth :: PieceFunction T (Integer,Int);
    theYearAndMonth = MkPieceFunction
    {
        pieceEval = \t -> case toGregorian (localDay t) of
        {
            (y,m,_) -> (y,m);
        },
        pieceJoints = toDayStarts monthFirsts
    };

    theMonth :: PieceFunction T Int;
    theMonth = fmap snd theYearAndMonth;

    isYearAndMonth :: Integer -> Int -> PieceSet T;
    isYearAndMonth year month = fmap (\ym -> toMU $ ym == (year,month)) theYearAndMonth;

    isMonth :: Int -> PieceSet T;
    isMonth i = fmap (\(_,m) -> toMU $ i == m) theYearAndMonth;

    monthPhase :: TimePhase;
    monthPhase = PeriodTimeSet $ fmap Just theMonth;

    dayOfMonth :: Int -> PointSet Day;
    dayOfMonth dd = let
    {
        dayYMonth :: Day -> Integer;
        dayYMonth day = case toGregorian day of
        {
            (y,m,_d) -> y * 12 + (fromIntegral (m - 1));
        };

        ymonthDay :: Integer -> Maybe Day;
        ymonthDay i = let
        {
            y = div i 12;
            m = (fromIntegral (mod i 12)) + 1;
        } in fromGregorianValid y m dd;
    }
    in pointSet $ pointsSearch dayYMonth ymonthDay;

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

    theYear :: PieceFunction T Integer;
    theYear = MkPieceFunction
    {
        pieceEval = \t -> case toGregorian (localDay t) of
        {
            (y,_,_) -> y;
        },
        pieceJoints = toDayStarts yearFirsts
    };

    yearPhase :: TimePhase;
    yearPhase = PeriodTimeSet $ fmap Just theYear;

    isYear :: Integer -> PieceSet T;
    isYear year = fmap (\y -> toMU $ y == year) theYear;

    yearOfDay :: Day -> Integer;
    yearOfDay day = case toGregorian day of
    {
        (y,_,_) -> y;
    };

    dayEachYear :: (Integer -> Day) -> PointSet Day;
    dayEachYear f = knownToPointSet (kpsEach yearOfDay f);

    maybeDayEachYear :: (Integer -> Maybe Day) -> PointSet Day;
    maybeDayEachYear = pointSet . (pointsSearch yearOfDay);

    timeOfDay :: TimeOfDay -> PointSet T;
    timeOfDay tod = knownToPointSet (kpsEach (\t -> localDay t) (\day -> LocalTime day tod));
}
