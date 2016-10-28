module Data.TimePhase.Time where
{
    import Control.Applicative (liftA2);
    import Data.Time;
    import Data.SetSearch;

    type M = Either String;
    reportError :: String -> M a;
    reportError = Left;

    type T = LocalTime;

    getNow :: IO T;
    getNow = fmap zonedTimeToLocalTime getZonedTime;

    toDayStart :: Day -> T;
    toDayStart localDay = let
    {
        localTimeOfDay = midnight;
    } in LocalTime{..};

    firstTime :: T;
    firstTime = toDayStart $ ModifiedJulianDay (-10000000);

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
}
