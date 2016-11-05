module Data.Recurrence.Time.Recurrence where
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

    data Recurrence = forall a. Eq a => PeriodRecurrence (PiecePartialFunction T a) | InstantRecurrence (PointSet T) | EmptyRecurrence;

    instance BasedOn Recurrence where
    {
        type Base Recurrence = T;
    };

    instance RemapBase Recurrence Recurrence where
    {
        remapBase pq qp (PeriodRecurrence (pf :: PiecePartialFunction T count)) = PeriodRecurrence (remapBase pq qp pf :: PiecePartialFunction T count);
        remapBase pq qp (InstantRecurrence ps) = InstantRecurrence $ remapBase pq qp ps;
        remapBase _ _ EmptyRecurrence = EmptyRecurrence;
    };

    recAlways :: Recurrence;
    recAlways = PeriodRecurrence $ piecePartialConst ();

    recNever :: Recurrence;
    recNever = EmptyRecurrence;

    recIntersectPeriod :: forall count. Eq count => Recurrence -> PiecePartialFunction T count -> Recurrence;
    recIntersectPeriod EmptyRecurrence _ = EmptyRecurrence;
    recIntersectPeriod (PeriodRecurrence p1) p2 = PeriodRecurrence $ liftA2 (liftA2 (,)) p1 p2;
    recIntersectPeriod (InstantRecurrence i1) p2 = InstantRecurrence $ pointIntersectPiece (piecePartialToSet p2) i1;

    recIntersect :: Recurrence -> Recurrence -> Recurrence;
    recIntersect tp1 (PeriodRecurrence p2) = recIntersectPeriod tp1 p2;
    recIntersect _ EmptyRecurrence = EmptyRecurrence;
    recIntersect (PeriodRecurrence p1) (InstantRecurrence i2) = InstantRecurrence $ pointIntersectPiece (piecePartialToSet p1) i2;
    recIntersect (InstantRecurrence i1) (InstantRecurrence i2) = InstantRecurrence $ intersect i1 i2;
    recIntersect EmptyRecurrence _ = EmptyRecurrence;

    recUnion :: Recurrence -> Recurrence -> M Recurrence;
    recUnion EmptyRecurrence tp = return tp;
    recUnion tp EmptyRecurrence = return tp;
    recUnion (PeriodRecurrence p1) (PeriodRecurrence p2) = return $ PeriodRecurrence $ liftA2 toEitherBoth p1 p2;
    recUnion (InstantRecurrence i1) (InstantRecurrence i2) = return $ InstantRecurrence $ union i1 i2;
    recUnion _ _ = reportError "cannot union instants and periods";

    recInvert :: Recurrence -> M Recurrence;
    recInvert EmptyRecurrence = return recAlways;
    recInvert (PeriodRecurrence pf) = return $ PeriodRecurrence $ invert $ piecePartialToSet pf;
    recInvert (InstantRecurrence _) = reportError "cannot invert instants";

    recDiff :: Recurrence -> Recurrence -> M Recurrence;
    recDiff tp1 EmptyRecurrence = return tp1;
    recDiff tp1 (PeriodRecurrence p2) = return $ recIntersectPeriod tp1 (invert $ piecePartialToSet p2);
    recDiff EmptyRecurrence _ = return EmptyRecurrence;
    recDiff (InstantRecurrence i1) (InstantRecurrence i2) = return $ InstantRecurrence $ diff i1 i2;
    recDiff (PeriodRecurrence _) (InstantRecurrence _) = reportError "cannot subtract instants from periods";

    recStart :: Recurrence -> PointSet T;
    recStart (InstantRecurrence ps) = ps;
    recStart (PeriodRecurrence ppf) = pointSet $ piecePartialStarts ppf;
    recStart EmptyRecurrence = empty;

    recEnd :: Recurrence -> PointSet T;
    recEnd (InstantRecurrence ps) = ps;
    recEnd (PeriodRecurrence ppf) = pointSet $ piecePartialEnds ppf;
    recEnd EmptyRecurrence = empty;

    recFromTo :: Recurrence -> Recurrence -> PieceSet T;
    recFromTo tsOn tsOff = let {?first=firstTime} in
        pieceSetFromToPoints Nothing (recStart tsOn) (recEnd tsOff);

    recFromUntil :: Recurrence -> Recurrence -> PieceSet T;
    recFromUntil tsOn tsOff = let {?first=firstTime} in
        pieceSetFromToPoints Nothing (recStart tsOn) (recStart tsOff);

    recFrom :: Recurrence -> PieceSet T;
    recFrom ts = let {?first = firstTime} in
        pieceSetAfterPoint (recStart ts);

    recNthIn :: Int -> Recurrence -> Recurrence -> Recurrence;
    recNthIn n (InstantRecurrence subject) (PeriodRecurrence delimiter) = let {?first = firstTime} in InstantRecurrence $ pointCountedIn n subject delimiter;
    recNthIn n (PeriodRecurrence subject) (PeriodRecurrence delimiter) = let {?first = firstTime} in PeriodRecurrence $ pieceCountedIn n subject delimiter;
    recNthIn _ _ _ = EmptyRecurrence;

    recNthFrom :: Int -> Recurrence -> Recurrence -> Recurrence;
    recNthFrom _n EmptyRecurrence _delimiter = EmptyRecurrence;
    recNthFrom n (InstantRecurrence subject) delimiter = let {?first = firstTime} in InstantRecurrence $ pointCountedOnAfter n subject (recStart delimiter);
    recNthFrom n (PeriodRecurrence subject) delimiter = let {?first = firstTime} in PeriodRecurrence $ pieceCountedOnAfter n subject (recStart delimiter);

    recBetween :: Recurrence -> Recurrence;
    recBetween EmptyRecurrence = recAlways;
    recBetween (PeriodRecurrence pf) = PeriodRecurrence $ invert $ piecePartialToSet pf;
    recBetween (InstantRecurrence ps) = let {?first = firstTime} in PeriodRecurrence $ fmap Just $ piecePartialBetweenPoint ps;

    recOf :: Recurrence -> Recurrence -> Recurrence;
    recOf picker (PeriodRecurrence pf) = let {?first = firstTime} in PeriodRecurrence $ piecePartialOf (piecePartialToSet pf) (recStart picker);
    recOf _ _ = EmptyRecurrence;
}
