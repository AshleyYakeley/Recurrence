module Data.Recurrence.Time.Day
    (
    timeToDay,
    aDay,
    isTimeOfDay,
    isSingleInterval,
    isSingleDay,
    ) where
{
    import Data.Time;
    import Data.SetSearch;
    import Data.Recurrence.Time.Recurrence;


    timeToDay :: MonotonicSurjection T Day;
    timeToDay = let
    {
        msEval (LocalTime d _) = d;
        msImage d = (LocalTime d midnight,LocalTime (succ d) midnight);
    } in MkMonotonicSurjection{..};

    theDay :: PieceFunction T Day;
    theDay = pieceEverySurjection timeToDay;

    aDay :: Recurrence;
    aDay = PeriodRecurrence $ fmap Just theDay;

    isTimeOfDay :: TimeOfDay -> PointSet T;
    isTimeOfDay tod = knownToPointSet (kpsEach (\t -> localDay t) (\day -> LocalTime day tod));

    -- | first day on and first day off (not last day)
    ;
    isSingleInterval :: Day -> Day -> PieceSet T;
    isSingleInterval startDay endDay = let {?first = firstTime} in
        pieceSetSingleInterval (toDayStart startDay) (toDayStart endDay);

    isSingleDay :: Day -> PieceSet T;
    isSingleDay d = isSingleInterval d (succ d);
}