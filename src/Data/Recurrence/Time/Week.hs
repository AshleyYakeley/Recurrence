module Data.Recurrence.Time.Week
(
    WeekNumber,
    aWeek,

    DayOfWeek,
    isDayOfWeek,
) where
{
    import Prelude hiding (id,(.));
    import Control.Category;
    import Data.Time;
    import Data.SetSearch;
    import Data.Recurrence.Time.Recurrence;
    import Data.Recurrence.Time.Day;


    -- Week

    type WeekNumber = Integer;
    type DayOfWeek = Int;

    weekToFirstDay :: WeekNumber -> Day;
    weekToFirstDay wn = ModifiedJulianDay $ (wn * 7) - 3;

    fromWeek :: WeekNumber -> DayOfWeek -> Maybe Day;
    fromWeek _ d | d < 1 = Nothing;
    fromWeek _ d | d > 7 = Nothing;
    fromWeek wn d = let
    {
        day0 = weekToFirstDay wn;
        d' = toInteger $ d - 1;
    } in Just $ addDays d' day0;

    toWeek :: Day -> (WeekNumber, DayOfWeek);
    toWeek (ModifiedJulianDay mjd) = let
    {
        (wn,d') = divMod (mjd + 3) 7;
        d = (fromInteger d') + 1;
    } in (wn,d);

    theWeek :: PieceFunction T WeekNumber;
    theWeek = let
    {
        dayToWeekNumber :: MonotonicSurjection Day WeekNumber;
        dayToWeekNumber = enumSurjection (fst . toWeek) weekToFirstDay;
    } in pieceEverySurjection (dayToWeekNumber . timeToDay);

    aWeek :: Recurrence;
    aWeek = PeriodRecurrence $ fmap Just theWeek;

    miDayOfWeek :: DayOfWeek -> MonotonicInjection WeekNumber Day;
    miDayOfWeek = splitInjection fromWeek toWeek;

    isDayOfWeek :: DayOfWeek -> PieceSet T;
    isDayOfWeek d = piecePartialToSet $ pieceMapSurjection timeToDay $ piecePartialEnumPoint $ pointEveryInjection $ miDayOfWeek d;
}
