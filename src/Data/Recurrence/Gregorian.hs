module Data.Recurrence.Gregorian where
{
    import Prelude hiding (id,(.));
    import Control.Category;
    import Data.Time;
    import Data.Time.Calendar.OrdinalDate;
    import Data.SetSearch;
    import Data.Recurrence.Time;
    import Data.Recurrence.Day;


    -- Gregorian year

    type Year = Integer;

    isSingleYear :: Year -> PieceSet T;
    isSingleYear year = let
    {
        startDay = fromOrdinalDate year 1;
        endDay = fromOrdinalDate (succ year) 1;
    } in isSingleInterval startDay endDay;

    yearOfDay :: Day -> Year;
    yearOfDay = fst . toOrdinalDate;

    theYear :: PieceFunction T Year;
    theYear = let
    {
        dayToYear :: MonotonicSurjection Day Year;
        dayToYear = let
        {
            yToFirstDay year = fromOrdinalDate year 1;
        } in enumSurjection yearOfDay yToFirstDay;
    } in pieceEverySurjection (dayToYear . timeToDay);

    aYear :: Recurrence;
    aYear = PeriodRecurrence $ fmap Just theYear;


    -- Gregorian year and month

    type MonthOfYear = Int;

    isSingleMonth :: Year -> MonthOfYear -> PieceSet T;
    isSingleMonth year month = let
    {
        startDay = fromGregorian year month 1;
        endDay = addGregorianMonthsClip 1 startDay;
    } in isSingleInterval startDay endDay;

    newtype MonthNumber = MkMonthNumber Integer deriving (Eq,Ord,Enum);

    toMonthNumber :: Year -> MonthOfYear -> Maybe MonthNumber;
    toMonthNumber _ m | m < 1 = Nothing;
    toMonthNumber _ m | m > 12 = Nothing;
    toMonthNumber y m = Just $ MkMonthNumber $ (y * 12) + (toInteger $ pred m);

    fromMonthNumber :: MonthNumber -> (Year,MonthOfYear);
    fromMonthNumber (MkMonthNumber mn) = let
    {
        (year,month0) = divMod mn 12;
    } in (year,fromInteger $ succ month0);

    miMonthOfYear :: MonthOfYear -> MonotonicInjection Year MonthNumber;
    miMonthOfYear = splitInjection toMonthNumber fromMonthNumber;

    dayToMonthNumber :: MonotonicSurjection Day MonthNumber;
    dayToMonthNumber = let
    {
        dayToMN = fst . dayToMNDay;
        mnToFirstDay mn = let
        {
            (y,m) = fromMonthNumber mn;
        } in fromGregorian y m 1;
    } in enumSurjection dayToMN mnToFirstDay;

    timeToMonthNumber :: MonotonicSurjection T MonthNumber;
    timeToMonthNumber = dayToMonthNumber . timeToDay;

    theMonthNumber :: PieceFunction T MonthNumber;
    theMonthNumber = pieceEverySurjection timeToMonthNumber;

    -- | Each "month" must be uniquely identified (with MonthNumber)
    ;
    aMonth :: Recurrence;
    aMonth = PeriodRecurrence $ fmap Just theMonthNumber;

    isMonthOfYear :: MonthOfYear -> PieceSet T;
    isMonthOfYear m = piecePartialToSet $ pieceMapSurjection timeToMonthNumber $ piecePartialEnumPoint $ pointEveryInjection $ miMonthOfYear m;

    dayEachYear :: (Year -> Maybe Day) -> PiecePartialFunction T Year;
    dayEachYear getDay = pieceMapSurjection timeToDay $ piecePartialEnumPoint $ pointEveryInjection $ ordInjection getDay yearOfDay;


    -- Gregorian year, month and day

    type DayOfMonth = Int;

    dayToMNDay :: Day -> (MonthNumber,DayOfMonth);
    dayToMNDay day = let
    {
        (y,m,d) = toGregorian day;
        Just mn = toMonthNumber y m;
    } in (mn,d);

    miDayOfMonth :: DayOfMonth -> MonotonicInjection MonthNumber Day;
    miDayOfMonth = let
    {
        mnToDay :: MonthNumber -> DayOfMonth -> Maybe Day;
        mnToDay mn d = let
        {
            (y,m) = fromMonthNumber mn;
        } in fromGregorianValid y m d;
    } in splitInjection mnToDay dayToMNDay;

    isDayOfMonth :: DayOfMonth -> PieceSet T;
    isDayOfMonth d = piecePartialToSet $ pieceMapSurjection timeToDay $ piecePartialEnumPoint $ pointEveryInjection $ miDayOfMonth d;

    miMonthAndDayOfYear :: (MonthOfYear,DayOfMonth) -> MonotonicInjection Year Day;
    miMonthAndDayOfYear = let
    {
        fromGregorianValid' :: Year -> (MonthOfYear, DayOfMonth) -> Maybe Day;
        fromGregorianValid' y (m,d) = fromGregorianValid y m d;

        toGregorian' :: Day -> (Year, (MonthOfYear, DayOfMonth));
        toGregorian' day = let
        {
            (y,m,d) = toGregorian day;
        } in (y,(m,d));
    } in splitInjection fromGregorianValid' toGregorian';

    isMonthAndDayOfYear :: MonthOfYear -> DayOfMonth -> PieceSet T;
    isMonthAndDayOfYear m d = piecePartialToSet $ pieceMapSurjection timeToDay $ piecePartialEnumPoint $ pointEveryInjection $ miMonthAndDayOfYear (m,d);


    -- Day of week

    type WeekNumber = Integer;
    type DayOfWeek = Int;

    miDayOfWeek :: DayOfWeek -> MonotonicInjection WeekNumber Day;
    miDayOfWeek = let
    {
        fromWeek :: WeekNumber -> DayOfWeek -> Maybe Day;
        fromWeek _ d | d < 1 = Nothing;
        fromWeek _ d | d > 7 = Nothing;
        fromWeek wn d = let
        {
            d' = toInteger $ d - 1;
            mjd = (wn * 7 + d') - 3;
        } in Just $ ModifiedJulianDay mjd;

        toWeek :: Day -> (WeekNumber, DayOfWeek);
        toWeek (ModifiedJulianDay mjd) = let
        {
            (wn,d') = divMod (mjd + 3) 7;
            d = (fromInteger d') + 1;
        } in (wn,d);
    } in splitInjection fromWeek toWeek;

    isDayOfWeek :: DayOfWeek -> PieceSet T;
    isDayOfWeek d = piecePartialToSet $ pieceMapSurjection timeToDay $ piecePartialEnumPoint $ pointEveryInjection $ miDayOfWeek d;
}
