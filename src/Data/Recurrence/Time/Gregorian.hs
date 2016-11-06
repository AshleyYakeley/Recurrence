module Data.Recurrence.Time.Gregorian
(
    Year,
    isSingleYear,
    yearOfDay,
    aYear,
    dayEachYear,

    MonthOfYear,
    isSingleMonth,
    aMonth,
    isMonthOfYear,

    DayOfMonth,
    isDayOfMonth,
    isMonthAndDayOfYear,
) where
{
    import Prelude hiding (id,(.));
    import Control.Category;
    import Data.Time;
    import Data.Time.Calendar.OrdinalDate;
    import Data.SetSearch;
    import Data.Recurrence.Time.Recurrence;
    import Data.Recurrence.Time.Day;


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
            yearToFirstDay :: Year -> Day;
            yearToFirstDay year = fromOrdinalDate year 1;
        } in enumSurjection yearOfDay yearToFirstDay;
    } in pieceEverySurjection (dayToYear . timeToDay);

    aYear :: Recurrence;
    aYear = PeriodRecurrence $ fmap Just theYear;

    dayEachYear :: (Year -> Maybe Day) -> PiecePartialFunction T Year;
    dayEachYear getDay = pieceMapSurjection timeToDay $ piecePartialEnumPoint $ pointEveryInjection $ ordInjection getDay yearOfDay;


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
}
