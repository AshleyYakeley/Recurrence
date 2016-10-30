module Data.Recurrence.Calendar.Item where
{
    import Data.Recurrence.Time;

    data Item = MkItem String Recurrence;

    type Calendar = [Item];
}
