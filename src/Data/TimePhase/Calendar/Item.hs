module Data.TimePhase.Calendar.Item where
{
    import Data.TimePhase.Time;

    data Item = MkItem String TimePhase;

    type Calendar = [Item];
}
