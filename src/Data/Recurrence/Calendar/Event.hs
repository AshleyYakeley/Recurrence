module Data.Recurrence.Calendar.Event(Event(..),allEvents) where
{
    import Data.SetSearch;
    import Data.Recurrence.Time;
    import Data.Recurrence.Interval;
    import Data.Recurrence.Calendar.Item;

    data Event a = MkEvent String (Interval a) deriving Eq;

    instance (Show a) => Show (Event a) where
    {
        show (MkEvent name int) = name ++ ": " ++ (show int);
    };

    instance BasedOn (Event a) where
    {
        type Base (Event a) = a;
    };

    allEvents :: Item -> T -> T -> [Event T];
    allEvents (MkItem name phase) cut limit = fmap (MkEvent name) $ allIntervals phase cut limit;
}
