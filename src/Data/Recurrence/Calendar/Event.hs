module Data.Recurrence.Calendar.Event(Interval(..),Event(..),allEvents) where
{
    import Data.SetSearch;
    import Data.Recurrence.Time;
    import Data.Recurrence.Calendar.Item;

    data Interval a = MkInterval (Maybe a) (Maybe a) deriving Eq;

    instance (Show a) => Show (Interval a) where
    {
        show (MkInterval start end) = let
        {
            showIStart Nothing = "ongoing";
            showIStart (Just t) = show t;
            showIEnd Nothing = "whenever";
            showIEnd (Just t) = show t;
        } in (showIStart start) ++ " until " ++ (showIEnd end);
    };

    instance (Ord a) => Ord (Interval a) where
    {
        compare (MkInterval s1 e1) (MkInterval s2 e2) = let
        {
            compareStart Nothing Nothing = EQ;
            compareStart Nothing (Just _) = LT;
            compareStart (Just _) Nothing = GT;
            compareStart (Just a) (Just b) = compare a b;

            compareEnd Nothing Nothing = EQ;
            compareEnd Nothing (Just _) = GT;
            compareEnd (Just _) Nothing = LT;
            compareEnd (Just a) (Just b) = compare a b;

            sc = compareStart s1 s2;
        } in if sc == EQ then compareEnd e1 e2 else sc;
    };

    pointInterval :: t -> Interval t;
    pointInterval t = MkInterval (Just t) (Just t);

    data Event a = MkEvent String (Interval a) deriving Eq;

    instance (Show a) => Show (Event a) where
    {
        show (MkEvent name int) = name ++ ": " ++ (show int);
    };

    instance BasedOn (Event a) where
    {
        type Base (Event a) = a;
    };

    allIntervals :: TimePhase -> T -> T -> [Interval T];
    allIntervals EmptyTimeSet _ _ = [];
    allIntervals (InstantTimeSet ps) t0 limit = fmap pointInterval $ pointSetIncluding ps t0 limit;
    allIntervals (PeriodTimeSet pf) t0 limit = let
    {
        changes = pointsIncluding (pieceChanges pf) t0 limit;

        scan Nothing [] = [];
        scan (Just mt) [] = [MkInterval mt Nothing];
        scan Nothing ((t,(_,Just _)):rr) = scan (Just (Just t)) rr;
        scan Nothing ((_t,(_,Nothing)):rr) = scan Nothing rr;
        scan (Just mt) ((t,(_,Just _)):rr) = (MkInterval mt (Just t)):(scan (Just (Just t)) rr);
        scan (Just mt) ((t,(_,Nothing)):rr) = (MkInterval mt (Just t)):(scan Nothing rr);

        start = if piecePartialIs pf t0
          then if member (pieceChangeSet pf) t0
            then Nothing
            else Just $ Nothing
          else Nothing;
    } in scan start changes;

    allEvents :: Item -> T -> T -> [Event T];
    allEvents (MkItem name phase) cut limit = fmap (MkEvent name) $ allIntervals phase cut limit;
}
