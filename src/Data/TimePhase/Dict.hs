module Data.TimePhase.Dict (dict) where
{
    import Data.Fixed;
    import Data.Time;
    import Data.TimePhase.Value;
    import Data.SetSearch;

    startOf :: TimePhase -> PointSet T;
    startOf ps = union (psExceptions ps) (intervalsStartOf (psIntervals ps));

    endOf :: TimePhase -> PointSet T;
    endOf ps = union (psExceptions ps) (intervalsEndOf (psIntervals ps));

    to :: TimePhase -> TimePhase -> Intervals T;
    to pa pb = let {?first = firstTime} in
        intervalsFromTo (startOf pa) (endOf pb);
    
    delay :: NominalDiffTime -> TimePhase -> TimePhase;
    delay dt = remapBase (addLocalTime dt) (addLocalTime (negate dt));

    midnights :: KnownPointSet T;
    midnights = MkKnownPointSet
    {
        kpsMember = \t -> (localTimeOfDay t == midnight),
        kpsFirstAfter = \t -> let
        {
            t' = LocalTime
            {
                localDay = addDays 1 (localDay t),
                localTimeOfDay = midnight
            };
        } in Just t',
        kpsLastBefore = \t -> let
        {
            today = localDay t;
            t' = LocalTime
            {
                localDay = if localTimeOfDay t > midnight
                 then today
                 else addDays (-1) today,
                localTimeOfDay = midnight
            };
        } in Just t'
    };
    
    midnightOf :: Day -> LocalTime;
    midnightOf day = LocalTime
    {
        localDay = day,
        localTimeOfDay = midnight
    };
    
    midnightsBefore :: KnownPointSet Day -> KnownPointSet T;
    midnightsBefore kpsday = MkKnownPointSet
    {
        kpsMember = \t -> (localTimeOfDay t == midnight) && (kpsMember kpsday (localDay t)),
        kpsFirstAfter = \t -> do
        {
            day <- kpsFirstAfter kpsday (localDay t);
            return (midnightOf day);
        },
        kpsLastBefore = \t -> do
        {
            let {today = localDay t};
            day <- if (localTimeOfDay t > midnight) && (kpsMember kpsday today)
             then return today
             else kpsLastBefore kpsday today;
            return (midnightOf day);
        }
    };
 
    theDay :: StepFunction T Day;
    theDay = MkStepFunction
    {
        sfValue = localDay,
        sfPossibleChanges = knownToPointSet midnights
    };
    
    weekDay :: Integer -> Intervals T;
    weekDay i = fmap (\day -> mod' (toModifiedJulianDay day) 7 == i) theDay;
    
    monthFirsts :: KnownPointSet Day;
    monthFirsts = MkKnownPointSet
    {
        kpsMember = \day -> case toGregorian day of
        {
            (_,_,1) -> True;
            _ -> False;
        },
        kpsFirstAfter = \day -> Just (case toGregorian (addGregorianMonthsClip 1 day) of
        {
            (y,m,_) -> fromGregorian y m 1;
        }),
        kpsLastBefore = \day -> Just (case toGregorian (addDays (-1) day) of
        {
            (y,m,_) -> fromGregorian y m 1;
        })
    };
 
    yearAndMonth :: StepFunction T (Integer,Int);
    yearAndMonth = MkStepFunction
    {
        sfValue = (\day -> case toGregorian day of
        {
            (y,m,_) -> (y,m);
        }) . localDay,
        sfPossibleChanges = knownToPointSet (midnightsBefore monthFirsts)
    };

    isMonth :: Int -> Intervals T;
    isMonth i = fmap (\(_,m) -> i == m) yearAndMonth;

    dict :: String -> Maybe Value;

    dict "never" = Just (toValue (empty :: TimePhase));
    dict "always" = Just (toValue (full :: TimePhase));
    dict "not" = Just (toValue (invert :: TimePhase -> TimePhase));
    dict "when" = Just (toValue (intersectAll :: [TimePhase] -> TimePhase));
    dict "and" = Just (toValue (unionAll :: [TimePhase] -> TimePhase));
    dict "start" = Just (toValue startOf);
    dict "end" = Just (toValue endOf);
    dict "to" = Just (toValue to);
    
    dict "delay" = Just (toValue delay);

    dict "midnight" = Just (toValue (knownToPointSet midnights));

    dict "Wednesday" = Just (toValue (weekDay 0));
    dict "Thursday" = Just (toValue (weekDay 1));
    dict "Friday" = Just (toValue (weekDay 2));
    dict "Saturday" = Just (toValue (weekDay 3));
    dict "Sunday" = Just (toValue (weekDay 4));
    dict "Monday" = Just (toValue (weekDay 5));
    dict "Tuesday" = Just (toValue (weekDay 6));

    dict "January" = Just (toValue (isMonth 1));
    dict "February" = Just (toValue (isMonth 2));
    dict "March" = Just (toValue (isMonth 3));
    dict "April" = Just (toValue (isMonth 4));
    dict "May" = Just (toValue (isMonth 5));
    dict "June" = Just (toValue (isMonth 6));
    dict "July" = Just (toValue (isMonth 7));
    dict "August" = Just (toValue (isMonth 8));
    dict "September" = Just (toValue (isMonth 9));
    dict "October" = Just (toValue (isMonth 10));
    dict "November" = Just (toValue (isMonth 11));
    dict "December" = Just (toValue (isMonth 12));

    dict s = Nothing;
}
