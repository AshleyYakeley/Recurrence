{-# OPTIONS -Wno-orphans #-}
module Data.SetSearch.PieceSet where
{
    import Data.Maybe;
    import Control.Applicative hiding (empty);
    import Data.SetSearch.Set;
    import Data.SetSearch.PointFunction;
    import Data.SetSearch.PointSet;
    import Data.SetSearch.DeltaSmaller;
    import Data.SetSearch.PieceFunction;
    import Data.SetSearch.PiecePartialFunction;

    type PieceSet t = PiecePartialFunction t ();

    instance (Ord a) => Set (PieceSet a) where
    {
        empty = pure empty;
        union = liftA2 union;
        diff = liftA2 diff;
        intersect = liftA2 intersect;
        symdiff = liftA2 symdiff;
    };

    instance (Ord a) => SetFull (PieceSet a) where
    {
        full = pure full;
        invert = fmap invert;
    };

    instance (Ord a) => SetMember (PieceSet a) where
    {
        member ps t = isJust $ pieceEval ps t;
    };

    pointIntersectPiece :: Ord t => PieceSet t -> PointFunction t a -> PointFunction t a;
    pointIntersectPiece ints pf = fmap snd $ pointPiecePartialBoth ints pf;

    pointDiffPiece :: (Ord a) => PieceSet a -> PointSet a -> PointSet a;
    pointDiffPiece i = pointIntersectPiece (invert i);

    pieceSetFromToPoints :: (Ord a,?first :: a) => Maybe () -> PointSet a -> PointSet a -> PieceSet a;
    pieceSetFromToPoints ambient psOn psOff = let
    {
        ff (EBBoth _ _) = ambient;
        ff (EBLeft _) = Just ();
        ff (EBRight _) = Nothing;
    } in pieceLatestPoint ambient $ fmap ff $ pointEitherBoth psOn psOff;

    pieceSetSingleInterval :: (Ord t,?first :: t) => t -> t -> PieceSet t;
    pieceSetSingleInterval t0 t1 = pieceSetFromToPoints Nothing (single t0) (single t1);

    -- | step after the first value in the point set
    ;
    pieceSetAfterPoint :: (?first :: a) => PointSet a -> PieceSet a;
    pieceSetAfterPoint ps = pieceLatestPoint Nothing $ fmap (const $ Just ()) ps;

    pieceSetAfterTime :: (Ord a) => a -> PieceSet a;
    pieceSetAfterTime start = MkPieceFunction
    {
        pieceEval = \t -> if t >= start then Just () else Nothing,
        pieceJoints = single start
    };

    pieceSetBeforeTime :: (Ord a) => a -> PieceSet a;
    pieceSetBeforeTime = invert . pieceSetAfterTime;

    pieceSetTimeInterval :: (Ord a) => a -> a -> PieceSet a;
    pieceSetTimeInterval start end = intersect (pieceSetAfterTime start) (pieceSetBeforeTime end);

    piecePartialToSet :: PiecePartialFunction t count -> PieceSet t;
    piecePartialToSet = fmap (fmap (const ()));

    piecePartialIdentifySet :: forall t. (?first::t,DeltaSmaller t) => PieceSet t -> PiecePartialFunction t t;
    piecePartialIdentifySet ps = let
    {
        ff :: (t,Maybe ()) -> Maybe t;
        ff (t,mu) = mu >> return t;

        changes :: PointFunction t (Maybe t);
        changes = fmap ff $ pointBase $ fmap snd $ pieceChanges ps;
    } in pieceLatestPoint Nothing changes;

    piecePartialBetweenPoint :: (?first :: t) => PointSet t -> PiecePartialFunction t t;
    piecePartialBetweenPoint ps = pieceLatestPoint Nothing $ fmap (\(t,_) -> Just t) $ pointBase ps;

    piecePartialOf :: forall t. (Ord t,?first :: t) => PieceSet t -> PointSet t -> PiecePartialFunction t t;
    piecePartialOf int ps = piecePartialLift (\() t -> t) int $ piecePartialBetweenPoint ps;

    pieceSetCountedOnAfter :: (Ord t,?first :: t) => Int -> PointFunction t a -> PointFunction t b -> PieceSet t;
    pieceSetCountedOnAfter n subject delimiter = let
    {
        memory = pointEitherBoth subject delimiter;
        thought Nothing = Left n;
        thought (Just (eb,0)) = Right $ case eb of
        {
            EBLeft _ -> Nothing; -- another subject, so no
            EBRight _ -> Just (); -- delimiter, so yes
            EBBoth _ _ -> Nothing; -- another subject on delimiter, so no
        };
        thought (Just (eb,i)) = case eb of
        {
            EBLeft _ -> Left (i - 1); -- another subject, so try next
            EBRight _ -> Right Nothing; -- delimiter too soon, so no
            EBBoth _ _ -> Right $ if i == 1 then Just () else Nothing; -- another subject on delimiter, so only if n is one
        };
    } in pieceRememberPoints Nothing (MkStateMachine thought) memory;

    pointCountedOnAfter :: (Ord t,?first :: t) => Int -> PointFunction t a -> PointFunction t b -> PointFunction t a;
    pointCountedOnAfter n subject delimiter = pointIntersectPiece (pieceSetCountedOnAfter n subject delimiter) subject;

    pieceCountedOnAfter :: (DeltaSmaller t,?first :: t,Eq a) => Int -> PiecePartialFunction t a -> PointSet t -> PiecePartialFunction t a;
    pieceCountedOnAfter n subject delimiter = fmap (fmap snd) $ piecePartialBoth (pieceSetCountedOnAfter n (piecePartialStarts subject) delimiter) subject;

    pieceSetCountedIn :: forall t a b. (DeltaSmaller t,?first :: t,Eq b) =>
        Int -> PointFunction t a -> PiecePartialFunction t b -> PieceSet t;
    pieceSetCountedIn n subject delimiter = let
    {
        memory :: PointFunction t (EitherBoth a (Maybe b,Maybe b));
        memory = pointEitherBoth subject (pieceChanges delimiter);

        thought :: Maybe (EitherBoth a (Maybe b,Maybe b),Int) -> Either Int (Maybe ());
        thought Nothing = Left n;
        thought (Just (eb,0)) = Right $ case eb of
        {
            EBLeft _ -> Nothing; -- another subject, so no
            EBRight (_,Just _) -> Just (); -- start delimiter, so yes
            EBRight (_,Nothing) -> Nothing; -- end-only delimiterm so no
            EBBoth _ _ -> Nothing; -- another subject on delimiter, so no
        };
        thought (Just (eb,i)) = case eb of
        {
            EBLeft _ -> Left (i - 1); -- another subject, so try next
            EBRight _ -> Right Nothing; -- delimiter too soon, so no
            EBBoth _ (_,Just _) -> Right $ if i == 1 then Just () else Nothing; -- another subject on start delimiter, so only if n is one
            EBBoth _ (_,Nothing) -> Right Nothing; -- end-only delimiterm so no
        };
    } in pieceRememberPoints Nothing (MkStateMachine thought) memory;

    pointCountedIn :: (DeltaSmaller t,?first :: t,Eq b) => Int -> PointSet t -> PiecePartialFunction t b -> PointSet t;
    pointCountedIn n subject delimiter = pointIntersectPiece (pieceSetCountedIn n subject delimiter) subject;

    pieceCountedIn :: (DeltaSmaller t,?first :: t,Eq a,Eq b) => Int -> PiecePartialFunction t a -> PiecePartialFunction t b -> PiecePartialFunction t a;
    pieceCountedIn n subject delimiter = fmap (fmap snd) $ piecePartialBoth (pieceSetCountedIn n (piecePartialStarts subject) delimiter) subject;
}
