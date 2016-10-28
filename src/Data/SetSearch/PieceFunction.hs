module Data.SetSearch.PieceFunction where
{
    import Data.SetSearch.Base;
    import Data.SetSearch.Set;
    import Data.SetSearch.MonotonicFunction;
    import Data.SetSearch.PointFunction;
    import Data.SetSearch.PointSet;
    import Data.SetSearch.DeltaSmaller;

    data PieceFunction t a = MkPieceFunction
    {
        pieceEval :: t -> a,
        pieceJoints :: PointSet t
    };

    instance Functor (PieceFunction t) where
    {
        fmap bc sf = MkPieceFunction
        {
            pieceEval = bc . (pieceEval sf),
            pieceJoints = pieceJoints sf
        };
    };

    instance (Ord t) => Applicative (PieceFunction t) where
    {
        pure a = MkPieceFunction
        {
            pieceEval = pure a,
            pieceJoints = empty
        };

        sfbc <*> sfb = MkPieceFunction
        {
            pieceEval = (pieceEval sfbc) <*> (pieceEval sfb),
            pieceJoints = union (pieceJoints sfbc) (pieceJoints sfb)
        };
    };

    instance BasedOn (PieceFunction t a) where
    {
        type Base (PieceFunction t a) = t;
    };

    instance RemapBase (PieceFunction a x) (PieceFunction b x) where
    {
        remapBase ab ba sfa = MkPieceFunction
        {
            pieceEval = remapBase ab ba (pieceEval sfa),
            pieceJoints = remapBase ab ba (pieceJoints sfa)
        };
    };

    pieceChanges :: (DeltaSmaller a,Eq b) => PieceFunction a b -> PointFunction a (b,b);
    pieceChanges MkPieceFunction{..} = let
    {
        values a _ = let
        {
            ma' = deltaSmaller a;
            upper = pieceEval a;
            lower x = case pointsExcluding pieceJoints (x,a) of
            {
                [] -> pieceEval x;
                (x',_):_ -> lower x';
            };
        }
        in case ma' of
        {
            Nothing -> Nothing;
            Just a' -> if lower a' == upper then Nothing else Just (lower a',upper);
        }
    }
    in pointCollect values pieceJoints;

    pieceChangeSet :: (DeltaSmaller a,Eq b) => PieceFunction a b -> PointSet a;
    pieceChangeSet sf = pointSet $ pieceChanges sf;

    pieceMatchChanges :: (DeltaSmaller t,Eq a) => ((a,a) -> Maybe b) -> PieceFunction t a -> PointFunction t b;
    pieceMatchChanges match sf = pointCollect (\_ b -> match b) $ pieceChanges sf;

    data StateMachine a b = forall s. MkStateMachine (Maybe (a,s) -> Either s b);

    -- | search pastwards, using the state machine
    ;
    pieceRememberPoints :: (?first :: t) => b -> StateMachine a b -> PointFunction t a -> PieceFunction t b;
    pieceRememberPoints def (MkStateMachine tf) pf@(MkPointFunction f) = let
    {
        runState _pts (Right b) = b;
        runState ((_,a):rest) (Left s) = runState rest $ tf $ Just (a,s);
        runState [] (Left _s) = def;

        pieceEval t = runState (f t ?first) (tf Nothing);

        pieceJoints = pointSet pf;
    } in MkPieceFunction {..};

    pieceLatestPoint :: (?first :: t) => a -> PointFunction t a -> PieceFunction t a;
    pieceLatestPoint def = let
    {
        machine Nothing = Left ();
        machine (Just (a,())) = Right a;
    } in pieceRememberPoints def $ MkStateMachine machine;

    pieceMapInjection :: (Ord tp,Ord tq) => MonotonicInjection tp tq -> PieceFunction tp a -> PieceFunction tq a;
    pieceMapInjection mi MkPieceFunction{..} = MkPieceFunction
    {
        pieceEval = \tq -> pieceEval $ fst $ miBack mi tq,
        pieceJoints = pointMapInjection mi $ pieceJoints
    };

    pieceMapSurjection :: (Ord tp,Ord tq) => MonotonicSurjection tp tq -> PieceFunction tq a -> PieceFunction tp a;
    pieceMapSurjection ms MkPieceFunction{..} = MkPieceFunction
    {
        pieceEval = \tp -> pieceEval $ msEval ms tp,
        pieceJoints = let
        {
            -- this injection is "the first tp for this tq"
            miEval q = Just $ fst $ msImage ms q;
            miBack p = let
            {
                q0 = msEval ms p;
                (p0,p1) = msImage ms q0; -- the closed-open interval of tp that have the same map as p
                -- already know: msEval p0 = q0
                q1 = if p0 == p then q0 else msEval ms p1;
            } in (q0,q1);
            startInjection = MkMonotonicInjection {..};
        } in pointMapInjection startInjection $ pieceJoints
    };

    pieceEveryEnum :: (Ord t,Enum t) => PieceFunction t t;
    pieceEveryEnum = let
    {
        pieceEval = id;
        pieceJoints = pointSet pointEveryEnum;
    } in MkPieceFunction{..};

    pieceEveryInjection :: (Ord tp,Ord tq,Enum tp) => MonotonicInjection tp tq -> PieceFunction tq tp;
    pieceEveryInjection mi = pieceMapInjection mi pieceEveryEnum;

    pieceEverySurjection :: (Ord tp,Ord tq,Enum tq) => MonotonicSurjection tp tq -> PieceFunction tp tq;
    pieceEverySurjection ms = pieceMapSurjection ms pieceEveryEnum;
{-
    pieceSurjection :: () => MonotonicSurjection t a -> PieceFunction t a;
    pieceSurjection ms = let
    {
        pieceEval = msEval ms;
        pieceJoints = MkPointFunction $ \t0 t1 -> let
        {
            msSameImage ms :: t -> (t,t)

            runForwards t =

            ts = if t0 > t1 then runBackwards t0 else runForwards t0;
        } in fmap (\t -> (t,())) ts;
    } in MkPieceFunction{..};
-}
}
