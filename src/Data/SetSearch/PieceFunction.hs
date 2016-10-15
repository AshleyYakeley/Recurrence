module Data.SetSearch.PieceFunction where
{
    import Data.SetSearch.Base;
    import Data.SetSearch.Set;
    import Data.SetSearch.PointFunction;
    import Data.SetSearch.PointSet;
    import Data.SetSearch.DeltaSmaller;

    data PieceFunction a b = MkPieceFunction
    {
        pieceEval :: a -> b,
        pieceJoints :: PointSet a
    };

    instance Functor (PieceFunction a) where
    {
        fmap bc sf = MkPieceFunction
        {
            pieceEval = bc . (pieceEval sf),
            pieceJoints = pieceJoints sf
        };
    };

    instance (Ord a) => Applicative (PieceFunction a) where
    {
        pure b = MkPieceFunction
        {
            pieceEval = pure b,
            pieceJoints = empty
        };

        sfbc <*> sfb = MkPieceFunction
        {
            pieceEval = (pieceEval sfbc) <*> (pieceEval sfb),
            pieceJoints = union (pieceJoints sfbc) (pieceJoints sfb)
        };
    };

    instance BasedOn (PieceFunction a b) where
    {
        type Base (PieceFunction a b) = a;
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
}
