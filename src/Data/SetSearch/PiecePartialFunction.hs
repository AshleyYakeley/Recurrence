module Data.SetSearch.PiecePartialFunction where
{
    import Data.Maybe;
    import Control.Applicative;
    import Data.SetSearch.Base;
    import Data.SetSearch.Set;
    import Data.SetSearch.DeltaSmaller;
    import Data.SetSearch.PointFunction;
    import Data.SetSearch.PointSet;
    import Data.SetSearch.PieceFunction;

    type PiecePartialFunction t count = PieceFunction t (Maybe count);

    piecePartialStarts :: (DeltaSmaller t,Eq a) => PiecePartialFunction t a -> PointFunction t a;
    piecePartialStarts = pieceMatchChanges $ \(_,a) -> a;

    piecePartialEnds :: (DeltaSmaller t,Eq a) => PiecePartialFunction t a -> PointFunction t a;
    piecePartialEnds = pieceMatchChanges $ \(a,_) -> a;

    piecePartialEmpty :: Ord t => PiecePartialFunction t count;
    piecePartialEmpty = pure Nothing;

    piecePartialConst :: Ord t => count -> PiecePartialFunction t count;
    piecePartialConst count = pure (Just count);

    piecePartialIs :: PiecePartialFunction t count -> t -> Bool;
    piecePartialIs ppf t = isJust $ pieceEval ppf t;

    piecePartialLift :: (Ord t) => (a -> b -> c) -> PiecePartialFunction t a -> PiecePartialFunction t b -> PiecePartialFunction t c;
    piecePartialLift abc = liftA2 (liftA2 abc);

    piecePartialBoth :: (Ord t) => PiecePartialFunction t c1 -> PiecePartialFunction t c2 -> PiecePartialFunction t (c1,c2);
    piecePartialBoth = piecePartialLift (,);

    piecePartialEnumPoint :: (Ord t,Enum t) => PointFunction t a -> PiecePartialFunction t a;
    piecePartialEnumPoint pf = let
    {
        pieceEval = pointEval pf;
        pset = pointSet pf;
        pieceJoints = union pset $ remapBase succ pred pset;
    } in MkPieceFunction {..};

    pointPiecePartialBoth :: forall t a b. Ord t => PiecePartialFunction t a -> PointFunction t b -> PointFunction t (a,b);
    -- pointPiecePartialBoth ints pf = pointCollect (\t b -> fmap (\a -> (a,b)) (pieceEval ints t)) pf; -- naive (slow) implementation
    pointPiecePartialBoth MkPieceFunction{..} (MkPointFunction ttltb) = MkPointFunction $ \t0 t1 -> let
    {
        MkPointFunction ttltu = pieceJoints;

        tb_tab :: a -> (t,b) -> (t,(a,b));
        tb_tab a (t,b) = (t,(a,b));

        op :: t -> t -> Bool;
        op = if t1 >= t0 then (<) else (>=);

        combine _ _ [] = [];
        combine Nothing [] _ = [];
        combine (Just a) [] ltb = fmap (tb_tab a) ltb;
        combine state js@((tj,_):_) (tb:ltb) | (fst tb) `op` tj = case state of
        {
            Just a -> (tb_tab a tb):(combine state js ltb);
            Nothing -> combine state js ltb;
        };
        combine _ (_:js) ltb@((t,_):_) = combine (pieceEval t) js ltb;

    } in combine (pieceEval t0) (ttltu t0 t1) (ttltb t0 t1);
}
