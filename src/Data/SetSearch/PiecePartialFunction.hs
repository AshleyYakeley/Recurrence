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
    piecePartialIs phase t = isJust $ pieceEval phase t;

    piecePartialBoth :: (Ord t) => PiecePartialFunction t c1 -> PiecePartialFunction t c2 -> PiecePartialFunction t (c1,c2);
    piecePartialBoth = liftA2 (liftA2 (,));

    piecePartialEnumPoint :: (Ord t,Enum t) => PointFunction t a -> PiecePartialFunction t a;
    piecePartialEnumPoint pf = let
    {
        pieceEval = pointEval pf;
        pset = pointSet pf;
        pieceJoints = union pset $ remapBase succ pred pset;
    } in MkPieceFunction {..};
}
