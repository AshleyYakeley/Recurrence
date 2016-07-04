{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
module Main where
{
    import Data.SetSearch;

    showBool :: Bool -> Char;
    showBool False = 'F';
    showBool True = 'T';

    testOne name expected set = if member set 0 /= expected
     then putStrLn ("FAILED: "++name++"="++[showBool expected]) else return ();

    testPS context set1 set2 = let
    {
        v1 = member set1 0;
        v2 = member set2 0;
    } in do
    {
        testOne (context ++ "union") (v1 || v2) (union set1 set2);
        testOne (context ++ "intersect") (v1 && v2) (intersect set1 set2);
        testOne (context ++ "diff") (v1 && not v2) (diff set1 set2);
    };

    testPoint v1 v2 = let
    {
        set1 :: PointSet Int = if v1 then single 0 else empty;
        set2 :: PointSet Int = if v2 then single 0 else empty;
        context = "Points:"++[showBool v1] ++ ":" ++ [showBool v2] ++ ":";
    } in testPS context set1 set2;

    testIntervals v1 v2 = let
    {
        set1 :: Intervals Int = if v1 then full else empty;
        set2 :: Intervals Int = if v2 then full else empty;
        context = "Intervals:"++[showBool v1] ++ ":" ++ [showBool v2] ++ ":";
    } in testPS context set1 set2;

    class DoPair t where
    {
        doPair :: t -> IO ();
    };

    instance DoPair (IO ()) where
    {
        doPair = id;
    };

    instance DoPair a => DoPair (Bool -> a) where
    {
        doPair t = do
        {
            doPair (t False);
            doPair (t True);
        };
    };

    main :: IO ();
    main = do
    {
        doPair testPoint;
        doPair testIntervals;
    };
}
