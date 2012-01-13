{-# LANGUAGE FlexibleInstances #-}
module Main where
{
    import Data.SetSearch;
     
    showBool :: Bool -> Char;
    showBool False = 'F';
    showBool True = 'T';

    showPS :: PhaseSet Int -> String;
    showPS (MkPhaseSet i x) = fmap showBool [member i 0,member x 0];

    testOne :: String -> Bool -> PhaseSet Int -> IO ();
    testOne name expected ps = if member ps 0 /= expected
     then putStrLn ("FAILED: "++name++"="++[showBool expected]++", got "++(showPS ps)) else return ();
    
    testPS context ps1 ps2 = let
    {
        v1 = member ps1 0;
        v2 = member ps2 0;
    } in do
    {
        testOne (context ++ "union") (v1 || v2) (union ps1 ps2);
        testOne (context ++ "intersect") (v1 && v2) (intersect ps1 ps2);
        testOne (context ++ "diff") (v1 && not v2) (diff ps1 ps2);
    };
    
    test i1 x1 i2 x2 = let
    {
        ps1 = MkPhaseSet
        {
            psIntervals = if i1 then full else empty,
            psExceptions = if x1 then single 0 else empty
        };
        ps2 = MkPhaseSet
        {
            psIntervals = if i2 then full else empty,
            psExceptions = if x2 then single 0 else empty
        };
        context = [showBool i1,showBool x1] ++ ":" ++ [showBool i2,showBool x2] ++ ":";
    } in testPS context ps1 ps2;

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
    main = doPair test;
}
