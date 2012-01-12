{-# LANGUAGE FlexibleInstances #-}
module Main where
{
    import Data.SetSearch;
     
    showBool :: Bool -> Char;
    showBool False = 'F';
    showBool True = 'T';

    showPS :: PhaseSet Int -> String;
    showPS (MkPhaseSet i a d) = fmap showBool [member i 0,member a 0,member d 0];

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
    
    test i1 a1 d1 i2 a2 d2 = let
    {
        ps1 = MkPhaseSet
        {
            psIntervals = if i1 then full else empty,
            psAdditions = if a1 then single 0 else empty,
            psDeletions = if d1 then single 0 else empty
        };
        ps2 = MkPhaseSet
        {
            psIntervals = if i2 then full else empty,
            psAdditions = if a2 then single 0 else empty,
            psDeletions = if d2 then single 0 else empty
        };
        context = [showBool i1,showBool a1,showBool d1] ++ ":" ++ [showBool i2,showBool a2,showBool d2] ++ ":";
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
