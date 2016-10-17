module Sets(tests) where
{
    import Data.Searchable;
    import Data.SetSearch;
    import Test.Tasty;
    import Test.Tasty.HUnit;

    testOne :: (SetMember s, Base s ~ Int) => String -> Bool -> s -> TestTree;
    testOne name expected set = testCase name $ assertEqual "" expected (member set 0);

    testPS :: (SetMember s, Base s ~ Int) => s -> s -> [TestTree];
    testPS set1 set2 = let
    {
        v1 = member set1 0;
        v2 = member set2 0;
    } in
    [
        testOne "union" (v1 || v2) (union set1 set2),
        testOne "intersect" (v1 && v2) (intersect set1 set2),
        testOne "diff" (v1 && not v2) (diff set1 set2)
    ];

    testPoint :: (Bool,Bool) -> [TestTree];
    testPoint (v1,v2) = let
    {
        set1 :: PointSet Int = if v1 then single 0 else empty;
        set2 :: PointSet Int = if v2 then single 0 else empty;
    } in testPS set1 set2;

    testIntervals :: (Bool,Bool) -> [TestTree];
    testIntervals (v1,v2) = let
    {
        set1 :: PieceSet Int = if v1 then full else empty;
        set2 :: PieceSet Int = if v2 then full else empty;
    } in testPS set1 set2;

    class TestAll t where
    {
        testAll :: t -> [TestTree];
    };

    instance TestAll [TestTree] where
    {
        testAll = id;
    };

    instance (Show a,Finite a,TestAll t) => TestAll (a -> t) where
    {
        testAll at = fmap (\a -> testGroup (show a) (testAll (at a))) allValues;
    };

    tests :: TestTree;
    tests = testGroup "sets"
    [
        testGroup "points" $ testAll testPoint,
        testGroup "pieces" $ testAll testIntervals
    ];
}
