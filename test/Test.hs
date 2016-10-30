module Main(main) where
{
    import Test.Tasty;
    import qualified Sets;
    import qualified PointSet;
    import qualified Interval;
    import qualified Golden;

    tests :: TestTree;
    tests = testGroup "recurrence"
    [
        Sets.tests,
        PointSet.tests,
        Interval.tests,
        Golden.tests
    ];

    main :: IO ();
    main = defaultMain tests;
}
