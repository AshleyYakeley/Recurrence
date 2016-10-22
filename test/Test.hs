module Main(main) where
{
    import Test.Tasty;
    import qualified Sets;
    import qualified PointSet;
    import qualified Item;
    import qualified Golden;

    tests :: TestTree;
    tests = testGroup "phase"
    [
        Sets.tests,
        PointSet.tests,
        Item.tests,
        Golden.tests
    ];

    main :: IO ();
    main = defaultMain tests;
}
