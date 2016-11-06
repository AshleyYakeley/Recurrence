module Main(main) where
{
    import Test.Tasty;
    import qualified Sets;
    import qualified PointSet;
    import qualified Interval;
    import qualified Golden;
    import qualified Examples;


    main :: IO ();
    main = do
    {
        exampleTests <- Examples.makeTests;
        defaultMain $ testGroup "recurrence"
        [
            Sets.tests,
            PointSet.tests,
            Interval.tests,
            Golden.tests,
            exampleTests
        ];
    };
}
