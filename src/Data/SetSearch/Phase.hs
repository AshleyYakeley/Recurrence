module Data.SetSearch.Phase where
{
    import Data.SetSearch.PointSet;
    import Data.SetSearch.PhaseSet;
    
    data Phase a = MkPhase
    {
        phaseSet :: PhaseSet a,
        phaseChanges :: PointSet a
    };
}
