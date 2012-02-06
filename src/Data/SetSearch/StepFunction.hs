module Data.SetSearch.StepFunction where
{
    import Control.Applicative hiding (empty);
    import Data.SetSearch.Set;
    import Data.SetSearch.PointSet;
    import Data.SetSearch.DeltaSmaller;
    import Data.SetSearch.Cut;
    import Data.SetSearch.ValueSet;
    
    data StepFunction a b = MkStepFunction
    {
        sfUpwardValue :: Cut a -> b,
        sfPossibleChanges :: PointSet (Cut a)
    };
    
    sfValue :: StepFunction a b -> a -> b;
    sfValue sf a = sfUpwardValue sf (justBefore a);
    
    instance Functor (StepFunction a) where
    {
        fmap bc sf = MkStepFunction
        {
            sfUpwardValue = bc . (sfUpwardValue sf),
            sfPossibleChanges = sfPossibleChanges sf
        };
    };
    
    instance (Ord a) => Applicative (StepFunction a) where
    {
        pure b = MkStepFunction
        {
            sfUpwardValue = pure b,
            sfPossibleChanges = empty
        };
        
        sfbc <*> sfb = MkStepFunction
        {
            sfUpwardValue = (sfUpwardValue sfbc) <*> (sfUpwardValue sfb),
            sfPossibleChanges = union (sfPossibleChanges sfbc) (sfPossibleChanges sfb)
        };
    };
    
    instance BasedOn (StepFunction a b) where
    {
        type Base (StepFunction a b) = a;
    };
    
    instance RemapBase (StepFunction a x) (StepFunction b x) where
    {
        remapBase ab ba sfa = MkStepFunction
        {
            sfUpwardValue = remapBase (remapBase ab ba) (remapBase ba ab) (sfUpwardValue sfa),
            sfPossibleChanges = remapBase (remapBase ab ba) (remapBase ba ab) (sfPossibleChanges sfa)
        };
    };
    
    sfCutBefore :: (Ord a) => StepFunction a b -> StepFunction (Cut a) b;
    sfCutBefore sf = MkStepFunction
    {
        sfUpwardValue = \(MkCut ca _) -> sfUpwardValue sf ca,
        sfPossibleChanges = pointsCutBefore (sfPossibleChanges sf)
    };
    
    sfChanges :: (DeltaSmaller a,Eq b) => StepFunction a b -> PointSet (Cut a);
    sfChanges sf = filterIntersect (\cuta -> case deltaSmaller cuta of
    {
        Just cuta' -> case lastBeforeUntil (sfPossibleChanges sf) cuta cuta' of
        {
            Just cuta'' -> sfUpwardValue sf cuta'' /= sfUpwardValue sf cuta;
            Nothing -> sfUpwardValue sf cuta' /= sfUpwardValue sf cuta;
        };
        Nothing -> False;
    }) (sfPossibleChanges sf);
   
    sfMatchUpwardChanges :: (DeltaSmaller a,Eq b) => StepFunction a b -> (b -> Bool) -> PointSet (Cut a);
    sfMatchUpwardChanges sf match = filterIntersect (match . (sfUpwardValue sf)) (sfChanges sf);

    -- | The number of subjects since (just before) delimiter
    sfCountSince :: (Ord a,?first :: Cut a) => PointSet (Cut a) -> PointSet (Cut a) -> StepFunction a (Maybe Int);
    sfCountSince delimiter subject = MkStepFunction
    {
        sfUpwardValue = \cut -> do
        {
            dcut <- let {?first = justBefore ?first} in psPrevious delimiter (justAfter cut);
            return (length (vsForwards (psValues subject dcut cut)));
        },
        sfPossibleChanges = union delimiter subject
    };
}
