module Data.SetSearch.StepFunction where
{
    import Control.Applicative hiding (empty);
    import Data.SetSearch.Set;
    import Data.SetSearch.PointSet;
    import Data.SetSearch.DeltaSmaller;
    
    data StepFunction a b = MkStepFunction
    {
        sfValue :: a -> b,
        sfPossibleChanges :: PointSet a
    };
    
    instance Functor (StepFunction a) where
    {
        fmap bc sf = MkStepFunction
        {
            sfValue = bc . (sfValue sf),
            sfPossibleChanges = sfPossibleChanges sf
        };
    };
    
    instance (Ord a) => Applicative (StepFunction a) where
    {
        pure b = MkStepFunction
        {
            sfValue = pure b,
            sfPossibleChanges = empty
        };
        
        sfbc <*> sfb = MkStepFunction
        {
            sfValue = (sfValue sfbc) <*> (sfValue sfb),
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
            sfValue = remapBase ab ba (sfValue sfa),
            sfPossibleChanges = remapBase ab ba (sfPossibleChanges sfa)
        };
    };
    
    sfChanges :: (DeltaSmaller a,Eq b) => StepFunction a b -> PointSet a;
    sfChanges sf = filterIntersect (\a -> case deltaSmaller a of
    {
        Just a' -> case pointsLastBeforeUntil (sfPossibleChanges sf) a a' of
        {
            Just a'' -> sfValue sf a'' /= sfValue sf a;
            Nothing -> sfValue sf a' /= sfValue sf a;
        };
        Nothing -> False;
    }) (sfPossibleChanges sf);
    
    sfMatchPossibleChanges :: (Ord a) => StepFunction a b -> (b -> Bool) -> PointSet a;
    sfMatchPossibleChanges sf match = filterIntersect (match . (sfValue sf)) (sfPossibleChanges sf);
    
    sfMatchChanges :: (DeltaSmaller a,Eq b) => StepFunction a b -> (b -> Bool) -> PointSet a;
    sfMatchChanges sf match = filterIntersect (match . (sfValue sf)) (sfChanges sf);

    sfCountSince :: (Ord a,?first :: a) => PointSet a -> PointSet a -> StepFunction a (Maybe Int);
    sfCountSince delimiter subject = MkStepFunction
    {
        sfValue = \a -> do
        {
            lastdel <- if member delimiter a
             then return a
             else pointsLastBefore delimiter a;
            let
            {
                count t = case pointsLastBeforeUntil subject t lastdel of
                {
                    Nothing -> 0;
                    Just t' -> 1 + (count t');
                };
            };
            return ((count a) + (if member subject a then 1 else 0));
        },
        sfPossibleChanges = union delimiter subject
    };
}
