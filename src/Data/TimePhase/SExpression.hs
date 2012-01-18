module Data.TimePhase.SExpression where
{
    import Data.Char;
    import Data.List;
    import Text.Read;
    import Text.ParserCombinators.ReadPrec;
    import Data.TimePhase.SExpression.Read;

    data SExpression a = AtomSExpression a | ListSExpression [SExpression a];
    
    instance Functor SExpression where
    {
        fmap ab (AtomSExpression a) = AtomSExpression (ab a);
        fmap ab (ListSExpression list) = ListSExpression (fmap (fmap ab) list);
    };
    
    showSExpression :: (a -> String) -> SExpression a -> String;
    showSExpression showAtom (AtomSExpression a) = showAtom a;
    showSExpression showAtom (ListSExpression list) = "(" ++ (intercalate " " (fmap (showSExpression showAtom) list)) ++ ")";
    
    instance Show a => Show (SExpression a) where
    {
        show = showSExpression show;
    };
   
    readComment :: ReadPrec ();
    readComment = do
    {
        readThis '#';
        readZeroOrMore_ (readMatching  (not . isLineBreak));
        (readMatching isLineBreak >> return ()) <++ readEnd;
    };
    
    readAnyWhiteSpace :: ReadPrec ();
    readAnyWhiteSpace = readZeroOrMore_ (readComment <++ (readMatching isSpace >> return ()));

    readSExpression :: forall a. ReadPrec a -> ReadPrec (SExpression a);
    readSExpression readAtom = readExp where
    {
        readExp :: ReadPrec (SExpression a);
        readExp = do
        {
            readAnyWhiteSpace;
            (fmap ListSExpression readList) <++ (fmap AtomSExpression readAtom);
        };
        
        readList :: ReadPrec [SExpression a];
        readList = do
        {
            readThis '(';
            exps <- readZeroOrMore readExp;
            readAnyWhiteSpace;
            readThis ')';
            readAnyWhiteSpace;
            return exps;
        };
    };
    
    instance Read a => Read (SExpression a) where
    {
        readPrec = readSExpression readPrec;
    };
}
