module Language.SExpression where
{
    import Data.Char;
    import Data.List;
    import Text.Read;
    import Text.ParserCombinators.ReadPrec;

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

    readMatching :: (Char -> Bool) -> ReadPrec Char;
    readMatching match = do
    {
        found <- get;
        if match found then return found else pfail;
    };
    
    readThis :: Char -> ReadPrec ();
    readThis expected = readMatching (\found -> expected == found) >> return ();
    
    readOneOrMore :: forall b. ReadPrec b -> ReadPrec [b];
    readOneOrMore reader = do
    {
        b <- reader;
        bs <- readZeroOrMore reader;
        return (b:bs);
    };
    
    readZeroOrMore :: forall b. ReadPrec b -> ReadPrec [b];
    readZeroOrMore reader = (readOneOrMore reader) <++ (return []);
    
    readZeroOrMore_ :: forall b. ReadPrec b -> ReadPrec ();
    readZeroOrMore_ reader = (do
        {
            _ <- reader;
            readZeroOrMore_ reader;
        }) <++ (return ());
    
    readAnyWhiteSpace :: ReadPrec ();
    readAnyWhiteSpace = readZeroOrMore_ (readMatching isSpace);

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
