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

    matchChar :: (Char -> Bool) -> ReadPrec Char;
    matchChar match = do
    {
        found <- get;
        if match found then return found else pfail;
    };
    
    isChar :: Char -> ReadPrec ();
    isChar expected = matchChar (\found -> expected == found) >> return ();
    
    readAll :: forall b. ReadPrec b -> ReadPrec [b];
    readAll reader = (do
        {
            b <- reader;
            bs <- readAll reader;
            return (b:bs);
        }) <++ (return []);
    
    readAll_ :: forall b. ReadPrec b -> ReadPrec ();
    readAll_ reader = (do
        {
            _ <- reader;
            readAll_ reader;
        }) <++ (return ());
    
    readAnyWhiteSpace :: ReadPrec ();
    readAnyWhiteSpace = readAll_ (matchChar isSpace);

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
            isChar '(';
            exps <- readAll readExp;
            readAnyWhiteSpace;
            isChar ')';
            readAnyWhiteSpace;
            return exps;
        };
    };
    
    instance Read a => Read (SExpression a) where
    {
        readPrec = readSExpression readPrec;
    };
}
