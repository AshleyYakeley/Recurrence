module Language.SExpression where
{
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
    
    readSExpression :: forall a. ReadPrec a -> ReadPrec (SExpression a);
    readSExpression readAtom = readExp where
    {
        matchChar :: (Char -> Bool) -> ReadPrec ();
        matchChar match = do
        {
            found <- get;
            if match found then return () else pfail;
        };
        
        isChar :: Char -> ReadPrec ();
        isChar expected = matchChar (\found -> expected == found);
        
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
        
        ws :: Char -> Bool;
        ws ' ' = True;
        ws '\t' = True;
        ws '\n' = True;
        ws '\r' = True;
        ws '\f' = True;
        ws _ = False;
        
        readAnyWhiteSpace :: ReadPrec ();
        readAnyWhiteSpace = readAll_ (matchChar ws);
        
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
