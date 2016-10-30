module Data.SExpression
    (
    SExpression(..),
    readAnyWhiteSpace,
    ) where
{
    import Data.Char;
    import Data.List;
    import Text.Read;
    import Data.SExpression.Read;

    data SExpression a = AtomSExpression a | ListSExpression [SExpression a];

    deriving instance Eq a => Eq (SExpression a);

    instance Functor SExpression where
    {
        fmap ab (AtomSExpression a) = AtomSExpression (ab a);
        fmap ab (ListSExpression list) = ListSExpression (fmap (fmap ab) list);
    };

    instance Applicative SExpression where
    {
        pure = AtomSExpression;
        (AtomSExpression f) <*> expr = fmap f expr;
        (ListSExpression lfexp) <*> expr = ListSExpression $ fmap (\fexp -> fexp <*> expr) lfexp;
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
            (fmap ListSExpression readExpList) <++ (fmap AtomSExpression readAtom);
        };

        readExpList :: ReadPrec [SExpression a];
        readExpList = do
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
