module QRN.Helpers where


type Err a = Either String a

fromErr :: Monad m => m (Err a) -> m a
fromErr io = do
  mx <- io
  case mx of
      Left msg -> error msg
      Right x  -> return x

fromErrWith :: Monad m => String -> m (Err a) -> m a
fromErrWith str io = do
  mx <- io
  case mx of
      Left msg -> error $ unlines [str,msg]
      Right x  -> return x

infixl 4 <$$>, <$$$>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<$$$>) = fmap . fmap . fmap
