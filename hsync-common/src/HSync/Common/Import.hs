module HSync.Common.Import where

import Data.Text(Text,pack)

showT :: Show a => a -> Text
showT = pack . show

protect                     :: Monad m => m Bool -> m a -> m a -> m a
protect g thenCase elseCase = g >>= \b -> if b then thenCase else elseCase
