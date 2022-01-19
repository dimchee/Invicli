module Src.Utils (
    module Src.Utils,
    module Control.Monad.Except
) where

import qualified Prelude
import Prelude hiding (last, head)

import Control.Monad.Except
import Data.List.NonEmpty
import Data.Functor ((<&>))

instance Monad m => Semigroup (ExceptT err m a) where
    x <> y = ExceptT $ do
        x <- runExceptT x
        case x of
            Right x -> return $ Right x
            Left _ -> do
                y <- runExceptT y
                case y of
                    Right x -> return $ Right x
                    Left err -> return $ Left err

failWith :: Monad m => e -> Maybe a -> ExceptT e m a
failWith err may = failWithE err $ ExceptT . return . Right <$> may

failWithM :: Monad m => e -> Maybe (m a) -> ExceptT e m a
failWithM err may = failWithE err $ (\mx -> ExceptT $ mx <&> Right) <$> may

failWithE :: Monad m => e -> Maybe (ExceptT e m a) -> ExceptT e m a
failWithE err may = case may of
    Nothing -> ExceptT $ return $ Left err
    Just ex -> ex

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left  x) = Left $ f x
mapLeft _ (Right x) = Right x

report :: Show e => ExceptT e IO () -> IO ()
report x = do
    e <- runExceptT x
    case e of
        Left err -> print err
        Right x -> return ()
tryIO:: (Show e, Monoid a) => ExceptT e IO a -> IO a
tryIO x = do
    e <- runExceptT x
    case e of
        Left err -> print err >>= return mempty
        Right x -> return x

onError :: String -> ExceptT e IO a -> ExceptT e IO a
onError message x = ExceptT $ do
    e <- runExceptT x
    case e of
        Left _  -> print message >> return e
        Right _ -> return e
