module Autogestao.Driven.Monad (
  App (..),
  runApp,
  AppEnv,
) where

import Autogestao.Driving.Env (Env)
import Control.Monad.IO.Unlift (
  MonadUnliftIO,
 )

type AppEnv = Env App
newtype App a = App {unApp :: ReaderT AppEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader AppEnv)

runApp :: AppEnv -> App a -> IO a
runApp env = usingReaderT env . unApp
