module Periodic.Worker
  ( WK
  , WorkerT
  , runWorkerT
  , addFunc
  , work

  , Job
  , JobT
  , done
  , fail
  , schedLater
  , funcName
  , name
  , workload
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Data.Newtype (class Newtype)
import Control.Monad.Reader (ReaderT, runReaderT)

foreign import data Job :: Type
foreign import data Worker :: Type
foreign import newWorker :: forall a. a -> Effect Worker
foreign import _work :: Worker -> Int -> Effect Unit
foreign import _addFunc :: Worker -> String -> (Job -> Effect Unit) -> Effect Unit

foreign import _done :: Job -> Effect Unit
foreign import _fail :: Job -> Effect Unit
foreign import _schedLater :: Job -> Int -> Effect Unit

foreign import _funcName :: Job -> Effect String
foreign import _name :: Job -> Effect String
foreign import _workload :: Job -> Effect String

data WK m = WK (m Unit -> Effect Unit) Worker

newtype WorkerT m a = WorkerT (ReaderT (WK m) m a)
newtype JobT m a = JobT (ReaderT Job m a)

runWorkerT
  :: forall a m. MonadEffect m
   => (m Unit -> Effect Unit)
   -> a -> WorkerT m Unit -> m Unit
runWorkerT runEff a (WorkerT m) = do
  w <- liftEffect $ newWorker a
  runReaderT m (WK runEff w)

runJobT
  :: forall a m. MonadEffect m
  => Job -> JobT m a -> m a
runJobT a (JobT m) = runReaderT m a

derive instance newtypeWorkerT :: Newtype (WorkerT m a) _

instance functorWorkerT :: Functor m => Functor (WorkerT m) where
  map f (WorkerT m) = WorkerT $ map f m

instance applyWorkerT :: Monad m => Apply (WorkerT m) where
  apply = ap

instance applicativeWorkerT :: Monad m => Applicative (WorkerT m) where
  pure = WorkerT <<< pure

instance bindWorkerT :: Monad m => Bind (WorkerT m) where
  bind (WorkerT m) k = WorkerT $ do
    a <- m
    case k a of
      WorkerT b -> b

instance monadWorkerT :: Monad m => Monad (WorkerT m)

instance monadTransWorkerT :: MonadTrans (WorkerT) where
  lift = WorkerT <<< lift

instance monadEffectWorkerT :: MonadEffect m => MonadEffect (WorkerT m) where
  liftEffect = lift <<< liftEffect

instance monadAffWorkerT
  :: MonadAff m
  => MonadAff (WorkerT m) where
  liftAff = lift <<< liftAff

instance monadAskWorkerT :: Monad m => MonadAsk (WK m) (WorkerT m) where
  ask = WorkerT ask

derive instance newtypeJobT :: Newtype (JobT m a) _

instance functorJobT :: Functor m => Functor (JobT m) where
  map f (JobT m) = JobT $ map f m

instance applyJobT :: Monad m => Apply (JobT m) where
  apply = ap

instance applicativeJobT :: Monad m => Applicative (JobT m) where
  pure = JobT <<< pure

instance bindJobT :: Monad m => Bind (JobT m) where
  bind (JobT m) k = JobT $ do
    a <- m
    case k a of
      JobT b -> b

instance monadJobT :: Monad m => Monad (JobT m)

instance monadTransJobT :: MonadTrans JobT where
  lift = JobT <<< lift

instance monadEffectJobT :: MonadEffect m => MonadEffect (JobT m) where
  liftEffect = lift <<< liftEffect

instance monadAffJobT :: MonadAff m => MonadAff (JobT m) where
  liftAff = lift <<< liftAff

instance monadAskJobT :: Monad m => MonadAsk Job (JobT m) where
  ask = JobT ask

addFunc
  :: forall m. MonadEffect m
  => String -> JobT m Unit -> WorkerT m Unit
addFunc func m = do
  (WK runEff w) <- ask
  liftEffect $ _addFunc w func $ \job ->
    runEff $ runJobT job m

work
  :: forall m. MonadEffect m
  => Int -> WorkerT m Unit
work size = do
  (WK _ w) <- ask
  liftEffect $ _work w size

done
  :: forall m. MonadEffect m
  => JobT m Unit
done = liftEffect <<< _done =<< ask

fail
  :: forall m. MonadEffect m
  => JobT m Unit
fail = liftEffect <<< _fail =<< ask

schedLater
  :: forall m. MonadEffect m
  =>  Int -> JobT m Unit
schedLater delay = liftEffect <<< flip _schedLater delay =<< ask

funcName
  :: forall m. MonadEffect m
  => JobT m String
funcName = liftEffect <<< _funcName =<< ask

name
  :: forall m. MonadEffect m
  => JobT m String
name = liftEffect <<< _name =<< ask

workload
  :: forall m. MonadEffect m
  => JobT m String
workload = liftEffect <<< _workload =<< ask
