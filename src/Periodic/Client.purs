module Periodic.Client
  ( Client
  , newClient
  , submitJob
  , runJob
  , ping
  , status
  , dropFunc
  , removeJob
  , Job (..)
  ) where

import Prelude
import Effect (Effect)
import Data.Function.Uncurried (Fn2, runFn2)
import Effect.Aff (Aff)
import Effect.Aff.Compat (fromEffectFnAff, EffectFnAff)

foreign import data Client :: Type
foreign import _newClient :: forall a b. Fn2 a b (Effect Client)
foreign import _submitJob :: forall a. Client -> a -> EffectFnAff Unit
foreign import _runJob :: forall a. Client -> a -> EffectFnAff String
foreign import _ping :: Client -> EffectFnAff Boolean
foreign import _status :: forall a. Client -> EffectFnAff a
foreign import _dropFunc ::  Client -> String -> EffectFnAff Unit
foreign import _removeJob :: forall a. Client -> a -> EffectFnAff Unit

type Job opts = { name :: String, func :: String | opts}
  -- workload :: String
  -- sched_at :: Int

newClient :: forall a b. a -> b -> Effect Client
newClient a b = runFn2 _newClient a b

submitJob :: forall a. Client -> Job a -> Aff Unit
submitJob c = fromEffectFnAff <<< _submitJob c

runJob :: forall a. Client -> Job a -> Aff String
runJob c = fromEffectFnAff <<< _runJob c

ping ::  Client -> Aff Boolean
ping = fromEffectFnAff <<< _ping

status :: forall a. Client -> Aff a
status = fromEffectFnAff <<< _status

dropFunc ::  Client -> String -> Aff Unit
dropFunc c = fromEffectFnAff <<< _dropFunc c

removeJob :: forall a. Client -> Job a -> Aff Unit
removeJob c = fromEffectFnAff <<< _removeJob c
