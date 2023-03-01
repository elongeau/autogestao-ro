{-# LANGUAGE InstanceSigs #-}

module Autogestao.Core.Has where

import GHC.Records (
  HasField (getField),
 )
import GHC.TypeLits (
  Symbol,
 )

-- | From https://github.com/kowainik/cake-slayer/blob/744f072c0eeaf50e43210e4b548705e1948e5a39/src/CakeSlayer/Has.hs
class Has field env where
  obtain :: env -> field

-- | General function to retrieve fields with 'Has' class.
grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}

newtype Field (s :: Symbol) env = Field
  { unField :: env
  }

instance forall s f env. (HasField s env f) => Has f (Field s env) where
  obtain :: Field s env -> f
  obtain = getField @s . unField
  {-# INLINE obtain #-}
