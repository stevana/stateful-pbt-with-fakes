module Opaque where

newtype Opaque a = Opaque { unOpaque :: a }
  deriving Eq

instance Show (Opaque a) where
  show _ = "<opaque>"
