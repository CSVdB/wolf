{-# LANGUAGE DeriveGeneric #-}

module Wolf.Data.Export.Result where

import Import

data Result w e a
    = Success a
    | Warning w
              a
    | Error e
    deriving (Generic)

instance Functor (Result w e) where
    fmap f (Success a) = Success $ f a
    fmap f (Warning w a) = Warning w $ f a
    fmap f (Error e) = Error e

instance Monoid w => Applicative (Result w e) where
    pure = Success
    (<*>) (Success f) (Success a) = Success $ f a
    (<*>) (Warning w f) (Success a) = Warning w $ f a
    (<*>) (Success f) (Warning w a) = Warning w $ f a
    (<*>) (Warning w f) (Warning w2 a) = Warning (mappend w w2) $ f a
    (<*>) (Error e) _ = Error e
    (<*>) _ (Error e) = Error e

instance Monoid w => Monad (Result w e) where
    (>>=) (Success a) f = f a
    (>>=) (Warning w a) f =
        case f a of
            Success x -> Warning w x
            Warning w2 x -> Warning (mappend w w2) x
            Error e -> Error e
    (>>=) (Error e) _ = Error e

instance (Eq a, Eq w, Eq e) => Eq (Result w e a)

instance (Show a, Show w, Show e) => Show (Result w e a)

instance (Validity a, Validity w, Validity e) => Validity (Result w e a)
