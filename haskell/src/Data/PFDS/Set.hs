{-# LANGUAGE MultiParamTypeClasses #-}

module Data.PFDS.Set (Set(..)) where

class Set s a where
  empty :: s a
  insert :: a -> s a -> s a
  member :: a -> s a -> Bool
