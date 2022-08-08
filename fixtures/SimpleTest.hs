module SimpleTest where

class C a where
  c :: a

instance C Bool where
  c = False
