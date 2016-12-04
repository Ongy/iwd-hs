{-# LANGUAGE TypeFamilies #-}
module Zipper
    ( Zipper

    , advance
    , goBack
    , modify
    , modifyF
    , mapF
    , appF
    , app
    )
where

import GHC.Exts (IsList(..))

-- The zipper type. Focused, lefts rights. 
-- Most right value in lefts is head, most left value in rights is head
data Zipper a = Zipper !a [a] [a]

instance IsList (Zipper a) where
    type Item (Zipper a) = a
    toList (Zipper x ys zs) = reverse ys ++ x : zs
    fromList [] = error "Couldn't build empty Zipper :("
    fromList (x:xs) = Zipper x [] xs

instance Functor Zipper where
    fmap f (Zipper x ys zs) = Zipper (f x) (fmap f ys) (fmap f zs)

advance :: Zipper a -> Zipper a
advance x@(Zipper _ [] []) = x
advance (Zipper x ys@(_:_) []) =
    let (z:zs) = reverse (x:ys)
     in Zipper z [] zs
advance (Zipper x ys (z:zs)) = Zipper z (x:ys) zs

goBack :: Zipper a -> Zipper a
goBack x@(Zipper _ [] []) = x
goBack (Zipper x [] ys@(_:_)) =
    let (z:zs) = reverse (x:ys)
     in Zipper z zs []
goBack (Zipper x (y:ys) zs) = Zipper y ys (x:zs)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Zipper x ys zs) = Zipper (f x) ys zs

modifyF :: Functor f => (a -> f a) -> Zipper a -> f (Zipper a)
modifyF f (Zipper x ys zs) = 
    (\v -> Zipper v ys zs) `fmap` f x

mapF :: (Bool -> a -> b) -> Zipper a -> Zipper b
mapF f (Zipper x ys zs) =
    Zipper (f True x) (map (f False) ys) (map (f False) zs)

appF :: (a -> b) -> Zipper a -> b
appF f (Zipper x _ _ ) = f x

app :: a -> Zipper a -> Zipper a
app y (Zipper x xs ys) = Zipper x xs (ys ++ [y])
