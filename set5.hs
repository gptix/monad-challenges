{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude
import Numeric (readHex)
import Data.List.Split (chunksOf)

hed :: [a] -> a
hed [] = undefined
hed (a:as) = a

hexDecode :: String -> String
hexDecode = map (toEnum . fst . hed . readHex) . (chunksOf 2)

hD = hexDecode



-------------------------------------------

seed1 = mkSeed 1



fiveRands :: [Integer]
fiveRands = let (r1,s2) = rand seed1
                (r2,s3) = rand s2
                (r3,s4) = rand s3
                (r4,s5) = rand s4
                (r5,unneeded) = rand s5
                in [r1,r2,r3,r4,r5]

type Gen a = Seed -> (a, Seed)

randLetter :: Gen Char
--randLetter :: Seed -> (Char, Seed)
randLetter s = let (r,newSeed) = rand s
                   in (toLetter r, newSeed)

--randLetter' :: Seed -> (Integer -> Char) -> Gen Char
randLetter' = let f = toLetter in \s -> let (r,ns) = rand s
                                        in (f r, ns)

--randF :: Gen Char 
randF = \f -> randLetter'


foo fn seed = let (r,ns) = rand seed
                  in (fn r, ns)

foo' fn = \seed -> let (r,ns) = rand seed
                   in (fn r, ns)

randLetter'' = foo' toLetter


randString3 :: String
randString3 = let (c1,s2) = rand seed1
                  (c2,s3) = rand s2
                  (c3,unneeded) = rand s3
              in map toLetter [c1,c2,c3]


randEven s = let (r, ns) = rand s
             in ( 2 * r, ns)


randOdd s = let (r, ns) = randEven s
            in (r + 1, ns)

randTen s = let (r, ns) = randEven s
            in (5 * r, s)

randEven' = foo' (*2)

randOdd' = foo' (\x -> (2 * x) +1)

randTen' = foo' (*10)

foo'' :: (a -> b) -> Gen a -> Gen b
foo'' f1 monadConstructor = \x -> let (a, y) = monadConstructor x
                        in (f1 a, y)
               

-- * from Set 1


{-

mkGen :: a -> Gen a
generalA f s = let (r, ns) = rand s
               in (f r, ns)
generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair2 :: Gen a -> Gen b -> (a -> b -> c) -> Gen c
genTwo :: Gen a -> (a -> Gen b) -> Gen b


* from Set 2


chain :: (a -> Maybe b) -> Maybe a -> Maybe b
link :: Maybe a -> (a -> Maybe b) -> Maybe b
yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
mkMaybe :: a -> Maybe a



* Commonalities
A maker:
mkFoo :: a -> Foo a
mkFoo a = Foo a
-- or mkFoo = Foo

A linker
Foo a -> (a -> Foo b) -> Foo b



-}

--makeM :: a -> m a

--linkM :: m a -> (a -> m b) -> m b

--yLinkM :: (a -> b -> c) -> m a -> m b -> m c

{-
look at your generalB implementation and if you didn’t write it in terms of genTwo, do that now and call it generalB2. Doing this should get rid of the state threading between generators.
-}

{-
genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo gen transformer = \s -> let (r, s') = gen s
                               in transformer r s'


generalPair2 :: Gen a -> Gen b -> (a -> b -> c) -> Gen c
generalPair2 g1 g2 builder  = \sd0 -> (let (x0, s1) = g1 sd0
                                           (x1, s2) = g2 s1
                                       in  ((builder x0 x1), s2))

type Gen a = Seed -> (a, Seed) 

--generalB ::  
-}
