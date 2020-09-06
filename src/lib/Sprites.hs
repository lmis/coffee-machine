{-# LANGUAGE QuasiQuotes #-}
module Sprites
  ( base
  , pouring
  , spillFull
  , spillEmpty
  , cupEmpty
  , cupFull
  , noCup
  , heat1
  , heat2
  , heat3
  , heat4
  , heat5
  , beans1
  , beans2
  , water1
  , water2
  , water3
  , water4
  , water5
  )
where
import           Text.RawString.QQ

pouring :: [String]
pouring =
  [ pouring0
  , pouring1
  , pouring2
  , pouring3
  , pouring4
  , pouring5
  , pouring6
  , pouring7
  , pouring8
  , pouring9
  , pouring10
  , pouring11
  , pouring0
  ]

base :: String
base = [r|
  \    /
x--\  /----x
|       |  |
|     v |  |
|       |  |
|       |  |
x ______|  |
 \ . -- . /|]

pouring0 :: String
pouring0 = [r|
       /
           x
           |
           |
           |
           |
           |
          /
|]
pouring1 :: String
pouring1 = [r|
       /
           x
           |
           |
      '    |
           |
           |
          /
|]

pouring2 :: String
pouring2 = [r|
       /
           x
           |
           |
      |    |
           |
           |
          /
|]

pouring3 :: String
pouring3 = [r|
       /
           x
           |
           |
      |    |
      '    |
           |
          /
|]

pouring4 :: String
pouring4 = [r|
       /
           x
           |
           |
      |    |
      |    |
           |
          /
|]

pouring5 :: String
pouring5 = [r|
       /
           x
           |
           |
      |    |
      |    |
      '    |
          /
|]

pouring6 :: String
pouring6 = [r|
       /
           x
           |
           |
      |    |
      |    |
      |    |
          /
|]
pouring7 :: String
pouring7 = [r|
       /
           x
           |
           |
      .    |
      |    |
      |    |
          /
|]
pouring8 :: String
pouring8 = [r|
       /
           x
           |
           |
           |
      |    |
      |    |
          /
|]
pouring9 :: String
pouring9 = [r|
       /
           x
           |
           |
           |
      .    |
      |    |
          /
|]
pouring10 :: String
pouring10 = [r|
       /
           x
           |
           |
           |
           |
      |    |
          /
|]
pouring11 :: String
pouring11 = [r|
       /
           x
           |
           |
           |
           |
      .    |
          /
|]

spillEmpty :: String
spillEmpty = [r|
       /
           x
           |
           |
           |
           |
           |
   .    . /
|]
spillFull :: String
spillFull = [r|
       /
           x
           |
           |
           |
           |
           |
   %    % /
|]
noCup :: String
noCup = [r|
       /
           x
           |
           |
           |
           |
           |
          /
|]
cupEmpty :: String
cupEmpty = [r|
       /
           x
           |
           |
           |
           |
           |
     cu   /
|]

cupFull :: String
cupFull = [r|
       /
           x
           |
           |
           |
           |
           |
     cü   /
|]

heat1 :: String
heat1 = [r|
       /
           x
           |
           |
           |
           |
 *         |
          /
|]

heat2 :: String
heat2 = [r|
       /
           x
           |
           |
           |
 *         |
 *         |
          /
|]

heat3 :: String
heat3 = [r|
       /
           x
           |
           |
 *         |
 *         |
 *         |
          /
|]

heat4 :: String
heat4 = [r|
       /
           x
           |
 *         |
 *         |
 *         |
 *         |
          /
|]

heat5 :: String
heat5 = [r|
       /
           x
 *         |
 *         |
 *         |
 *         |
 *         |
          /
|]

beans1 :: String
beans1 = [r|
       /
    ..     x
           |
           |
           |
           |
           |
          /
|]

beans2 :: String
beans2 = [r|
   ..../
    ..     x
           |
           |
           |
           |
           |
          /
|]

water1 :: String
water1 = [r|
       /
           x
           |
           |
           |
           |
         ~~|
          /
|]
water2 :: String
water2 = [r|
       /
           x
           |
           |
           |
         ~~|
           |
          /
|]
water3 :: String
water3 = [r|
       /
           x
           |
           |
         ~~|
           |
           |
          /
|]
water4 :: String
water4 = [r|
       /
           x
           |
         ~~|
           |
           |
           |
          /
|]
water5 :: String
water5 = [r|
       /
           x
         ~~|
           |
           |
           |
           |
          /
|]
