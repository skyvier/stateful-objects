{-# LANGUAGE ExistentialQuantification, InstanceSigs, TypeSynonymInstances, FlexibleInstances  #-}

module Objects where

import Control.Monad.State

{- SDL Surface is simulated with an integer-}
type Surface = Int
type Position = (Int, Int)

{-
 - Animation and BinaryS are two SDL Surface containers
 -}
type Animation = State [Surface] Surface
type BinaryS = State (Surface, Surface) Surface

binaryS :: (Surface, Surface) -> BinaryS
binaryS (a, b) = state $ const (a, (a, b))

animation :: [Surface] -> Animation
animation (x:xs) = state $ const (x, xs ++ [x])

class SurfaceState a where
   create :: [Surface] -> a
   showState :: a -> String
   updateSurfaces :: a

instance SurfaceState BinaryS where
   create :: [Surface] -> BinaryS
   create (a:b:ss) = binaryS (a,b)

   updateSurfaces :: BinaryS
   updateSurfaces = state $ \(a, b) -> (b, (b,a))

   showState :: BinaryS -> String
   showState state = show $ runState state (0,0)

instance SurfaceState Animation where
   create :: [Surface] -> Animation
   create = animation

   updateSurfaces :: Animation
   updateSurfaces = state $ \(x:xs) -> (x, xs ++ [x])

   showState :: Animation -> String
   showState state = show $ runState state []

{-
 - Objects contain surface states and other information
 -}
data Object = Object Position BinaryS | Animated Position Animation

instance Show Object where
   show (Object pos state) = show pos ++ " , " ++ showState state
   show (Animated pos state) = show pos ++ " , " ++ showState state

update :: Object -> Object
update (Object pos state) = Object pos (state >> updateSurfaces)
update (Animated pos state) = Animated pos (state >> updateSurfaces)

print :: Object -> IO Object
print obj = do
   Prelude.print obj
   return obj


{-
 - Testing things out
 - This version of GameState enables MenuState action
-}

data GameState = GameState [Object]

updateAll :: GameState -> GameState
updateAll (GameState objs) = GameState $ map update objs

printAll :: GameState -> IO GameState
printAll (GameState objs) = do
   objs <- mapM Objects.print objs
   return $ GameState objs
