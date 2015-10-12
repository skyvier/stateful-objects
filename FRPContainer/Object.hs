{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}

module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks

import Control.Monad (when)
import System.IO

{-
 - Reactive banana type synonyms
 -}
type Trigger a = a -> IO ()
type HandlePair a = (AddHandler a, Trigger a)

{-
 - The game object types
 -}
type Surface = Int

type Position = (Int, Int)
type Positioner = Position -> Position

type FrpObject = (HandlePair Positioner, HandlePair Surface)
data Data = Data Position Surface

instance Num Position where
   (a1, a2) + (b1, b2) = (a1 + b1, a2 + b2)
   (a1, a2) - (b1, b2) = (a1 - b1, a2 - b2)

{-
 - Reactive banana work
 -}

{-
 - Currently this uses two objects for demonstration purposes.
 - When you change position, you change obj1,
 - when you change surface, you change obj2.
 -}
main :: IO ()
main = do
   sources <- (,) <$> newAddHandler <*> newAddHandler
   sources2 <- (,) <$> newAddHandler <*> newAddHandler
   obj1 <- compile $ setupObject sources
   obj2 <- compile $ setupObject sources2
   actuate obj1
   actuate obj2
   eventLoop [sources, sources2]

tutorial :: IO ()
tutorial = mapM_ putStrLn 
           [ "\nGame Object demonstration with FRP (reactive banana)"
           , "\nUsage: <command> ENTER <value>"
           , "Commands:"
           , " position : change position to value"
           , " addposition : add value to position"
           , " minusposition : subtract value from position"
           , " surface : change surface to int value"
           , " quit: quit the program\n" ]

newPosition :: FrpObject -> Trigger Positioner
newPosition (hPos, _) = snd hPos

newSurface :: FrpObject -> Trigger Surface
newSurface (_, hSur) = snd hSur

eventLoop :: [FrpObject] -> IO ()
eventLoop (obj1:obj2:objs) = tutorial >> loop
   where 
   loop = do
      putStr "> "
      hFlush stdout
      s <- getLine
      case s of
         "position" -> do
                           pos <- getLine
                           newPosition obj1 $ const (read pos)
         "addposition" -> do
                           pos <- getLine
                           newPosition obj1 $ (+) (read pos)
         "minusposition" -> do
                           pos <- getLine
                           newPosition obj1 $ subtract (read pos)
         "surface" -> do
                           sur <- getLine
                           newSurface obj2 (read sur)
         "quit" -> return ()
         _ -> putStrLn "unknown"
      when (s /= "quit") loop


setupObject :: forall t. Frameworks t =>
   FrpObject -> Moment t ()
setupObject (position, surface) = do
   newPosition <- fromAddHandler (fst position)
   newSurface <- fromAddHandler (fst surface)

   let
      {-
       - Definition for a single object
       -}

      eObject :: Event t Data
      bObject :: Behavior t Data
      (eObject, bObject) = let def = Data (1,1) 1
                           in mapAccum def . fmap (\f x -> (f x, f x)) $
                              ((changePos <$> newPosition)
                              `union` (changeSurface <$> newSurface))

      changePos :: Positioner -> Data -> Data
      changePos f (Data pos sur) = Data (f pos) sur
      
      changeSurface :: Surface -> Data -> Data
      changeSurface sur (Data pos _) = Data pos sur

   reactimate $ putStrLn . showObject <$> eObject
   where showObject (Data pos sur) = "The new object is: " ++ show pos ++ ", " ++ show sur ++ "!"
