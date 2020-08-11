{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Apecs
import Apecs.Gloss
import Linear
import System.Random
import System.Exit
import Control.Monad
import Data.Monoid
import Data.Semigroup (Semigroup)

newtype Snake = Snake ([V4 Float]) deriving Show
instance Component Snake where type Storage Snake = Unique Snake

data Direction = DirXpos | DirXneg | DirYpos | DirYneg | DirZpos | DirZneg | DirWpos | DirWneg
instance Component Direction where type Storage Direction = Unique Direction

newtype Apple = Apple (V4 Float) deriving Show
instance Component Apple where type Storage Apple = Unique Apple

newtype DeathScreen = DeathScreen (V4 Float, V4 Float) deriving Show
instance Component DeathScreen where type Storage DeathScreen = Unique DeathScreen

newtype Dead = Dead Bool deriving Show
instance Semigroup Dead where (<>) a b = b
instance Monoid Dead where mempty = Dead False
instance Component Dead where type Storage Dead = Global Dead

newtype Time = Time Float deriving (Show, Num)
instance Semigroup Time where (<>) = (+)
instance Monoid Time where mempty = 0
instance Component Time where type Storage Time = Global Time

makeWorld "World" [''Snake, ''Direction, ''Apple, ''DeathScreen, ''Dead, ''Time, ''Camera]

type System' a = System World a

stepTime :: Float
stepTime = 0.2

initialize :: System' ()
initialize = do 
  snekEty <- newEntity (Snake [V4 0 0 0 0], DirYpos)
  replicateM_ 7 growTail
  newApple

incrTime :: Float -> System' ()
incrTime dT = modify global $ \(Time t) -> Time (t+dT)

newApple :: System' ()
newApple = cmapM_ $ \(Snake snek) -> do
  let genCoord = (* 10) . fromIntegral <$> (liftIO $ randomRIO (-16, (16 :: Int)))
  x <- genCoord
  y <- genCoord
  w <- genCoord
  z <- genCoord
  if V4 x y w z `elem` snek
    then newApple
    else do
      elseaplEny <- newEntity (Apple (V4 x y w z))
      return ()

moveHead :: Direction -> V4 Float -> V4 Float
moveHead d (V4 x y w z) = case d of
  DirXpos -> (V4 (x + 10) y w z)
  DirXneg -> (V4 (x - 10) y w z)
  DirYpos -> (V4 x (y + 10) w z)
  DirYneg -> (V4 x (y - 10) w z)
  DirWpos -> (V4 x y (w + 10) z)
  DirWneg -> (V4 x y (w - 10) z)
  DirZpos -> (V4 x y w (z + 10))
  DirZneg -> (V4 x y w (z - 10))

stepPosition :: System' ()
stepPosition = cmap $ \(Snake snek@(h : _), d :: Direction) -> Snake (moveHead d h : init snek)
  
growTail :: System' ()
growTail = cmap $ \(Snake snek@(h : _), d :: Direction) -> Snake (moveHead d h : snek)

killSnek :: System' ()
killSnek = cmapM_ $ \(Snake (h : _), d :: Direction) -> do
  dsEty <- newEntity (DeathScreen (moveHead d h, h))
  set global (Dead True)

stepSnek :: System' ()
stepSnek = cmapM_ $ \(Apple apos) -> cmapM_ $ \(Snake snek@(h : _), d :: Direction) -> do
  let ns@(V4 x y w z) = moveHead d h
  Dead dead <- get global
  if dead
    then return ()
    else if ns `elem` snek || any (\a -> a > 160 || a < -160) [x, y, z, w]
      then killSnek
      else if apos /= ns
        then stepPosition
        else do
          growTail
          newApple

triggerEvery :: Float -> Float -> Float -> System' a -> System' ()
triggerEvery dT period phase sys = do
  Time t <- get global
  let t' = t + phase
      trigger = floor (t'/period) /= floor ((t'+dT)/period)
  when trigger $ void sys

step :: Float -> System' ()
step dT = do
  incrTime dT
  triggerEvery dT stepTime 0 stepSnek

restartGame :: System' ()
restartGame = cmapM_ $ \ (DeathScreen ds, dsEty) -> do 
  snekEty <- newEntity (Snake [V4 0 0 0 0], DirYpos)
  replicateM_ 7 growTail
  newApple
  destroy dsEty (Proxy :: Proxy DeathScreen)
  set global (Dead False)

handleEvent :: Event -> System' ()
handleEvent (EventKey (SpecialKey KeyLeft)  Down   _ _) =
  cmap $ \(d :: Direction) -> case d of
    DirXpos -> d
    _ -> DirXneg
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) =
  cmap $ \(d :: Direction) -> case d of
    DirXneg -> d
    _ -> DirXpos
handleEvent (EventKey (SpecialKey KeyUp) Down   _ _) =
  cmap $ \(d :: Direction) -> case d of
    DirYneg -> d
    _ -> DirYpos
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) =
  cmap $ \(d :: Direction) -> case d of
    DirYpos -> d
    _ -> DirYneg
handleEvent (EventKey (Char 'a')  Down   _ _) =
  cmap $ \(d :: Direction) -> case d of
    DirWpos -> d
    _ -> DirWneg
handleEvent (EventKey (Char 'd') Down _ _) =
  cmap $ \(d :: Direction) -> case d of
    DirWneg -> d
    _ -> DirWpos
handleEvent (EventKey (Char 'w') Down   _ _) =
  cmap $ \(d :: Direction) -> case d of
    DirZneg -> d
    _ -> DirZpos
handleEvent (EventKey (Char 's') Down _ _) =
  cmap $ \(d :: Direction) -> case d of
    DirZpos -> d
    _ -> DirZneg
handleEvent (EventKey (Char 'r') Down _ _) = restartGame
handleEvent _ = return ()

translatexy, translatexz, translatewz, translatewy :: V4 Float -> Picture -> Picture
translatexy (V4 x y _ _) = translate (x + 17 * 10) (y + 17 * 10)
translatexz (V4 x _ _ z) = translate (x + 17 * 10) (z - 17 * 10)
translatewz (V4 _ _ w z) = translate (w - 17 * 10) (z - 17 * 10)
translatewy (V4 _ y w _) = translate (w - 17 * 10) (y + 17 * 10)

square :: Picture
square = Polygon [(0.5, 0.5), (0.5, (-0.5)), ((-0.5), (-0.5)), ((-0.5), 0.5)]

hLine, vLine :: Float -> Picture
hLine len = scale len 1 square
vLine len = scale 1 len square

box :: Float -> Picture
box size = l <> r <> u <> d <> hLine size <> vLine size
  where l = translate ((-size) / 2 + 0.5) 0 $ vLine size
        r = translate (size / 2 - 0.5) 0 $ vLine size
        u = translate 0 (size / 2 - 0.5) $ hLine size
        d = translate 0 ((-size) / 2 + 0.5) $ hLine size

draw :: System' Picture
draw = do 
  snekxy <- foldDraw $ \(Snake pos) -> pictures $ map (\x -> translatexy x . color (greyN 0.5) . scale 10 10 $ square) pos
  snekxz <- foldDraw $ \(Snake pos) -> pictures $ map (\x -> translatexz x . color (greyN 0.5) . scale 10 10 $ square) pos
  snekwz <- foldDraw $ \(Snake pos) -> pictures $ map (\x -> translatewz x . color (greyN 0.5) . scale 10 10 $ square) pos
  snekwy <- foldDraw $ \(Snake pos) -> pictures $ map (\x -> translatewy x . color (greyN 0.5) . scale 10 10 $ square) pos
  let snek = snekxy <> snekxz <> snekwz <> snekwy
  deathScreenxy <- foldDraw $ \(DeathScreen (a, b)) -> pictures $ map (\x -> translatexy x . color red . scale 10 10 $ square) [a, b]
  deathScreenxz <- foldDraw $ \(DeathScreen (a, b)) -> pictures $ map (\x -> translatexz x . color red . scale 10 10 $ square) [a, b]
  deathScreenwz <- foldDraw $ \(DeathScreen (a, b)) -> pictures $ map (\x -> translatewz x . color red . scale 10 10 $ square) [a, b]
  deathScreenwy <- foldDraw $ \(DeathScreen (a, b)) -> pictures $ map (\x -> translatewy x . color red . scale 10 10 $ square) [a, b]
  let deathScreen = deathScreenxy <> deathScreenxz <> deathScreenwz <> deathScreenwy
  applexy <- foldDraw $ \(Apple pos) -> translatexy pos . color green . scale 10 10 $ square
  applexz <- foldDraw $ \(Apple pos) -> translatexz pos . color green . scale 10 10 $ square
  applewz <- foldDraw $ \(Apple pos) -> translatewz pos . color green . scale 10 10 $ square
  applewy <- foldDraw $ \(Apple pos) -> translatewy pos . color green . scale 10 10 $ square
  let apple = applexy <> applexz <> applewz <> applewy
  let field = color white . scale 10 10 $ box 69
  return $ field <> apple <> snek <> deathScreen

main :: IO()
main = do
    w <- initWorld
    runWith w $ do
        initialize
        play (InWindow "hSnek" (800, 800) (10, 10)) black 60 draw handleEvent step