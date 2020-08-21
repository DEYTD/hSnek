{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
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

newtype Snake = Snake [V4 Float] deriving Show
instance Component Snake where type Storage Snake = Unique Snake

data Dir = DirX | DirY | DirZ | DirW deriving (Show, Eq)
data Sign = Pos | Neg deriving (Show, Eq)
newtype Direction = Direction (Dir, Sign) deriving (Show, Eq)
instance Component Direction where type Storage Direction = Unique Direction

newtype Apple = Apple (V4 Float) deriving Show
instance Component Apple where type Storage Apple = Unique Apple

newtype DeathScreen = DeathScreen (V4 Float, V4 Float) deriving Show
instance Component DeathScreen where type Storage DeathScreen = Unique DeathScreen

newtype InputQueue = InputQueue [Direction] deriving (Show, Semigroup, Monoid)
instance Component InputQueue where type Storage InputQueue = Global InputQueue

newtype Time = Time Float deriving (Show, Num)
instance Semigroup Time where (<>) = (+)
instance Monoid Time where mempty = 0
instance Component Time where type Storage Time = Global Time

makeWorld "World" [''Snake, ''Direction, ''Apple, ''DeathScreen, ''InputQueue, ''Time, ''Camera]

type System' a = System World a

stepTime :: Float
stepTime = 0.2

initialize :: System' ()
initialize = do 
  snekEty <- newEntity (Snake [V4 0 0 0 0], Direction (DirY, Pos))
  replicateM_ 7 growTail
  newApple

incrTime :: Float -> System' ()
incrTime dT = modify global $ \(Time t) -> Time (t+dT)

triggerEvery :: Float -> Float -> Float -> System' a -> System' ()
triggerEvery dT period phase sys = do
  Time t <- get global
  let t' = t + phase
      trigger = floor (t'/period) /= floor ((t'+dT)/period)
  when trigger $ void sys

stepDir :: System' ()
stepDir = do
  InputQueue queue <- get global
  if null queue
    then return()
    else do
      global $= InputQueue (init queue)
      cmap $ \(Direction (d,s)) -> if d /= (\(Direction d) -> fst d) (last queue) then last queue else Direction (d, s)

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
moveHead (Direction (d, s)) (V4 x y w z) = case d of
  DirX -> (V4 (x + 10 * eval s) y w z)
  DirY -> (V4 x (y + 10 * eval s) w z)
  DirW -> (V4 x y (w + 10 * eval s) z)
  DirZ -> (V4 x y w (z + 10 * eval s))
  where eval Pos = 1
        eval Neg = -1

stepPosition :: System' ()
stepPosition = cmap $ \(Snake snek@(h : _), d :: Direction) -> Snake (moveHead d h : init snek)
  
growTail :: System' ()
growTail = cmap $ \(Snake snek@(h : _), d :: Direction) -> Snake (moveHead d h : snek)

stepSnek :: System' ()
stepSnek = cmapM_ $ \(Apple apos) -> cmapM_ $ \(Snake snek@(h : _), d :: Direction) -> do
  let ns@(V4 x y w z) = moveHead d h
  dead <- cfold (\_ (ds :: DeathScreen) -> True) False
  if dead
    then return ()
    else if ns `elem` snek || any (\a -> a > 160 || a < -160) [x, y, z, w]
      then void $ newEntity (DeathScreen (ns, h))
      else if apos /= ns
        then stepPosition
        else do
          growTail
          newApple

step :: Float -> System' ()
step dT = do
  incrTime dT
  triggerEvery dT stepTime 0 $ stepDir >> stepSnek

restartGame :: System' ()
restartGame = cmapM_ $ \ (DeathScreen ds, dsEty) -> do 
  initialize
  destroy dsEty (Proxy :: Proxy DeathScreen)

handleEvent :: Event -> System' ()
handleEvent (EventKey (SpecialKey KeyLeft)  Down   _ _) = global $~ mappend (InputQueue [Direction (DirX, Neg)])
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) = global $~ mappend (InputQueue [Direction (DirX, Pos)])
handleEvent (EventKey (SpecialKey KeyUp) Down   _ _) = global $~ mappend (InputQueue [Direction (DirY, Pos)])
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) = global $~ mappend (InputQueue [Direction (DirY, Neg)])
handleEvent (EventKey (Char 'a')  Down   _ _) = global $~ mappend (InputQueue [Direction (DirW, Neg)])
handleEvent (EventKey (Char 'd') Down _ _) = global $~ mappend (InputQueue [Direction (DirW, Pos)])
handleEvent (EventKey (Char 'w') Down   _ _) = global $~ mappend (InputQueue [Direction (DirZ, Pos)])
handleEvent (EventKey (Char 's') Down _ _) = global $~ mappend (InputQueue [Direction (DirZ, Neg)])
handleEvent (EventKey (Char 'r') Down _ _) = restartGame
handleEvent (EventKey (SpecialKey KeyEsc) Down   _ _) = liftIO exitSuccess
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

translateSquares :: [V4 Float] -> Picture
translateSquares pos = pictures $ concatMap (\x -> [translatexy, translatexz, translatewz, translatewy] <*> [x] <*> [scale 10 10 square]) pos

draw :: System' Picture
draw = do 
  snek <- foldDraw $ \(Snake pos) -> color (greyN 0.5) $ translateSquares pos
  deathScreen <- foldDraw $ \(DeathScreen (a, b)) -> color red $ translateSquares [a, b]
  apple <- foldDraw $ \(Apple pos) -> color green $ translateSquares [pos]
  let field = color white . scale 10 10 $ box 69
  return $ field <> apple <> snek <> deathScreen

main :: IO()
main = do
    w <- initWorld
    runWith w $ do
        initialize
        play (InWindow "hSnek" (800, 800) (10, 10)) black 60 draw handleEvent step