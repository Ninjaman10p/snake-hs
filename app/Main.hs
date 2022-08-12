{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}

module Main where
  
import Brick
import Brick.BChan
import Brick.Widgets.Border
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

import Control.Monad
import Control.Lens
import Control.Arrow

import Control.Concurrent (forkIO, threadDelay)

import qualified Data.Map.Strict as M
import Data.Time.Clock

import System.Random
import Control.Monad.State.Class

type Name = ()
type Age = Int

data SnakeObj = Tail Age Dir Dir | Food
  deriving (Ord, Eq, Show)

data Tick = Tick { tickTime :: UTCTime }
  deriving (Eq, Ord, Show)

data Dir = DUp | DDown | DLeft | DRight
  deriving (Eq, Ord, Show)

data SnakeGame = SnakeGame
  { _objects :: M.Map (Int, Int) SnakeObj
  , _direction :: Dir
  , _headPos :: (Int, Int)
  , _growth :: Int
  , _randSeed :: StdGen
  , _lastTick :: UTCTime
  , _lastDir :: Dir
  , _alive :: Bool
  } deriving (Eq, Show)
$(makeLenses ''SnakeGame)

main :: IO ()
main = runSnake

runSnake :: IO ()
runSnake = do
  chan <- newBChan 10
  void . forkIO . forever $ do
    time <- getCurrentTime
    writeBChan chan $ Tick time
    threadDelay 10000
  time <- getCurrentTime
  gen <- getStdGen
  let builder = V.mkVty V.defaultConfig
      game = SnakeGame
        { _objects = M.empty
        , _headPos = (5, 5)
        , _direction = DDown
        , _growth = 3
        , _randSeed = gen
        , _lastTick = time
        , _lastDir = DDown
        , _alive = True
        }
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) snakeApp game

snakeApp :: App SnakeGame Tick Name
snakeApp = App { appDraw = return . drawSnake
               , appChooseCursor = neverShowCursor
               , appHandleEvent = handleEvent
               , appStartEvent = spawnFood
               , appAttrMap = mkAttrMap
               }

renderSize = (40, 20)

tickLength :: NominalDiffTime
tickLength = 0.5

drawSnake :: SnakeGame -> Widget Name
drawSnake s = C.center . border . limit renderSize . handleDead s . drawGrid $ s

handleDead :: SnakeGame -> Widget Name -> Widget Name
handleDead s = if view alive s
                  then id
                  else forceAttr (attrName "dead")

drawGrid :: SnakeGame -> Widget Name
drawGrid = hBox . sequence (fmap drawColumn $ range $ view _1 renderSize)

drawColumn :: Int -> SnakeGame -> Widget Name
drawColumn x = vBox . sequence (fmap (drawCell x) $ range $ view _2 renderSize)

drawCell :: Int -> Int -> SnakeGame -> Widget Name
drawCell x y = do
  hp <- view headPos
  if (x, y) == hp
    then return snakeHeadCell
    else do
      cell <- view $ objects . at (x, y)
      return $ case cell of
        Nothing -> bgCell x y
        Just Food -> foodCell
        Just (Tail _ dir dir') -> tailCell dir dir'
    
snakeHeadCell :: Widget Name
snakeHeadCell = withAttr (attrName "head") $ str "@"

bgCell :: Int -> Int -> Widget Name
bgCell _ _ = withAttr (attrName "dim") $ str " "

foodCell :: Widget Name
foodCell = withAttr (attrName "food") $ str "%"

tailCell :: Dir -> Dir -> Widget Name
tailCell d d' = withAttr (attrName "tail") . str $ _tailCell d d'
  where _tailCell :: Dir -> Dir -> String
        _tailCell DUp DRight = "┏"
        _tailCell DUp DLeft = "┓"
        _tailCell DDown DRight = "┗"
        _tailCell DDown DLeft = "┛"
        _tailCell DDown DDown = "┃"
        _tailCell DRight DRight = "━"
        _tailCell a b = _tailCell (swapDir b) (swapDir a)

range :: Int -> [Int]
range n = [0..n - 1]
  
limit :: (Int, Int) -> Widget n -> Widget n
limit = uncurry (.) <<< hLimit *** vLimit

handleEvent :: BrickEvent Name Tick -> EventM Name SnakeGame ()
handleEvent (AppEvent (Tick time)) = do
  tick <- view lastTick <$> get
  alive <- view alive <$> get
  if diffUTCTime time tick >= tickLength && alive
    then do
      modify $ set lastTick time
      moveSnake
    else return ()
handleEvent (VtyEvent (V.EvKey (V.KChar c) ms)) = handleKey c ms
handleEvent _ = return ()

handleKey :: Char -> [V.Modifier] -> EventM Name SnakeGame ()
handleKey 'c' [V.MCtrl] = halt
handleKey 'j' [] = setDir DDown
handleKey 'k' [] = setDir DUp
handleKey 'h' [] = setDir DLeft
handleKey 'l' [] = setDir DRight

setDir :: Dir -> EventM Name SnakeGame ()
setDir dir = do
  d <- view lastDir <$> get
  if dir == swapDir d
    then return ()
    else modify $ set direction dir

swapDir :: Dir -> Dir
swapDir d = case d of
              DUp -> DDown
              DDown -> DUp
              DLeft -> DRight
              DRight -> DLeft

moveSnake :: MonadState SnakeGame m => m ()
moveSnake = do
  direction <- _direction <$> get
  pos <- _headPos <$> get
  let handleDir (l, n) = over l (+n) pos
  moveHead . handleDir $ case direction of
    DUp ->  (_2, (-1))
    DDown -> (_2, 1)
    DLeft -> (_1, (-1))
    DRight -> (_1, 1)
  modify $ filterTails
  testHead

moveHead :: MonadState SnakeGame m => (Int, Int) -> m ()
moveHead pos = do
  oldPos <- view headPos <$> get
  dir <- view direction <$> get
  oldDir <- view lastDir <$> get
  modify $ set lastDir dir
  modify $ over objects $ M.insert oldPos (Tail 0 oldDir dir)
  modify $ set headPos pos

testHead :: MonadState SnakeGame m => m ()
testHead = do
  (x, y) <- view headPos <$> get
  killIf $ x < 0 || y < 0
  killIf $ x >= fst renderSize || y >= snd renderSize
  cell <- view (objects . at (x, y)) <$> get
  case cell of
    Nothing -> return ()
    Just (Tail _ _ _) -> killIf True
    Just Food -> eatFood
  
eatFood :: MonadState SnakeGame m => m ()
eatFood = do
  (x, y) <- view headPos <$> get
  modify $ set (objects . at (x, y)) Nothing
  modify $ over growth (+1)
  spawnFood

-- TODO / WARNING: spawnFood will hang when board is full
spawnFood :: MonadState SnakeGame m => m ()
spawnFood = do
  x <- state . liftGen $ uniformR (0, fst renderSize - 1)
  y <- state . liftGen $ uniformR (0, snd renderSize - 1)
  cell <- view (objects . at (x, y)) <$> get
  case cell of
    Nothing -> modify $ set (objects . at (x, y)) $ Just Food
    _ -> spawnFood

killIf :: MonadState SnakeGame m => Bool -> m ()
killIf b = do
  if b
    then modify $ set alive False
    else return ()

filterTails :: SnakeGame -> SnakeGame
filterTails = do
  snakeLength <- view growth
  over objects $ M.mapMaybe $ \x -> case x of
                                      Tail a d d' -> if a < snakeLength
                                                  then Just $ Tail (a + 1) d d'
                                                  else Nothing
                                      Food -> Just Food

liftGen :: (StdGen -> (a, StdGen)) -> SnakeGame -> (a, SnakeGame)
liftGen f s = let (a, gen) = f $ _randSeed s
              in  (a, set randSeed gen s)

mkAttrMap :: SnakeGame -> AttrMap
mkAttrMap _ = attrMap (fg V.white)
  [ (attrName "dim", fg V.white `V.withStyle` V.dim)
  , (attrName "head", fg V.brightGreen)
  , (attrName "tail", fg V.green)
  , (attrName "food", fg V.red `V.withStyle` V.bold)
  , (attrName "dead", fg V.red `V.withStyle` V.dim)
  ]
