{-# LANGUAGE ScopedTypeVariables #-}
module H2048
    ( run
    ) where

import           Control.Concurrent
import           Control.Monad                            (forM, forM_, join,
                                                           unless, when)
import           Data.Bits                                ((.&.), (.|.))
import           Data.Function                            (on)
import           Data.Functor                             (void)
import           Data.IORef                               (IORef,
                                                           atomicWriteIORef,
                                                           newIORef, readIORef)
import           Data.List                                (groupBy, transpose)
import qualified Data.List.NonEmpty                       as NE

import qualified Graphics.UI.Qtah.Core.QCoreApplication   as QCoreApplication
import qualified Graphics.UI.Qtah.Core.QEvent             as QEvent
import qualified Graphics.UI.Qtah.Core.Types              as QType
import           Graphics.UI.Qtah.Event
import           Graphics.UI.Qtah.Gui.QKeyEvent           (QKeyEvent)
import qualified Graphics.UI.Qtah.Gui.QKeyEvent           as QKeyEvent
import           Graphics.UI.Qtah.Signal                  (connect_)
import qualified Graphics.UI.Qtah.Signal                  as QSignal
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import           Graphics.UI.Qtah.Widgets.QAction         (triggeredSignal)
import qualified Graphics.UI.Qtah.Widgets.QAction         as QAction
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout      as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QFileDialog     as QFileDialog
import qualified Graphics.UI.Qtah.Widgets.QGridLayout     as QGridLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel          as QLabel
import           Graphics.UI.Qtah.Widgets.QMainWindow     (QMainWindow)
import qualified Graphics.UI.Qtah.Widgets.QMainWindow     as QMainWindow
import qualified Graphics.UI.Qtah.Widgets.QMenu           as QMenu
import qualified Graphics.UI.Qtah.Widgets.QMenuBar        as QMenuBar
import qualified Graphics.UI.Qtah.Widgets.QMessageBox     as QMessageBox
import qualified Graphics.UI.Qtah.Widgets.QPushButton     as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QSplitter       as QSplitter
import           Graphics.UI.Qtah.Widgets.QTextEdit       (QTextEdit,
                                                           copyAvailableSignal,
                                                           redoAvailableSignal,
                                                           textChangedSignal,
                                                           undoAvailableSignal)
import qualified Graphics.UI.Qtah.Widgets.QTextEdit       as QTextEdit
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout     as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget         as QWidget



import           System.FilePath                          (takeFileName)
import           System.Random

run :: IO ()
run = do
  mainWindow <- makeMainWindow
  QWidget.show mainWindow

makeMainWindow :: IO QMainWindow
makeMainWindow = do
  window <- QMainWindow.new
  QWidget.resizeRaw window 640 480
  QWidget.setWindowTitle window "h2048 by qtah"

  gameRef <- newGame >>= newIORef

  board <- QWidget.new
  QWidget.setStyleSheet board "QWidget{background-color: rgb(187,173,160)}"
  QWidget.setGeometryRaw board 100 100 500 555

  gridLayout <- QGridLayout.new
  QWidget.setLayout board gridLayout
  labels <- initBoard gridLayout

  readIORef gameRef >>= flip drawBoard labels

  listenKey <- newIORef True
  _ <- onEvent window $ \(event :: QKeyEvent) -> do
    t <- QEvent.eventType event
    v <- QKeyEvent.key event
    listen <- readIORef listenKey
    if listen && t == QEvent.KeyPress
      then do
        let move = getKeyMove (toEnum v)
        game <- readIORef gameRef
        moveGame game move labels >>= atomicWriteIORef gameRef
        return True
      else return False

  rightBox <- QWidget.new
  rightBoxLayout <- QVBoxLayout.new
  QWidget.setLayout rightBox rightBoxLayout

  runButton <- QPushButton.newWithText "&RunAI"
  stopButton <- QPushButton.newWithText "&Stop"
  dumpButton <- QPushButton.newWithText "&DumpBoard"
  reDrawButton <- QPushButton.newWithText "&ReDraw"
  clearButton <- QPushButton.newWithText "&Clear"
  QBoxLayout.addStretch rightBoxLayout
  QBoxLayout.addWidget rightBoxLayout runButton
  QBoxLayout.addWidget rightBoxLayout stopButton
  QBoxLayout.addWidget rightBoxLayout dumpButton
  QBoxLayout.addWidget rightBoxLayout clearButton
  QBoxLayout.addWidget rightBoxLayout reDrawButton

  aiRunning <- newIORef False

  _ <- forkIO $ runAI gameRef aiRunning labels

  connect_ runButton QAbstractButton.clickedSignal $ \_ -> do
    atomicWriteIORef aiRunning True
    atomicWriteIORef listenKey False

  connect_ stopButton QAbstractButton.clickedSignal $ \_ -> do
    atomicWriteIORef aiRunning False
    atomicWriteIORef listenKey True

  connect_ dumpButton QAbstractButton.clickedSignal $ \_ -> do
    game <- readIORef gameRef
    print game

  connect_ clearButton QAbstractButton.clickedSignal $ \_ -> do
    game <- newGame
    atomicWriteIORef gameRef game
    drawBoard game labels

  connect_ reDrawButton QAbstractButton.clickedSignal $ \_ -> do
    game <- readIORef gameRef
    drawBoard game labels

  splitter <- QSplitter.new
  QSplitter.addWidget splitter board
  QSplitter.addWidget splitter rightBox
  QSplitter.setSizes splitter [600 :: Int, 200]

  main <- QWidget.new
  layout <- QVBoxLayout.newWithParent main
  QBoxLayout.addWidgetWithStretch layout splitter 1

  QMainWindow.setCentralWidget window main
  return window

runAI :: IORef Game -> IORef Bool -> [QLabel.QLabel] -> IO ()
runAI gameRef aiRunning labels = do
  running <- readIORef aiRunning
  when running $ do
    game <- readIORef gameRef
    move <- getAIMove game maxExpt
    case move of
      Nothing    -> atomicWriteIORef aiRunning False
      Just move' -> moveGame game move' labels >>= atomicWriteIORef gameRef
  threadDelay (200*1000)
  runAI gameRef aiRunning labels

moveGame :: Game -> (Game -> Game) -> [QLabel.QLabel] -> IO Game
moveGame game move labels = do
        nextGame <- nextMove move game
        drawBoard nextGame labels
        return nextGame

getKeyMove :: QType.QtKey -> (Game -> Game)
getKeyMove QType.KeyLeft  = moveLeft
getKeyMove QType.KeyUp    = moveUp
getKeyMove QType.KeyDown  = moveDown
getKeyMove QType.KeyRight = moveRight
getKeyMove _              = id

nextMove :: (Game -> Game) -> Game -> IO Game
nextMove move g | nextGame == g = return g
                | otherwise = addTile nextGame
                where nextGame = move g

type Move = Game -> Game
type Stragety = (NE.NonEmpty (Move, Game) -> IO Move)

randomStategy :: NE.NonEmpty (Move, Game) -> IO Move
randomStategy ms = fst . (ms NE.!!) <$> randomRIO (0, NE.length ms -1)

maxHuer :: NE.NonEmpty (Move, Game) -> IO Move
maxHuer = return . fst . NE.head . NE.sortWith (heurScore.snd)

score = [4^15,4^14,4^13,4^12,4^8,4^9,4^10,4^11,4^7,4^6,4^5,4^4,4^0,4^1,4^2,4^3]
heurScore :: Game -> Int
heurScore (Game board) = negate $ maximum
  [sum $ zipWith (*) score (join board),
   sum $ zipWith (*) score (join $ transpose board),
   sum $ zipWith (*) score (join $ map reverse board),
   sum $ zipWith (*) score (join . map reverse $ transpose board)]

maxExpt :: NE.NonEmpty (Move, Game) -> IO Move
maxExpt = return . fst . NE.head . NE.sortWith (exptScore.snd)

exptScore :: Game -> Int
exptScore g = let boards = allBoards [(1,g)] 8
              in ceiling . sum . map (\(i,g') -> i * (fromInteger . toInteger . heurScore) g') $ boards

allBoards :: [(Double, Game)] -> Int -> [(Double, Game)]
allBoards gs 0 = gs
allBoards gs n = let nbs = nextBoards gs in allBoards nbs (n-1)
  where nextBoards :: [(Double,Game)] -> [(Double,Game)]
        nextBoards gs' = map mergePoss . groupBy ((==) `on` snd) . join $ map nextBoard gs'
        mergePoss gs' = (sum . map fst $ gs' ,snd $ head gs')
        nextBoard :: (Double, Game) -> [(Double, Game)]
        nextBoard (i, g) = let emptyCeils = boardEmptyCeils g
                               l = length emptyCeils
                               dl = (fromInteger. toInteger $l) in
          if null emptyCeils || i <= 0.01 then [(i,g)]
          else
            map ((,) (0.8*i/dl) . bestMove . flip (addInIndex' g) 2) emptyCeils
            ++ map ((,) (0.2*i/dl) . bestMove . flip (addInIndex' g) 4) emptyCeils

bestMove :: Game -> Game
bestMove g = NE.head . NE.sortWith heurScore . NE.map (\m-> m g) . NE.fromList
  $ [moveUp,moveLeft, moveDown, moveRight]

getAIMove :: Game -> Stragety -> IO (Maybe (Game -> Game))
getAIMove g s = let cs = filter ((/= g) . snd) $ map (\f-> (f, f g)) [moveUp, moveDown, moveLeft, moveRight] in
  case s <$> NE.nonEmpty cs of
    Nothing -> return Nothing
    Just m  -> Just <$> m

canMove :: Game -> (Game -> Game) -> Bool
canMove g move = move g /= g

color :: Int -> String
color 2 ="rgb(119,110,101)"
color 4 ="rgb(119,110,101)"
color 0 ="rgb(0,0,0)"
color _ = "rgb(255,255,255)"

bgcolor :: Int -> String
bgcolor 0    = "rgb(204,192,179)"
bgcolor 2    = "rgb(238,228,218)"
bgcolor 4    = "rgb(237,224,200)"
bgcolor 8    = "rgb(242,177,121)"
bgcolor 16   = "rgb(245,150,100)"
bgcolor 32   = "rgb(245,125,95)"
bgcolor 64   = "rgb(245,95,60)"
bgcolor 128  = "rgb(237,207,114)"
bgcolor 256  = "rgb(237,204,97)"
bgcolor 512  = "rgb(237,200,80)"
bgcolor 1024 = "rgb(210,161,68)"
bgcolor 2048 = "rgb(237,194,46)"
bgcolor _    = "rgb(47,43,37)"

font :: Int -> String
font 128   = "32pt"
font 256   = "32pt"
font 512   = "32pt"
font 1024  = "24pt"
font 2048  = "24pt"
font 4096  = "24pt"
font 8192  = "24pt"
font 16384 = "18pt"
font _     = "40pt"

vtext :: Int -> String
vtext 0 = ""
vtext a = show a

style :: Int -> String
style v = "QLabel{background: " ++ bgcolor v ++
          "; color: " ++ color v ++
          "; font: bold; border-radius: 10px; font:" ++ font v ++
          ";}"

initBoard :: QGridLayout.QGridLayout -> IO [QLabel.QLabel]
initBoard gridLayout = forM indexes $ \(r,c) -> do
    label <- QLabel.new
    QLabel.setAlignment label (QType.alignHCenter .|. QType.alignVCenter)
    QGridLayout.addWidget gridLayout label r c
    return label

drawBoard :: Game -> [QLabel.QLabel] -> IO ()
drawBoard (Game board) labels =
  forM_ (zip labels (join board)) $ \(l, v) -> do
    QLabel.setText l (vtext v)
    QWidget.setStyleSheet l (style v)

--- 2048 data
rowLen :: Int
rowLen = 4

colLen :: Int
colLen = 4

newGame :: IO Game
newGame = addTile . Game . replicate rowLen . replicate colLen $ 0

indexes :: [(Int,Int)]
indexes = [(x,y)| x<- [0..rowLen-1], y <- [0..colLen-1]]

boardWithIndex :: Game -> [((Int,Int), Tile)]
boardWithIndex (Game board) = zip indexes . join $ board

boardEmptyCeils :: Game -> [(Int, Int)]
boardEmptyCeils = map fst . filter ((== 0) . snd) . boardWithIndex

addTile :: Game -> IO Game
addTile g = do
  let emptyCeils = boardEmptyCeils g
      len = length emptyCeils
  case len of
    0 -> return g
    _ -> randomRIO (0, len-1) >>= addInIndex g . (emptyCeils !!)

choices :: [Int]
choices = [2,2,2,2,4]

getChoice :: IO Int
getChoice = (choices !!) <$> randomRIO (0,length choices - 1)

addInIndex :: Game -> (Int,Int) -> IO Game
addInIndex g i = addInIndex' g i <$> getChoice

addInIndex' :: Game -> (Int,Int) -> Int -> Game
addInIndex' (Game rows) i = Game . inRow i rows
  where inRow (_,_) [] _     = []
        inRow (0,y) (r:rs) v = inCol y r v : rs
        inRow (x,y) (r:rs) v = r : inRow (x-1,y) rs v
        inCol _ [] _     = []
        inCol 0 (_:cs) v = v:cs
        inCol y (c:cs) v = c:inCol (y-1) cs v

moveLeft' :: [[Tile]] -> [[Tile]]
moveLeft' arr = let mergedRows = map mergeRow arr in mergedRows

moveLeft :: Game -> Game
moveLeft (Game arr) = Game  . moveLeft' $ arr

mergeRow :: [Tile] -> [Tile]
mergeRow r = pad0 len . merge . filter0 $ r
  where filter0 = filter (/= 0)
        len = length r

pad0 :: Int -> [Tile] -> [Tile]
pad0 0 a      = a
pad0 n []     = 0 : pad0 (n-1) []
pad0 n (y:ys) = y:pad0 (n-1) ys

merge :: [Tile] -> [Tile]
merge [] = []
merge [x] = [x]
merge (x:y:ys)
  | x == y = 2*x:merge ys
  | otherwise = x: merge (y:ys)

moveRight :: Game -> Game
moveRight (Game arr) = Game . map reverse . moveLeft' . map reverse $ arr

moveUp :: Game -> Game
moveUp (Game arr) = Game . transpose .  moveLeft' . transpose $ arr

moveDown :: Game -> Game
moveDown (Game arr) = Game . transpose . map reverse . moveLeft' . map reverse . transpose $ arr

type Tile = Int
type Board = [[Tile]]
newtype Game = Game Board deriving (Eq, Show)

