{-# LANGUAGE ScopedTypeVariables #-}
module H2048
    ( run
    ) where

import           Control.Monad                          (forM_, unless, when)
import           Data.Bits                              ((.&.), (.|.))
import           Data.Functor                           (void)
import           Data.IORef                             (IORef, newIORef,
                                                         readIORef, writeIORef)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Core.QEvent           as QEvent
import qualified Graphics.UI.Qtah.Core.Types            as QType
import           Graphics.UI.Qtah.Event
import           Graphics.UI.Qtah.Gui.QKeyEvent         (QKeyEvent)
import qualified Graphics.UI.Qtah.Gui.QKeyEvent         as QKeyEvent
import           Graphics.UI.Qtah.Signal                (connect_)
import           Graphics.UI.Qtah.Widgets.QAction       (triggeredSignal)
import qualified Graphics.UI.Qtah.Widgets.QAction       as QAction
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout    as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QFileDialog   as QFileDialog
import qualified Graphics.UI.Qtah.Widgets.QGridLayout   as QGridLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel        as QLabel
import           Graphics.UI.Qtah.Widgets.QMainWindow   (QMainWindow)
import qualified Graphics.UI.Qtah.Widgets.QMainWindow   as QMainWindow
import qualified Graphics.UI.Qtah.Widgets.QMenu         as QMenu
import qualified Graphics.UI.Qtah.Widgets.QMenuBar      as QMenuBar
import qualified Graphics.UI.Qtah.Widgets.QMessageBox   as QMessageBox
import           Graphics.UI.Qtah.Widgets.QTextEdit     (QTextEdit,
                                                         copyAvailableSignal,
                                                         redoAvailableSignal,
                                                         textChangedSignal,
                                                         undoAvailableSignal)
import qualified Graphics.UI.Qtah.Widgets.QTextEdit     as QTextEdit
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout   as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget       as QWidget



import           System.FilePath                        (takeFileName)
import           System.Random

run :: IO ()
run = do
  mainWindow <- makeMainWindow
  QWidget.show mainWindow

makeMainWindow :: IO QMainWindow
makeMainWindow = do
  window <- QMainWindow.new
  QWidget.resizeRaw window 640 480

  gameRef <- newGame >>= newIORef

  widget <- QWidget.new
  QWidget.setStyleSheet widget "QWidget{background-color: rgb(187,173,160)}"
  QWidget.setWindowTitle widget "h2048 by qtah"
  QWidget.setGeometryRaw widget 100 100 500 555

  gridLayout <- QGridLayout.new
  QWidget.setLayout widget gridLayout
  readIORef gameRef >>= flip drawBoard gridLayout
  _ <- onEvent window $ \(event :: QKeyEvent) -> do
    t <- QEvent.eventType event
    v <- QKeyEvent.key event
    if t == QEvent.KeyPress 
      then do
        game' <- readIORef gameRef
        nextGame <- addTier game'
        drawBoard nextGame gridLayout
        writeIORef gameRef nextGame
        return True
      else return False
  QMainWindow.setCentralWidget window widget
  return window

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

drawBoard :: Game -> QGridLayout.QGridLayout -> IO ()
drawBoard (Game tiers) gridLayout =
  forM_ (zip [0..] tiers) $ \(i,Tier v) -> do
    label <- QLabel.new
    let (r,c) = i `quotRem` 4
    QLabel.setText label (vtext v)
    QLabel.setAlignment label (QType.alignHCenter .|. QType.alignVCenter)
    QWidget.setStyleSheet label (style v)
    QGridLayout.addWidget gridLayout label r c

newGame :: IO Game
newGame = addTier . Game . replicate 16 $ Tier 0

addTier :: Game -> IO Game
addTier g@(Game tiers) = do
  let len = length $ filter (== Tier 0) tiers
  case len of
    0 -> return (Game tiers)
    _ -> randomRIO (0, len-1) >>= (`addInI` g)

choices :: [Int]
choices = [2,2,2,4]

getChoice :: IO Int
getChoice = (choices !!) <$> randomRIO (0,length choices - 1)

addInI :: Int -> Game -> IO Game
addInI i g = (\v -> addVInI v i g) <$> getChoice

addVInI :: Int -> Int -> Game -> Game
addVInI v i (Game ts) = Game $ addVInIForTiers v i ts

addVInIForTiers :: Int -> Int -> [Tier] -> [Tier]
addVInIForTiers v 0 (Tier 0:ts)     = Tier v:ts
addVInIForTiers v 0 (t:ts)          = t:addVInIForTiers v 0 ts
addVInIForTiers v i (t@(Tier 0):ts) = t:addVInIForTiers v (i-1) ts
addVInIForTiers v i (t:ts)          = t:addVInIForTiers v i ts
addVInIForTiers _ _ _               = error "wrong number of empty hole"

newtype Tier = Tier Int deriving (Eq, Show)
newtype Game = Game [Tier] deriving Show

