{-# LANGUAGE ScopedTypeVariables #-}
module H2048
    ( run
    ) where

import           Control.Monad                          (forM_, unless, when)
import           Data.Bits                              ((.|.))
import           Data.Functor                           (void)
import           Data.IORef                             (IORef, newIORef,
                                                         readIORef, writeIORef)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Core.QEvent           as QEvent
import           Graphics.UI.Qtah.Event
import           Graphics.UI.Qtah.Gui.QCloseEvent       (QCloseEvent)
import           Graphics.UI.Qtah.Signal                (connect_)
import           Graphics.UI.Qtah.Widgets.QAction       (triggeredSignal)
import qualified Graphics.UI.Qtah.Widgets.QAction       as QAction
import qualified Graphics.UI.Qtah.Widgets.QFileDialog   as QFileDialog
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

  let game = newGame
  return window

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

