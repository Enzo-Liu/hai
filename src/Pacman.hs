module Pacman where

import           Graphics.UI.Qtah.Widgets.QMainWindow (QMainWindow)
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import qualified Graphics.UI.Qtah.Widgets.QWidget     as QWidget

run :: IO ()
run = do
  mainWindow <- makeMainWindow
  QWidget.show mainWindow

makeMainWindow :: IO QMainWindow
makeMainWindow = do
  window <- QMainWindow.new
  QWidget.resizeRaw window 640 480
  return window
