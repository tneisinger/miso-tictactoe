module Frontend.Update where

import Control.Monad.State
import Control.Lens ((.=), view)
import qualified Miso
import TicTacToe.Exports (Cell, GameState, Mark(..), checkGSForOutcome,
                          doComputerMove, doHumanMove, initGameState,
                          newGameState)

import qualified Common.Model as Common
import qualified Common.Routes as Common

updateModel :: Common.Action -> Miso.Transition Common.Action Common.Model ()
updateModel action = case action of
  Common.NoOp          -> pure ()
  -- Handle History/Routing.
  Common.ChangeURI uri -> Miso.scheduleIO $ do
    Miso.pushURI uri
    pure Common.NoOp
  Common.HandleURI uri -> Common.uri .= uri
  Common.PickMarkX -> startGame X
  Common.PickMarkO -> startGame O >> computerMove
  Common.FillCell cell -> do
    maybeGS <- Common._gameState <$> get
    case maybeGS of
      Nothing -> pure ()
      Just gs -> humanMove gs cell
  Common.ComputerMove  -> computerMove
  Common.HideGame -> Common.showGame .= False

startGame :: Mark -> Miso.Transition Common.Action Common.Model ()
startGame mark = do
  maybeGS <- Common._gameState <$> get
  case maybeGS of
    Nothing -> Common.gameState .= (Just $ initGameState mark 42)
    Just gs -> Common.gameState .= (Just $ newGameState gs mark)
  Common.showGame .= True

humanMove :: GameState -> Cell -> Miso.Transition Common.Action Common.Model ()
humanMove gs cell = do
  case execStateT (doHumanMove cell) gs of
    Left err -> Miso.scheduleIO_ $ putStrLn $ show err
    Right gs' -> do
      Common.gameState .= Just gs'
      pcMoveIfStillGame gs'

-- | Do a ComputerMove action if the game isn't over yet
pcMoveIfStillGame :: GameState -> Miso.Transition Common.Action Common.Model ()
pcMoveIfStillGame gs =
  case checkGSForOutcome gs of
    Nothing -> Miso.scheduleIO $ pure Common.ComputerMove
    Just _  -> pure ()

computerMove :: Miso.Transition Common.Action Common.Model ()
computerMove = do
  maybeGS <- Common._gameState <$> get
  case maybeGS of
    Nothing -> pure ()
    Just gs -> do
      case execStateT doComputerMove gs of
        Left err -> Miso.scheduleIO_ $ putStrLn $ show err
        Right gs' -> Common.gameState .= Just gs'
