{-# LANGUAGE TemplateHaskell #-}
module Common.Model where

import Control.Lens
import qualified Servant.API as Servant

import TicTacToe.Exports (Cell, GameState)

data Model = Model
  { _uri :: !Servant.URI
  -- ^ Current URI of the application.
  , _gameState :: !(Maybe GameState)
  -- ^ a tictactoe gamestate
  , _showGame :: Bool
  -- ^ Determines whether to show the game or the mark selection menu.
  } deriving (Eq, Show)

makeLenses ''Model

-- | Set up the initial model/state.
initialModel :: Servant.URI -> Model
initialModel initialUri = Model {_uri = initialUri
                                , _gameState = Nothing
                                , _showGame = False
                                }

data Action
  = NoOp -- ^ Empty/No operation.
  | ChangeURI !Servant.URI -- ^ Push a new URI on the History stack.
  | HandleURI !Servant.URI -- ^ Handle a URI change (e.g. popstate events).
  | PickMarkX
  | PickMarkO
  | FillCell Cell
  | ComputerMove
  | HideGame
  deriving (Show, Eq)
