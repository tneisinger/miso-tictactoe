{-# LANGUAGE TemplateHaskell #-}
module Common.Model where

import Control.Lens
import qualified Miso.String as Miso
import qualified Servant.API as Servant

import TicTacToe.Exports (Cell, Difficulty(..), GameState)

data Model = Model
  { _uri :: !Servant.URI
  -- ^ Current URI of the application.
  , _gameDifficulty :: Difficulty
  -- ^ The difficulty of the next game
  , _gameState :: !(Maybe GameState)
  -- ^ a tictactoe gamestate
  , _showGame :: Bool
  -- ^ Determines whether to show the game or the mark selection menu.
  } deriving (Eq, Show)

makeLenses ''Model

-- | Set up the initial model/state.
initialModel :: Servant.URI -> Model
initialModel initialUri = Model {_uri = initialUri
                                , _gameDifficulty = Easy
                                , _gameState = Nothing
                                , _showGame = False
                                }

data Action
  = NoOp -- ^ Empty/No operation.
  | ChangeURI !Servant.URI -- ^ Push a new URI on the History stack.
  | HandleURI !Servant.URI -- ^ Handle a URI change (e.g. popstate events).
  | ChangeDifficulty Miso.MisoString
  | PickMarkX
  | PickMarkO
  | FillCell Cell
  | ComputerMove
  | HideGame
  deriving (Show, Eq)
