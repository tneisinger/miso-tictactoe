module Common.View where

import Data.Maybe (isJust)
import Data.Proxy (Proxy(..))
import qualified Miso
import Miso (View)
import Miso.Html
import qualified Miso.String as Miso
import Servant.API ((:<|>)(..))

import TicTacToe.Exports (Cell(..), GameOutcome(..), GameState(gameBoard),
                          Player(..), checkGSForOutcome, getWinningCells,
                          showCellContents)

import Common.Model
import Common.Routes

-- Checks which URI is open and shows the appropriate view.
viewModel :: Model -> View Action
viewModel model = view
 where
  view = either (const page404View) id
    $ Miso.runRoute (Proxy :: Proxy ViewRoutes) handlers _uri model
  handlers = homeView

homeView :: Model -> View Action
homeView m =
  div_
  [class_ "container"]
  [ selectView m ]

selectView :: Model -> View Action
selectView m =
  case (_showGame m, _gameState m) of
    (True, Just gs) -> gameView gs
    _               -> pickMarkView

-- View function of the Home route.
pickMarkView :: View Action
pickMarkView = div_
  [class_ "pick-mark"]
  [ p_ [] [ text "Play as"
          , button_ [class_ "mark-button", onClick PickMarkX] [text "X"]
          , text "or"
          , button_ [class_ "mark-button", onClick PickMarkO] [text "O"]
          , text "?"
          ]
  ]

gameView :: GameState -> View Action
gameView gs =
  div_
  [class_ "tictactoe-board"]
  [ table_
    []
    [ tbody_
      []
      [ tr_
        []
        [ td_
          [class_ $ Miso.ms $ show $ getCellState gs Cell00]
          [ button_
            [onClick (FillCell Cell00)]
            [text $ Miso.ms $ showCellContents (gameBoard gs) Cell00]
          ]
        , td_
          [class_ $ Miso.ms $ show $ getCellState gs Cell01]
          [ button_
            [onClick (FillCell Cell01)]
            [text $ Miso.ms $ showCellContents (gameBoard gs) Cell01]
          ]
        , td_
          [class_ $ Miso.ms $ show $ getCellState gs Cell02]
          [ button_
            [onClick (FillCell Cell02)]
            [text $ Miso.ms $ showCellContents (gameBoard gs) Cell02]
          ]
        ],
        tr_
        []
        [ td_
          [class_ $ Miso.ms $ show $ getCellState gs Cell10]
          [ button_
            [onClick (FillCell Cell10)]
            [text $ Miso.ms $ showCellContents (gameBoard gs) Cell10]
          ]
        , td_
          [class_ $ Miso.ms $ show $ getCellState gs Cell11]
          [ button_
            [onClick (FillCell Cell11)]
            [text $ Miso.ms $ showCellContents (gameBoard gs) Cell11]
          ]
        , td_
          [class_ $ Miso.ms $ show $ getCellState gs Cell12]
          [ button_
            [onClick (FillCell Cell12)]
            [text $ Miso.ms $ showCellContents (gameBoard gs) Cell12]
          ]
        ],
        tr_
        []
        [ td_
          [class_ $ Miso.ms $ show $ getCellState gs Cell20]
          [ button_
            [onClick (FillCell Cell20)]
            [text $ Miso.ms $ showCellContents (gameBoard gs) Cell20]
          ]
        , td_
          [class_ $ Miso.ms $ show $ getCellState gs Cell21]
          [ button_
            [onClick (FillCell Cell21)]
            [text $ Miso.ms $ showCellContents (gameBoard gs) Cell21]
          ]
        , td_
          [class_ $ Miso.ms $ show $ getCellState gs Cell22]
          [ button_
            [onClick (FillCell Cell22)]
            [text $ Miso.ms $ showCellContents (gameBoard gs) Cell22]
          ]
        ]
      ]
    ],
    p_ [class_ "outcome-message"] [text $ makeOutcomeMsg gs],
    button_ [onClick $ HideGame] [text "New Game"]
  ]

makeOutcomeMsg :: GameState -> Miso.MisoString
makeOutcomeMsg gs =
  case checkGSForOutcome gs of
    Nothing                -> " "
    Just Draw              -> "Draw!"
    Just (Winner Computer) -> "Computer wins!"
    Just (Winner Human)    -> "You win!"

data CellState = NormalCell | WinCell
  deriving (Eq)

instance Show CellState where
  show NormalCell = "normal-cell"
  show WinCell    = "win-cell"

getCellState :: GameState -> Cell -> CellState
getCellState gs cell =
  if cell `elem` getWinningCells (gameBoard gs)
     then WinCell
     else NormalCell

-- Handle 404 errors.
page404View :: View Action
page404View = text "Yo, 404, page unknown. Go to / or /flipped. Shoo!"
