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
  let outcome = checkGSForOutcome gs
   in div_
      [class_ "tictactoe-board"]
      [ table_
        []
        [ tbody_
          []
          [ tr_
            []
            [ td_
              [class_ $ Miso.ms $ show $ getCellState outcome gs Cell00]
              [ button_
                [onClick (FillCell Cell00)]
                [text $ Miso.ms $ showCellContents (gameBoard gs) Cell00]
              ]
            , td_
              [class_ $ Miso.ms $ show $ getCellState outcome gs Cell01]
              [ button_
                [onClick (FillCell Cell01)]
                [text $ Miso.ms $ showCellContents (gameBoard gs) Cell01]
              ]
            , td_
              [class_ $ Miso.ms $ show $ getCellState outcome gs Cell02]
              [ button_
                [onClick (FillCell Cell02)]
                [text $ Miso.ms $ showCellContents (gameBoard gs) Cell02]
              ]
            ],
            tr_
            []
            [ td_
              [class_ $ Miso.ms $ show $ getCellState outcome gs Cell10]
              [ button_
                [onClick (FillCell Cell10)]
                [text $ Miso.ms $ showCellContents (gameBoard gs) Cell10]
              ]
            , td_
              [class_ $ Miso.ms $ show $ getCellState outcome gs Cell11]
              [ button_
                [onClick (FillCell Cell11)]
                [text $ Miso.ms $ showCellContents (gameBoard gs) Cell11]
              ]
            , td_
              [class_ $ Miso.ms $ show $ getCellState outcome gs Cell12]
              [ button_
                [onClick (FillCell Cell12)]
                [text $ Miso.ms $ showCellContents (gameBoard gs) Cell12]
              ]
            ],
            tr_
            []
            [ td_
              [class_ $ Miso.ms $ show $ getCellState outcome gs Cell20]
              [ button_
                [onClick (FillCell Cell20)]
                [text $ Miso.ms $ showCellContents (gameBoard gs) Cell20]
              ]
            , td_
              [class_ $ Miso.ms $ show $ getCellState outcome gs Cell21]
              [ button_
                [onClick (FillCell Cell21)]
                [text $ Miso.ms $ showCellContents (gameBoard gs) Cell21]
              ]
            , td_
              [class_ $ Miso.ms $ show $ getCellState outcome gs Cell22]
              [ button_
                [onClick (FillCell Cell22)]
                [text $ Miso.ms $ showCellContents (gameBoard gs) Cell22]
              ]
            ]
          ]
        ],
        p_ [class_ "outcome-message"] [text $ makeOutcomeMsg outcome],
        button_ [onClick $ HideGame] [text "New Game"]
      ]

makeOutcomeMsg :: Maybe (GameOutcome Player) -> Miso.MisoString
makeOutcomeMsg Nothing = " "
makeOutcomeMsg (Just Draw) = "Draw!"
makeOutcomeMsg (Just (Winner Computer)) = "Computer wins!"
makeOutcomeMsg (Just (Winner Human)) = "You win!"

data CellState = NormalCell | HumanWinCell | ComputerWinCell
  deriving (Eq)

instance Show CellState where
  show NormalCell      = "normal-cell"
  show HumanWinCell    = "human-win-cell"
  show ComputerWinCell = "computer-win-cell"

getCellState :: Maybe (GameOutcome Player) -> GameState -> Cell -> CellState
getCellState Nothing _ _ = NormalCell
getCellState (Just Draw) _ _ = NormalCell
getCellState (Just (Winner player)) gs cell =
    case (isWinCell, player) of
      (False, _)       -> NormalCell
      (True, Human)    -> HumanWinCell
      (True, Computer) -> ComputerWinCell
  where isWinCell = cell `elem` getWinningCells (gameBoard gs)

-- Handle 404 errors.
page404View :: View Action
page404View = text "Yo, 404, page unknown. Go to / or /flipped. Shoo!"
