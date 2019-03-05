module Common.View where

import Data.Maybe (isJust)
import Data.Proxy (Proxy(..))
import qualified Miso
import Miso (View)
import Miso.Html
import qualified Miso.String as Miso
import Servant.API ((:<|>)(..))

import TicTacToe.Exports (Cell(..), Difficulty(..), GameOutcome(..),
                          GameState(gameBoard), Player(..), allDifficulties,
                          checkForOutcome, getWinningCells, showCellContents)

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
    (True, Just gs) -> playView gs
    _               -> gameSetupView m

-- View function of the Home route.
gameSetupView :: Model -> View Action
gameSetupView m =
  div_
  []
  [ div_
    [class_ "pick-mark"]
    [ p_ [] [ text "Play as"
            , button_ [class_ "mark-button", onClick PickMarkX] [text "X"]
            , text "or"
            , button_ [class_ "mark-button", onClick PickMarkO] [text "O"]
            , text "?"
            ]
    ]
  , div_
    [ class_ "pick-difficulty"]
    [ label_
      [ for_ "difficulty-select" ] [ text "Difficulty:" ]
    , select_
      [ id_ "difficulty-select"
      , onChange (\misoStr -> ChangeDifficulty misoStr)
      ]
      (map (makeDifficultyOption m) allDifficulties)
    ]
  ]

playView :: GameState -> View Action
playView gs =
  let outcome = checkForOutcome gs
   in div_
      [class_ "tictactoe-board"]
      [ table_
        []
        [ tbody_
          []
          [ tr_
            []
            (map (makeTDCell outcome gs) [Cell00 .. Cell02])
          , tr_
            []
            (map (makeTDCell outcome gs) [Cell10 .. Cell12])
          , tr_
            []
            (map (makeTDCell outcome gs) [Cell20 .. Cell22])
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

makeDifficultyOption :: Model -> Difficulty -> View Action
makeDifficultyOption m d =
  option_
  [ selected_ (_gameDifficulty m == d)
  , value_ $ Miso.ms $ show d
  ]
  [ text (Miso.ms $ show d) ]

makeTDCell :: Maybe (GameOutcome Player) -> GameState -> Cell -> View Action
makeTDCell outcome gs cell =
  td_
  [class_ $ Miso.ms $ show $ getCellState outcome gs cell]
  [ button_
    [onClick (FillCell cell)]
    [text $ Miso.ms $ showCellContents (gameBoard gs) cell]
  ]

-- Handle 404 errors.
page404View :: View Action
page404View = text "Yo, 404, page unknown. Go to / or /flipped. Shoo!"
