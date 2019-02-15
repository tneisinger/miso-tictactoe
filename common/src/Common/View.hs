module Common.View where

import Data.Proxy (Proxy(..))
import qualified Miso
import Miso (View)
import Miso.Html
import qualified Miso.String as Miso
import Servant.API ((:<|>)(..))

import TicTacToe

import Common.Model
import Common.Routes

-- Checks which URI is open and shows the appropriate view.
viewModel :: Model -> View Action
viewModel model = view
 where
  view = either (const page404View) id
    $ Miso.runRoute (Proxy :: Proxy ViewRoutes) handlers _uri model
  handlers = homeView :<|> flippedView

-- View function of the Home route.
homeView :: Model -> View Action
homeView m = div_
  [class_ "tictactoe-board"]
  [ table_
    []
    [ tr_
        []
        [ td_ [] [button_ [] [text "cell00"]]
        , td_ [] [button_ [] [text "cell01"]]
        , td_ [] [button_ [] [text "cell02"]]
        ],
      tr_
        []
        [ td_ [] [button_ [] [text "cell10"]]
        , td_ [] [button_ [] [text "cell11"]]
        , td_ [] [button_ [] [text "cell12"]]
        ],
      tr_
      []
      [ td_ [] [button_ [] [text "cell20"]]
      , td_ [] [button_ [] [text "cell21"]]
      , td_ [] [button_ [] [text "cell22"]]
      ]
    ]
  ]

-- View function of the Home route.
flippedView :: Model -> View Action
flippedView m = div_
  []
  [ div_
    []
    [ button_ [onClick AddOne] [text "+"]
    , text $ Miso.ms $ show $ _counterValue m
    , button_ [onClick SubtractOne] [text "-"]
    ]
  , button_ [onClick $ ChangeURI homeLink] [text "Go to /"]
  ]

-- Handle 404 errors.
page404View :: View Action
page404View = text "Yo, 404, page unknown. Go to / or /flipped. Shoo!"
