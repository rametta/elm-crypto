module CryptoHipster exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http


type alias Model =
    { response : String
    , errorMessage : Maybe String
    , symbol : String
    }


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error String)
    | Symbol String


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "font-family", "arial" )
            , ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "align-items", "center" )
            ]
        ]
        [ div []
            [ input [ placeholder "Ex: BTC...", onInput Symbol ] []
            , button [ onClick SendHttpRequest ] [ text "Go" ]
            ]
        , viewResponseOrError model
        ]


viewResponseOrError : Model -> Html Msg
viewResponseOrError model =
    case model.errorMessage of
        Just message ->
            viewError message

        Nothing ->
            viewResponse model


viewError : String -> Html Msg
viewError errorMessage =
    div []
        [ h3 [] [ text "Couldn't fetch data at this time." ]
        , text ("Error: " ++ errorMessage)
        ]


viewResponse : Model -> Html Msg
viewResponse model =
    div []
        [ h3 [] [ text model.symbol ]
        , div [] [ text model.response ]
        ]


url : String -> String
url symbol =
    "https://min-api.cryptocompare.com/data/pricemultifull?fsyms=" ++ String.toUpper symbol ++ "&tsyms=CAD"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, Http.send DataReceived (Http.getString (url model.symbol)) )

        DataReceived (Ok responseStr) ->
            let
                response =
                    responseStr
            in
            ( { model | response = response }, Cmd.none )

        DataReceived (Err httpError) ->
            ( { model
                | errorMessage = Just (createErrorMessage httpError)
              }
            , Cmd.none
            )

        Symbol newSymbol ->
            ( { model | symbol = newSymbol }, Cmd.none )


createErrorMessage : Http.Error -> String
createErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "It appears you don't have an Internet connection right now."

        Http.BadStatus response ->
            response.status.message

        Http.BadPayload message response ->
            message


init : ( Model, Cmd Msg )
init =
    ( { response = ""
      , errorMessage = Nothing
      , symbol = ""
      }
    , Cmd.none
    )


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
