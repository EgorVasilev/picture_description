module HttpExample exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Http


type alias Model =
    { nicknames : List String
    , errorMessage : Maybe String
    }


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error String)


url : String
url =
    "http://localhost:5019/nicknames"


getNicknames : Cmd Msg
getNicknames =
    Http.get
        { url = url
        , expect = Http.expectString DataReceived
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, getNicknames )

        DataReceived result ->
            case result of
                Ok nicknamesStr ->
                    let
                        nicknames =
                            String.split "," nicknamesStr
                    in
                    ( { model | nicknames = nicknames }, Cmd.none )

                Err httpError ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendHttpRequest ]
            [ text "Get data from server" ]
        , h3 [] [ text "Old School Main Characters" ]
        , ul [] (List.map viewNickname model.nicknames)
        ]


viewNickname : String -> Html Msg
viewNickname nickname =
    li [] [ text nickname ]


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( { nicknames = [], errorMessage = Nothing }, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
