module HomePage exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (Renderable, Shape, circle, path, quadraticCurveTo, rect, shapes, texture)
import Canvas.Settings exposing (..)
import Canvas.Settings.Line exposing (..)
import Canvas.Texture as Texture exposing (Texture)
import Color exposing (lightBlue)
import Debug exposing (..)
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task


type Shapes
    = Line
    | Rectangle
    | Round


type alias Model =
    { activeMode : Shapes
    , toDraw : List Renderable
    , frame : Float
    , file : Maybe File
    , preview : String
    , texture : Load Texture
    }


initialModel =
    { activeMode = Line
    , toDraw = []
    , frame = 0
    , file = Nothing
    , preview = ""
    , texture = Loading
    }


defaultCanvasWidth =
    500


defaultCanvasHeight =
    500


type Load a
    = Loading
    | Success a
    | Failure


type Msg
    = SetLineMode
    | SetRectangleMode
    | SetRoundMode
    | AnimationFrame Float
    | OnImageUpload File
    | OnImageToUrl String
    | OnImageRequest
    | TextureLoaded (Maybe Texture)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetLineMode ->
            ( { model | toDraw = [ drawLine [ path ( 200, 100 ) [ quadraticCurveTo ( 250, 150 ) ( 240, 170 ) ] ] ] }, Cmd.none )

        SetRectangleMode ->
            ( { model | activeMode = Rectangle }, Cmd.none )

        SetRoundMode ->
            ( { model | toDraw = [ drawCircle [ circle ( 200, 100 ) 100 ] ] }, Cmd.none )

        AnimationFrame delta ->
            ( { model | frame = delta + 1 }, Cmd.none )

        OnImageRequest ->
            ( model
            , Select.file [ "image" ] OnImageUpload
            )

        OnImageUpload file ->
            let
                _ =
                    Debug.log "123" "changed"
            in
            ( { model | file = Just file }, Task.perform OnImageToUrl (File.toUrl file) )

        OnImageToUrl url ->
            ( { model | preview = url }, Cmd.none )

        TextureLoaded Nothing ->
            ( { model | texture = Failure }
            , Cmd.none
            )

        TextureLoaded (Just texture) ->
            ( { model | texture = Success texture }, Cmd.none )


drawCircle : List Shape -> Renderable
drawCircle circle =
    circle
        |> shapes
            [ lineCap RoundCap
            , lineJoin RoundJoin
            , lineWidth 20
            , stroke lightBlue
            ]


drawLine : List Shape -> Renderable
drawLine line =
    line
        |> shapes
            [ lineCap RoundCap
            , lineJoin RoundJoin
            , lineWidth 20
            , stroke lightBlue
            ]


view model =
    let
        canvasSize =
            getCanvasSize model
    in
    div []
        [ h1 [] [ text "Welcome to Dunder Mifflin!" ]
        , button
            [ classList [ ( "nav-item", True ), ( "active", model.activeMode == Line ) ]
            , onClick SetLineMode
            ]
            [ text "Line" ]
        , button [ Html.Events.onClick SetRectangleMode ] [ text "Rectangle" ]
        , button [ onClick SetRoundMode ] [ text "Round" ]
        , button []
            [ text
                (if model.activeMode == Rectangle then
                    "Draw line"

                 else
                    "Test"
                )
            ]
        , Canvas.toHtmlWith
            { width = canvasSize.width
            , height = canvasSize.height
            , textures = [ Texture.loadFromImageUrl model.preview TextureLoaded ]
            }
            [ style "touch-action"
                "none"

            --, onMouseDown StartAt
            --, onMouseUp EndAt
            , --, Mouse.onDown (.offsetPos >> StartAt)
              --, Mouse.onMove (.offsetPos >> MoveAt)
              --, Mouse.onUp (.offsetPos >> EndAt)
              --
              ---- These 2 get annoying sometimes when painting
              ---- , Mouse.onLeave (.offsetPos >> EndAt)
              ---- , Mouse.onContextMenu (.offsetPos >> EndAt)
              --, onTouch "touchstart" (touchCoordinates >> StartAt)
              --, onTouch "touchmove" (touchCoordinates >> MoveAt)
              --, onTouch "touchend" (touchCoordinates >> EndAt)
              id
                "canvas"
            , class "canvas"
            ]
            [ case model.texture of
                Loading ->
                    shapes [] [ rect ( 0, 0 ) 10 10 ]

                Failure ->
                    shapes [] [ rect ( 0, 0 ) 10 10 ]

                Success loadedTexture ->
                    texture [] ( 0, 0 ) loadedTexture
            ]
        , button [ onClick OnImageRequest ] [ text "Load Image" ]
        , div [] [ text (Debug.toString model.file) ]
        ]


getCanvasSize : Model -> { width : Int, height : Int }
getCanvasSize model =
    case model.texture of
        Loading ->
            { width = defaultCanvasWidth, height = defaultCanvasHeight }

        Failure ->
            { width = defaultCanvasWidth, height = defaultCanvasHeight }

        Success loadedTexture ->
            let
                textureSize =
                    Texture.dimensions loadedTexture
            in
            { width = ceiling textureSize.width, height = ceiling textureSize.height }


subscriptions : Model -> Sub Msg
subscriptions model =
    onAnimationFrameDelta AnimationFrame


main : Program () Model Msg
main =
    Browser.element
        { init = \flag -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
