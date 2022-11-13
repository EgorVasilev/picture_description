module HomePage exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (Point, Renderable, Shape, circle, path, quadraticCurveTo, rect, shapes, texture)
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
import Html.Events.Extra.Mouse as Mouse exposing (Event)
import Html.Events.Extra.Touch as Touch
import Task


type Shapes
    = Line
    | Rectangle
    | Round


type alias DrawingPointer =
    { previousMidpoint : Point, lastPoint : Point }


type alias Model =
    { activeMode : Shapes
    , toDraw : List Renderable
    , frame : Float
    , file : Maybe File
    , preview : String
    , texture : Load Texture
    , drawingPointer : Maybe DrawingPointer
    }


initialModel =
    { activeMode = Line
    , toDraw = []
    , frame = 0
    , file = Nothing
    , preview = ""
    , texture = Loading
    , drawingPointer = Nothing
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
    = AnimationFrame Float
    | OnImageUpload File
    | OnImageToUrl String
    | OnImageRequest
    | TextureLoaded (Maybe Texture)
    | StartAt Point
    | MoveAt Point
    | EndAt Point


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        StartAt point ->
            ( initialPoint point model, Cmd.none )

        MoveAt point ->
            case model.drawingPointer of
                Just pointer ->
                    ( drawPoint point pointer model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        EndAt point ->
            case model.drawingPointer of
                Just pointer ->
                    ( finalPoint point pointer model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


initialPoint point model =
    { model
        | drawingPointer = Just { previousMidpoint = point, lastPoint = point }
    }


drawPoint newPoint { previousMidpoint, lastPoint } model =
    let
        newMidPoint =
            controlPoint lastPoint newPoint
    in
    { model
        | drawingPointer = Just { previousMidpoint = newMidPoint, lastPoint = newPoint }
        , toDraw =
            List.append
                [ drawLine
                    [ path previousMidpoint [ quadraticCurveTo lastPoint newMidPoint ] ]
                ]
                model.toDraw
    }


finalPoint point { previousMidpoint, lastPoint } model =
    { model
        | drawingPointer = Nothing
        , toDraw =
            List.append
                [ drawLine
                    [ path previousMidpoint [ quadraticCurveTo lastPoint point ] ]
                ]
                model.toDraw
    }


drawLine : List Shape -> Renderable
drawLine line =
    line
        |> shapes
            [ lineCap RoundCap
            , lineJoin RoundJoin
            , lineWidth (toFloat 2)
            , stroke Color.orange
            ]



--calculates "control point" to make a curve between points sequence(more: https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/quadraticCurveTo)


controlPoint ( x1, y1 ) ( x2, y2 ) =
    ( x1 + (x2 - x1) / 2, y1 + (y2 - y1) / 2 )


view model =
    let
        canvasSize =
            getCanvasSize model
    in
    div []
        [ h1 [] [ text "Welcome to Dunder Mifflin!" ]
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
            , Mouse.onDown (.offsetPos >> StartAt)
            , Mouse.onMove (.offsetPos >> MoveAt)
            , Mouse.onUp (.offsetPos >> EndAt)
            , Mouse.onLeave (.offsetPos >> EndAt)
            , Mouse.onContextMenu (.offsetPos >> EndAt)

            --, onTouch "touchstart" (touchCoordinates >> StartAt)
            --, onTouch "touchmove" (touchCoordinates >> MoveAt)
            --, onTouch "touchend"
            --    (touchCoordinates >> EndAt)
            , id "canvas"
            , class "canvas"
            ]
            (List.append
                [ case model.texture of
                    Loading ->
                        shapes [] [ rect ( 0, 0 ) 10 10 ]

                    Failure ->
                        shapes [] [ rect ( 0, 0 ) 10 10 ]

                    Success loadedTexture ->
                        texture [] ( 0, 0 ) loadedTexture
                ]
                model.toDraw
            )
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
