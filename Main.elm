module Main exposing (Model(..), Msg(..), after, current, editor, main, previous, update, view)

-- import Html.Attributes exposing (..)

import Browser
import Debug
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, button, div)
import List exposing (..)
import String exposing (endsWith, fromList, toList, trim,words)


main =
    Browser.sandbox { init = Model [] 0, update = update, view = view }


type Msg
    = TextInput String
    | ReRender String


type Model
    = Model (List String) Int


update msg (Model xs i) =
    case msg of
        TextInput s ->
            let
                _ =
                    Debug.log ":" xs

                pre =
                    take i xs

                end =
                    drop (i + 1) xs

                cur =
                    trim s

                next =
                    if i == 0 then
                        List.map (\x -> fromList [ x ]) <| drop 1 <| toList cur

                    else
                        List.map (\x -> fromList [ x ]) <| toList cur
            in
            if endsWith " " s then
                Model (pre ++ [ trim cur ] ++ end ++ next) (i + 1)

            else
                Model (pre ++ [ cur ] ++ end) i

        ReRender s ->
            -- break s into words, and replace that many items in list with them, 
            let
                current_pre = words s

                end =
                    drop i xs

            in
            Model (current_pre ++ end) i


previous : Model -> String
previous (Model xs i) =
    case xs of
        [] ->
            ""

        _ ->
            String.concat <| intersperse " " <| take i xs


after : Model -> String
after (Model xs i) =
    case xs of
        [] ->
            ""

        _ ->
            String.concat <| intersperse " " <| drop (i + 1) xs


current : Model -> String
current (Model xs i) =
    let
        item =
            case xs of
                [] ->
                    Nothing

                _ ->
                    head <| take 1 <| drop i xs
    in
    case item of
        Nothing ->
            ""

        Just s ->
            s


view model =
    Element.layout [] <| editor model


editor model =
    paragraph [ centerY, centerX, width (fill |> Element.maximum 1000), Font.size 30 ]
        [ Input.text
            [ Border.widthEach { bottom = 0, left = 0, right = 0, top = 0 }
            , focused [ Border.shadow { offset = ( 0, 0 ), size = 0, blur = 0, color = rgb 0 0 0 } ]
            ]
            { onChange = ReRender, text = previous model, placeholder = Nothing, label = Input.labelHidden "" }
        , Input.text
            [ width (shrink |> Element.maximum 150)
            , Input.focusedOnLoad
            , Border.widthEach { bottom = 0, left = 0, right = 0, top = 0 }
            , focused [ Border.shadow { offset = ( 0, 0 ), size = 0, blur = 0, color = rgb 0 0 0 } ]
            ]
            { onChange = TextInput, text = current model, placeholder = Nothing, label = Input.labelHidden "" }
        , text <| after model
        ]
