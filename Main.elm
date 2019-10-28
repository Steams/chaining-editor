module Main exposing (Model(..), Msg(..), after, current, editor, main, previous, re_render, restrict_head_deletion, update, validate_prefix, view)

-- import Html.Attributes exposing (..)

import Browser
import Char exposing (isAlpha)
import Debug
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (textarea)
import Html.Attributes exposing (value)
import List exposing (..)
import String exposing (dropLeft, endsWith, fromList, toList, trim, words)
import Tuple


main =
    Browser.sandbox { init = Model [] 0, update = update, view = view }


type Msg
    = TextInput String
    | ReRender String


type Model
    = Model (List String) Int



-- take the previous state of all words and the current state and prevent the first letter of a word from being deleted


restrict_head_deletion prev_words curr_words =
    let
        compare_word p c =
            case c == p of
                True ->
                    p

                False ->
                    let
                        dif = String.length p - String.length c
                        all_equal str=
                            case String.toList str of
                                x::[] -> False
                                x::xs -> all (\ s -> s == x) <| String.toList str
                                [] -> False

                    in
                        if String.dropLeft dif p == c then
                            if all_equal (String.left (dif + 1) p) then
                                c
                            else
                                p

                    else
                        c
    in
    map2 compare_word prev_words curr_words



-- take the previous state of all words and the current state and re-render so it follows the prefix rules
-- checks if each word matches its prefix
-- upon finding the first invalid word, return the accumulated valid words and append the prefixes as the rest of words, also return the position of the new current word to be input
-- validate_prefix (l::ls) (p::ps) accumulator =
-- takes the currnt words, the prefix list, and an accumulator
-- if a word does not start with its paired prefix, return all the words that do match up until the mismatch and a new unused remaining prefix list
-- TODO change when u call this to not pass in all words, just up to and including current word


list_to_prefixes : List String -> List Char
list_to_prefixes input =
    input
        |> String.concat
        |> String.toList
        |> List.filter isAlpha


validate_prefix : List String -> List Char -> List String -> ( List String, List String )
validate_prefix sentence prefixes accumulator =
    case ( sentence, prefixes ) of
        ( l :: ls, p :: ps ) ->
            let
                _ =
                    Debug.log "CASE :"

                _ =
                    Debug.log "sentence" sentence

                _ =
                    Debug.log "prefixes" prefixes

                _ =
                    Debug.log "current word" l

                _ =
                    Debug.log "current prefix" p

                _ =
                    Debug.log "accumulated" accumulator

                first_char str =
                    case head (String.toList str) of
                        Just c ->
                            c

                        _ ->
                            '%'
            in
            case first_char l == p of
                True ->
                    validate_prefix ls ps (accumulator ++ [ l ])

                False ->
                    ( accumulator, drop (length accumulator) <| List.map String.fromChar <| list_to_prefixes accumulator )

        ( _, ps ) ->
            let
                _ =
                    Debug.log "BASE :"

                _ =
                    Debug.log "accumulated" accumulator

                _ =
                    Debug.log "index" (length accumulator - 1)
            in
            ( accumulator, drop (length accumulator) <| List.map String.fromChar <| list_to_prefixes accumulator )



-- Takes the state, and the section before the current word which contains chnages, and rerenders a valid new model


re_render : List String -> List String -> Int -> ( List String, Int )
re_render original_words curr_before_active curr_index =
    let
        i =
            curr_index

        original_before_active =
            take i original_words

        before_active =
            restrict_head_deletion original_before_active curr_before_active

        active_onwards =
            drop i original_words

        prefix_list =
            List.map String.toList before_active
                |> concat
                |> List.filter isAlpha

        _ =
            Debug.log "restricted before active " before_active

        current_and_before =
            take (i + 1) (before_active ++ active_onwards)

        _ =
            Debug.log "all words" current_and_before

        ( valid_words, remaining_prefixes ) =
            validate_prefix current_and_before prefix_list []

        new_words =
            valid_words ++ remaining_prefixes

        new_index =
            if length valid_words > i then
                i

            else
                length valid_words

        _ =
            Debug.log "new words" new_words

        _ =
            Debug.log "new index" new_index
    in
    ( new_words, new_index )


update msg (Model xs i) =
    case msg of
        TextInput s ->
            let
                pre =
                    take i xs

                _ =
                    Debug.log "input :" s

                old_input =
                    let
                        extract_input x =
                            drop i x |> take 1 |> head
                    in
                    case extract_input xs of
                        Just str ->
                            str

                        _ ->
                            ""

                new_input = restrict_head_deletion [old_input] [trim s]

                prefixes =
                    (pre ++ new_input )
                        |> String.concat
                        |> String.toList
                        |> List.filter isAlpha

                next =
                    List.map String.fromChar <| drop (i + 1) prefixes

                _ =
                    Debug.log "Model :" (pre ++ new_input++ next)
            in
            if endsWith " " s then
                Model (pre ++ new_input ++ next) (i + 1)

            else
                Model (pre ++ new_input ++ next) i

        ReRender s ->
            let
                ( new_words, new_index ) =
                    re_render xs (words s) i
            in
            Model new_words new_index




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
            , Border.widthEach { bottom = 0, left = 1, right = 0, top = 0 }
            , focused [ Border.shadow { offset = ( 0, 0 ), size = 0, blur = 0, color = rgb 0 0 0 } ]
            ]
            { onChange = TextInput, text = current model, placeholder = Nothing, label = Input.labelHidden "" }
        , text <| after model
        -- , html <| textarea [ value "area" ] []
        ]


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
