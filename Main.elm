-- module Main exposing (Model(..), Msg(..), after, current, editor, main, previous, re_render, restrict_head_deletion, update, validate_prefix, view)


module Main exposing (..)

-- import Html.Attributes exposing (..)

import Browser
import Browser.Dom as Dom
import Char exposing (isAlpha)
import Debug
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html as Html
import Html.Attributes as Attr
import Html.Events exposing (keyCode, on)
import Json.Decode as Json
import List exposing (..)
import String exposing (dropLeft, endsWith, fromList, toList, trim, words)
import Task
import Tuple exposing (first, second)


empty_chain =
    { words = [], current = 0 }


init_chains =
    -- [ { words = [ "The", "hen", "e" ], current = 2 }, { words = [ "\n" ], current = 1 }, { words = [ "The", "hen", "e" ], current = 2 } ]
    [ empty_chain ]


init_model =
    { chains = init_chains, active = 0, tooltips = False}


init : () -> ( Model, Cmd Msg )
init _ =
    ( init_model, focus_entry )


subs model =
    Sub.none


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }


type Msg
    = Select Int
    | Delete Int
    | AddChain
    | Copy
    | TextInput Int String
    | ReRender Int Int String
    | StrNop String
    | NoOp


type alias Chain =
    { words : List String, current : Int }


type alias Model =
    { chains : List Chain, active : Int, tooltips: Bool}


focus_entry =
    Task.attempt (\_ -> NoOp) (Dom.focus "entry")


restrict_head_deletion : List String -> List String -> List String
restrict_head_deletion prev_words curr_words =
    let
        compare_word p c =
            case c == p of
                True ->
                    p

                False ->
                    let
                        dif =
                            String.length p - String.length c

                        all_equal str =
                            case String.toList str of
                                x :: [] ->
                                    False

                                x :: xs ->
                                    all (\s -> s == x) <| String.toList str

                                [] ->
                                    False
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
            list_to_prefixes before_active

        current_and_before =
            take (i + 1) (before_active ++ active_onwards)

        ( valid_words, remaining_prefixes ) =
            validate_prefix current_and_before prefix_list []

        new_words =
            valid_words ++ remaining_prefixes

        new_index =
            if length valid_words > i then
                i

            else
                length valid_words
    in
    ( new_words, new_index )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextInput chain_id s ->
            let
                chain =
                    Maybe.withDefault empty_chain <| head << take 1 << drop chain_id <| model.chains

                pre =
                    take chain.current chain.words

                old_input =
                    drop chain.current chain.words
                        |> take 1
                        |> head
                        |> Maybe.withDefault ""

                new_input =
                    if endsWith "\n" s then
                        restrict_head_deletion [ old_input ] [ s ]

                    else
                        restrict_head_deletion [ old_input ] [ trim s ]

                prefixes =
                    list_to_prefixes (pre ++ new_input)

                next =
                    List.map String.fromChar <| drop (chain.current + 1) prefixes

                new_chain =
                    if endsWith " " s || endsWith "\n" s then
                        Chain (pre ++ new_input ++ next) (chain.current + 1)

                    else
                        Chain (pre ++ new_input ++ next) chain.current

                new_model =
                    let
                        prev_chains =
                            take model.active model.chains

                        next_chains =
                            drop (model.active + 1) model.chains
                    in
                    { model | chains = prev_chains ++ [ new_chain ] ++ next_chains }
            in
            ( new_model
            , focus_entry
            )

        ReRender chain_id text_position text_input ->
            -- text_poisition is position of the changed word in the chain
            let
                -- ( new_words, new_index ) =
                --     re_render xs (words s) i
                -- xs = all words in chain
                -- s = string of all words before chain.current but with text_input at word_position
                -- this is legacy logic
                chain =
                    Maybe.withDefault empty_chain <| head <| drop chain_id model.chains

                all_words =
                    chain.words

                before_active_word =
                    take chain.current chain.words

                edited_segment =
                    take text_position before_active_word ++ [ text_input ] ++ drop (text_position + 1) before_active_word

                ( new_words, new_index ) =
                    re_render all_words edited_segment chain.current

                new_chain =
                    { words = new_words, current = new_index }

                new_model =
                    let
                        prev_chains =
                            take chain_id model.chains

                        next_chains =
                            drop (chain_id + 1) model.chains
                    in
                    { model | chains = prev_chains ++ [ new_chain ] ++ next_chains }
            in
            ( new_model, Cmd.none )

        Select index ->
            let
                _ =
                    Debug.log "Selct" index
            in
            ( { model | active = index }, Cmd.none )

        AddChain ->
            let
                _ =
                    Debug.log "add chain" model
            in
            ( { model | chains = model.chains ++ [ empty_chain ], active = List.length model.chains }
            , focus_entry
            )

        Delete chain_id ->
            let
                _ =
                    Debug.log "add chain" model

                chains =
                    if List.length model.chains == 1 then
                        init_chains

                    else
                        take chain_id model.chains ++ drop (chain_id + 1) model.chains
            in
            ( { model | chains = chains, active = 0 }
            , focus_entry
            )

        Copy ->
            let
                _ =
                    Debug.log "Copy" <| """""" ++ fold_doc model
            in
            ( model, Cmd.none )

        _ ->
            ( model
            , Cmd.none
            )


fold_doc model =
    model.chains
        |> List.map (\x -> take x.current x.words |> intersperse " ")
        |> intersperse [ " " ]
        |> List.concat
        |> String.concat


view model =
    Element.layout [] <| editor model


editor model =
    let
        -- rendered_chains =
        --     List.indexedMap (show_chain model.active) model.chains
        rendered_chains =
            show_chains model.active model.chains
    in
    column
        [ width fill
        , height fill
        , Font.family [ Font.monospace ]
        , spacing 50
        , Background.color background_grey
        ]
        [ status_bar model
        , el
            [ width fill
            , height fill
            , paddingXY 200 0
            -- , behindContent (copy_target <| fold_doc model)
            ]
          <|
            el
                [ width (fill |> Element.minimum 1000)
                , height fill
                , centerX
                , Background.color white
                , Border.widthEach { edges | top = 1, bottom = 0, right = 1, left = 1 }
                , Border.color border_grey
                , Border.shadow { offset = ( 0, 0 ), size = 0, blur = 2, color = border_shadow }
                , paddingXY 0 120
                ]
            <|
                wrappedRow [ Font.size 30, centerX, width (px 1000), Font.color font_dark_grey] rendered_chains
        ]


-- copy_target val =
--     Input.text
--         [ htmlAttribute <| Attr.id "copy-me"
--         , alignBottom
--         , Font.color background_grey
--         , Border.width 0
--         , Background.color background_grey
--         ]
--         { onChange = StrNop
--         , text = val
--         , placeholder = Nothing
--         , label = Input.labelHidden ""
--         }


edges =
    { bottom = 0
    , left = 0
    , right = 0
    , top = 0
    }


border_grey =
    rgb255 235 235 235


border_shadow =
    rgb255 232 232 235


background_grey =
    rgb255 250 250 250


white =
    rgb255 255 255 255


blue =
    rgb255 5 100 245

font_dark_grey = rgb255 100 100 100

font_grey =
    rgb255 110 110 110


font_faded =
    rgb255 185 185 185


red =
    rgb255 225 140 140


input_background =
    rgb255 240 255 240

green =
    rgb255 140 225 140

transparent_green = rgba255 140 225 140 0.05
transparent_grey = rgba255 185 185 185 0.1

status_bar model =
    row
        [ width fill
        , height (px 80)
        , Background.color (rgb255 255 255 255)
        , Border.widthEach { edges | bottom = 1 }
        , Border.color border_grey
        , Border.shadow { offset = ( 0, 0 ), size = 0, blur = 1, color = border_shadow }
        , paddingXY 100 0
        ]
        [ status_display model
        , buttons_container <| fold_doc model
        ]


status_display model =
    let
        char_count = String.length <| String.filter (\c-> c /= '\n' ) <| fold_doc model
        chain_count = List.length model.chains
    in el [ alignLeft, Font.color font_grey ] <| text ("" ++ String.fromInt char_count ++ " characters | " ++ "chains " ++ String.fromInt chain_count )


buttons_container copy_text =
    row
        [ alignRight
        , height (px 50)
        , centerY
        , Border.widthEach { edges | left = 1 }
        , Border.color border_grey
        , spacing 10
        ]
        [ Input.button
            [ height (px 40)
            , centerY
            , Border.width 2
            , Border.color blue
            , Font.size 12
            , Font.center
            , Font.color blue
            , Font.bold
            , width (px 100)
            ]
            { onPress = Just AddChain
            , label = text "New Chain"
            }
        , Input.button
            [ htmlAttribute <| Attr.class "copy"
            , htmlAttribute <| Attr.attribute "data-clipboard-text" copy_text
            , height (px 40)
            , centerY
            , Border.width 2
            , Border.color blue
            , Font.size 12
            , Font.center
            , Font.color blue
            , Font.bold
            , width (px 100)
            ]
            { onPress = Just Copy
            , label = text "Copy"
            }
        ]


show_chains : Int -> List Chain -> List (Element Msg)
show_chains active chains =
    let
        document_flatmap : ( List String, List Int )
        document_flatmap =
            chains
                |> List.map (\x -> take x.current x.words)
                |> List.map (\x -> ( x, [ length x ] ))
                |> List.foldr
                    (\a b ->
                        ( List.append (first a) (first b)
                        , List.append (second a) (second b)
                        )
                    )
                    ( [], [] )

        _ =
            Debug.log "Flat map" document_flatmap
    in
    concat <| List.indexedMap (show_chain active document_flatmap) chains


show_chain : Int -> ( List String, List Int ) -> Int -> Chain -> List (Element Msg)
show_chain active flatmap chain_id chain =
    let
        previous_words =
            case chain.words of
                [] ->
                    [ none ]

                _ ->
                    -- [ input_previous_ (ReRender_ chain) chain_id (String.concat <| intersperse " " <| take chain.current chain.words) ]
                    List.indexedMap (input_previous flatmap chain.words chain_id) <| take chain.current chain.words

        -- List.map text <| take chain.current chain.words
        prefixes =
            case chain.words of
                [] ->
                    [ none ]

                _ ->
                    if active == chain_id then
                        List.map (\x -> el [ alignBottom, Font.color (rgb255 180 181 179) ] (text x)) <| intersperse " " <| drop (chain.current + 1) chain.words
                        -- List.indexedMap (input_previous chain.words chain_id) <| drop (chain.current + 1) chain.words

                    else
                        [ none ]

        current_word =
            let
                item =
                    case chain.words of
                        [] ->
                            Nothing

                        _ ->
                            head <| take 1 <| drop chain.current chain.words
            in
            case item of
                Nothing ->
                    input_current (active == chain_id) chain_id ""

                Just s ->
                    input_current (active == chain_id) chain_id s
    in
    previous_words ++ [ current_word ] ++ prefixes


input_previous flatmap words chain_id position word =
    let
        prev_in_current_chain =
            String.concat <| take position words

        previous_chains_combined_length =
            foldl (+) 0 <| take chain_id (second flatmap)

        previous_chains_combined =
            take previous_chains_combined_length (first flatmap)

        prev_words =
            String.concat <| previous_chains_combined ++ take position words

        prev_count =
            Maybe.withDefault (String.length prev_words) <| head <| String.indexes "\n" <| String.reverse prev_words

        label =
            if position == 0 then
                Input.labelAbove [ pointer, width fill, height fill ] <| el [ onClick (Delete chain_id), alignLeft, centerY, width (px 10), height (px 10), Border.rounded 10, Background.color red ] <| text ""

            else
                Input.labelHidden ""

        -- _ =
        --     if chain_id == 1 && position == 1 then
        --         Debug.log "position" position
        --     else
        --         -1
        -- _ =
        --     if chain_id == 1 && position == 1 then
        --         Debug.log "words" words
        --     else
        --         []
        -- _ =
        --     if chain_id == 1 && position == 1 then
        --         Debug.log "prev" prev_words
        --     else
        --         ""
        -- _ =
        --     if chain_id == 1 && position == 1 then
        --         Debug.log "count" prev_count
        --     else
        --         -1
        -- prev_count =
        --     Maybe.withDefault (String.length prev_chars) <| head <| String.indexes "\n" <| String.reverse <| String.concat <| take position words
    in
    Input.text
        [ Border.width 0
        , onClick (Select chain_id)
        , focused [ Border.shadow { offset = ( 0, 0 ), size = 0, blur = 0, color = rgb 0 0 0 } ]
        , width <|
            if String.endsWith "\n" word then
                px <| 1000 - (modBy 46 prev_count * 21) - 30

            else
                px (21 * String.length word)
        , padding 0
        , onEnter <| ReRender chain_id position (word ++ "\n")
        , alignBottom
        ]
        { onChange = ReRender chain_id position
        , text = word
        , placeholder = Nothing
        , label = label
        }


onEnter : Msg -> Attribute Msg
onEnter message =
    let
        isEnter code =
            if code == 13 then
                Json.succeed message

            else
                Json.fail "not ENTER"
    in
    htmlAttribute <| on "keydown" (Json.andThen isEnter keyCode)


input_current active chain_id word =
    let
        calc w =
            if String.length w > 0 && active then
                21 * String.length w

            else
                21

        (pointer_color,highlight)=
            if active then
                (green,transparent_green)

            else
                (font_faded,transparent_grey)

        label =
            Input.labelAbove [ pointer, width fill, height fill ] <| el [ alignLeft, centerY, width (px 10), height (px 10), Border.rounded 10, Background.color pointer_color ] <| text ""

    in
    Input.text
        [ Border.width 0
        , focused [ Border.shadow { offset = ( 0, 0 ), size = 0, blur = 0, color = rgb 0 0 0 } ]
        , onClick (Select chain_id)
        , Background.color highlight
        , Border.rounded 0
        , width <|
            px
                (if calc word < 50 && active then
                    50

                 else
                    calc word
                )
        , padding 0
        , alignBottom
        , onEnter <| TextInput chain_id (word ++ "\n")
        , htmlAttribute <|
            if active then
                Attr.id "entry"

            else
                Attr.id ""
        ]
        { onChange = TextInput chain_id
        , text =
            if active then
                word

            else
                ""
        , placeholder = Nothing
        , label = label
        }
