module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (re_render, restrict_backspace, validate_prefix)
import String exposing (words)
import Test exposing (..)


backspace_tests =
    let
        start =
            [ "The", "hen", "enters" ]
    in
    describe "restrict backspace"
        [ test "Unchanged text is valid" (\_ -> Expect.equal start <| restrict_backspace start start)
        , test "Backspace middle of word is valid"
            (\_ ->
                let
                    current =
                        [ "The", "hn", "enters" ]
                in
                Expect.equal current <| restrict_backspace start current
            )
        , test "Backspace end of word is valid"
            (\_ ->
                let
                    current =
                        [ "The", "he", "enters" ]
                in
                Expect.equal current <| restrict_backspace start current
            )
        , test "Backspace start of word is not valid"
            (\_ ->
                let
                    current =
                        [ "The", "en", "enters" ]
                in
                Expect.equal start <| restrict_backspace start current
            )
        ]


validate_prefix_tests =
    let
        words_start =
            [ "Abc", "bx", "cat" ]

        index =
            2

        -- current active word is "cat"
        prefixes_start =
            words_start |> String.concat |> String.toList
    in
    describe "Prefix Validation"
        [ test "Unchanged text is valid"
            (\_ ->
                let
                    output =
                        ( words_start, String.split " " "b x c a t" )
                in
                Expect.equal output <| validate_prefix words_start prefixes_start []
            )
        , describe "Backspacing unused letter just removes it from prefix list"
            [ test "When removing the last char"
                (\_ ->
                    let
                        input =
                            String.split " " "Abc bx ca"

                        output =
                            ( input, String.split " " "b x c a" )

                        prefixes =
                            input |> String.concat |> String.toList
                    in
                    Expect.equal output <| validate_prefix input prefixes []
                )
            , test "When removing a character that doesnt affect a word currently in the list"
                (\_ ->
                    let
                        input =
                            String.split " " "Abc b cat"

                        output =
                            ( input, String.split " " "b c a t" )

                        prefixes =
                            input |> String.concat |> String.toList
                    in
                    Expect.equal output <| validate_prefix input prefixes []
                )
            ]
        , test "Removing a character that affects another word in the list should truncate the list and return new prefixes"
            (\_ ->
                let
                    input =
                        String.split " " "Ab bx cat"

                    output =
                        ( String.split " " "Ab bx", String.split " " "b x" )

                    prefixes =
                        input |> String.concat |> String.toList

                    _ =
                        Debug.log "prefix list" prefixes
                in
                Expect.equal output <| validate_prefix input prefixes []
            )
        ]


re_render_tests =
    let
        start_string =
            "Abc bx cat b x c a t"

        start =
            String.split " " start_string

        index =
            2

        -- current word is "c"
    in
    describe "Re-rendering "
        [ test "Unchanged text is valid"
            (\_ ->
                let
                    changed_segment =
                        String.split " " "Abc bx"

                    output =
                        ( start, index )
                in
                Expect.equal output <| re_render start changed_segment index
            )
        , test ("Deleting prefix which coresponds to a future word should just remove that from prefix list | Removing x from " ++ start_string ++ " Where cat is active word, index 2")
            (\_ ->
                let
                    changed_segment =
                        String.split " " "Abc b"

                    output =
                        ( String.split " " "Abc b cat b c a t", index )
                in
                Expect.equal output <| re_render start changed_segment index
            )
        ]
