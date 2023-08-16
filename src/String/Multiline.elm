module String.Multiline exposing (here)

{-| Tiny module to solve multiline string indentation problems.

@docs here

-}


{-| When applied to a multi-line String (starting and ending with `"""`, see [elm docs section about literal syntax](https://elm-lang.org/docs/syntax#literals)),

  - removes the empty line directly after the opening `"""` and directly before the closing `"""`:

        """
        test
        """
        --→ "\ntest\n"

    becomes

        here """
        test
        """
        --> "test"

  - re-indents all lines to the lowest existing level of indentation including the last line but not the first

        """
            test
          """
        --→ "\n    test\n  "

    becomes

        here """
            test
          """
        --> "  test"

    and

        """
          test
            """
        --→ "\n  test\n    "

    becomes

        here """
          test
            """
        --> "test"

so together they change

    "a"
        ++ """
           test
           """
    --→ "a\n       test\n       "

to

    "a"
        ++ here """
           test
           """
    --> "atest"

  - if your first or last line accidentally aren't just whitespace, they will be formatted as normal content lines, but indentation of the first line will not be counted

        here """  oops
            test
            """
        --> "  oops\ntest"

        here """
            test
          oops"""
        --> "  test\noops"

  - single-line """ Strings are left as they are

        here """    test    """
        --> "    test    "

-}
here : String -> String
here string =
    case string |> String.lines of
        -- String.lines always returns a filled list
        [] ->
            string

        onlyLine :: [] ->
            -- single-line strings are kept
            onlyLine

        firstLine :: secondLine :: linesAfterSecond ->
            let
                linesAfterFirstUnindented : ListFilled { level : Int, unindented : String }
                linesAfterFirstUnindented =
                    ( secondLine, linesAfterSecond ) |> listFilledMap unindent

                minLevel : Int
                minLevel =
                    linesAfterFirstUnindented
                        |> listFilledMap .level
                        |> listFilledMinimum

                reIndent : List { level : Int, unindented : String } -> List String
                reIndent unindentedLines =
                    List.map
                        (\line ->
                            String.repeat (line.level - minLevel) " " ++ line.unindented
                        )
                        unindentedLines

                reIndentedLines : List String
                reIndentedLines =
                    case ( firstLine, listFilledLast linesAfterFirstUnindented |> .unindented ) of
                        ( "", "" ) ->
                            reIndent (listFilledInit linesAfterFirstUnindented)

                        ( "", _ ) ->
                            reIndent (listFilledToList linesAfterFirstUnindented)

                        ( _, "" ) ->
                            firstLine :: reIndent (listFilledInit linesAfterFirstUnindented)

                        ( _, _ ) ->
                            firstLine :: reIndent (listFilledToList linesAfterFirstUnindented)
            in
            reIndentedLines
                |> String.join "\n"



-- String helpers


unindent : String -> { level : Int, unindented : String }
unindent =
    unindentFromLevel 0


unindentFromLevel : Int -> String -> { level : Int, unindented : String }
unindentFromLevel alreadyReducedLevel partiallyUnindentedString =
    case String.uncons partiallyUnindentedString of
        Nothing ->
            { level = alreadyReducedLevel, unindented = "" }

        Just ( ' ', unindentedBy1More ) ->
            unindentFromLevel (alreadyReducedLevel + 1) unindentedBy1More

        Just ( contentHeadChar, contentTailString ) ->
            { level = alreadyReducedLevel
            , unindented = String.cons contentHeadChar contentTailString
            }



-- ListFilled helpers


type alias ListFilled a =
    ( a, List a )


listFilledToList : ListFilled a -> List a
listFilledToList ( head, tail ) =
    head :: tail


listFilledLast : ListFilled a -> a
listFilledLast listFilled =
    case listFilled of
        ( onlyElement, [] ) ->
            onlyElement

        ( _, tailHead :: tailTail ) ->
            listFilledLast ( tailHead, tailTail )


listFilledInit : ListFilled a -> List a
listFilledInit listFilled =
    case listFilled of
        ( _, [] ) ->
            []

        ( head, tailHead :: tailTail ) ->
            head :: listFilledInit ( tailHead, tailTail )


listFilledMap : (a -> b) -> ListFilled a -> ListFilled b
listFilledMap elementChange ( head, tail ) =
    ( elementChange head, List.map elementChange tail )


listFilledMinimum : ListFilled comparable -> comparable
listFilledMinimum ( head, tail ) =
    case List.minimum tail of
        Just tailMinimum ->
            Basics.min head tailMinimum

        Nothing ->
            head
