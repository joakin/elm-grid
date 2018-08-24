module GridTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Grid
import Test exposing (..)


suite : Test
suite =
    describe "Grid"
        [ describe "fold2d"
            [ test "Folds over a positive number of rows and columns" <|
                \_ ->
                    let
                        dimensions =
                            { rows = 3, cols = 2 }

                        result =
                            [ ( 0, 0 )
                            , ( 1, 0 )
                            , ( 0, 1 )
                            , ( 1, 1 )
                            , ( 0, 2 )
                            , ( 1, 2 )
                            ]

                        coords ( x, y ) res =
                            ( x, y ) :: res
                    in
                    Grid.fold2d dimensions coords []
                        |> List.reverse
                        |> Expect.equal result
            , test "Rows number smaller than 1 defaults to 0" <|
                \_ ->
                    let
                        dimensions =
                            { rows = -1, cols = 2 }

                        result =
                            []

                        coords ( x, y ) res =
                            ( x, y ) :: res
                    in
                    Grid.fold2d dimensions coords []
                        |> List.reverse
                        |> Expect.equal result
            , test "Rows number as 0 works fine" <|
                \_ ->
                    let
                        dimensions =
                            { rows = 0, cols = 2 }

                        result =
                            []

                        coords ( x, y ) res =
                            ( x, y ) :: res
                    in
                    Grid.fold2d dimensions coords []
                        |> List.reverse
                        |> Expect.equal result
            , test "Cols number smaller than 1 defaults to 0" <|
                \_ ->
                    let
                        dimensions =
                            { rows = 2, cols = -1 }

                        result =
                            []

                        coords ( x, y ) res =
                            ( x, y ) :: res
                    in
                    Grid.fold2d dimensions coords []
                        |> List.reverse
                        |> Expect.equal result
            , test "Cols number as 0 works fine" <|
                \_ ->
                    let
                        dimensions =
                            { rows = 2, cols = -1 }

                        result =
                            []

                        coords ( x, y ) res =
                            ( x, y ) :: res
                    in
                    Grid.fold2d dimensions coords []
                        |> List.reverse
                        |> Expect.equal result
            , test "Rows and Cols negative works fine" <|
                \_ ->
                    let
                        dimensions =
                            { rows = -2, cols = -1 }

                        result =
                            []

                        coords ( x, y ) res =
                            ( x, y ) :: res
                    in
                    Grid.fold2d dimensions coords []
                        |> List.reverse
                        |> Expect.equal result
            ]
        , describe "foldr2d"
            [ test "Folds right over a positive number of rows and columns" <|
                \_ ->
                    let
                        dimensions =
                            { rows = 3, cols = 2 }

                        result =
                            [ ( 0, 0 )
                            , ( 1, 0 )
                            , ( 0, 1 )
                            , ( 1, 1 )
                            , ( 0, 2 )
                            , ( 1, 2 )
                            ]
                    in
                    Grid.foldr2d dimensions (::) []
                        |> Expect.equal result
            ]
        , describe "fold3d"
            [ test "Folds over a positive number of rows, columns and depth" <|
                \_ ->
                    let
                        dimensions =
                            { rows = 3, cols = 2, depth = 2 }

                        result =
                            [ ( 0, 0, 0 )
                            , ( 1, 0, 0 )
                            , ( 0, 1, 0 )
                            , ( 1, 1, 0 )
                            , ( 0, 2, 0 )
                            , ( 1, 2, 0 )
                            , ( 0, 0, 1 )
                            , ( 1, 0, 1 )
                            , ( 0, 1, 1 )
                            , ( 1, 1, 1 )
                            , ( 0, 2, 1 )
                            , ( 1, 2, 1 )
                            ]

                        coords ( x, y, z ) res =
                            ( x, y, z ) :: res
                    in
                    Grid.fold3d dimensions coords []
                        |> List.reverse
                        |> Expect.equal result
            , test "Rows number smaller than 1 defaults to 0" <|
                \_ ->
                    let
                        dimensions =
                            { rows = -1, cols = 2, depth = 2 }

                        result =
                            []

                        coords ( x, y, z ) res =
                            ( x, y, z ) :: res
                    in
                    Grid.fold3d dimensions coords []
                        |> List.reverse
                        |> Expect.equal result
            , test "Rows number as 0 works fine" <|
                \_ ->
                    let
                        dimensions =
                            { rows = 0, cols = 2, depth = 2 }

                        result =
                            []

                        coords ( x, y, z ) res =
                            ( x, y, z ) :: res
                    in
                    Grid.fold3d dimensions coords []
                        |> List.reverse
                        |> Expect.equal result
            , test "Cols number smaller than 1 defaults to 0" <|
                \_ ->
                    let
                        dimensions =
                            { rows = 2, cols = -1, depth = 2 }

                        result =
                            []

                        coords ( x, y, z ) res =
                            ( x, y, z ) :: res
                    in
                    Grid.fold3d dimensions coords []
                        |> List.reverse
                        |> Expect.equal result
            , test "Cols number as 0 works fine" <|
                \_ ->
                    let
                        dimensions =
                            { rows = 2, cols = 0, depth = 2 }

                        result =
                            []

                        coords ( x, y, z ) res =
                            ( x, y, z ) :: res
                    in
                    Grid.fold3d dimensions coords []
                        |> List.reverse
                        |> Expect.equal result
            , test "Depth number smaller than 1 defaults to 0" <|
                \_ ->
                    let
                        dimensions =
                            { rows = 2, cols = 2, depth = -2 }

                        result =
                            []

                        coords ( x, y, z ) res =
                            ( x, y, z ) :: res
                    in
                    Grid.fold3d dimensions coords []
                        |> List.reverse
                        |> Expect.equal result
            , test "Depth number as 0 works fine" <|
                \_ ->
                    let
                        dimensions =
                            { rows = 2, cols = 2, depth = 0 }

                        result =
                            []

                        coords ( x, y, z ) res =
                            ( x, y, z ) :: res
                    in
                    Grid.fold3d dimensions coords []
                        |> List.reverse
                        |> Expect.equal result
            ]
        , describe "foldr3d"
            [ test "Folds right over a positive number of rows, columns and depth" <|
                \_ ->
                    let
                        dimensions =
                            { rows = 3, cols = 2, depth = 2 }

                        result =
                            [ ( 0, 0, 0 )
                            , ( 1, 0, 0 )
                            , ( 0, 1, 0 )
                            , ( 1, 1, 0 )
                            , ( 0, 2, 0 )
                            , ( 1, 2, 0 )
                            , ( 0, 0, 1 )
                            , ( 1, 0, 1 )
                            , ( 0, 1, 1 )
                            , ( 1, 1, 1 )
                            , ( 0, 2, 1 )
                            , ( 1, 2, 1 )
                            ]
                    in
                    Grid.foldr3d dimensions (::) []
                        |> Expect.equal result
            ]
        ]
