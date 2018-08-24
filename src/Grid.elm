module Grid exposing
    ( fold2d, foldr2d
    , fold3d, foldr3d
    )

{-| Functions to operate on a set of coordinates and do something with them.

You can think of them as kind of like list comprehension helpers for 2d and 3d
lists.

Very useful where you would write nested for loops in imperative languages. For
1 dimensional comprehensions use `List.range`.


# 2d grid

@docs fold2d, foldr2d


# 3d grid

@docs fold3d, foldr3d

-}


{-| Fold over a 2 dimensional grid.

Getting just coordinates:

    (fold2d
        {rows = 2, cols = 3 }
        (\(x, y) result -> (x, y) :: result)
        []
    )
        |> List.reverse
    {-
        [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1)]
    -}

-}
fold2d :
    { rows : Int, cols : Int }
    -> (( Int, Int ) -> result -> result)
    -> result
    -> result
fold2d { rows, cols } fn initial =
    let
        iter x y res =
            if y >= rows then
                res

            else if x >= cols then
                iter 0 (y + 1) res

            else
                iter (x + 1) y (fn ( x, y ) res)
    in
    iter 0 0 initial


{-| Fold over a 2 dimensional grid from the right. Starts at the end of the
dimensions space, and goes backwards.

Getting just coordinates:

    foldr2d
        {rows = 2, cols = 3 }
        (::)
        []
    {-
        [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1)]
    -}

-}
foldr2d :
    { rows : Int, cols : Int }
    -> (( Int, Int ) -> result -> result)
    -> result
    -> result
foldr2d ({ rows, cols } as dimensions) fn initial =
    fold2d dimensions
        (\( x, y ) result -> fn ( cols - x - 1, rows - y - 1 ) result)
        initial


{-| Fold over a 3 dimensional grid of coordinates

    (fold3d
        { rows = 2, cols = 3, depth = 2 }
        (\( x, y, z ) result -> ( x, y, z ) :: result)
        []
    )
        |> List.reverse
    {-
        -- First plane depth=0
        [(0,0,0),(1,0,0),(2,0,0)
        ,(0,1,0),(1,1,0),(2,1,0)
        -- Second plane depth=1
        ,(0,0,1),(1,0,1),(2,0,1)
        ,(0,1,1),(1,1,1),(2,1,1)]
    -}

-}
fold3d :
    { rows : Int, cols : Int, depth : Int }
    -> (( Int, Int, Int ) -> result -> result)
    -> result
    -> result
fold3d { rows, cols, depth } fn initial =
    let
        iter x y z res =
            if z >= depth then
                res

            else if y >= rows then
                iter 0 0 (z + 1) res

            else if x >= cols then
                iter 0 (y + 1) z res

            else
                iter (x + 1) y z (fn ( x, y, z ) res)
    in
    iter 0 0 0 initial


{-| Fold over a 3 dimensional grid of coordinates from the right. Starts at the
end of the
dimensions space, and goes backwards.

    foldr3d
        { rows = 2, cols = 3, depth = 2 }
        (\( x, y, z ) result -> ( x, y, z ) :: result)
        []
    {-
        -- First plane depth=0
        [(0,0,0),(1,0,0),(2,0,0)
        ,(0,1,0),(1,1,0),(2,1,0)
        -- Second plane depth=1
        ,(0,0,1),(1,0,1),(2,0,1)
        ,(0,1,1),(1,1,1),(2,1,1)]
    -}

-}
foldr3d :
    { rows : Int, cols : Int, depth : Int }
    -> (( Int, Int, Int ) -> result -> result)
    -> result
    -> result
foldr3d ({ rows, cols, depth } as dimensions) fn initial =
    fold3d dimensions
        (\( x, y, z ) result ->
            fn ( cols - x - 1, rows - y - 1, depth - z - 1 ) result
        )
        initial
