module Documentation.NoInvalidLinksTest exposing (all)

import Documentation.NoInvalidLinks exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


message : String
message =
    "TODO"


details : List String
details =
    [ "TODO" ]


all : Test
all =
    describe "NoInvalidLinks"
        [ test "should not report errors if there are no links" <|
            \() ->
                """module A exposing (..)
{-|
# Documentation
-}
{- Multi-line comment -}
-- comment
a = 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report local link that leads to no known section" <|
            \() ->
                """module A exposing (..)
{-| [Link](#unknownsection)
-}
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "#unknownsection"
                            }
                        ]
        , test "should report local link that leads to no known section, not on the first line" <|
            \() ->
                """module A exposing (..)
{-| this
 is
 some [Link](#unknownsection)
-}
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "#unknownsection"
                            }
                        ]
        ]
