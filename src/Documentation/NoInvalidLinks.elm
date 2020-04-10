module Documentation.NoInvalidLinks exposing (rule)

import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Regex exposing (Regex)
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "Documentation.NoInvalidLinks" initialContext
        |> Rule.withCommentsVisitor commentsVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    {}


initialContext : Context
initialContext =
    {}


commentsVisitor : List (Node String) -> Context -> ( List (Error {}), Context )
commentsVisitor comments context =
    let
        docComments : List String
        docComments =
            comments
                |> List.map Node.value
                |> List.filter (String.startsWith "{-")
                |> Debug.log "comments"

        links =
            comments
                |> List.concatMap
                    (\comment ->
                        comment
                            |> Node.value
                            |> Regex.find linkRegex
                            |> List.filterMap
                                (\match ->
                                    case match.submatches of
                                        [ Just linkText, Just link ] ->
                                            let
                                                offset : Offset
                                                offset =
                                                    case String.lines <| String.left match.index <| Node.value comment of
                                                        [] ->
                                                            -- Should not happen
                                                            SameLine <| match.index + String.length linkText + 3

                                                        _ :: [] ->
                                                            SameLine <| match.index + String.length linkText + 3

                                                        lines ->
                                                            NewLine (List.length lines) (String.length <| lastLine lines)
                                            in
                                            Just
                                                (Node.Node
                                                    (rangeOfMatch
                                                        offset
                                                        match
                                                        ( linkText, link )
                                                        (Node.range comment)
                                                    )
                                                    link
                                                )

                                        _ ->
                                            Nothing
                                )
                    )
                |> Debug.log "links"
    in
    ( List.map reportError links
    , context
    )


type Offset
    = SameLine Int
    | NewLine Int Int


reportError : Node String -> Error {}
reportError node =
    Rule.error
        { message = "TODO"
        , details = [ "TODO" ]
        }
        (Node.range node)


rangeOfMatch : Offset -> Regex.Match -> ( String, String ) -> Range -> Range
rangeOfMatch offset match ( linkText, link ) range =
    Debug.log "range" <|
        case offset of
            SameLine indexInString ->
                { start =
                    { row = range.start.row
                    , column = range.start.column + indexInString
                    }
                , end =
                    { row = range.start.row
                    , column = range.start.column + indexInString + String.length link
                    }
                }

            NewLine rows columns ->
                range


linkRegex : Regex
linkRegex =
    Regex.fromString "\\[([^\\[]+)\\]\\((.*)\\)"
        |> Maybe.withDefault Regex.never


lastLine : List String -> String
lastLine lines =
    case lines of
        [] ->
            ""

        str :: [] ->
            str

        _ :: restOfLines ->
            lastLine restOfLines
