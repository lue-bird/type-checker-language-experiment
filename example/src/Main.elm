module Main exposing (main)

import Browser
import Element as Ui
import Element.Background as UiBackground
import Element.Border as UiBorder
import Element.Font as UiFont
import Element.Input as UiInput
import Emptiable exposing (Emptiable)
import Forest.Navigate
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as Html
import Lau.TypeCheck
import List.Morph
import Morph exposing (Morph)
import Stack exposing (Stacked)
import Stack.Morph
import Tree exposing (Tree)
import TreeUi


type alias InitialJsData =
    ()


type alias State =
    { definesInput : String
    , definesDescriptionTreeState : TreeUi.State Morph.DescriptionOrErrorKind
    }


type Event
    = DefinesInputChangedTo String
    | DefinesDescriptionTreeEvent TreeUi.Event


main : Program InitialJsData State Event
main =
    Browser.document
        { init = init
        , update = reactTo
        , subscriptions = \_ -> Sub.none
        , view = ui
        }


init : InitialJsData -> ( State, Cmd Event )
init initialJsData =
    ( { definesInput = exampleDefinesInput
      , definesDescriptionTreeState = exampleDefinesInput |> toDescriptionState definesString
      }
    , Cmd.none
    )


exampleDefinesInput : String
exampleDefinesInput =
    """a : b
    valid


c : naturalAtLeast minimum
    minimum : 0
        valid
    minimum : 1Plus minimumFrom1
        c : 0
            invalid "less than minimum"
        c : 1Plus cFrom1
            cFrom1 : naturalAtLeast minimumFrom1
                valid
"""


toDescriptionState : Morph narrow_ String -> String -> TreeUi.State Morph.DescriptionOrErrorKind
toDescriptionState stringMorph =
    \stringInput ->
        case stringInput |> Morph.toNarrow stringMorph of
            Err error ->
                Morph.descriptionAndErrorToTree
                    (stringMorph |> Morph.description)
                    error
                    |> treeToState

            Ok _ ->
                Morph.descriptionToTree
                    (stringMorph |> Morph.description)
                    |> Tree.map
                        (\labelString ->
                            { kind = labelString.kind |> Morph.DescriptionKind, text = labelString.text }
                        )
                    |> treeToState


definesString : Morph Lau.TypeCheck.Defines String
definesString =
    Lau.TypeCheck.definesMorphChars |> Morph.rowFinish |> Morph.over List.Morph.string


treeToState :
    Tree { text : String, kind : Morph.DescriptionOrErrorKind }
    -> TreeUi.State Morph.DescriptionOrErrorKind
treeToState =
    \tree ->
        tree |> treeToTreeViewNode |> List.singleton


treeToTreeViewNode :
    Tree { text : String, kind : Morph.DescriptionOrErrorKind }
    -> Tree (TreeUi.Label Morph.DescriptionOrErrorKind)
treeToTreeViewNode tree =
    let
        label =
            tree |> Tree.label

        childrenViewNodes =
            tree |> Tree.children |> List.map treeToTreeViewNode
    in
    Tree.tree
        { text = label.text
        , style = label.kind
        , open =
            if
                (childrenViewNodes |> List.any (\child -> (child |> Tree.label |> .open) == TreeUi.Open))
                    || (label.kind == Morph.ErrorKind)
            then
                TreeUi.Open

            else
                TreeUi.Closed
        }
        childrenViewNodes


reactTo : Event -> (State -> ( State, Cmd Event ))
reactTo event =
    case event of
        DefinesInputChangedTo newDefinesInput ->
            \state ->
                ( { state
                    | definesInput = newDefinesInput
                    , definesDescriptionTreeState = newDefinesInput |> toDescriptionState definesString
                  }
                , Cmd.none
                )

        DefinesDescriptionTreeEvent (TreeUi.Toggled path) ->
            \state ->
                ( { state
                    | definesDescriptionTreeState =
                        Forest.Navigate.alter path
                            (Tree.mapLabel TreeUi.labelOpenClosedToggle)
                            state.definesDescriptionTreeState
                  }
                , Cmd.none
                )


ui : State -> Browser.Document Event
ui =
    \state ->
        { title = "morph example"
        , body =
            Ui.column [ Ui.spacing 0 ]
                [ inputAndDescriptionUi
                    { title = "defines"
                    , input = state.definesInput
                    , descriptionTreeState = state.definesDescriptionTreeState
                    , onInputChange = DefinesInputChangedTo
                    , onDescriptionTreeEvent = DefinesDescriptionTreeEvent
                    , morph = definesString
                    }
                ]
                |> Ui.layout
                    [ UiBackground.color (Ui.rgb 0 0 0)
                    , UiFont.color (Ui.rgb 1 1 1)
                    , UiFont.family [ UiFont.monospace ]
                    ]
                |> List.singleton
        }


inputAndDescriptionUi :
    { title : String
    , onInputChange : String -> event
    , input : String
    , morph : Morph narrow_ String
    , onDescriptionTreeEvent : TreeUi.Event -> event
    , descriptionTreeState : TreeUi.State Morph.DescriptionOrErrorKind
    }
    -> Ui.Element event
inputAndDescriptionUi config =
    Ui.column
        [ Ui.paddingXY 80 50
        , Ui.spacing 14
        ]
        [ Ui.el
            [ UiFont.size 30
            , Ui.paddingXY 0 20
            ]
            (Ui.text config.title)
        , Ui.row [ Ui.spacing 50, UiFont.size 17 ]
            [ Ui.column [ Ui.width Ui.fill ]
                [ Ui.el
                    [ Ui.paddingEach { eachSide0 | right = 5 }
                    ]
                    (Html.i
                        [ HtmlA.class "fa fa-pencil"
                        , HtmlA.attribute "aria-hidden" "true"
                        , HtmlA.attribute "color" "rgb(255, 255, 0)"
                        ]
                        []
                        |> Ui.html
                    )
                , UiInput.multiline
                    [ UiBackground.color (Ui.rgba 0 0 0 0)
                    , UiBorder.width 0
                    , HtmlA.style "color" "inherit" |> Ui.htmlAttribute
                    ]
                    { onChange = config.onInputChange
                    , text = config.input
                    , label = UiInput.labelHidden "sample defines string"
                    , placeholder = Nothing
                    , spellcheck = True
                    }
                    |> Ui.el [ Ui.width Ui.fill ]
                ]
            , Ui.column []
                [ case config.input |> Morph.toNarrow config.morph of
                    Err error ->
                        [ Ui.text "failed:" ] |> Ui.paragraph []

                    Ok defines ->
                        [ Ui.text "succeeded as"
                        , [ (defines |> Morph.toBroad config.morph) |> Html.text ]
                            |> Html.code []
                            |> Ui.html
                        ]
                            |> Ui.column [ Ui.spacing 23 ]
                , Ui.map config.onDescriptionTreeEvent
                    (TreeUi.ui toStyle config.descriptionTreeState)
                ]
            ]
        ]


eachSide0 : { left : Int, right : Int, top : Int, bottom : Int }
eachSide0 =
    { left = 0, right = 0, top = 0, bottom = 0 }


toStyle : Morph.DescriptionOrErrorKind -> TreeUi.Style
toStyle =
    \styleKind ->
        case styleKind of
            Morph.DescriptionKind Morph.DescriptionNameKind ->
                { attributes = [], icon = Just (TreeUi.iconAlways "fa fa-info") }

            Morph.DescriptionKind Morph.DescriptionStructureKind ->
                { attributes = [], icon = Just (TreeUi.iconAlways "fa fa-cog") }

            Morph.ErrorKind ->
                { attributes = [], icon = Just (TreeUi.iconAlways "fa fa-exclamation") }


treeLinesUi : Tree String -> Ui.Element event
treeLinesUi =
    \tree ->
        Ui.column []
            (tree
                |> Morph.treeToLines
                |> List.map
                    (\line ->
                        Ui.el [ HtmlA.style "white-space" "pre" |> Ui.htmlAttribute ]
                            (Ui.text line)
                    )
            )
