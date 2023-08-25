module Lau.TypeCheck exposing (Define, DefineCase(..), DefineInCase(..), DefineMatch, Defines, Identifier, InvalidInfo, InvalidInfoElement(..), Type(..), charOrEscapedQuoteMorph, defineCaseMorphChars, defineInCaseMorphChars, defineMatchMorphChars, defineMorphChars, definesMorphChars, identifierMorphChars, invalidInfoElementMorphChars, invalidInfoMorphChars, quotedMorphChars, typeMorphChars)

import AToZ exposing (AToZ)
import AToZ.Morph
import ArraySized
import ArraySized.Morph
import Emptiable exposing (Emptiable)
import Linear exposing (Direction(..))
import Maybe.Morph
import Morph exposing (Morph, MorphOrError, MorphRow)
import N exposing (In, N, N0, N9, n0, n1, n2, n9)
import N.Morph
import Stack exposing (Stacked)
import String.Morph
import Value.Morph


type alias Defines =
    -- an empty file will not be considered valid
    ListFilled Define


type alias Define =
    DefineCase


type DefineInCase
    = DefineValid
    | DefineInvalid InvalidInfo
    | DefineMatch DefineMatch


type alias DefineMatch =
    ListFilled DefineCase


type DefineCase
    = IsOfTypeSimplifiesTo { variable : Identifier, type_ : Type, simplified : DefineInCase }


type
    Type
    -- TODO literal set
    = TypeExcept Type
    | TypeSetCounting Type
    | TypeFunction { input : Type, output : Type }
    | TypeReference Identifier
    | TypeConstruct { name : Identifier, argument : Type }


type alias InvalidInfo =
    List InvalidInfoElement


type InvalidInfoElement
    = InvalidInfoVariable Identifier
    | InvalidInfoText String


type alias Identifier =
    ListFilled IdentifierElement


type IdentifierElement
    = Letter { letter : AToZ, case_ : AToZ.Case }
    | Digit (N (In N0 N9))


definesMorphChars : MorphRow Defines Char
definesMorphChars =
    listFilledMorphEndBeforeRecord
        |> Morph.overRow
            (Morph.untilNext
                { end =
                    defineMorphChars
                        |> Morph.match
                            (Morph.broad [ (), () ]
                                |> Morph.overRow (Morph.whilePossible (String.Morph.only "\n"))
                            )
                        |> Morph.match Morph.end
                , element =
                    defineMorphChars
                        |> Morph.match
                            (Morph.broad [ (), () ]
                                |> Morph.overRow (Morph.whilePossible (String.Morph.only "\n"))
                            )
                }
            )


listFilledMorphEndBeforeRecord :
    MorphOrError
        (ListFilled element)
        { beforeEnd : List element, end : element }
        error_
listFilledMorphEndBeforeRecord =
    Morph.oneToOne
        (\until ->
            until.beforeEnd
                |> Stack.fromList
                |> Stack.attachAdapt Up (until.end |> Stack.one)
                |> Stack.toTopBelow
        )
        (\stack ->
            let
                reverseStack : Emptiable (Stacked element) Never
                reverseStack =
                    stack |> Stack.fromTopBelow |> Stack.reverse
            in
            { end = reverseStack |> Stack.top
            , beforeEnd = reverseStack |> Stack.removeTop |> Stack.toList
            }
        )


defineMorphChars : MorphRow Define Char
defineMorphChars =
    defineCaseMorphChars { indentation = 0 }


defineCaseMorphChars : { indentation : Int } -> MorphRow DefineCase Char
defineCaseMorphChars config =
    morphLazy "case"
        (\() ->
            Morph.oneToOne IsOfTypeSimplifiesTo
                (\(IsOfTypeSimplifiesTo isOfTypeSimplifiesTo) -> isOfTypeSimplifiesTo)
                |> Morph.overRow
                    (Morph.narrow
                        (\variable type_ simplified ->
                            { variable = variable, type_ = type_, simplified = simplified }
                        )
                        |> Morph.grab .variable identifierMorphChars
                        |> Morph.match
                            (Morph.broad [ () ]
                                |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                            )
                        |> Morph.match (String.Morph.only ":")
                        |> Morph.match
                            (Morph.broad [ () ]
                                |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                            )
                        |> Morph.grab .type_ typeMorphChars
                        |> Morph.match
                            (String.Morph.only
                                ("\n" ++ String.repeat (config.indentation + 1) "    ")
                            )
                        |> Morph.grab .simplified (defineInCaseMorphChars { indentation = config.indentation + 1 })
                    )
        )


defineInCaseMorphChars : { indentation : Int } -> MorphRow DefineInCase Char
defineInCaseMorphChars config =
    Morph.named "define in case"
        (Morph.choice
            (\validVariant invalidVariant matchVariant defineInCase ->
                case defineInCase of
                    DefineValid ->
                        validVariant ()

                    DefineInvalid invalid ->
                        invalidVariant invalid

                    DefineMatch match ->
                        matchVariant match
            )
            |> Morph.rowTry (\() -> DefineValid)
                (Morph.choice (\check _ () -> check ())
                    |> Morph.rowTry (\() -> ()) (String.Morph.only "✓")
                    |> Morph.rowTry (\() -> ()) (String.Morph.only "valid")
                    |> Morph.choiceFinish
                )
            |> Morph.rowTry DefineInvalid
                (Morph.narrow (\invalidInfo -> invalidInfo)
                    |> Morph.match
                        (Morph.choice (\x _ () -> x ())
                            |> Morph.rowTry (\() -> ()) (String.Morph.only "⨯")
                            |> Morph.rowTry (\() -> ()) (String.Morph.only "invalid")
                            |> Morph.choiceFinish
                        )
                    |> Morph.match (String.Morph.only " ")
                    |> Morph.match
                        (Morph.broad []
                            |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                        )
                    |> Morph.grab (\invalidInfo -> invalidInfo) invalidInfoMorphChars
                )
            |> Morph.rowTry DefineMatch (defineMatchMorphChars config)
            |> Morph.choiceFinish
        )


identifierMorphChars : MorphRow Identifier Char
identifierMorphChars =
    Morph.named "identifier"
        (Morph.narrow listFilledFromHeadAndTail
            |> Morph.grab listFilledHead (identifierElementMorphChar |> Morph.one)
            |> Morph.grab listFilledTail (Morph.whilePossible (identifierElementMorphChar |> Morph.one))
        )


identifierElementMorphChar : Morph IdentifierElement Char
identifierElementMorphChar =
    Morph.choice
        (\letterVariant digitVariant element ->
            case element of
                Letter letter ->
                    letterVariant letter

                Digit digit ->
                    digitVariant digit
        )
        |> Morph.try Letter AToZ.Morph.char
        |> Morph.try Digit
            (Morph.oneToOne N.inToNumber N.inToOn
                |> Morph.over (N.Morph.inChar ( n0, n9 ))
            )
        |> Morph.choiceFinish


invalidInfoMorphChars : MorphRow InvalidInfo Char
invalidInfoMorphChars =
    Morph.named "invalid info"
        (Morph.whilePossible invalidInfoElementMorphChars)


invalidInfoElementMorphChars : MorphRow InvalidInfoElement Char
invalidInfoElementMorphChars =
    Morph.choice
        (\textVariant variableVariant element ->
            case element of
                InvalidInfoText text ->
                    textVariant text

                InvalidInfoVariable variable ->
                    variableVariant variable
        )
        |> Morph.rowTry InvalidInfoText quotedMorphChars
        |> Morph.rowTry InvalidInfoVariable identifierMorphChars
        |> Morph.choiceFinish


defineMatchMorphChars : { indentation : Int } -> MorphRow DefineMatch Char
defineMatchMorphChars config =
    Morph.named "match"
        (Morph.narrow listFilledFromHeadAndTail
            |> Morph.grab listFilledHead (defineCaseMorphChars config)
            |> Morph.match
                (String.Morph.only
                    ("\n" ++ String.repeat config.indentation "    ")
                )
            |> Morph.grab listFilledTail
                (Morph.whilePossible
                    (Morph.narrow (\case_ -> case_)
                        |> Morph.match
                            (String.Morph.only
                                ("\n" ++ String.repeat config.indentation "    ")
                            )
                        |> Morph.grab (\case_ -> case_) (defineCaseMorphChars config)
                    )
                )
        )


typeMorphChars : MorphRow Type Char
typeMorphChars =
    Morph.recursive "type"
        (\step ->
            Morph.choice
                (\exceptVariant setCountingVariant functionVariant referenceVariant typeConstructVariant type_ ->
                    case type_ of
                        TypeExcept negativeType ->
                            exceptVariant negativeType

                        TypeSetCounting typeSet ->
                            setCountingVariant typeSet

                        TypeFunction wiring ->
                            functionVariant wiring

                        TypeReference defined ->
                            referenceVariant defined

                        TypeConstruct argument ->
                            typeConstructVariant argument
                )
                |> Morph.rowTry TypeExcept
                    (Morph.named "except"
                        (Morph.narrow (\negativeType -> negativeType)
                            |> Morph.match (String.Morph.only "except")
                            |> Morph.match
                                (Morph.broad [ () ]
                                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                                )
                            |> Morph.grab (\negativeType -> negativeType) step
                        )
                    )
                |> Morph.rowTry TypeSetCounting
                    (Morph.named "set counting"
                        (Morph.narrow (\negativeType -> negativeType)
                            |> Morph.match (String.Morph.only "setCounting")
                            |> Morph.match (String.Morph.only " ")
                            |> Morph.match
                                (Morph.broad []
                                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                                )
                            |> Morph.grab (\negativeType -> negativeType) step
                        )
                    )
                |> Morph.rowTry TypeFunction
                    (Morph.named "function"
                        (Morph.narrow (\input output -> { input = input, output = output })
                            |> Morph.match (String.Morph.only "function")
                            |> Morph.match (String.Morph.only " ")
                            |> Morph.match
                                (Morph.broad []
                                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                                )
                            |> Morph.match (String.Morph.only "{")
                            |> Morph.match
                                (Morph.broad [ () ]
                                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                                )
                            |> Morph.match (String.Morph.only "in")
                            |> Morph.match (String.Morph.only " ")
                            |> Morph.match
                                (Morph.broad []
                                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                                )
                            |> Morph.grab .input step
                            |> Morph.match (String.Morph.only ",")
                            |> Morph.match
                                (Morph.broad []
                                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                                )
                            |> Morph.match (String.Morph.only "out")
                            |> Morph.match (String.Morph.only " ")
                            |> Morph.match
                                (Morph.broad []
                                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                                )
                            |> Morph.grab .output step
                            |> Morph.match
                                (Morph.broad [ () ]
                                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                                )
                            |> Morph.match (String.Morph.only "}")
                        )
                    )
                |> Morph.rowTry TypeReference (Morph.named "defined" identifierMorphChars)
                |> Morph.rowTry TypeConstruct
                    (Morph.named "defined with argument"
                        (Morph.narrow (\name argument -> { name = name, argument = argument })
                            |> Morph.match (String.Morph.only "(")
                            |> Morph.match
                                (Morph.broad []
                                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                                )
                            |> Morph.grab .name identifierMorphChars
                            |> Morph.match (String.Morph.only " ")
                            |> Morph.match
                                (Morph.broad [ () ]
                                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                                )
                            |> Morph.grab .argument step
                            |> Morph.match
                                (Morph.broad []
                                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                                )
                            |> Morph.match (String.Morph.only ")")
                        )
                    )
                |> Morph.choiceFinish
        )



--


quotedMorphChars : MorphRow String Char
quotedMorphChars =
    Morph.narrow (\text -> text)
        |> Morph.match (String.Morph.only "\"")
        |> Morph.grab (\text -> text)
            (String.Morph.list
                |> Morph.over Morph.broadEnd
                |> Morph.overRow
                    (Morph.untilNext
                        { element = charOrEscapedQuoteMorph
                        , end = String.Morph.only "\""
                        }
                    )
            )


charOrEscapedQuoteMorph : MorphRow Char Char
charOrEscapedQuoteMorph =
    Morph.choice
        (\quoteVariant nonQuoteVariant char ->
            case char of
                '"' ->
                    quoteVariant ()

                nonQuote ->
                    nonQuoteVariant nonQuote
        )
        |> Morph.rowTry (\() -> '"') (String.Morph.only "\\\"")
        |> Morph.rowTry (\nonQuote -> nonQuote) (Morph.keep |> Morph.one)
        |> Morph.choiceFinish



-- to eliminate


morphLazy :
    String
    -> (() -> Morph.MorphIndependently (beforeToNarrow -> narrow) (beforeToBroad -> broad))
    -> Morph.MorphIndependently (beforeToNarrow -> narrow) (beforeToBroad -> broad)
morphLazy structureName buildMorph =
    { description =
        Morph.RecursiveDescription
            { name = structureName
            , description = \unit -> buildMorph unit |> Morph.description
            }
    , toNarrow = \beforeToNarrow -> beforeToNarrow |> Morph.toNarrow (buildMorph ())
    , toBroad = \beforeToBroad -> beforeToBroad |> Morph.toBroad (buildMorph ())
    }



--


type alias ListFilled a =
    ( a, List a )


listFilledFromHeadAndTail head tail =
    ( head, tail )


listFilledHead =
    \( headElement, _ ) -> headElement


listFilledTail =
    \( _, tailList ) -> tailList
