module Lau.TypeCheck exposing (Define, DefineAtom(..), DefineCase(..), DefineInCase(..), DefineMatch, Defines, Identifier, InvalidInfo, InvalidInfoElement(..), Type(..), charOrEscapedQuoteMorph, defineAtomMorphChars, defineCaseMorphChars, defineInCaseMorphChars, defineMatchMorphChars, defineMorphChars, definesMorphChars, identifierMorphChars, invalidInfoElementMorphChars, invalidInfoMorphChars, invalidInfoTextMorphChars, typeMorphChars)

import AToZ exposing (AToZ)
import AToZ.Morph
import ArraySized
import ArraySized.Morph
import Maybe.Morph
import Morph exposing (Morph, MorphRow)
import N exposing (In, N, N0, N9, n0, n1, n2, n9)
import N.Morph
import String.Morph
import Value.Morph


type alias Defines =
    -- an empty file will not be considered valid
    ListFilled Define


type alias Define =
    DefineCase


type DefineInCase
    = DefineAtom DefineAtom
    | DefineMatch DefineMatch


type DefineAtom
    = DefineValid
    | DefineInvalid InvalidInfo


type alias DefineMatch =
    ListFilled DefineCase


type DefineCase
    = IsOfTypeSimplifiesTo { variable : Identifier, type_ : Type, simplified : DefineInCase }


type
    Type
    -- TODO literal set
    = TypeVariable Identifier
    | TypeExcept Type
    | TypeSetCounting Type
    | TypeFunction { input : Type, output : Type }
    | TypeDefined { name : Identifier, argument : Maybe Type }


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
    Morph.narrow listFilledFromHeadAndTail
        |> Morph.grab listFilledHead defineMorphChars
        |> Morph.grab listFilledTail
            (Morph.narrow (\def1Up -> def1Up)
                |> Morph.match (String.Morph.only "\n")
                |> Morph.match
                    (Morph.broad [ () ]
                        |> Morph.overRow
                            (Morph.whilePossible (String.Morph.only "\n"))
                    )
                |> Morph.grab (\def1Up -> def1Up)
                    (Morph.whilePossible defineMorphChars)
            )
        |> Morph.match
            (Morph.broad [ () ]
                |> Morph.overRow (Morph.whilePossible (String.Morph.only "\n"))
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
            (\atomVariant matchVariant defineInCase ->
                case defineInCase of
                    DefineAtom atom ->
                        atomVariant atom

                    DefineMatch match ->
                        matchVariant match
            )
            |> Morph.rowTry DefineAtom defineAtomMorphChars
            |> Morph.rowTry DefineMatch (defineMatchMorphChars config)
            |> Morph.choiceFinish
        )


defineAtomMorphChars : MorphRow DefineAtom Char
defineAtomMorphChars =
    Morph.choice
        (\valid invalid atom ->
            case atom of
                DefineValid ->
                    valid ()

                DefineInvalid invalidInfo ->
                    invalid invalidInfo
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
                |> Morph.grab (\invalidInfo -> invalidInfo) invalidInfoMorphChars
            )
        |> Morph.choiceFinish


identifierMorphChars : MorphRow Identifier Char
identifierMorphChars =
    Morph.narrow listFilledFromHeadAndTail
        |> Morph.grab listFilledHead (identifierElementMorphChar |> Morph.one)
        |> Morph.grab listFilledTail (Morph.whilePossible (identifierElementMorphChar |> Morph.one))


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
    Morph.whilePossible invalidInfoElementMorphChars


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
        |> Morph.rowTry InvalidInfoText invalidInfoTextMorphChars
        |> Morph.rowTry InvalidInfoVariable identifierMorphChars
        |> Morph.choiceFinish


invalidInfoTextMorphChars : MorphRow String Char
invalidInfoTextMorphChars =
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
                (\variableVariant exceptVariant setCountingVariant functionVariant definedWithArgumentVariant type_ ->
                    case type_ of
                        TypeVariable variableName ->
                            variableVariant variableName

                        TypeExcept negativeType ->
                            exceptVariant negativeType

                        TypeSetCounting typeSet ->
                            setCountingVariant typeSet

                        TypeFunction wiring ->
                            functionVariant wiring

                        TypeDefined argument ->
                            definedWithArgumentVariant argument
                )
                |> Morph.rowTry TypeVariable identifierMorphChars
                |> Morph.rowTry TypeExcept
                    (Morph.narrow (\negativeType -> negativeType)
                        |> Morph.match (String.Morph.only "except")
                        |> Morph.match
                            (Morph.broad [ () ]
                                |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                            )
                        |> Morph.grab (\negativeType -> negativeType) step
                    )
                |> Morph.rowTry TypeSetCounting
                    (Morph.narrow (\negativeType -> negativeType)
                        |> Morph.match (String.Morph.only "setCounting")
                        |> Morph.match (String.Morph.only " ")
                        |> Morph.match
                            (Morph.broad []
                                |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                            )
                        |> Morph.grab (\negativeType -> negativeType) step
                    )
                |> Morph.rowTry TypeFunction
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
                |> Morph.rowTry TypeDefined
                    (Morph.narrow (\name argument -> { name = name, argument = argument })
                        |> Morph.match (String.Morph.only "(")
                        |> Morph.match
                            (Morph.broad []
                                |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                            )
                        |> Morph.grab .name identifierMorphChars
                        |> Morph.grab .argument
                            (Maybe.Morph.row
                                (Morph.narrow (\argument -> argument)
                                    |> Morph.match
                                        (Morph.broad [ () ]
                                            |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                                        )
                                    |> Morph.grab (\argument -> argument) step
                                )
                            )
                        |> Morph.match
                            (Morph.broad []
                                |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                            )
                        |> Morph.match (String.Morph.only ")")
                    )
                |> Morph.choiceFinish
        )


type alias ListFilled a =
    ( a, List a )


listFilledFromHeadAndTail head tail =
    ( head, tail )


listFilledHead =
    \( headElement, _ ) -> headElement


listFilledTail =
    \( _, tailList ) -> tailList



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
