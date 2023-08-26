module Lau.TypeCheck exposing (Define, DefineCase, DefineInCase(..), DefineMatch, Defines, Identifier, InvalidInfo, InvalidInfoElement(..), Type(..), charOrEscapedQuoteMorph, defineCaseMorphChars, defineInCaseMorphChars, defineMatchMorphChars, defineMorphChars, definesMorphChars, identifierMorphChars, invalidInfoElementMorphChars, invalidInfoMorphChars, quotedMorphChars, typeMorphChars)

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


type alias DefineCase =
    { variable : Identifier, type_ : Type, simplified : DefineInCase }


type Type
    = TypeReference Identifier
    | -- dictionary
      TypeDictionaryEmpty
    | TypeDictionaryFilled { element : Type, setExceptElement : Type }
    | -- construct
      TypeExceptConstruct Type
    | TypeDictionaryConstruct { key : Type, value : Type }
    | TypeFunctionConstruct { input : Type, output : Type }
    | TypeConstruct { name : Identifier, argument : Type }


type alias InvalidInfo =
    ListFilled InvalidInfoElement


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
                    Morph.narrow (\end -> end)
                        |> Morph.match
                            (Morph.broad [ () ]
                                |> Morph.overRow (Morph.whilePossible (String.Morph.only "\n"))
                            )
                        |> Morph.grab (\end -> end) defineMorphChars
                        |> Morph.match
                            (Morph.broad [ () ]
                                |> Morph.overRow (Morph.whilePossible (String.Morph.only "\n"))
                            )
                        |> Morph.match Morph.end
                , element =
                    Morph.narrow (\element -> element)
                        |> Morph.match
                            (Morph.broad [ () ]
                                |> Morph.overRow (Morph.whilePossible (String.Morph.only "\n"))
                            )
                        |> Morph.grab (\element -> element) defineMorphChars
                        |> Morph.match (String.Morph.only "\n")
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
            , beforeEnd = reverseStack |> Stack.removeTop |> Stack.toList |> List.reverse
            }
        )


defineMorphChars : MorphRow Define Char
defineMorphChars =
    defineCaseMorphChars { indentation = 0 }


defineCaseMorphChars : { indentation : Int } -> MorphRow DefineCase Char
defineCaseMorphChars config =
    morphLazy "case"
        (\() ->
            Morph.narrow
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
                |> Morph.grab .simplified (caseSimplifiedMorphChars config)
        )


caseSimplifiedMorphChars : { indentation : Int } -> MorphRow DefineInCase Char
caseSimplifiedMorphChars config =
    Morph.choice
        (\keepGoing valid defineInCase ->
            case defineInCase of
                DefineValid ->
                    valid ()

                invalidOrMatch ->
                    keepGoing invalidOrMatch
        )
        |> Morph.rowTry (\simplified -> simplified)
            (Morph.narrow (\simplified -> simplified)
                |> Morph.match
                    (String.Morph.only
                        ("\n" ++ String.repeat (config.indentation + 1) "    ")
                    )
                |> Morph.grab (\simplified -> simplified)
                    (defineInCaseMorphChars { indentation = config.indentation + 1 })
            )
        |> Morph.rowTry (\() -> DefineValid) (Morph.narrow ())
        |> Morph.choiceFinish


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
                    |> Morph.match separatingSpacesMorph
                    |> Morph.grab (\invalidInfo -> invalidInfo) invalidInfoMorphChars
                )
            |> Morph.rowTry DefineMatch (defineMatchMorphChars config)
            |> Morph.choiceFinish
        )


identifierMorphChars : MorphRow Identifier Char
identifierMorphChars =
    Morph.named "identifier"
        (Morph.narrow listFilledHeadTail
            |> Morph.grab listFilledHead (identifierElementMorphChar |> Morph.one)
            |> Morph.grab listFilledTail
                (Morph.whilePossible (identifierElementMorphChar |> Morph.one))
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
        (Morph.narrow listFilledHeadTail
            |> Morph.grab listFilledHead invalidInfoElementMorphChars
            |> Morph.grab listFilledTail
                (Morph.whilePossible
                    (Morph.narrow (\element -> element)
                        |> Morph.match separatingSpacesMorph
                        |> Morph.grab (\element -> element) invalidInfoElementMorphChars
                    )
                )
        )


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
        (Morph.narrow listFilledHeadTail
            |> Morph.grab listFilledHead (defineCaseMorphChars config)
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
                (\dictionaryEmptyVariant dictionaryFilledVariant exceptConstructVariant dictionaryConstructVariant functionConstructVariant typeConstructVariant referenceVariant type_ ->
                    case type_ of
                        TypeDictionaryEmpty ->
                            dictionaryEmptyVariant ()

                        TypeReference defined ->
                            referenceVariant defined

                        TypeDictionaryFilled dictionaryFilled ->
                            dictionaryFilledVariant dictionaryFilled

                        TypeExceptConstruct negativeType ->
                            exceptConstructVariant negativeType

                        TypeDictionaryConstruct typeSet ->
                            dictionaryConstructVariant typeSet

                        TypeFunctionConstruct wiring ->
                            functionConstructVariant wiring

                        TypeConstruct argument ->
                            typeConstructVariant argument
                )
                |> Morph.rowTry (\() -> TypeDictionaryEmpty) typeDictionaryEmptyMorphChars
                |> Morph.rowTry TypeDictionaryFilled (typeDictionaryFilledMorphChars step)
                |> Morph.rowTry TypeExceptConstruct (typeExceptConstructMorphChar step)
                |> Morph.rowTry TypeDictionaryConstruct (typeDictionaryConstructMorphChars step)
                |> Morph.rowTry TypeFunctionConstruct (typeFunctionConstructMorphChars step)
                |> Morph.rowTry TypeConstruct (typeConstructMorphChars step)
                |> Morph.rowTry TypeReference (Morph.named "reference" identifierMorphChars)
                |> Morph.choiceFinish
        )


separatingSpacesMorph : MorphRow () Char
separatingSpacesMorph =
    Morph.named "separating spaces"
        (Morph.narrow ()
            |> Morph.match (String.Morph.only " ")
            |> Morph.match
                (Morph.broad []
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
        )


typeDictionaryConstructMorphChars : MorphRow Type Char -> MorphRow { key : Type, value : Type } Char
typeDictionaryConstructMorphChars step =
    Morph.named "dictionary construct"
        (Morph.narrow (\key value -> { key = key, value = value })
            |> Morph.match (String.Morph.only "dictionary")
            |> Morph.match separatingSpacesMorph
            |> Morph.match (String.Morph.only "{")
            |> Morph.match
                (Morph.broad []
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
            |> Morph.match (String.Morph.only "key")
            |> Morph.match separatingSpacesMorph
            |> Morph.grab .key step
            |> Morph.match separatingSpacesMorph
            |> Morph.match (String.Morph.only "..")
            |> Morph.match
                (Morph.broad []
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
            |> Morph.match (String.Morph.only "{")
            |> Morph.match
                (Morph.broad []
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
            |> Morph.match (String.Morph.only "value")
            |> Morph.match separatingSpacesMorph
            |> Morph.grab .value step
            |> Morph.match
                (Morph.broad [ () ]
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
            |> Morph.match (String.Morph.only "..")
            |> Morph.match
                (Morph.broad []
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
            |> Morph.match typeDictionaryEmptyMorphChars
            |> Morph.match (String.Morph.only "}")
            |> Morph.match
                (Morph.broad []
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
            |> Morph.match (String.Morph.only "}")
        )


typeDictionaryFilledMorphChars : MorphRow Type Char -> MorphRow { element : Type, setExceptElement : Type } Char
typeDictionaryFilledMorphChars step =
    Morph.named "dictionary filled"
        (Morph.narrow (\element setExceptElement -> { element = element, setExceptElement = setExceptElement })
            |> Morph.match (String.Morph.only "{")
            |> Morph.match
                (Morph.broad []
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
            |> Morph.grab .element step
            |> Morph.match
                (Morph.broad [ () ]
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
            |> Morph.match (String.Morph.only "..")
            |> Morph.match
                (Morph.broad []
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
            |> Morph.grab .setExceptElement step
            |> Morph.match
                (Morph.broad []
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
            |> Morph.match (String.Morph.only "}")
        )


typeDictionaryEmptyMorphChars : MorphRow () Char
typeDictionaryEmptyMorphChars =
    Morph.named "dictionary empty"
        (Morph.narrow ()
            |> Morph.match (String.Morph.only "{")
            |> Morph.match
                (Morph.broad []
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
            |> Morph.match (String.Morph.only "}")
        )


typeFunctionConstructMorphChars : MorphRow Type Char -> MorphRow { input : Type, output : Type } Char
typeFunctionConstructMorphChars step =
    Morph.named "function construct"
        (Morph.narrow (\input output -> { input = input, output = output })
            |> Morph.match (String.Morph.only "function")
            |> Morph.match separatingSpacesMorph
            |> Morph.match (String.Morph.only "{")
            |> Morph.match
                (Morph.broad []
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
            |> Morph.match (String.Morph.only "in")
            |> Morph.match separatingSpacesMorph
            |> Morph.grab .input step
            |> Morph.match separatingSpacesMorph
            |> Morph.match (String.Morph.only "..")
            |> Morph.match
                (Morph.broad []
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
            |> Morph.match (String.Morph.only "{")
            |> Morph.match
                (Morph.broad []
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
            |> Morph.match (String.Morph.only "out")
            |> Morph.match separatingSpacesMorph
            |> Morph.grab .output step
            |> Morph.match
                (Morph.broad [ () ]
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
            |> Morph.match (String.Morph.only "..")
            |> Morph.match
                (Morph.broad []
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
            |> Morph.match typeDictionaryEmptyMorphChars
            |> Morph.match (String.Morph.only "}")
            |> Morph.match
                (Morph.broad []
                    |> Morph.overRow (Morph.whilePossible (String.Morph.only " "))
                )
            |> Morph.match (String.Morph.only "}")
        )


typeExceptConstructMorphChar : MorphRow Type Char -> MorphRow Type Char
typeExceptConstructMorphChar step =
    Morph.named "except construct"
        (Morph.narrow (\negativeType -> negativeType)
            |> Morph.match (String.Morph.only "except")
            |> Morph.match separatingSpacesMorph
            |> Morph.grab (\negativeType -> negativeType) step
        )


typeConstructMorphChars : MorphRow Type Char -> MorphRow { name : Identifier, argument : Type } Char
typeConstructMorphChars step =
    Morph.named "construct"
        (Morph.narrow (\name argument -> { name = name, argument = argument })
            |> Morph.grab .name identifierMorphChars
            |> Morph.match separatingSpacesMorph
            |> Morph.grab .argument step
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
        (\slashVariant quoteVariant nonQuoteVariant char ->
            case char of
                '\\' ->
                    slashVariant ()

                '"' ->
                    quoteVariant ()

                nonQuote ->
                    nonQuoteVariant nonQuote
        )
        |> Morph.rowTry (\() -> '\\') (String.Morph.only "\\\\")
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


listFilledHeadTail head tail =
    ( head, tail )


listFilledHead =
    \( headElement, _ ) -> headElement


listFilledTail =
    \( _, tailList ) -> tailList


listFilledToList =
    \( headElement, tailElement ) -> headElement :: tailElement
