An attempt at creating a language to handle the logic of a possible future type checker for [the programming language lau](https://github.com/lue-bird/lau).

Currently includes
- an elm representation of that language's AST
- a text parser and printer as one [morph](https://dark.elm.dmy.fr/packages/lue-bird/elm-morph/latest/)

An example of what it roughly looks like
```lausdiemaus
c : choice possibilities
    possibilities : {}
        ⨯ "none of the possibilities matched"
    possibilities : {a ..x}
        c : a
            c : a
            c : except a
                a : choice x

c : set element
    c : {}
    c : setFilled {a ..b}

c : setFilled {a ..b}
    c : {a ..b}
        a : {value count 1 ..{key aElement ..{}}}
            aElement : element
                b : set element
        a : {value count 1Plus countFrom1 ..{key aElement ..{}}}
            ⨯ aElement "is present multiple times"

c : character
    c : naturalInRange {start 0 ..{end 1114112 ..{}}}

c : naturalInRange {start minimum ..{end maximum..{}}}
    c : 0
        minimum : 0
        minimum : 1Plus minimumFrom1
            ⨯ "lower than start"
    c : 1Plus cFrom1
        maximum : 0
            ⨯ "greater than end"
        maximum : 1Plus maximumFrom1
            minimum : 0
                c : naturalInRange {start 0 ..{end 1Plus maximumFrom1 ..{}}}
            minimum : 1Plus minimumFrom1
                cFrom1 : naturalInRange {start minimumFrom1 ..{end 1Plus maximumFrom1 ..{}}}
```
