An attempt at creating a language to handle the logic of a possible future type checker for [the programming language lau](https://github.com/lue-bird/lau).

Currently includes
- an elm representation of that language's AST
- a text parser and printer

An example of what it roughly looks like
```
c : (choice possibilities)
    possibilities : {}
        ⨯ "none of the possibilities matched"
    possibilities : { a, ...x }
        c : a
            c : a
            c : (except a)
                a : (choice x)

c : set element
    c : {}
    c : { a, ...b }
        a : { (count 1) (counted aElement) }
            aElement : element
                b : set
        a : { count (1Plus countFrom1), (counted a) }
            ⨯ a "is present multiple times"

c : character
    c : naturalInRange { (start 0) (end 1114112) }

c : naturalInRange { (start minimum), (end maximum) }
    c : 0
        minimum : 0
        minimum : (1Plus minimumFrom1)
            ⨯ "lower than start"
    c : (1Plus cFrom1)
        maximum : 0
            invalid "greater than end"
        maximum : (1Plus maximumFrom1)
            minimum : 0
                c : (naturalInRange { (start 0) (end (1Plus maximumFrom1)) })
            minimum : (1Plus minimumFrom1)
                cFrom1 : (naturalInRange { (start minimumFrom1) (end (1Plus maximumFrom1)) })
```
