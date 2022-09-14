# boolegen
[![License: GPLv3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://opensource.org/licenses/gpl-3.0.html) ![checks](https://github.com/delhoyo31415/boolegen/actions/workflows/ci.yml/badge.svg)

`boolegen` is a simple CLI (for now) tool which calculates the complete truth table of boolean expressions and outputs a file which can be read by LPL Boole.

[LPL Boole](https://www.gradegrinder.net/images/static-page-img/Boole-main-linux.png) is a program used to practice writing [truth tables](https://en.wikipedia.org/wiki/Truth_table). It allows you to write the value of subexpressions of the main expression so that you can more easily calculate the final value of it. It also checks whether the truth values you have typed are indeed the correct ones.
However, for medium sized expressions with more than three variables this task can become quite tedious.

⚠️Warning: This has only been tested with the version of LPL Boole I was given (4.0.0), so if you try to load the file with another version it might fail.

# Usage
```
boolegen 
Generate a truth table for a given expression and outputs a file compatible with LPL Boole

USAGE:
    boolegen [OPTIONS] <EXPRESSIONS>...

ARGS:
    <EXPRESSIONS>...
            The boolean expressions for which you want to generate a truth table

OPTIONS:
    -a, --all-subexpressions
            Alias for -s 0

    -d, --duration <SECONDS_SPENT>...
            The time spent (in seconds) with the LPL Boole file open. If two arguments are given,
            then this quantity will be a number chosen randomly between the given numbers

    -h, --help
            Print help information

    -o, --output <OUTPUT>
            Output filename

    -s, --subexpressions <MIN_DEGREE>
            Show subexpressions in different columns with a degree of at least <MIN_DEGREE>

            In case the given number is greater than the degree of the main expression then only the
            main expression will be written

    -t, --transform
            Modify the AST of the expression to get an equivalent one using the associative property
            of AND and OR binary operators, resulting in a expression with less parenthesis
```

## Expression format
These are the operators currently accepted by `boolegen` and its representation
| Boolean operator | Logical symbol |`boolegen` representation |
|:---:|:---:|:---:|
|NOT| ¬ | `~`|
|AND| ∧| `&`|
|OR | ∨| `\|`|
|CONDITIONAL| → | `->`|
|BICONDITIONAL| ↔ | `<->`|

A sequence of alphabetic ascii characters is understood as a variable name. 

The variables must start with capital letters because that is the format that LPL Boole accepts. Otherwise, `boolegen`'s generator will reject the expression. 

### Operator precedence and associativity
In logic, there does not exist consensus about the precedence and associativity of logical operators. The rules implemented for `boolegen` are the ones I used in my logic course. This is the table of precedences
|Operator| Precedence|
|:--------:|:--------:|
|`->`, `<->` | 1 |
|`&`, `\|` | 2|
| `~` | 3 |

Given the expression `A α B β C`, where `α` and `β` are binary operators:
- If the precedence of `α` is higher than the one of `β`, then the expression is parsed as `(A α B) β C`. If `β` has a higher one, then it is parsed as `A α (B β C)`
- If the precedence of `α` and `β` is the same then
    - If `α` = `β` (i.e, they are the same operator) and it is associative, then the expression is parsed as `(A α B) β C`
    - In any other case, the above expression is rejected for being ambiguous.

LPL Boole forces you to write more parenthesis but you only have to have in mind the rules stated above because `boolegen` will parenthesize the expression as needed by the program.

### Examples
- `A & B & C -> C | E` is a valid expression
- `A & B | C` is **NOT** valid for being ambiguous (`&` and `|` are different operators)
-  `A -> B -> C` is **NOT** valid for being ambiguous (`->` is not associative)

## `--transform` flag
Internally, `boolegen` transforms the string given by `<EXPRESSION>` into an AST, a process known as parsing. For example, an expression such as `A & (B & C)` will be parsed as
```
  &
 / \
A   &
   / \
  B   C
```

However, since `&` is associative, the above expression is equivalent to
```
    &
   / \
  &   C
 / \
A   B
```
which corresponds to the string `A & B & C`. This is what `boolgen` does when you give the `-t` flag. As you can see, applying this transformation reduces the number of parenthesis. However, I can't just print the first AST as if it represented the second expression because, in that case, LPL Boole would generate the second AST. Since the AST `boolegen` has is different than the one from LPL Boole, the value for **the subexpressions may be different** even though the value for the main expressions are guaranteed to be the same.

## Examples
- This will generate a LPL Boole file with the maximum amount of parenthesis removed (because of the `-t` flag) to a file named `expr.tt`.
```
boolegen "A -> B & C & (D <-> ~(E | F) & A & P)" -t -o expr.tt
```

- This is the same as above except that only those subexpression with a degree (number of operators) of at least 3 will be written to the file. Notice that the previous statements includes the main expression.
```
boolegen "A -> B & C & (D <-> ~(E | F) & A & P)" -t -s 3 -o expr.tt
```

- The following command will write two expressions with the maximum parenthesis removed and all the subexpressions of each one to a file named `expr.tt`
```
boolegen "A -> (B -> C)" "(E & D) | A" -t -a -o expr.tt
```
# Build
You need a Rust installation in your system. Once installed, use `cargo` to compile it
```
$ git clone https://github.com/delhoyo31415/boolegen
$ cd boolegen
$ cargo build --release
```

The executable will be located in `target/release/boolegen`.

# Objectives
- [X] Generate file which can be correctly read by LPL Boole
- [X] Show each subexpression in different columns
- [X] Write several expressions to the same file, not just one
- [ ] Add a simple GUI, preferably built with [iced](https://github.com/iced-rs/iced)

# Additional notes
- Right now, the user experience on Windows is awful since there is not a strong tradition of CLI on it. This might change if I managed to add a CLI.
- Every commit is licensed under the GPLv3 license even though the text with its content does not appear in some of them.