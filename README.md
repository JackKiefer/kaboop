# Kaboop!

Kaboop is a little command-line utility for numeric computations in [Reverse-Polish notation](https://en.wikipedia.org/wiki/Reverse_Polish_notation). 

Behold some examples!
```
$ kaboop 4 6 +

    10.000000000000000           

$ kaboop 0.5 cos 0.5 sin + 

    1.3570081004945758           

$ kaboop 2 3 ^ 1 - factorial

    5040.0000000000000           

$ kaboop 4 2 choose

    6.0000000000000000           

$ kaboop 2 exp

    7.3890560989306504           
```

## Installation

Requires `gfortran`

```
git clone https://github.com/JackKiefer/kaboop
cd kaboop
make
```

## Usage

To perform a computation, simply:
```
kaboop <expression>
```

### Function and operand guide

| Expression  | Function             | Note                                   |
|-------------|----------------------|----------------------------------------|
| `+`         | addition             |                                        |
| `-`         | subtraction          |                                        |
| `/`         | division             |                                        |
| `*`         | multiplication       | Use as `\*` to prevent shell weirdness |
| `mult`      | multiplication       |                                        |
| `^`         | exponentiation       |                                        |
| `**`        | exponentiation       |                                        |
| `factorial` | factorial            |                                        |
| `choose   ` | binomial coefficient |                                        |
| `abs`       | absolute value       |                                        |
| `sqrt`      | square root          |                                        | 
| `sin`       | sine                 |                                        | 
| `cos`       | cosine               |                                        | 
| `tan`       | tangent              |                                        | 
| `arcsin`    | inverse sine         |                                        | 
| `arccos`    | inverse cosine       |                                        | 
| `arctan`    | inverse tangent      |                                        | 
| `exp`       | base _e_ exponential   |                                        | 
| `log`       | natural logarithm    |                                        | 
