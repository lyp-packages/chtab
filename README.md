# chtab: Alternative TabStaff algorithm for Lilypond

This package provides an alternative, better tab fingering algorithm, developed by Chuanjun He, and implemented in Lilypond/Scheme by Christopher Heckman. This algorithm was discussed on [lilypond-dev](http://lilypond.1069038.n5.nabble.com/Is-Anyone-Working-on-a-Better-Tablature-Algorithm-td196422.html).

## Installation

```bash
lyp install chtab
```

## Usage

```lilypond
\require "chtab"

music = { ... }

<<
  \music
  \chTab {
    \new TabStaff \with { 
      stringTunings = #guitar-tuning
    } \music } 
>>
...
```

For more information see the included [example](test/chtab-test.ly).

