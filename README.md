The **Proportional Electoral College** (PEC) is described in
my 2016 essay _[Dragging the Electoral College into the 21st
century](https://galen.xyz/electoral/)_.  This demonstration code
calculates PEC results for any election data input.  A sub-module
under `data` includes data for American presidential elections from
1828 to 2020.

##  How to use

The recommended (easiest) way to build this is via Haskell Stack.
If you're familiar with Cabal, you can use it instead, but will have
to install [hpre](https://github.com/galenhuntington/hpre) manually.

The binary can be compiled initially using `stack build` (if this
step is skipped, it will be compiled the first time you run it).
Then run using `stack run`.  Example:

```
$ stack run data/2000.dat
      t       A    B    C           v/e                Δ
AL    9       6    3    0       200,000       79,943 B→A
AK    3       3    0    0       100,000       44,198 A→B
AZ    8       5    3    0       200,000       56,630 B→A *
AR    6       4    2    0       200,000       64,485 B→A *
CA   54      22   30    2       205,000       13,552 C→B
CO    8       5    3    0       240,000       43,095 B→A
CT    8       2    6    0       200,000       29,096 B→A
DE    3       0    3    0       150,000       21,391 B→A
DC    3       0    3    0       100,000       76,926 B→A
FL   25      14   11    0       242,700          269 A→B
(etc.)
```

(You can alternatively use `stack install` and run the binary directly,
which is slightly faster.)

##  Output

The columns are as follows:

*  The state.  Two-letter abbreviations are used to keep the table
compact.

*  The total number of electors assigned to the state.

*  After that, each candidate is designated with letters starting with
`A`.  The data file can be consulted to determine which candidate is
referred to, in cases when it's not obvious.  Each column indicates
the number of electors that candidate will receive for the state.

*  `v/e` means votes per proportional elector, which is chosen so
that the number of electors hits the target.  There will be a range
of numbers that suffices, so the output will aim to choose a nice
round number.  In the case of a very close election, it is possible
that no whole number will be adequate, in which case it is approximate.

*  The final column indicates how close the results in that state
were, by showing the smallest vote shift (voters switching from one
candidate to another) that will change the slate of _proportional_
electors.  This does not account for the two at-large electors,
which are determined by plurality (and thus easy to calculate).
If the plurality winner could be changed by a smaller vote shift than
the one listed, this is notated with an asterisk `*`.

##  Ties

Ties are a theoretical nuisance but of little practical relevance.
I try to provide sensible results when there's an exact tie for an
elector, but don't worry myself too much about it.  The final column
may read `1 *→*` to indicate a knife's edge, that any number of
single vote changes would change the results.

The vote shift values avoid ties.  E.g., if shifting _n_ votes would
induce a tie for an elector, it will list _n+1_ just to be sure.

