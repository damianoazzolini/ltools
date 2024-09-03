# ltools: list manipulations in Prolog

This module contains predicates for list manipulation in Prolog.
It is inspired by Python's [itertools](https://docs.python.org/3/library/itertools.html) and [more-itertools](https://more-itertools.readthedocs.io/en/stable/toctree.html) modules.

## Installation and Usage
The library was tested on SWI prolog.
However, it should be easy to make it work also on other Prolog systems, since few built-in predicates were used.

In SWI, you can install it with:
```
?- pack_install('https://github.com/damianoazzolini/ltools').
```

Then
```
?- use_module(library(ltools)).
true.
```
and you are ready to go.

## Available Predicates
The source code is documented and should contain the explanation of each predicate, together with examples.
Furthermore, also tests can guide you with several examples.

Available predicates:
- `count/2`
- `count/3`
- `cycle/2`
- `repeat/2`
- `repeat/3`
- `accumulate/3`
- `batched/3`
- `slice/3`
- `slice/4`
- `pairwise/2`
- `chunked/3`
- `divide/3`
- `split_at_index/4`
- `window/3`
- `window/4`
- `triplewise/2`
- `intersperse/3`
- `intersperse/4`
- `padded_right/4`
- `padded_left/4`
- `repeat_each/3`
- `cartesian_product/2`
- `permutations/2`
- `permutations/3`
- `combinations/3`
- `combinations_with_replacement/3`

Details and usage.

```
count(+Start:int, -N:int)
?- count(1, N), N = 1 ; N = 2 ; N = 3 ; ...
```
Unifies `N` with a number starting from `Start` and up to infinity.

```
count(+Start:int, +Step:int, -N:int)
?- count(2,2,N), N = 2 ; N = 4 ; N = 6 ; ...
```
Unifies `N` with a number starting from `Start` and up to infinity with step `Step`.

```
cycle(+List, -El)
?- cycle([1,2], El), El = 1 ; El = 2 ; El = 1 ; El = 2 ; ... 

```
Cycles through the elements `El` of the list `List`.


```
repeat(El,El)
?- repeat(1,El), El = 1 ; El = 1 ; ...
```
Repeats `El` indefinitely.


```
repeat(El,N,El)
?- repeat(1,2,El), El = 1 ; El = 1
```
Repeats `El` up to N times. 


```
accumulate(+Predicate, +L:list, -V:list)
?- accumulate(plus, [1,2,4], L), L = [1, 3, 7]
?- accumulate(times, [1,2,4], L), L = [1, 2, 8]
```
Accumulates the list `L` according the predicate `Predicate`.
`Predicate` can be `times` or `plus`.
This is essentially a wrapper for `scanl/4`.

```
batched(+L:list, +N:int, -V:list) 
?- batched([1,2,4,5,6], 3, V), V = [1,2,4] ; V = [5,6]
```
Creates batches of size `N` from the list `L`, computes all on backtracking.
The last one may of length less than `N`.


```
slice(+L:list, +Start:int, +End:int, -S:list) 
?- slice([1,2,4,5,6], 2, S), S = [1, 2].
```
Unifies `S` with the sub list extracted from `L` between `0` and the `End`
index.

```
slice(+L:list, +Start:int, +End:int, -S:list) 
?- slice([1,2,4,5,6], 0, 10, S), S = [1, 2, 4, 5, 6].
```

Unifies `S` with the sub list extracted from `L` between Start and `End` indexes
counting from 1.

```
pairwise(+List, -S:list)
?- pairwise([1,2,3], S), S = [1, 2] ; S = [2, 3].
?- pairwise([1], S), false.
```
Unifies in backtracking each element of `L` taken pairwise with `S`.

```
cartesian_product(+List:list, -P:list)
?- cartesian_product([[1,2,3],[4,5,6]],P).
P = [1, 4] ; P = [1, 5] ; P = [1, 6] ; P = [2, 4] ; P = [2, 5]
P = [2, 6] ; P = [3, 4] ; P = [3, 5] ; P = [3, 6].
```
Computes the cartesian product of the input list of lists.

```
permutations(+List:list, -P:list)
permutations(+List:list, +Len:int -P:list)
?- permutations([1, 2, 3, 4], 2, Res).
Res = [1, 2] ; Res = [1, 3] ; Res = [1, 4] ; Res = [2, 1] ; 
Res = [2, 3] ; Res = [2, 4] ; Res = [3, 1] ; Res = [3, 2] ; 
Res = [3, 4] ; Res = [4, 1] ; Res = [4, 2] ; Res = [4, 3]
```
Computes the permutations of length `Len` of the input list `List`.
If `Len` is not provided, it defaults to the length of the input
list `List`.

```
combinations(+List:list, +Len:int -P:list)
?- combinations([1, 2, 3, 4], 2, Res).
Res = [1, 2] ; Res = [1, 3] ; Res = [1, 4] ;
Res = [2, 3] ; Res = [2, 4] ; 
Res = [3, 4]
```
Computes the combinations of the input list `List`.

```
combinations_with_replacements(+List:list, +Len:int -P:list)
?- combinations_with_replacements([1, 2, 3, 4], 2, Res).
Res = [1, 1] ; Res = [1, 2] ; Res = [1, 3] ; Res = [1, 4] ;
Res = [2, 2] ; Res = [2, 3] ; Res = [2, 4] ; 
Res = [3, 3] ; Res = [3, 4]
Res = [4, 4]
```
Computes the combinations with replacements of the input list `List`.

```
chunked(+List:list, +Size:int, -Chunk:list)
?- chunked([1, 2, 3, 4, 5, 6], 3, L).
L = [1, 2, 3] ;
L = [4, 5, 6]
?- chunked([1, 2, 3, 4, 5], 3, L).
L = [1, 2, 3]
L = [4, 5]
```
Splits the list `List` into chunks of size `Size` and unifies
the result with `Chunk`. If the length of the list is not
divisible by `Size`, the last chunk will be of length 
less than `Size`.

```
divide(+List:list, +Parts:int, -Divided:list)
?- divide([1, 2, 3, 4, 5, 6], 2, L).
L = [1, 2, 3] ;
L = [4, 5, 6]
?- divide([1, 2, 3, 4, 5,6,7], 3, L).
L = [1, 2, 3] ;
L = [4, 5, 6] ;
L = [7]
```
Divides the list `List` into `Parts` parts of equal length. 
If the length of the list is not divisible by `Parts`, 
the last part will be shorter.

```
split_at_index(+List:list, +Pos:int, -L0:list, -L1:List)
?- split_at_index([1, 2, 3, 4, 5, 6], 2, L0,L1).
L0 = [1, 2],
L1 = [3, 4, 5, 6]
?- split_at_index([1, 2, 3, 4, 5, 6], 0, L0,L1).
L0 = [],
L1 = [1, 2, 3, 4, 5, 6]
?- split_at_index([1, 2, 3, 4, 5, 6], 7, L0,L1).
false
```
Splits the list `List` at position `Pos`, starting from 0, and unifies `L0` and `L1`
with the resulting lists. The index `Pos` will be in `L1`.
Fails if `Pos` is greater that the length of the list.

```
window(+List:list, +Size:int, +Step:int, -Res:list)
window(+List:list, +Size:int, -Res:list)
?- window([1, 2, 3], 2, 1, Res).
Res = [1, 2] ;
Res = [2, 3]
?- window([1, 2, 3], 2, 2, Res).
Res = [1, 2]
?- window([1, 2, 3, 4], 2, 2, Res).
Res = [1, 2] ;
Res = [3, 4]
```
Unifies `Res` with a sliding window of size `Size` of `List`, which increments
each time by `Step`. If `Step` is not provided, it defaults to 1.


```
triplewise(+List, -S:list)
?- triplewise([1, 2, 3, 4], Res).
Res = [1, 2, 3] ;
Res = [2, 3, 4]
?- triplewise([1, 2], Res).
false
```
Unifies in backtracking each element of `L` taken triplewise with `S`.



```
intersperse(+List:list, +Value, +Step:int, -Res).
intersperse(+List:list, +Value, -Res).
?- intersperse([1, 2, 3, 4], 10, 3, Res)
Res = [1, 2, 3, 10, 4]
?- intersperse([1, 2, 3, 4], 10, 1, Res).
Res = [1, 10, 2, 10, 3, 10, 4, 10]
```
Interleaves the elements of the list `List` with the element `Value`
each `Step` positions and unifies the result with Res. If `Step` is
not provided it defaults to 1.


```
padded_right(+List:list, +Element, +Length:int, -Res:list).
padded_left(+List:list, +Element, +Length:int, -Res:list).
?- padded_right([1, 2, 3, 4], a, 1, Res).
Res = [1, 2, 3, 4]
?- padded_left([1, 2, 3, 4], a, 1, Res).
Res = [1, 2, 3, 4]
?- padded_right([1, 2, 3, 4], a, 7, Res).
Res = [1, 2, 3, 4, a, a, a]
?- padded_left([1, 2, 3, 4], a, 7, Res).
Res = [a, a, a, 1, 2, 3, 4]
```
Unifies the list `Res` with the list `List` padded right (resp. left) with 
the element `Element` repeated until length `Length` is reached.

```
repeat_each(+List:list, +Times:int, -Res:list)
?- repeat_each([1, 2, 3, 4], 3, Res).
Res = [1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4]
?- repeat_each([1, 2, 3, 4], 1, Res).
Res = [1, 2, 3, 4]
```
Repeats each element in the list `List` `Times` times and unifies
the result with `Res`.

## How to contribute
Pull requests or issues, also to request further predicates - anything is welcome.

## DISCLAIMER
This software is provided as it is. It may contain bugs.