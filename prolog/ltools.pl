:- module(ltools, [
    count/2,
    count/3,
    cycle/2,
    repeat/2,
    repeat/3,
    accumulate/3,
    batched/3,
    slice/3,
    slice/4,
    pairwise/2,
    % more itertools
    chunked/3,
    divide/3,
    split_at_index/4,
    window/3,
    window/4,
    triplewise/2,
    intersperse/3,
    intersperse/4,
    padded_right/4,
    padded_left/4,
    repeat_each/3,
    % combinatorics
    cartesian_product/2,
    permutations/2,
    permutations/3,
    combinations/3,
    combinations_with_replacement/3
]).

% https://docs.python.org/3/library/itertools.html#itertools.count
% https://more-itertools.readthedocs.io/en/stable/api.html

times(A,B,C):-
    C is B * A.

/**
 * count(+Start:int, -N:int)
 * Unifies N with a number starting from Start and up to infinity.
 * count(1, N), N = 1 ; N = 2 ; N = 3 ; ...
*/
count_(N,_,N).
count_(N,Step,IIn):-
    N1 is N + Step,
    count_(N1,Step,IIn).

count_check_args(Start,Step,N):-
    must_be(integer, Start),
    must_be(integer, Step),
    must_be(var, N),
    count_(Start,Step,N).


count(Start,N):-
    count_(Start,1,N).
/**
 * count(+Start:int, +Step:int, -N:int)
 * Unifies N with a number starting from Start and up to infinity with step Step.
 * count(2,2,N), N = 2 ; N = 4 ; N = 6 ; ...
*/
count(Start, Step, N):-
    count_(Start,Step,N).

/**
 * cycle(+List, -El)
 * Cycles through the elements El of the list.
 * cycle([1,2], El), El = 1 ; El = 2 ; El = 1 ; El = 2 ; ... 
 * cycle([], _) 
*/
cycle_([H|_],_,H).
cycle_([_|T],L,H):-
    cycle_(T,L,H).
cycle_([],L,H):-
    cycle_(L,L,H).

cycle_check_args(L,C):-
    must_be(list,L),
    must_be(var,C),
    ( L = [] -> C = _ ; cycle_(L,L,C)).

cycle(L,C):-
    cycle_check_args(L,C).

/**
 * repeat(El,El)
 * repeats El indefinitely 
 * repeat(1,El), El = 1 ; El = 1 ; ...
*/
repeat_(V,V).
repeat_(V,V):-
    repeat_(V,V).

repeat(N,V):-
    must_be(var, V),
    repeat_(N,V).


/**
 * repeat(El,N,El)
 * repeats El up to N times 
 * repeat(1,2,El), El = 1 ; El = 1
*/
repeat_(V, 1, V):-  !.
repeat_(V, T, V):-  T > 0.
repeat_(V, T, V):- 
    T > 0,
    T1 is T - 1,
    repeat_(V, T1, V).

repeat(N,Times,V):-
    must_be(positive_integer, Times),
    must_be(var, V),
    repeat_(N,Times,V).

/**
 * accumulate(+Predicate, +L:list, -V:list)
 * Accumulates the list L according the predicate Predicate.
 * Predicate can be times or plus.
 * This is essentially a wrapper for scanl/4
 * accumulate(plus, [1,2,4], L), L = [1, 3, 7]
 * accumulate(times, [1,2,4], L), L = [1, 2, 8]
*/
accumulate(times, L, V):-
    must_be(list(number),L),
    scanl(times, L, 1, [_|V]).
accumulate(plus, L, V):-
    must_be(list(number),L),
    scanl(plus, L, 0, [_|V]).

/**
 * batched(+L:list, +N:int, -V:list) 
 * Creates batches of size N from the list L, computes all on backtracking.
 * The last one may of length less than N.
 * batched([1,2,4,5,6], 3, V), V = [1,2,4] ; V = [5,6]
 * 
*/
batched_([], Sz, _, L, L):- Sz > 0.
batched_(_, 0, _, L, L).
batched_([H|T], 0, Size, _, L):-
    batched_([H|T], Size, Size, [], L).
batched_([H|T], N, Size, L, LO):-
    N > 0,
    append(L,[H],LT),
    N1 is N - 1,
    batched_(T,N1,Size,LT,LO).

batched_check_args(L, V, Batch):-
    must_be(list,L),
    must_be(positive_integer, V),
    batched_(L, V, V, [], Batch).

batched(L, V, Batch):-
    batched_check_args(L, V, Batch).

/**
 * slice(+L:list, +Start:int, +End:int, -S:list) 
 * Unifies St with the sub list extracted from L between 0 and the End
 * index.
 * slice([1,2,4,5,6], 2, S), S = [1, 2].
*/
slice(L,End,Sublist):-
    slice_check_args(L, 0, End, Sublist).

/**
 * slice(+L:list, +Start:int, +End:int, -S:list) 
 * Unifies S with the sub list extracted from L between Start and End indexes
 * counting from 1.
 * slice([1,2,4,5,6], 0, 10, S), S = [1, 2, 4, 5, 6].
*/
slice_(L, Start, End, Sublist) :-
    findall(V, (between(Start, End, I), nth1(I, L, V)), Sublist).

slice_check_args(L, Start, End, Sublist):-
    must_be(list,L),
    must_be(nonneg, Start),
    must_be(nonneg, End),
    slice_(L, Start, End, Sublist).

slice(L,Start,End,Sublist):-
    slice_check_args(L, Start, End, Sublist).

/**
 * pairwise(+List, -S:list)
 * Unifies in backtracking each element of L taken pairwise with S.
 * pairwise([1,2,3], S), S = [1, 2] ; S = [2, 3].
 * pairwise([1], S), false.
*/
pairwise_([A,B|_],[A,B]).
pairwise_([_,B|T],L):-
    pairwise_([B|T],L).

pairwise_check_args(L, Sublist):-
    must_be(list,L),
    pairwise_(L, Sublist).

pairwise(L,LO):-
    pairwise_check_args(L, LO).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Combinatorics predicates

/**
 * cartesian_product(+List:list, -P:list)
 * Computes the cartesian product of the input list of lists.
 * cartesian_product([[1,2,3],[4,5,6]],P).
 * P = [1, 4] ; P = [1, 5] ; P = [1, 6] ; P = [2, 4] ; P = [2, 5]
 * P = [2, 6] ; P = [3, 4] ; P = [3, 5] ; P = [3, 6].
*/
cartesian_product([],L,L).
cartesian_product([List|T],LT,LO):-
    member(El,List),
    append(LT,[El],LT1),
    cartesian_product(T,LT1,LO).

cartesian_product(L,Res):-
    cartesian_product(L,[],Res).

/**
 * permutations(+List:list, -P:list)
 * permutations(+List:list, +Len:int -P:list)
 * Computes the permutations of length Len of the input list List.
 * If Len is not provided, it defaults to the length of the input
 * list List.
 * permutations([1, 2, 3, 4], 2, Res).
 * Res = [1, 2] ; Res = [1, 3] ; Res = [1, 4] ; Res = [2, 1] ; 
 * Res = [2, 3] ; Res = [2, 4] ; Res = [3, 1] ; Res = [3, 2] ; 
 * Res = [3, 4] ; Res = [4, 1] ; Res = [4, 2] ; Res = [4, 3]
*/
permutations_(ToConsider,Len,Current,P):-
    length(Current,N),
    ( N >= Len ->
    	P = Current ;
    	select(El,ToConsider,Rem),
    	% member(El,ToConsider),
        append(Current,[El],C1),
        permutations_(Rem,Len,C1,P)
    ).
permutations(List,P):-
    length(List,N),
    permutations(List,N,P).
permutations(List,Len,P):-
    must_be(positive_integer, Len),
    select(El,List,ToConsider),
    permutations_(ToConsider,Len,[El],P).

/**
 * combinations(+List:list, +Len:int -P:list)
 * Computes the combinations of the input list List.
 * combinations([1, 2, 3, 4], 2, Res).
 * Res = [1, 2] ; Res = [1, 3] ; Res = [1, 4] ;
 * Res = [2, 3] ; Res = [2, 4] ; 
 * Res = [3, 4]
*/
combinations(List,Len,P):-
    must_be(positive_integer, Len),
    combinations_(List,Len,P).
combinations_([H|T],Len,P):-
    permutations_(T,Len,[H],P).
combinations_([_|T],Len,P):-
    combinations(T,Len,P).

/**
 * combinations_with_replacements(+List:list, +Len:int -P:list)
 * Computes the combinations with replacements of the input list List.
 * combinations_with_replacements([1, 2, 3, 4], 2, Res).
 * Res = [1, 1] ; Res = [1, 2] ; Res = [1, 3] ; Res = [1, 4] ;
 * Res = [2, 2] ; Res = [2, 3] ; Res = [2, 4] ; 
 * Res = [3, 3] ; Res = [3, 4]
 * Res = [4, 4]
*/
combinations_with_replacement(List,Len,P):-
    must_be(positive_integer, Len),
    combinations_with_replacement_(List,Len,P).
combinations_with_replacement_([H|T],Len,P):-
    permutations_([H|T],Len,[H],P).
combinations_with_replacement_([_|T],Len,P):-
    combinations_with_replacement(T,Len,P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% more itertools

/***
 * chunked(+List:list, +Size:int, -Chunk:list)
 * Splits the list List into chunks of size Size and unifies
 * the result with Chunk. If the length of the list is not
 * divisible by Size, the last chunk will be of length 
 * less than Size.
 * chunked([1, 2, 3, 4, 5, 6], 3, L).
 * L = [1, 2, 3] ;
 * L = [4, 5, 6]
 * chunked([1, 2, 3, 4, 5], 3, L).
 * L = [1, 2, 3]
 * L = [4, 5]
*/
chunked_(L, Size, L):-
    length(L,N),
    Size > N,
    N > 0.
chunked_(List, Size, Chunk):-
    length(Chunk,Size),
    append(Chunk, _, List).
chunked_(List, Size, Chunk):-
    length(Chunk_,Size),
    append(Chunk_, Rem, List),
    chunked_(Rem, Size, Chunk).

chunked(List, Size, Chunk):-
    must_be(nonneg, Size),
    chunked_(List, Size, Chunk).

/**
 * divide(+List:list, +Parts:int, -Divided:list)
 * Divides the list List into Parts parts of equal length. 
 * If the length * of the list is not divisible by Parts, 
 * the last part will be shorter.
 * divide([1, 2, 3, 4, 5, 6], 2, L).
 * L = [1, 2, 3] ;
 * L = [4, 5, 6]
 * divide([1, 2, 3, 4, 5,6,7], 3, L).
 * L = [1, 2, 3] ;
 * L = [4, 5, 6] ;
 * L = [7]
*/ 
divide(List, Parts, Divided):-
    must_be(nonneg, Parts),
    length(List,N),
    Parts > 0, 
    Parts =< N,
    Chunk is ceil(N/Parts),
    chunked_(List, Chunk, Divided).

/**
 * split_at_index(+List:list, +Pos:int, -L0:list, -L1:List)
 * Splits the list List at position Pos, starting from 0, and unifies L0 and L1
 * with the resulting lists. The index Pos will be in L1.
 * Fails if Pos is greater that the length of the list
 * split_at_index([1, 2, 3, 4, 5, 6], 2, L0,L1).
 * L0 = [1, 2],
 * L1 = [3, 4, 5, 6]
 * split_at_index([1, 2, 3, 4, 5, 6], 0, L0,L1).
 * L0 = [],
 * L1 = [1, 2, 3, 4, 5, 6]
 * split_at_index([1, 2, 3, 4, 5, 6], 7, L0,L1).
 * false
*/
split_at_index(List,Index,L0,L1):-
    must_be(nonneg, Index),
    length(L0, Index),
    append(L0,L1,List).

/**
 * window(+List:list, +Size:int, +Step:int, -Res:list)
 * window(+List:list, +Size:int, -Res:list)
 * Unifies Res with a sliding window of size Size of List, which increments
 * each time by Step. If Step is not provided, it defaults to 1.
 * window([1, 2, 3], 2, 1, Res).
 * Res = [1, 2] ;
 * Res = [2, 3]
 * window([1, 2, 3], 2, 2, Res).
 * Res = [1, 2]
 * window([1, 2, 3, 4], 2, 2, Res).
 * Res = [1, 2] ;
 * Res = [3, 4]
*/
window_(List,Size,_Step,Window):-
    length(List, N),
    N >= Size,
    length(Window,Size),
    append(Window,_,List).
window_(List,Size,Step,Window):-
    length(ToRemove,Step),
    append(ToRemove,LRem,List),
    window_(LRem,Size,Step,Window).
window(List, Size, Window):-
    window(List, Size, 1, Window).
window(List, Size, Step, Window):-
    must_be(nonneg, Size),
    window_(List,Size,Step,Window).

/**
 * triplewise(+List, -S:list)
 * Unifies in backtracking each element of L taken triplewise with S.
 * triplewise([1, 2, 3, 4], Res).
 * Res = [1, 2, 3] ;
 * Res = [2, 3, 4]
 * triplewise([1, 2], Res).
 * false
*/
triplewise_([A,B,C|_],[A,B,C]).
triplewise_([_,B|T],L):-
    triplewise_([B|T],L).
triplewise_check_args(L, Sublist):-
    must_be(list,L),
    triplewise_(L, Sublist).
triplewise(L,LO):-
    triplewise_check_args(L, LO).

/**
 * intersperse(+List:list, +Value, +Step:int, -Res).
 * intersperse(+List:list, +Value, -Res).
 * Interleaves the elements of the list List with the element Value
 * each Step positions and unifies the result with Res. If Step is
 * not provided it defaults to 1.
 * intersperse([1, 2, 3, 4], 10, 3, Res)
 * Res = [1, 2, 3, 10, 4]
 * intersperse([1, 2, 3, 4], 10, 1, Res).
 * Res = [1, 10, 2, 10, 3, 10, 4, 10]
*/
intersperse([],_,_,L,L):- !.
intersperse(List, El, Step, LT, Res):-
    length(L,Step),
    length(List,N),
    ( N >= Step ->  
    	append(L,Rem,List),
        append(L,[El],LTT),
    	append(LT,LTT,LO) ;
    	append(LT,List,LO),
        Rem = []
    ),
    intersperse(Rem,El,Step,LO,Res).

intersperse(List, El, Res):-
    intersperse(List, El, 1, [], Res).
intersperse(List, El, Step, Res):-
    intersperse(List, El, Step, [], Res).

/**
 * padded_right(+List:list, +Element, +Length:int, -Res:list).
 * padded_left(+List:list, +Element, +Length:int, -Res:list).
 * Unifies the List Res with the list List padded right (resp. left) with 
 * the element Element * repeated until length Length is reached.
 * padded_right([1, 2, 3, 4], a, 1, Res).
 * Res = [1, 2, 3, 4]
 * padded_left([1, 2, 3, 4], a, 1, Res).
 * Res = [1, 2, 3, 4]
 * padded_right([1, 2, 3, 4], a, 7, Res).
 * Res = [1, 2, 3, 4, a, a, a]
 * padded_left([1, 2, 3, 4], a, 7, Res).
 * Res = [a, a, a, 1, 2, 3, 4]
*/
padded_right(List,Element,TargetLen,Result):-
    padded(List,Element,TargetLen,right,Result).
padded_left(List,Element,TargetLen,Result):-
    padded(List,Element,TargetLen,left,Result).
padded(List,Element,TargetLen,Type,Result):-
    must_be(integer, TargetLen),
    length(List,N),
    ( TargetLen =< N ->  
        Result = List ;
        R is TargetLen - N,
        findall(I, repeat(Element,R,I), LPad),
        (Type = right ->
            append(List,LPad,Result);
            append(LPad,List,Result)
        )
    ).

/**
 * repeat_each(+List:list, +Times:int, -Res:list)
 * Repeats each element in the list List Times times and unifies
 * the result with Res.
 * repeat_each([1, 2, 3, 4], 3, Res).
 * Res = [1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4]
 * repeat_each([1, 2, 3, 4], 1, Res).
 * Res = [1, 2, 3, 4]
*/
repeat_each_([],_,L,L).
repeat_each_([H|T], Times, LT, Res):-
    findnsols(Times, I, repeat(H,Times,I), LR),
    append(LT, LR, LT1),
    repeat_each_(T, Times, LT1, Res).
repeat_each(L, Times, Res):-
    must_be(positive_integer, Times),
    repeat_each_(L, Times, [], Res).