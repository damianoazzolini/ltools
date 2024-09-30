:- module(test_ltools, [test_ltools/0]).
:- use_module(library(plunit)).

:- set_random(seed(100)).
:- ['../prolog/ltools'].

test_list([
    count,
    cycle,
    repeat,
    accumulate,
    batched,
    slice,
    pairwise,
    % combinatorics
    cartesian_product,
    permutations,
    combinations,
    combinations_with_replacement,
    % more itertools
    chunked,
    divide,
    split_at_index,
    window,
    triplewise,
    intersperse,
    padded,
    repeat_each
  ]).

test_ltools:-
    test_list(L),
    length(L,N),
    write('Testing '), write(N), writeln(' predicates.'),
	run_tests(L).

:- begin_tests(count, []).
test(count_1, [nondet]):- findnsols(10, V, count(1,V), [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]).
test(count_2, [nondet]):- findnsols(10, V, count(5,V), [5, 6, 7, 8, 9, 10, 11, 12, 13, 14]).
test(count_3, [nondet]):- findnsols(10, V, count(-2,V), [-2, -1, 0, 1, 2, 3, 4, 5, 6, 7]).
test(count_4, [nondet]):- findnsols(10, V, count(1, 2, V), [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]).
test(count_5, [nondet]):- findnsols(10, V, count(5, 3, V), [5, 8, 11, 14, 17, 20, 23, 26, 29, 32]).
test(count_6, [nondet]):- findnsols(10, V, count(-2, -2, V), [-2, -4, -6, -8, -10, -12, -14, -16, -18, -20]).
test(count_7):- catch(count(1, 3), error(_,_), true).
:- end_tests(count).

:- begin_tests(cycle, []).
test(cycle_1, [nondet]):- findnsols(10, V, cycle([1,2], V), [1, 2, 1, 2, 1, 2, 1, 2, 1, 2]).
test(cycle_2):- cycle([], L), L = [].
:- end_tests(cycle).

:- begin_tests(repeat, []).
test(repeat_1, [nondet]):- findnsols(3, V, repeat(1, V), [1, 1, 1]).
test(repeat_2, [nondet]):- findnsols(4, V, repeat(1, 3, V), [1, 1, 1]).
test(repeat_3, [nondet]):- findnsols(4, V, repeat(V, 3, 1), [1, 1, 1]).
test(repeat_4, [nondet]):- repeat(1, 1).
test(repeat_5):- catch(repeat(1, -3, _), error(type_error(positive_integer,-3),_), true).
:- end_tests(repeat).

:- begin_tests(accumulate, []).
test(accumulate_1):- accumulate(plus, [1,2,4], L), L = [1, 3, 7].
test(accumulate_2):- accumulate(times, [1,2,4], L), L = [1, 2, 8].
test(accumulate_3):- catch(accumulate(times, [1,_,4], _), error(_,_), true).
:- end_tests(accumulate).

:- begin_tests(batched, []).
test(batched_1):- findall(V,batched([1,2,4,5,6], 3, V),LV),  LV = [[1,2,4], [5,6]].
test(batched_2):- catch(batched(_, -3, _), error(_,_), true).
test(batched_3):- catch(batched(1, _, _), error(_,_), true).
test(batched_4, [nondet]):- batched([1,2,4,5,6], 3, [1,2,4]).
:- end_tests(batched).

:- begin_tests(slice, []).
test(slice_1):- slice([1,2,4,5,6], 3, [1,2,4]).
test(slice_2):- catch(slice(_, -3, _), error(_,_), true).
test(slice_3):- slice([1,2,4,5,6], 3, 5, [4, 5, 6]).
test(slice_4):- catch(slice(_, -3, _, _), error(_,_), true).
:- end_tests(slice).

:- begin_tests(pairwise, []).
test(pairwise_1):- findall(S,pairwise([1,2,3], S),LS), LS = [[1,2],[2,3]].
test(pairwise_2, fail):- pairwise([1], _).
test(pairwise_3):- catch(pairwise(1, -3), error(_,_), true).
:- end_tests(pairwise).


%%%%%%%%%%%%%%%%%%%%%
% combinatorics
:- begin_tests(cartesian_product, []).
test(cartesian_product_1):- 
    findall(S,cartesian_product([[1,2,3],[4,5,6]],S), LS),
    LS = [[1, 4], [1, 5], [1, 6], [2, 4], [2, 5], [2, 6], [3, 4], [3, 5], [3, 6]].
:- end_tests(cartesian_product).

:- begin_tests(permutations, []).
test(permutations_1):- 
    findall(S,permutations([1, 2, 3], S), LS),
    LS = [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]].
test(permutations_2):- 
    findall(S,permutations([1, 2, 3, 4], 2, S), LS),
    LS = [[1, 2], [1, 3], [1, 4], [2, 1], [2, 3], [2, 4], [3, 1], [3, 2], [3, 4], [4, 1], [4, 2], [4, 3]].
:- end_tests(permutations).

:- begin_tests(combinations, []).
test(combinations_1):- 
    findall(S,combinations([1, 2, 3, 4], 2, S), LS),
    LS = [[1, 2], [1, 3], [1, 4], [2, 3], [2, 4], [3, 4]].
:- end_tests(combinations).

:- begin_tests(combinations_with_replacement, []).
test(combinations_with_replacement_1):- 
    findall(S,combinations_with_replacement([1, 2, 3, 4], 2, S),LS),
    LS = [[1, 1], [1, 2], [1, 3], [1, 4], [2, 2], [2, 3], [2, 4], [3, 3], [3, 4], [4, 4]].
:- end_tests(combinations_with_replacement).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% more itertools
:- begin_tests(chunked, []).
test(chunked_1):- findall(S,chunked([1, 2, 3, 4, 5, 6], 3, S),LS), LS = [[1,2,3],[4,5,6]].
test(chunked_2):- findall(S,chunked([1, 2, 3, 4, 5], 3, S),LS), LS = [[1,2,3],[4,5]].
:- end_tests(chunked).

:- begin_tests(divide, []).
test(divide_1):- findall(S,divide([1, 2, 3, 4, 5, 6], 2, S),LS), LS = [[1,2,3],[4,5,6]].
test(divide_2):- findall(S,divide([1, 2, 3, 4, 5, 6, 7], 3, S),LS), LS = [[1,2,3],[4,5,6],[7]].
:- end_tests(divide).

:- begin_tests(split_at_index, []).
test(split_at_index_1):- split_at_index([1, 2, 3, 4, 5, 6], 2, [1, 2], [3, 4, 5, 6]).
test(split_at_index_2):- split_at_index([1, 2, 3, 4, 5, 6], 0, [], [1, 2, 3, 4, 5, 6]).
test(split_at_index_3, [fail]):- split_at_index([1, 2, 3, 4, 5, 6], 7, _, _).
:- end_tests(split_at_index).

:- begin_tests(window, []).
test(window_1):- findall(Res, window([1, 2, 3], 2, 1, Res), LR), LR = [[1,2],[2,3]].
test(window_2):- findall(Res, window([1, 2, 3], 2, 2, Res), LR), LR = [[1,2]].
test(window_3):- findall(Res, window([1, 2, 3, 4], 2, 2, Res), LR), LR = [[1,2],[3,4]].
test(window_4):- findall(Res, window([1, 2, 3], 2, Res), LR), LR = [[1,2],[2,3]].
:- end_tests(window).

:- begin_tests(triplewise, []).
test(triplewise_1):- findall(Res, triplewise([1, 2, 3, 4], Res), LR), LR = [[1, 2, 3],[2, 3, 4]].
test(triplewise_2, [fail]):- triplewise([1, 2], _).
:- end_tests(triplewise).

:- begin_tests(intersperse, []).
test(intersperse_1):- intersperse([1, 2, 3, 4], 10, 3, Res), Res = [1, 2, 3, 10, 4].
test(intersperse_2):- intersperse([1, 2, 3, 4], 10, 1, Res), Res = [1, 10, 2, 10, 3, 10, 4, 10].
test(intersperse_3):- intersperse([1, 2, 3, 4], 10, Res), Res = [1, 10, 2, 10, 3, 10, 4, 10].
:- end_tests(intersperse).

:- begin_tests(padded, []).
test(padded_right_1):- padded_right([1, 2, 3, 4], a, 1, Res), Res = [1, 2, 3, 4].
test(padded_right_2):- padded_left([1, 2, 3, 4], a, 1, Res), Res = [1, 2, 3, 4].
test(padded_left_1):- padded_right([1, 2, 3, 4], a, 7, Res), Res = [1, 2, 3, 4, a, a, a].
test(padded_left_2):- padded_left([1, 2, 3, 4], a, 7, Res), Res = [a, a, a, 1, 2, 3, 4].
:- end_tests(padded).

:- begin_tests(repeat_each, []).
test(repeat_each_1):- repeat_each([1, 2, 3, 4], 3, Res), Res = [1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4].
test(repeat_each_2):- repeat_each([1, 2, 3, 4], 1, Res), Res = [1, 2, 3, 4].
:- end_tests(repeat_each).

:- begin_tests(distribute, []).
% no need to test since it calls batched/3.
test(distribute_0):- true.
:- end_tests(distribute).


