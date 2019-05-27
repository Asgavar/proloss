:- use_module(library(clpfd)).

:- dynamic word/1.

process_words_file(Filename) :-
    open(Filename, read, Descriptor),
    repeat,
    read_string(Descriptor, '\n', '\r', IsItEnd, WordAsString),
    atom_chars(WordAsAtom, WordAsString),
    atom_chars(WordAsAtom, WordAsLetterList),
    assert(word(WordAsLetterList)),
    IsItEnd == -1,
    close(Descriptor),
    !.

letter(T) -->
    [?], {length(T, 1)}, !.
letter([■]) -->
    [■], !.
letter([Letter]) -->
    [Letter], !.

word([]) -->
    [].
word(Word) -->
    letter(Letter), word(Tail), {append(Letter, Tail, Word)}.

process_map_file_row(Descriptor, Acc) :-
    read_string(Descriptor, '\n', '\r', IsItEnd, RowString),
    atom_chars(RowString, RowLetterList),
    (
        IsItEnd == -1 ->
        phrase(word(Acc), RowLetterList)
    ;   phrase(word(CurrWord), RowLetterList),
        process_map_file_row(Descriptor, NextAcc),
        Acc = [CurrWord|NextAcc]
    ).

process_map_file(Filename, PartlyMaterializedMap) :-
    open(Filename, read, Descriptor),
    process_map_file_row(Descriptor, PartlyMaterializedMap),
    close(Descriptor),
    !.

materialize_row([], [], []) :- !.
materialize_row([], WordCollector, RealWord) :-
    reverse(WordCollector, NotReversedWordCollector),
    NotReversedWordCollector = RealWord,
    word(RealWord),
    !.
materialize_row([Block|Tail], [], [■|RetTail]) :-
    \+ var(Block),
    Block = ■,
    materialize_row(Tail, [], RetTail).
materialize_row([Block|Tail], WordCollector, Ret) :-
    \+ var(Block),
    Block = ■,
    reverse(WordCollector, NotReversedWordCollector),
    NotReversedWordCollector = RealWord,
    word(RealWord),
    materialize_row(Tail, [], PrevRet),
    append(RealWord, [■], Head),
    append(Head, PrevRet, Ret).
materialize_row([Letter|Tail], WordCollector, Ret) :-
    materialize_row(Tail, [Letter|WordCollector], Ret).

materialize_all_rows([], []) :- !.
materialize_all_rows([RawRow|RawTail], [MaterializedRow|MaterializedTail]) :-
    materialize_row(RawRow, [], MaterializedRow),
    materialize_all_rows(RawTail, MaterializedTail).

%% could be nicely written as:
%%     display_one_row([]) :- nl.
%% but then there would be a trailing space
%% at every row's end.
display_one_row([Head|[]]) :-
    write(Head),
    nl,
    !.
display_one_row([Head|Tail]) :-
    write(Head),
    write(' '),
    display_one_row(Tail).

display_all_rows([]).
display_all_rows([Head|Tail]) :-
    display_one_row(Head),
    display_all_rows(Tail).

main(Map) :-
    working_directory(_, '/home/asgavar/proloss'),
    process_words_file('samples/input/proloss-words-pl'),
    process_map_file('samples/input/proloss-map-3', RawMap),
    materialize_all_rows(RawMap, PartlyMaterializedMap),
    %% transpose(PartlyMaterializedMap, TransposedMap),
    %% display_all_rows(TransposedMap),
    %% materialize_all_rows(TransposedMap, Map),
    display_all_rows(PartlyMaterializedMap).

%% ===TESTING AREA AHEAD===

%% ?- process_words_file('samples/input/proloss-words-pl').
%% ?- materialize_row([■,A,k,C,D,E,■,■,■], [], X).
%@ A = C, C = E, E = o,
%@ D = ł,
%@ X = [■, o, k, o, ł, o, ■, ■, ■] ;

%% ?- phrase(letter(Output), X).
%% ?- phrase(letter(Output), [a]).
%% ?- phrase(word(Output), [a,b,c,■,?,f,?,?]).
%% ?- display_one_row([t,ę,g,i,e,■,k,r,ó,w,s,k,o]).

%% ?- main(_).

%% TODO:
%% - deterministyczny output
%% - losowy wynik
%% - brak powtarzających się wyników
%% - vertical/horizontal/both
