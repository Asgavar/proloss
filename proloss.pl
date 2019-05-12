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

process_map_file(Filename, Map) :-
    open(Filename, read, Descriptor),
    process_map_file_row(Descriptor, Map),
    close(Descriptor),
    !.

main(Map) :-
    working_directory(_, '/home/asgavar/proloss'),
    process_words_file('samples/input/proloss-words-pl'),
    process_map_file('samples/input/proloss-map', Map).

%% ?- phrase(letter(Output), X).
%% ?- phrase(letter(Output), [a]).
%% ?- phrase(word(Output), [a,b,c,■,?,f,?,?]).
%@ Output = [a, b, c, ■, _2032, f, _2044, _2050] ;
%@ Output = [a, b, c, ■, _7258, f, _7270, _7276] ;
%% ?- main(Map).
%@ Map = [[t, o, t, _8252, l, _8264, o, _8276|...], [■, ■, _8472, _8478, _8484, _8490, _8496|...]].

%% - funkcja-wrapper budująca [H|T] z kolejnych wierszy
%% - właściwy algos na tym, co wyjdzie z powyższych
