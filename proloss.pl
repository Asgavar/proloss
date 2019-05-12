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
    [?], { length(T, 1) }, !.
letter([Letter]) -->
    [Letter], !.

word([]) -->
    [].
word([■]) -->
    [■].
word(Word) -->
    letter(Letter), word(Tail), {append(Letter, Tail, Word)}.

%% ?- phrase(letter(Output), X).
%% ?- phrase(letter(Output), [a]).
%% ?- phrase(word(Output), [a,b,c,■,?,f,?,?]).
%@ Output = [a, b, c, ■, _7258, f, _7270, _7276] ;


%% ?- working_directory(_, '/home/asgavar/').
%% ?- process_words_file('samples/input/proloss-words-pl').
%% ?- listing.

%% - funkcja parsująca jeden wiersz - atom -> atom, ? -> length(T, 1)
%% [a,b,?,?,d] -> [a,b,_G1,_G2,d]
%% - funkcja-wrapper budująca [H|T] z kolejnych wierszy
%% - właściwy algos na tym, co wyjdzie z powyższych
