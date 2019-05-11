:- dynamic word/1.

process_words_file(Filename) :-
    open(Filename, read, Descriptor),
    repeat,
    read_string(Descriptor, '\n', '\r', IsItEnd, WordAsString),
    atom_chars(WordAsAtom, WordAsString),
    assert(word(WordAsAtom)),
    IsItEnd == -1,
    close(Descriptor).

%% ?- working_directory(_, '/home/asgavar/').
%% ?- process_words_file('samples/input/proloss-words-pl').
%% ?- listing.
