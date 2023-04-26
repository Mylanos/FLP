read_line(L,C) :-
    get_char(C),
    (isEOFEOL(C), L = [], !;
        read_line(LL,_),
        (C == ' ', L = LL, !;
         [C|LL] = L)).

isEOFEOL(C) :-
    C == end_of_file;
    (char_code(C,Code), Code==10).

read_lines(Ls) :-
    read_line(L, C),
    (C == end_of_file, Ls=[] ;
    (read_lines(LLs), [L|LLs] = Ls)).

remove_last_line(Input, Output) :-
    append(Output, [_], Input).

extract_last_line([], []).
extract_last_line([L], [L]).
extract_last_line([_|T], L) :-
    extract_last_line(T, L).

writeLine([]) :- write('\n').
writeLine([Item|NextItems]) :- write(Item),writeLine(NextItems).

writeLines([]).
writeLines([Item|NextItems]) :- writeLine(Item),writeLines(NextItems).

% --------------------------

% searches through all rules for matching state and symbol on tape
search_rules([H|T], SearchedState, CurrSymbol, RESULT) :-
    (analyze_rule(H, SearchedState, CurrSymbol, RESULT), !); search_rules(T, SearchedState, CurrSymbol, RESULT).

% checks if the rule has a matching state and symbol
analyze_rule(Rule, SearchedState, CurrSymbol, FoundTransition) :-
    [CurrStateA, CurrSymbolA, CurrStateB, CurrSymbolB] = Rule,
        CurrStateA == SearchedState, CurrSymbolA == CurrSymbol,
            FoundTransition = [ CurrStateB, CurrSymbolB], !.

% load the rules consisting of states and symbols
load_rules([], []).
load_rules([H|T], LoadedRules) :- 
    [StateA, SymbolA, StateB, SymbolB] = H,
    (
          Rule = [StateA, SymbolA, StateB, SymbolB],
          load_rules(T, Res1),
          LoadedRules = [Rule|Res1]
    ).

start :-
    prompt(_, ''),
    % nacteme vstup
    read_lines(INPUT),
    remove_last_line(INPUT, RULES),
    extract_last_line(INPUT, PASKA_NO_S),
    load_rules(RULES, LOADED),
    append(['S'], PASKA_NO_S, PASKA),
    % false,
    search_rules(RULES, 'B', 'c', RESULT),
    % analyze_rule(['a', 'b', 'C', 'D'], 'a', 'b', RESULT),
    writeLine(RESULT),
    halt.