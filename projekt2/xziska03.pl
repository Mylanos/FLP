% Logicky Projekt 2 FLP 2022/23 - Turingov stroj
% Autor Bc. Marek Ziska
% Login xziska03


%-------------credits to course materials provided in labs------------------
read_line(L,C) :-
    get_char(C),
    (isEOFEOL(C), L = [], !;
        read_line(LL,_),
%        (C == ' ', L = LL, !;
         [C|LL] = L).

isEOFEOL(C) :-
    C == end_of_file;
    (char_code(C,Code), Code==10).

read_lines(Ls) :-
    read_line(L, C),
    (C == end_of_file, Ls=[] ;
    (read_lines(LLs), [L|LLs] = Ls)).

remove_last_line(Input, Output) :-
    append(Output, [_], Input).
%---------------------------------------------------------------------------

% extracts last line from the given input
extract_last_line([], []).
extract_last_line([TAPE], TAPE).
extract_last_line([_|T], TAPE) :-
    extract_last_line(T, TAPE).

% writes line to stdin with newline
writeLine([]) :- write('\n').
writeLine([Item|NextItems]) :- write(Item),writeLine(NextItems).

% writes lines to stdin with newlines between lines
writeLines([]).
writeLines([Item|NextItems]) :- writeLine(Item), writeLines(NextItems).

% --------------------------------------

% searches for a matching state and symbol on tape through all rules a return a list of possible next State-Symbol configurations
search_rules([], _, _, Acc, Acc).
search_rules([H|T], CurrState, CurrSymbol, Acc, RESULT) :-
    (analyze_rule(H, CurrState, CurrSymbol, SEMI_RESULT), search_rules(T, CurrState, CurrSymbol, [SEMI_RESULT|Acc], RESULT), !); search_rules(T, CurrState, CurrSymbol, Acc, RESULT).

% checks if the rule has a matching state and symbol
analyze_rule(Rule, CurrState, CurrSymbol, FoundTransition) :-
    [CurrStateA, CurrSymbolA, CurrStateB, CurrSymbolB] = Rule,
        CurrStateA == CurrState, CurrSymbolA == CurrSymbol,
            FoundTransition = [ CurrStateB, CurrSymbolB], !.

% checks if given symbol is upper
is_state(C) :- char_type(C, upper).
nonempty([_|_]) :- true.

% iterates over rules in reversed order, looks for possible FINAL states, if not found return first specified transition
select_next_state_and_symbol([H], RESULT) :- RESULT = H.
select_next_state_and_symbol([H|T], RESULT) :-
    H = [NextState, _], NextState == 'F', RESULT = H, !;
    select_next_state_and_symbol(T, RESULT). 

% searches for current state and current symbol in tape
search_state_in_tape([], _, _, RESULT) :- RESULT = [].
search_state_in_tape([A | T], CurrState, CurrSymbol, RESULT) :- is_state(A), CurrState = A,  T = [B | X], CurrSymbol = B, RESULT = X, ! ;
        search_state_in_tape(T, CurrState, CurrSymbol, RESULT).

% searches through reversed tape for symbols before actual state.
search_previous_symbols_in_tape([], _, RESULT) :- RESULT = [], !.
search_previous_symbols_in_tape([A | T], RESULT) :- is_state(A), RESULT = T, ! ;
    search_previous_symbols_in_tape(T, RESULT).

% modifies tape according to current rule matching the state and symbol on tape
tape_modification(CURRENT_TAPE, NewTapeState, NewTapeSymbol, ReadSymbol, PreviousSymbols, START_OF_TAPE, RESULT) :-
    NewTapeSymbol == 'R',
    (   
        nonempty(CURRENT_TAPE),
        (
            nonempty(PreviousSymbols),
            (   
                % in case there are characters in front of the current state extract first symbol from previous symbols and position it according to rigth shift
                [PreviousSymbol| TMP] = PreviousSymbols,
                reverse(TMP, START_OF_TAPE),
                NewSymbolList = [PreviousSymbol, ReadSymbol, NewTapeState],
                append(NewSymbolList, CURRENT_TAPE, RESULT)
            );
            (   
                % in case there are no characters in front of the current state just shift right
                START_OF_TAPE = [],
                NewSymbolList = [ReadSymbol, NewTapeState],
                append(NewSymbolList, CURRENT_TAPE, RESULT)
            )
        );
        % in case there are no symbols after current state, right shift causes the TS to stop abnormally
        append([ReadSymbol], PreviousSymbols, RESULT), !
    );
    NewTapeSymbol == 'L',
    (
        nonempty(PreviousSymbols),
        (
            % in case there are characters in front of the current state 
            % extract first symbol from previous symbols and position it with new state and symbol conforming to the left shift
            [PreviousSymbol| TMP] = PreviousSymbols,
            reverse(TMP, START_OF_TAPE),
            NewSymbolList = [NewTapeState, PreviousSymbol, ReadSymbol],
            append(NewSymbolList, CURRENT_TAPE, RESULT)
        );
            % in case there are no characters in front of the current state, left shift causes the TS to stop abnormally
            NewSymbolList = [ReadSymbol],
            append(NewSymbolList, CURRENT_TAPE, RESULT), !
    );
    (
        nonempty(PreviousSymbols),
        (
            % in case there are characters in front of the current state 
            % append them in front of current state and replace old state and symbol with new one
            [PreviousSymbol| TMP] = PreviousSymbols,
            reverse(TMP, START_OF_TAPE),
            NewSymbolList = [PreviousSymbol, NewTapeState, NewTapeSymbol], 
            append(NewSymbolList, CURRENT_TAPE, RESULT)
        );
        (
            % in case there are no characters in front of the current state 
            % just replace old state and symbol with new one
            START_OF_TAPE = [],
            NewSymbolList = [NewTapeState, NewTapeSymbol], 
            append(NewSymbolList, CURRENT_TAPE, RESULT)
        )
    ).

% checks if new state is not final
isnotfinal(A) :- A == 'F', false; true.

% orchestrates the run of TS, calls functions in given order to satisfy assignment
execute_turing([], _).
execute_turing(TAPE, RULES) :-
    % search for state in TAPE, return Current State, Current symbol and rest of the tape after current state and symbol
    (search_state_in_tape(TAPE, CurrTapeState, CurrTapeSymbol, REST_OF_TAPE),
        % search for rules that match current state and symbol, return array of posssible transitions
        search_rules(RULES, CurrTapeState, CurrTapeSymbol, [], POSSIBLE_TRANSITIONS),
        % search for possible Final state in the set of possible transitions, if not found return the first specified transition
        select_next_state_and_symbol(POSSIBLE_TRANSITIONS, NEXT_TRANSITION),
        [NewTapeState, NewTapeSymbol] = NEXT_TRANSITION,
        % get previous content (START_OF_TAPE) on Tape before current state and symbol
        reverse(TAPE, REVERSED_TAPE),
        search_previous_symbols_in_tape(REVERSED_TAPE, START_OF_TAPE_REVERSED),
        % modify the tape according to current values
        tape_modification(REST_OF_TAPE, NewTapeState, NewTapeSymbol, CurrTapeSymbol, START_OF_TAPE_REVERSED, START_OF_TAPE, MODIFIED_TAPE), 
        % append the two parts of the tape with modified tape ()
        append(START_OF_TAPE, MODIFIED_TAPE, NEW_TAPE),
        % print out
        writeLine(NEW_TAPE),
        isnotfinal(NewTapeState),
        ( 
            % in case there are no final states continues calculation
            execute_turing(NEW_TAPE, RULES) 
        ); 
        ( 
            !
        )
    );
    (
        writeln('Failed to find the corresponding rule for given TAPE'), !
    ).

% load the rules consisting of states and symbols
remove_spaces([], []).
remove_spaces([H|T], LoadedRules) :- 
    [StateA, _ , SymbolA, _, StateB, _, SymbolB] = H,
    (
          remove_spaces(T, Res1),
          LoadedRules = [[StateA, SymbolA, StateB, SymbolB] |Res1]
    ).


% main 
start :-
    prompt(_, ''),
    % geting input
    read_lines(INPUT),
    % remove last line from input - RULESTMP
    remove_last_line(INPUT, RULESTMP),
    % remove spaces bwtween characters in the specified rules 
    remove_spaces(RULESTMP, RULES),
    % extract last line from input - TAPE
    extract_last_line(INPUT, TAPE_NO_S),
    % Add starting symbol to tape
    append(['S'], TAPE_NO_S, TAPE),
    % execute the turing machine on top of Tape with given Loaded Rules
    execute_turing(TAPE, RULES),
    halt.