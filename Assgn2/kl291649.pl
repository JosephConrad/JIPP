% Konrad Lisiecki 291649 
:- ensure_loaded(library(lists)).
:- op(500, xfx, <>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                      Glowna funkcja i czytywanie danych
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
user:runtime_entry(start) :- 
	current_prolog_flag(argv, [N, File]) ->
		main(N, File)
	;
		write('Niepoprawne wywolanie, usage: verify <N> <program>').

main(N, File) :-
	(checkProcNumber(N, L) ->
		readFile(File, Vars, program(Stmt)), 
		add(0, Stmt, Stmts),
		Program = program(Stmts),
		init(Vars, Program, State, L), 
		findAllStates(Program, [State], [], StatesPom), 
		distinct(StatesPom, States),  
		length(States, NumStates),  
		critSec(Program, 0, [], CritSec),
		czy2(CritSec, States, [], StateInvalid), 
		info(CritSec, NumStates, StateInvalid)
	;
		write('Parametr '), write(N), write(' powinien byc liczba > 0')).


checkProcNumber(N, L) :-
	atom_codes(N, Chars),
	number_codes(L, Chars),
	L > 0.


readFile(File, Term, Program) :-
	set_prolog_flag(fileerrors, off),
	see(File),
	read(Term),
	read(Program),
	seen.

readFile(File, _, _) :-
         format('Error: niepoprawna nazwa pliku - ~p.', [File]), halt.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                           Szukanie przeplotu
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
critSec(program([]), _, CritSec, CritSec) :- !.
critSec(program([H|T]), N, Acc, CritSec) :-
	M is N+1, 
	(H = sekcja ->
		add(N, Acc, Acc1),
		critSec(program(T), M, Acc1, CritSec)
	;
		critSec(program(T), M, Acc, CritSec)).


czy2(_, [], Res, Res) :- !.
czy2(CritSec, States, Acc, Res) :-
	States = [Stan|T],
	Stan = state(_, _, _, _, LicznikiRozkazowOut, _),
	inSection(CritSec, LicznikiRozkazowOut, 0, N),
	(N == 2 -> 
		czy2(CritSec, [], [Stan], Res)
	;
		czy2(CritSec, T, Acc, Res)). 


inSection([], _, Wynik, Wynik) :- !.
inSection(L1, L2, Acc, Wynik) :-
	L1 = [H|T],
	count(L2, H, N),
	Acc1 is Acc + N,
	inSection(T, L2, Acc1, Wynik). 


count([],_,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X,count(T,X,Z).
 

info(_, N, []) :- 
	format('Liczba stanow: ~d.~n', N), 
	print('Program jest poprawny (bezpieczny).').
info(CritSec, N, StateInvalid) :-
	format('Liczba stanow: ~d.~n', N), 
	format('Program jest niepoprawny: ~n', []),
	StateInvalid = [state(_, _, _, _, LicznikiRozkazow, Hist)],
	reverse(Hist, History),
	printStates(History),
	inSec(CritSec, LicznikiRozkazow, 0, [], WSekcji), 
	print('Procesy w sekcji: '),
	printtab(WSekcji), nl .


inSec(_, [], _, WSekcji, WSekcji).
inSec(CritSec, LicznikiRozkazow, X, Acc, WSekcji) :-
	LicznikiRozkazow = [Licznik | T], 
	XNext is X + 1,
	(member(Licznik, CritSec) ->
		append([X], Acc, Acc1),
		inSec(CritSec, T, XNext, Acc1, WSekcji)
	;
		inSec(CritSec, T, XNext, Acc, WSekcji)). 


printStates([]).
printStates([move(Process, Stmt)|T]) :- 
	format('Proces ~d: ~d ~n', [Process, Stmt]),  
	printStates(T). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                               Inicjalizacja
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(vars(SV, TV), _, state(SV, TV, SVV, TVV, InstrPntr, []), L) :-
	length(SV, SVL),
	length(TV, TVL),
	initList(TVL, TVV, L),
	initTable(SVL, SVV), 
	initTable1(L, InstrPntr).

initList(0, [], _) :- !.
initList(N, [Tab|Rest], L) :- 
	M is N-1, M >=0, 
	initTable(L, Tab), 
	initList(M, Rest, L).

initTable1(0, []) :- !.
initTable1(N, [1|Tab]) :- M is N-1, M >= 0, initTable1(M, Tab).

initTable(0, []) :- !.
initTable(N, [0|Tab]) :- M is N-1, M >= 0, initTable(M, Tab).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         Znajdowanie wszystkich stanow	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
findAllStates(_, [], _, []).
findAllStates(Program, NotVisited, Visited, StatesOut) :-
	NotVisited = [StateIn|T], 
	(\+ member1(StateIn, Visited) ->  
		steps(Program, StateIn, States),
		add(StateIn, Visited, Visited1),
		append(States, T, NotVisited1),
		findAllStates(Program, NotVisited1, Visited1, StatesMid), 
		append(States, StatesMid, StatesOut)
	;  
		findAllStates(Program, T, Visited, StatesOut)).

steps(Program, StateIn, States) :- 
	StateIn = state(_, _, _, _, LR, _), 
	length(LR, L),  
	LPrev is L - 1,
	steps1(Program, StateIn, States, LPrev).

steps1(_, _, [], -1). 
steps1(Program, StateIn, States, L) :- 
	L > -1, 
	LPrev is L - 1,
	step(Program, StateIn, L, State),
	steps1(Program, StateIn, StateOut, LPrev),
	add(State, StateOut, States).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                  Step	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
step(program(Stmts), State, PrId, StateOut) :-
	State = state(_, _, _, _, LR, _),
	elt(PrId, LR, InstNo),
	elt(InstNo, Stmts, Inst),
	perform(Inst, InstNo, NextInstNo, State, StateMid, PrId),
	replace(LR, PrId, NextInstNo, LRNew),
	StateMid = state(SV, TV, SVV, TVV, _, HistoriaInst), 
	NewStep = move(PrId, InstNo), 
	add(NewStep, HistoriaInst, NewHistoriaInstr),
	StateOut = state(SV, TV, SVV, TVV, LRNew, NewHistoriaInstr).


perform(condGoto(LogExp, NextInst), InstNo, NextInstNo, State, StateOut, PrId):-
	StateOut = State,
	evalLog(LogExp, Result, PrId, State),
	(Result = true->
		NextInstNo = NextInst
	;
		NextInstNo is InstNo + 1).

perform(assign(Var, Exp), InstNo, NextInstNo, State, StateOut, PrId) :-
	State = state(SV, TV, SVV, TVV, IP, HistoriaInstr),
	NextInstNo is InstNo + 1,
	(member(Var, SV) ->
		eval(Exp, Result, PrId, State), 	  % znajdz ELEMENT TABLICY do zmiany
		indexOf(SV, Var, Index), 			  % znajdz index zmiennej Var
		replace(SVV, Index, Result, SVVOut),
		StateOut = state(SV, TV, SVVOut, TVV, IP, HistoriaInstr)
	;
		Var = arr(Ident, Exp1),
		eval(Exp, Result, PrId, State), 	  % wyliczenie NOWEJ wartości danej 
		eval(Exp1, TabElt, PrId, State), 	  % znajdz ELEMENT TABLICY do zmiany
		indexOf(TV, Ident, Tab),
		rep(TVV, Tab, TabElt,Result, TVVOut),
		StateOut = state(SV, TV, SVV, TVVOut, IP, HistoriaInstr)).

perform(sekcja, InstNo, NextInstNo, State, State, _) :- 
	NextInstNo is InstNo + 1.

perform(goto(NextInstNo), _, NextInstNo, State, State, _).


evalLog(A<B, CV, PrId, State) :- eval(A,AV,PrId,State), eval(B,BV,PrId,State),
		(AV < BV  ->  CV = true ; CV = false).
evalLog(A>B, CV, PrId, State) :- eval(A,AV,PrId,State), eval(B,BV,PrId,State),
		(AV > BV  ->  CV = true ; CV = false).
evalLog(A=B, CV, PrId, State) :- eval(A,AV,PrId,State), eval(B,BV,PrId,State), 
		(AV = BV  ->  CV = true ; CV = false).
evalLog(A<>B, CV, PrId, State) :- eval(A,AV,PrId,State), eval(B,BV,PrId,State), 
		(AV \= BV  ->  CV = true ; CV = false).

% Funkcja zamieniajaca element w tablicy
rep(TVV, TabNo, TabElt,Result, TVVOut) :-
	elt(TabNo, TVV, Tab),
	replace(Tab, TabElt, Result, TabUpdated),
	replace(TVV, TabNo, TabUpdated, TVVOut).


evalArExp(Exp, PrId, Res) :- eval(Exp, Res, PrId). 


eval(A+B,CV, PrId, State) :- eval(A,AV, PrId, State), eval(B,BV, PrId, State), 
	CV is AV+BV.
eval(A-B,CV, PrId, State) :- eval(A,AV, PrId, State), eval(B,BV, PrId, State), 
	CV is AV-BV.
eval(A*B,CV, PrId, State) :- eval(A,AV, PrId, State), eval(B,BV, PrId, State), 
	CV is AV*BV.
eval(A/B,CV, PrId, State) :- eval(A,AV, PrId, State), eval(B,BV, PrId, State), 
	CV is AV/BV.
eval(Exp,CV, PrId, State) :-
	State = state(SV, TV, SVV, TVV, _, _),
	(member(Exp, SV) ->
		indexOf(SV, Exp, Index), 	
		elt(Index, SVV, CV)		
	; Exp = id ->
	 	CV = PrId
	; number(Exp) ->
		CV = Exp
	;	
		Exp = arr(Name, Ind),
		eval(Ind, IndexInTab, PrId, State), 
		indexOf(TV, Name, IndexOfTab),
		elt(IndexOfTab, TVV, Tab),
		elt(IndexInTab, Tab, CV)).


replace([_|T], 0, X, [X|T]) :- !.
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).


indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
indexOf(Tail, Element, Index1), !, Index is Index1+1.  
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                      Wypisywanie i dodatkowe funkcje	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
printtab([H]) :- write(H), print('.').
printtab([H|T]) :- print(H), print(', '), printtab(T).  

add(X,List,[X|List]).

elt(0, [H|_], H).
elt(N, [_|T], Elt) :- M is N - 1, M >= 0, elt(M, T, Elt).

emptyTab([]).

genPids(0, []).
genPids(N, [M|T]) :- M is N-1, M >=0, genPids(M, T). 

distinct([],[]).
distinct([H|T],C) :- member1(H,T),!, distinct(T,C).
distinct([H|T],[H|C]) :- distinct(T,C).

member1(state(A, B, X, Y, Z, _), [state(A, B, X, Y, Z, _)|_]).
member1(X,[_|T]) :- member1(X,T).  