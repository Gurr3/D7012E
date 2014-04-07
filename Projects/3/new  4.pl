/* solvefgb(+State, +Dest, +N, -Trace):-
state: [[fox, west][goose,west][beans,west][boat,west]]
dest : west
N    : 9
Trace: [Newstate | Oldstates]
*/

run1(T):-
solvefgb((west,west,west,west),east,6,T). %FAILS

run2(T):-
solvefgb((west,west,west,west),east,7,T). %succeeds TWICE

run3(T):-
solvefgb((east,east,east,east),west,9,T). %succeeds, but gets (...,...) ?

solvefgb((A,A,A,A), A, _ , []). %base case
%state: fox, goose, beans, Boat
solvefgb((Fox,Goose,Beans,Boat), Dest, N, [(Changer,BoatAfter) | Trace]):- %add Changer and direction to Trace
	not(state(Fox,Goose,Beans,Boat)), %not in an illegal state
	N > 0, %not to deep recursion
	move(Fox,Goose,Beans,Boat,FoxAfter,GooseAfter,BeansAfter,BoatAfter,Changer),
	solvefgb((FoxAfter,GooseAfter,BeansAfter,BoatAfter),Dest,N -1,Trace). %recursion call

%possible moves
move(Fox,Goose,Beans,Boat,FoxAfter,GooseAfter,BeansAfter,BoatAfter,Changer):- %all possible legal moves
	different(Boat, BoatAfter),
	different(Fox, FoxAfter),
	same(Goose,GooseAfter),
	same(Beans,BeansAfter),
	change(Changer, fox)
	;
	different(Boat, BoatAfter),
	different(Goose, GooseAfter),
	same(Fox,FoxAfter),
	same(Beans,BeansAfter),
	change(Changer, goose)
	;
	different(Boat, BoatAfter),
	different(Beans, BeansAfter),
	same(Fox,FoxAfter),
	same(Goose,GooseAfter),
	change(Changer, beans)
	;
	different(Boat, BoatAfter),
	same(Fox,FoxAfter),
	same(Goose,GooseAfter),
	same(Beans,BeansAfter),
	change(Changer, boat)
	.

%set Changer to the object that changes side value for Trace
change(fox, fox).
change(goose,goose).
change(beans,beans).
change(nothing,boat).

%make sure stuff are on the same side
same(west,west).
same(east,east).

%make sure A and B are different in the illegal state prat
different(west,east).
different(east,west).

%illegal states
state(A,A,_,B):-
	different(A,B).
state(_,A,A,B):-
	different(A,B).