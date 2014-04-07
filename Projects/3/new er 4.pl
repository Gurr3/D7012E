/* solvefgb(+State, +Dest, +N, -Trace):-
state: [[fox, west][goose,west][beans,west][boat,west]]
dest : west
N    : 9
Trace: [Newstate | Oldstates]
*/

run1(T):-
solvefgb((west,west,west,west),east,6,T).

run2(T):-
solvefgb((west,west,west,west),east,7,T).

solvefgb((A,A,A,A), A, _ , []). %base case
%state: fox, goose, beans, Boat
solvefgb((Fox,Goose,Beans,Boat), Dest, N, [(Changer,BoatAfter) | Trace]):- 
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

change(fox, fox).
change(goose,goose).
change(beans,beans).
change(nothing,boat).
%make sure illegalstates A and B are different
different(west,east).
different(east,west).

%make sure stuff are on the same side
same(west,west).
same(east,east).

%illegal states
state(A,A,_,B):-
	different(A,B).
state(_,A,A,B):-
	different(A,B).