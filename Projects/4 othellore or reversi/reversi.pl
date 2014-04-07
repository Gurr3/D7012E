/* 			Reversi / Othello 
	Author: William Gustafsson
			gurr72@gmail.com
*/


	/*legalmove(+Color,+Board,X,Y) , X and Y can be either input or output*/
legalmove(Color, Board, X, Y):- %wrapper to match the given specifications
	dolegalmove(Color, Board, X, Y,_).
	
	/*does the legal move according to the specification, with the added functionality of providing a trace, this will be used in makemove*/
dolegalmove(Color, Board, X, Y,Trace):- 
	differentColor(Color,NotColor),
	member(X,[a,b,c,d,e,f,g,h]),
	member(Y,[1,2,3,4,5,6,7,8]),
	%not(alreadythere(Board,X,Y)),
	not(member((_,X,Y),Board)),
	findline(NotColor,Board,X,Y,Trace)
.

findline(NotColor, Board,X,Y,Trace):-
	%	Xright
	getnext(X,Xright),							%getnext spot
	member((NotColor,Xright,Y),Board),			%the brick exists
	xrightline(NotColor,Board,Xright,Y,Trace)	%call the check function for right line
	;
	%	Xleft
	getnext(Xleft,X),
	member((NotColor,Xleft,Y),Board),
	xleftline(NotColor,Board,Xleft,Y,Trace)
	;
	%	Yup
	getnext(Yup,Y),
	member((NotColor,X,Yup),Board),
	yupline(NotColor,Board,X,Yup,Trace)
	;
	% Ydown
	getnext(Y,Ydown),
	member((NotColor,X,Ydown),Board),
	ydownline(NotColor,Board,X,Ydown,Trace)
	;
	%	Xright Yup		>^
	getnext(X,Xright),
	getnext(Yup,Y),
	member((NotColor,Xright,Yup),Board),
	diag1line(NotColor,Board,Xright,Yup,Trace)
	; 
	%	Xright Ydown 	>v
	getnext(X,Xright),
	getnext(Y,Ydown),
	member((NotColor,Xright,Ydown),Board),
	diag2line(NotColor,Board,Xright,Ydown,Trace)
	; 
	%	Xleft Ydown		v<
	getnext(Xleft,X),
	getnext(Y,Ydown),
	member((NotColor,Xleft,Ydown),Board),
	diag3line(NotColor,Board,Xleft,Ydown,Trace)
	;
	%	Xleft Yup		^<
	getnext(Xleft,X),
	getnext(Yup,Y),
	member((NotColor,Xleft,Yup),Board),
	diag4line(NotColor,Board,Xleft,Yup,Trace).

xrightline(Color,Board,X,Y,[ (Color,X,Y)| Trace]):-
	getnext(X,Xright),
	(
		member((Color,Xright,Y),Board),			%the next brick is part of the board
		xrightline(Color,Board,Xright,Y, Trace),! %	continue recursion
		;
		getemptylist(Trace), %bind trace
		differentColor(Color,NotColor), %get othercolor
		member((NotColor,Xright,Y),Board)%	the brick exists, therefore done, recurse back, fill trace.
	)
.
xleftline(Color,Board,X,Y,[ (Color,X,Y)| Trace]):-
	getnext(Xleft,X),
	(
		member((Color,Xleft,Y),Board),
		xleftline(Color,Board,Xleft,Y, Trace),! %	continue recursion
		;
		getemptylist(Trace),
		differentColor(Color,NotColor),
		member((NotColor,Xleft,Y),Board)%	done
	)
.
yupline(Color,Board,X,Y,[ (Color,X,Y)| Trace]):-
	getnext(Yup,Y),
	(
		member((Color,X,Yup),Board),
		yupline(Color,Board,X,Yup, Trace),! %	continue recursion
		;
		getemptylist(Trace),
		differentColor(Color,NotColor),
		member((NotColor,X,Yup),Board)%	done
	)
.
ydownline(Color,Board,X,Y,[ (Color,X,Y)| Trace]):-
	getnext(Y,Ydown),
	(
		member((Color,X,Ydown),Board),
		ydownline(Color,Board,X,Ydown, Trace),! %	continue recursion
		;
		getemptylist(Trace),
		differentColor(Color,NotColor),
		member((NotColor,X,Ydown),Board)%	done
	)
.
diag1line(Color,Board,X,Y,[ (Color,X,Y)| Trace]):- %	Xright Yup		>^
	getnext(X,Xright),
	getnext(Yup,Y),
	(
		member((Color,Xright,Yup),Board),
		diag1line(Color,Board,Xright,Yup, Trace),!
		;
		getemptylist(Trace),
		differentColor(Color,NotColor),
		member((NotColor,Xright,Yup),Board)
	)
.
diag2line(Color,Board,X,Y,[ (Color,X,Y)| Trace]):- %	Xright Ydown 	>v
	getnext(X,Xright),
	getnext(Y,Ydown),
	(
		member((Color,Xright,Ydown),Board),
		diag2line(Color,Board,Xright,Ydown, Trace),!
		;
		getemptylist(Trace),
		differentColor(Color,NotColor),
		member((NotColor,Xright,Ydown),Board)
	)
.
diag3line(Color,Board,X,Y,[ (Color,X,Y)| Trace]):- %	Xleft Ydown		v<
	getnext(Xleft,X),
	getnext(Y,Ydown),
	(
		member((Color,Xleft,Ydown),Board),
		diag3line(Color,Board,Xleft,Ydown, Trace),!
		;
		getemptylist(Trace),
		differentColor(Color,NotColor),
		member((NotColor,Xleft,Ydown),Board)
	)
.
diag4line(Color,Board,X,Y,[ (Color,X,Y)| Trace]):- %	Xleft Yup		^<
	getnext(Xleft,X),
	getnext(Yup,Y),
	(
		member((Color,Xleft,Yup),Board),
		diag4line(Color,Board,Xleft,Yup, Trace),!
		;
		getemptylist(Trace),
		differentColor(Color,NotColor),
		member((NotColor,Xleft,Yup),Board)
	)
.

	/*makemove(+Color, +Board, +X, +Y, -NewBoard)*/
makemove(Color, Board, X, Y, NewBoard):-
	%is X,Y a legal move?
	legalmove(Color, Board, X, Y),
	
	%finds all bricks that will be flipped, 
	findall(Trace, dolegalmove(Color, Board, X, Y, Trace), ToBeFlipped),
	flatten(ToBeFlipped,ToBeFlipped_flattened),
	
	%find all unchanged bricks
	subtract(Board,ToBeFlipped_flattened,Unchanged),
	
	doflip(ToBeFlipped_flattened,Flipped),
	addtolist((Color,X,Y),Flipped,Flipped_input),
	%flattens and merges Flipped and unchanged
	flatten([Flipped_input | Unchanged], NewBoard),!
	.


	/*makemoves(+Color, +Board, +N, -Moves, -NewBoard)*/
makemoves(_, Board, 0, [], Board).
makemoves(Color, Board, N, [(Color,X,Y) | Moves], NewBoard):-
	N>0,
	N2 is N -1,
	differentColor(Color,NotColor),
	legalmove(Color,Board,X,Y),
	makemove(Color,Board,X,Y,NewB),
	makemoves(NotColor, NewB, N2, Moves, NewBoard),!
.
makemoves(Color, Board, N, [(Color,n,n) | Moves], NewBoard):-
	N>0,
	N2 is N-1,
	differentColor(Color,NotColor),
	not(legalmove(Color, Board, _, _)), %this can only happen if not legalmove unbound X,Y
	makemoves(NotColor, Board, N2, Moves, NewBoard)
.

	/*findbestmove(+Color, +Board, +N, -X, -Y)*/
findbestmove(Color, Board, N, X, Y):-

	differentColor(Color,BadColor),
	/*get a list of all moves possible, in the possible depth*/
	findall(Result, 	(
							makemoves(Color, Board, N, [Move|_], NewBoard),
							valueof(Color,NewBoard,Points),
							valueof(BadColor,NewBoard,BadPoints),
							ResultPoint is Points - BadPoints, %decide points with this
							Result=(Move,ResultPoint)
						),ListofMoves),
	max(ListofMoves, Best),
	member(((_,X,Y),Best),ListofMoves)
.

max( [(_,Best)], Best).
max( [(_,Point) | Points], Point):- max(Points,Best), Point>=Best.
max( [(_,Point) | Points],  Best):- max(Points,Best), Point< Best.


	/*valueof(+Color,+Board,-Value)*/
valueof(Color, Board, Points):-
	findall((Color,X,Y), member((Color,X,Y), Board), ColoredList), %only evaluate one of the players

	cornerlist(Cornerlist),
	edgeslist(Edgeslist),

	intersection(Cornerlist,ColoredList,LC),
	intersection(Edgeslist, ColoredList,LE),

	length(LC, LenCorner),
	length(LE, LenEdges),

	subtract(ColoredList, LC, A),
	subtract(A, LE, Lrest),

	length(Lrest, Lenrest),

	Points is (3*LenCorner) + (2*LenEdges) + (Lenrest)   
.

	/*Help functions*/
%for valueof
cornerlist([ (_,a,1),(_,a,8),(_,h,1),(_,h,8) ]).
edgeslist([
		  (_,a,2),(_,a,3),(_,a,4),(_,a,5),(_,a,6),(_,a,7),
		  (_,h,2),(_,h,3),(_,h,4),(_,h,5),(_,h,6),(_,h,7),
		  (_,b,1),(_,c,1),(_,d,1),(_,e,1),(_,f,1),(_,g,1),
		  (_,b,8),(_,c,8),(_,d,8),(_,e,8),(_,f,8),(_,g,8)
		  ]).

%adds A as head of B, can be reversed by changing A and B
addtolist(A,B,[A | B]).

%change the color of the move list
doflip([],[]).
doflip([(Color,X,Y) | Unflipped], [(NotColor,X,Y) | Flipped] ):-
	differentColor(Color,NotColor),
	doflip(Unflipped,Flipped)
.
%what's the other color?
differentColor(white,black).
differentColor(black,white).

getemptylist([]).

%used to get the next in a line
getnext(a,b).
getnext(b,c).
getnext(c,d).
getnext(d,e).
getnext(e,f).
getnext(f,g).
getnext(g,h).

getnext(1,2).
getnext(2,3).
getnext(3,4).
getnext(4,5).
getnext(5,6).
getnext(6,7).
getnext(7,8).