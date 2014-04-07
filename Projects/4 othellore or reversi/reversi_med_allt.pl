/*TEST Start                                                                    */

/*teststraight
1
2
3
4       w b
5       b w
6
7
8
  a b c d e f g h
*/
board(board1, [(white, d, 4),(white, e, 5), (black, e, 4), (black, d, 5)]).
/*testdiag
1
2
3     w   w
4       b
5     w   w
6
7
8
  a b c d e f g h
*/
board(board2, [(black, d, 4),(white, c, 5), (white, c, 3), (white, e, 3), (white, e, 5)]).
/* testflip
1
2
3
4       b b b w
5
6
7
8
  a b c d e f g h
*/
board(board3, [(black, d, 4),(black, e, 4), (black, f, 4), (white, g, 4)]).
/* testflip
1
2     w   w   w
3       b b b 
4     w b * b w
5       b b b 
6     w   w   w
7
8
  a b c d e f g h
*/
board(board4, 
[(white, c, 2), (white, e, 2), (white, g, 2),
 (black, d, 3), (black, e, 3), (black, f, 3),
 (white, c, 4), (black, d, 4), (black, f, 4), (white,g, 4),
 (black, d, 5), (black, e, 5), (black, f, 5),
 (white, c, 6), (white, e, 6), (white, g, 6)
 ]).
 
/* testvalueof , B = 10,W =4
1 b b b
2
3
4
5         w
6
7
8 b             w
  a b c d e f g h
*/
board(board5,[(white, h, 8), (white, e, 5),(black, a, 1),(black, b, 1),(black, c, 1),(black, a, 8)]).
/* testflip
1
2     w   w   w
3       b b b 
4     w b * b w
5       b b b 
6   b w   w   w
7
8
  a b c d e f g h
*/
board(board6, 
[(white, c, 2), (white, e, 2), (white, g, 2),
 (black, d, 3), (black, e, 3), (black, f, 3),
 (white, c, 4), (black, d, 4), (black, f, 4), (white,g, 4),
 (black, d, 5), (black, e, 5), (black, f, 5),
 (black, b, 6), (white, c, 6), (white, e, 6), (white, g, 6)
 ]).

/*
   a b c d e f g h
 1|x| |o| | | |o| |1
 2|x|x|o| |o|o| | |2
 3|x|x|o| |o| | | |3
 4|x|x|o|o|o| | | |4
 5|x|x|o|o|o| | | |5
 6|x|x|o| |o| | | |6
 7|x| | | | | | | |7
 8| | | | | | | | |8
*/
board(board7, 
[(black, a, 1), (white, c, 1), (white, g, 1),
 (black, a, 2), (black, b, 2), (white, c, 2), (white, e, 2), (white, f, 2),
 (black, a, 3), (black, b, 3), (white, c, 3), (white, e, 3),
 (black, a, 4), (black, b, 4), (white, c, 4), (white, d, 4), (white, e, 4),
 (black, a, 5), (black, b, 5), (white, c, 5), (white, d, 5), (white, e, 5),
 (black, a, 6), (black, b, 6), (white, c, 6), (white, e, 6),
 (black, a, 7)
 ]).

	/*test of legalmove*/
testLMstr(L) :- findall((Color,X,Y),testLM(X,Y,board1,Color),L).
testLMdiag(L) :- findall((Color,X,Y),testLM(X,Y,board2,Color),L).
	%call testLM with testLMstr or testLMdiag
	testLM(X,Y,Board,Color) :- 
		board(Board,Board1),
		legalmove(Color,Board1, X, Y).
	/*test of makemove*/
testMM1(N) :- testMM(white,c,4, board3,N).
testMM2(N) :- testMM(black,X,Y, board2,N);testMM(white,X,Y, board2,N).
testMM3(N) :- testMM(white,_,_, board6,N).
	%call testMM with testMM1 or testMM2
	testMM(Color,X,Y,Board,N) :- 
		board(Board,Board1),
		makemove(Color, Board1, X,Y,N).
	/*test of makemoves*/
testMMs1(N,M,NB) :- testMMs(black, board1, N, M, NB).
testMMs2(N,M,NB) :- testMMs(white, board6, N, M, NB).
	testMMs(Color, Board, N, Moves, NewBoard):-
		board(Board,Board1),
		makemoves(Color, Board1, N, Moves, NewBoard).
	/*test of valueof*/
testVO1(PointB,PointW) :- testVO(black, board5,PointB), testVO(white,board5,PointW).
	testVO(Color,Board,Points):-
		board(Board,Board1),
		valueof(Color, Board1, Points).
	/*test of findbestmove*/
testFBM1(N,X,Y):- testFBM(black,board1,N,X,Y).
testFBM2(N,X,Y):- testFBM(black,board2,N,X,Y).
testFBM3(N,X,Y):- testFBM(black,board3,N,X,Y).
testFBM4(N,X,Y):- testFBM(black,board4,N,X,Y).
testFBM5(N,X,Y):- testFBM(black,board5,N,X,Y).
testFBM6(N,X,Y):- testFBM(white,board6,N,X,Y).
testFBM7(X,Y):- testFBM(white,board7,3,X,Y).
testFBM8(X,Y):- testFBM(black,board7,2,X,Y).
	testFBM(Color,Board,N,X,Y):-
		board(Board,Board1),
		findbestmove(Color, Board1, N, X, Y).
/*TEST End                                                                      */


	/*legalmove(+Color,+Board,X,Y) , X and Y can be either input or output*/
legalmove(Color, Board, X, Y):-
	dolegalmove(Color, Board, X, Y,_).

%checks if a piece is already in the specified place, call with not().
%	alreadythere([(_, X, Y) | _], X, Y).
%	alreadythere([_ | Tail], X, Y):-
%		alreadythere(Tail,X,Y).
	
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
	getnext(X,Xright),						%getnext spot
	member((NotColor,Xright,Y),Board),		%the brick exists
	xrightline(NotColor,Board,Xright,Y,Trace)%call the check function for right line
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
		member((Color,Xright,Y),Board),			%the brick is part of the board
		xrightline(Color,Board,Xright,Y, Trace),! %	continue recursion
		;
		getemptylist(Trace), %bind trace
		differentColor(Color,NotColor), %get othercolor
		member((NotColor,Xright,Y),Board)%	the brick exists, therefore done
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
	
	%subtract(+Set, +Delete, -Result)
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
	makemove(Color,Board,X,Y,NewB),
	makemoves(NotColor, NewB, N2, Moves, NewBoard),!
.
makemoves(Color, Board, N, [(Color,n,n) | Moves], NewBoard):-
	N>0,
	N2 is N-1,
	differentColor(Color,NotColor),
	not(legalmove(Color, Board, _, _)),
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
							ResultPoint is Points - BadPoints,
							Result=(Move,ResultPoint)
						),ListofMoves),
	max(ListofMoves, Best),
	member(((_,X,Y),Best),ListofMoves)
.

max( [(_,Best)], Best).
max( [(_,Point) | Points], Point):- max(Points,Best), Point>=Best.
max( [(_,Point) | Points],  Best):- max(Points,Best), Point< Best.



/*		while there is still moves in the list
			see which move may result in the biggest point gain of the opponent, destroy this
			see which of the moves results in the biggest gain of me, keep this */ 


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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

board([(white, d,4),(black, e,4),(black, d,5),(white, e,5)]).
start(B):-board(B),printBoard(B),compvsrand(B,10),!.
compvscomp(Board):-
       findbestmove(black, Board, 4, Xa, Ya),!,
       writef('Black: X:%n, ',[Xa]),!,
       writef('Y:%q\n\n',[Ya]),!,
       makemove(black, Board, Xa, Ya, NewBoard),!,
       printBoard(NewBoard),!,
       findbestmove(white, NewBoard, 3, Xbest, Ybest),!,
       writef('White: X:%n, ',[Xbest]),!,
       writef('Y:%q\n\n',[Ybest]),!,
       makemove(white, NewBoard, Xbest, Ybest, DatBoard),!,
       printBoard(DatBoard),!,
       (       (
                       length(DatBoard,64),
                       write('length64\n')
               );(
                       Xa==Ya, Ya==Xbest, Xbest==Ybest,
                       write('loadsofn\n')
               );(
                       compvscomp(DatBoard)
       )       ).
compvsrand(Board,0):-
       compvscomp(Board),!.
compvsrand(Board,N):-
       printBoard(Board),!,
       findbestmove(black, Board, 2, Xa, Ya),!,
       writef('Black: X:%n, ',[Xa]),!,
       writef('Y:%q\n\n',[Ya]),!,
       makemove(black, Board, Xa, Ya, NewBoard),!,
       printBoard(NewBoard),!,
       randmove(white, NewBoard, Xrand, Yrand),!,
       writef('White: X:%n, ',[Xrand]),!,
       writef('Y:%q\n\n',[Yrand]),!,
       makemove(white, NewBoard, Xrand, Yrand, DatBoard),!,
       New is N-1,!,
       compvsrand(DatBoard,New),!.
youvscomp(Board):-
       printBoard(Board),
       get(X),
       writef('You: X:%n, ',[X]),
       string_to_atom([X],Xa),
       get(Y),
       writef('Y:%n\n\n',[Y]),
       string_to_atom([Y],Yn),
       atom_number(Yn, Ya),
       legalmove(black, Board, Xa, Ya),
       makemove(black, Board, Xa, Ya, NewBoard),
       printBoard(NewBoard),
       findbestmove(white, NewBoard, 3, Xbest, Ybest),
       writef('Com: X:%n, ',[Xbest]),
       writef('Y:%q\n\n',[Ybest]),
       makemove(white, NewBoard, Xbest, Ybest, DatBoard),
       youvscomp(DatBoard).

 /*
       Random legal move..
 */
randmove(Color, Board, X, Y):- %redo, make it do bagof legalmove and then make random of them, if it fails, make it so it plays n n
       repeat, random(1,9,Xi), random(1,9, Y),
       xInt(X,Xi),
       legalmove(Color,Board,X,Y),!.

 /*
 Takes the list of a board and constructs
 a list of a elements for all coordinates.
 */
buildcomplete(_,[],[]).
buildcomplete(Board,[(X,Y)|SB],[x|Map]):-
      member((black,X,Y),Board),
      buildcomplete(Board,SB,Map).
buildcomplete(Board,[(X,Y)|SB],[o|Map]):-
      member((white,X,Y),Board),
      buildcomplete(Board,SB,Map).
buildcomplete(Board,[_|SB],[' '|Map]):-
      buildcomplete(Board,SB,Map).

 /*
 Used to.. you guessed it! Print the board!
 */
printBoard(Board):-
       writef('   %n %n %n %n %n %n %n %n\n',[a,b,c,d,e,f,g,h]),!,
       buildcomplete(Board,[(a,1),(b,1),(c,1),(d,1),(e,1),(f,1),(g,1),(h,1)],Map1),
       writef(' 1|%n|%n|%n|%n|%n|%n|%n|%n|1\n',Map1),
       buildcomplete(Board,[(a,2),(b,2),(c,2),(d,2),(e,2),(f,2),(g,2),(h,2)],Map2),
       writef(' 2|%n|%n|%n|%n|%n|%n|%n|%n|2\n',Map2),
       buildcomplete(Board,[(a,3),(b,3),(c,3),(d,3),(e,3),(f,3),(g,3),(h,3)],Map3),
       writef(' 3|%n|%n|%n|%n|%n|%n|%n|%n|3\n',Map3),
       buildcomplete(Board,[(a,4),(b,4),(c,4),(d,4),(e,4),(f,4),(g,4),(h,4)],Map4),
       writef(' 4|%n|%n|%n|%n|%n|%n|%n|%n|4\t',Map4),
       valueof(black,Board,V),
       writef('Value: %q\n',[V]),
       buildcomplete(Board,[(a,5),(b,5),(c,5),(d,5),(e,5),(f,5),(g,5),(h,5)],Map5),
       writef(' 5|%n|%n|%n|%n|%n|%n|%n|%n|5\t',Map5),
       %scoreof(black,Board,S),
       writef('Score: %q\n',[0]),
       buildcomplete(Board,[(a,6),(b,6),(c,6),(d,6),(e,6),(f,6),(g,6),(h,6)],Map6),
       writef(' 6|%n|%n|%n|%n|%n|%n|%n|%n|6\n',Map6),
       buildcomplete(Board,[(a,7),(b,7),(c,7),(d,7),(e,7),(f,7),(g,7),(h,7)],Map7),
       writef(' 7|%n|%n|%n|%n|%n|%n|%n|%n|7\n',Map7),
       buildcomplete(Board,[(a,8),(b,8),(c,8),(d,8),(e,8),(f,8),(g,8),(h,8)],Map8),
       writef(' 8|%n|%n|%n|%n|%n|%n|%n|%n|8\n',Map8),
       writef('   %n %n %n %n %n %n %n %n\n\n',[a,b,c,d,e,f,g,h]).

xInt(1,a).
xInt(2,b).
xInt(3,c).
xInt(4,d).
xInt(5,e).
xInt(6,f).
xInt(7,g).
xInt(8,h).

xInt(a,1).
xInt(b,2).
xInt(c,3).
xInt(d,4).
xInt(e,5).
xInt(f,6).
xInt(g,7).
xInt(h,8).
