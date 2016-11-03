:- dynamic board/1.
:- dynamic turnColor/1.
:- dynamic player/5.

board(
	[
	[empty, empty, empty, empty], 
	[empty, empty, empty, empty, empty],
	[empty, empty, empty, empty, empty, empty],
	[empty, [w, 0, 0], empty, empty, empty, [b, 0, 0], empty],
	[empty, empty, empty, empty, empty, empty],
	[empty, empty, empty, empty, empty],
	[empty, empty, empty, empty] 
	]).
% player(color, adaptoids, legs, pincers, score)
player(w, 12, 12, 12, 0).
player(b, 12, 12, 12, 0).
		
		
testPrintBoard(1) :- board(X), printBoard(X).

printBoard(B) :- 
	printBlanckChars(15), 
	printTopBoard(4), 
	nl,
	printBoardRows(B, 1), 
	printBlanckChars(14), 
	printBottomBoard(4).
					
printTopBoard(0).
printTopBoard(N) :-  
	N > 0,
	NumColumn is 5 - N,
	write('  / \\ c'), write(NumColumn),
	N1 is N-1, 
	printTopBoard(N1).

printBottomBoard(0).
printBottomBoard(N) :- 
	N > 0, 
	write('   \\ /  '), 
	N1 is N-1, 
	printBottomBoard(N1).

printBoardRows([], _).
printBoardRows([R|Rs], NumRow) :- 
	printRow(R, NumRow),
	nl,
	N1 is NumRow + 1,
	printBoardRows(Rs, N1).
									
printRow(R, NumRow) :- 
	NumRow < 4,
	Nspaces is (4 - NumRow) * 4 + 2,
	printBlanckChars(Nspaces),
	printTopCells(R, NumRow),
	nl,
	N2spaces is (4 - NumRow) * 4,
	printBlanckChars(N2spaces), 
	write('r'), write(NumRow), write('|'),
	printMidCells(R), 
	nl,
	N3spaces is (4 - NumRow - 1) * 4 + 2,
	printBlanckChars(N3spaces), 
	write('   / \\ '),
	printDownCells(R, NumRow),
	NumColumn is 5 + NumRow - 1,
	write('c'), write(NumColumn).
printRow(R, NumRow) :- 
	NumRow =:= 4,
	printBlanckChars(2), printTopCells(R, NumRow),
	nl,
	write('r'), write(NumRow), write('|'),
	printMidCells(R),
	nl,
	printBlanckChars(2), printDownCells(R, NumRow).
printRow(R, NumRow) :- 
	NumRow > 4,
	Nspaces is (NumRow - 5) * 4 + 2,
	printBlanckChars(Nspaces),
	write('   \\ / '),
	printTopCells(R, NumRow),
	nl,
	Nspaces2 is (NumRow - 4) * 4,
	printBlanckChars(Nspaces2),
	write('r'), write(NumRow), write('|'),
	printMidCells(R),
	nl,
	Nspaces3 is Nspaces2 + 2, 
	printBlanckChars(Nspaces3), 
	printDownCells(R, NumRow).
					
printTopCells([], _).							
printTopCells([Cell|Cells], NumRow) :- 
	NumRow < 4,
	write(' / '), 
	printTopCell(Cell),
	write(' \\'), 
	printTopCells(Cells, NumRow).											
printTopCells([Cell|Cells], NumRow) :- 
	NumRow =:= 4,
	write(' / '),
	printTopCell(Cell),
	write(' \\'),
	printTopCells(Cells, NumRow).										
printTopCells([Cell|Cells], NumRow) :- 
	NumRow > 4,
	printTopCell(Cell),
	write(' \\ / '),
	printTopCells(Cells, NumRow).
									
printMidCells([]).										
printMidCells([Cell|Cells]) :- 
	write('  '),
	printMidCell(Cell),
	write('  |'), 
	printMidCells(Cells).
					
					
printDownCells([], _).
printDownCells([Cell|Cells], NumRow) :- 
	NumRow < 4,
	printDownCell(Cell),
	write(' / \\ '), 
	printDownCells(Cells, NumRow).																
printDownCells([Cell|Cells], NumRow) :- 
	NumRow =:= 4,
	write(' \\ '),
	printDownCell(Cell),
	write(' /'),
	printDownCells(Cells, NumRow).										
printDownCells([Cell|Cells], NumRow) :- 
	NumRow > 4,
	write(' \\ '),
	printDownCell(Cell),
	write(' /'),
	printDownCells(Cells, NumRow).
												
printTopCell(empty) :- write('   ').
printTopCell([_, Legs, _]) :-
	write('L='), write(Legs). 

printMidCell(empty) :- write('   ').
printMidCell([Color, _, _]) :- 
	write(' '), 
	write(Color),
	write(' ').
																
printDownCell(empty) :- write('   ').
printDownCell([_, _, Pencils]) :- 
	write('P='), write(Pencils).
	
printBlanckChars(0).
printBlanckChars(N) :- 
	N > 0, 
	write(' '), 
	N1 is N-1, 
	printBlanckChars(N1).
	
% logicToCliCoords(LogicRow, LogicCol, CliRow, CliCol)
logicToCliCoords(R, C, R, C) :- R =< 4.
logicToCliCoords(R, C, R, CliC) :-
	R > 4,
	CliC is C + R - 4.
	
% CliToLogicCoords(CliRow, CliCol, LogicRow, LogicCol)
cliToLogicCoords(R, C, R, C) :- R =< 4.
cliToLogicCoords(R, C, R, LogC) :-
	R > 4,
	LogC is C - (R - 4).
    
testEnd :- 
    player(w, _, _, _, Score),
    Score > 4.
testEnd :- 
    player(b, _, _, _, Score),
    Score > 4.
testEnd:-
    board(Board),
    findall([R,C], getPiece(R,C,Board,[w|_]), Pieces),
    Pieces = [].
testEnd:-
    board(Board),
    findall([R,C], getPiece(R,C,Board,[b|_]), Pieces),
    Pieces = [].
    
showResults:- 
    player(w, _, _, _, WhiteScore),
    player(b, _, _, _, BlackScore),
    write('White '), write(WhiteScore), write(' - '), write(BlackScore), write(' Black'), nl,
    writeWhoWon(WhiteScore,BlackScore).
    
writeWhoWon(WhiteScore,BlackScore):-
    WhiteScore > BlackScore, !,
    write('White won!!!'), nl .
writeWhoWon(WhiteScore,BlackScore):-
    WhiteScore < BlackScore, !,
    write('Black won!!!'), nl .
%If captures are equal the last to move wins
writeWhoWon(_,_):-
    turnColor(b),
    write('White won!!!'), nl .
writeWhoWon(_,_):-
    turnColor(w),
    write('Black won!!!'), nl .  
    
% game(Type)
game(Type):-
    %inicializações
    assert(turnColor(w)),
    %ciclo de jogo
    repeat,
        turnColor(ColorIn),
        once(play(Type, ColorIn, ColorOut)),
        retract(turnColor(ColorIn)),
        assert(turnColor(ColorOut)),
        testEnd,
    showResults.

% joga(Type, ColorIn, ColorOut)
play(hh, w, b):-
    board(BoardIn),
    player(w, Adaptoids, Legs, Pincers, Score),
    %jogada
    nl,
    write('White playing'),
    nl,
    printBoard(BoardIn),
    nl,
    userMoveAndCapture(w, BoardIn, Board1),
    printBoard(Board1),
    nl,
    write('Enter option: 1-create new 2-add pincer 3-add leg'),
    read(Option),
    userCreateOrUpdate(Option, w, Board1, Board2),
    printBoard(Board2),
    nl,
    captureAdaptoids(b, Board2, BoardOut),
    %Update board
    retract(board(BoardIn)),
    assert(board(BoardOut)).
   
play(hh, b, w):-
    board(BoardIn),
    player(w, Adaptoids, Legs, Pincers, Score),
    %jogada
    nl,
    write('Black playing'),
    nl,
    printBoard(BoardIn),
    nl,
    userMoveAndCapture(b, BoardIn, Board1),
    printBoard(Board1),
    nl,
    write('Enter option: 1-create new 2-add pincer 3-add leg'),
    read(Option),
    userCreateOrUpdate(Option, b, Board1, Board2),
    printBoard(Board2),
    nl,
    captureAdaptoids(w, Board2, BoardOut),
    %Update board
    retract(board(BoardIn)),
    assert(board(BoardOut)).
    
userMoveAndCapture(Color, BoardIn, BoardIn):-
    findall(Legs, getPiece(_R,_C,BoardIn,[Color,Legs,_]), Pieces),
    checkIfNoLegs(Pieces), !.
    

userMoveAndCapture(Color, BoardIn, BoardOut):-
    write('Enter Coordinates of adaptoid to move: '),
    read(UserRowFrom), read(UserColFrom),
    cliToLogicCoords(UserRowFrom, UserColFrom, RowFrom, ColFrom),
    write('Enter Coordinates of destination: '),
    read(UserRowTo), read(UserColTo),
    cliToLogicCoords(UserRowTo, UserColTo, RowTo, ColTo),    
    moveAndCapture(Color,RowFrom,ColFrom,RowTo,ColTo,BoardIn,BoardOut).

checkIfNoLegs([]).    
checkIfNoLegs([L|Legs]):-
    L is 0,
    checkIfNoLegs(Legs).
    
userCreateOrUpdate(1, Color, BoardIn, BoardOut):-
    write('Enter Coordinates of new adaptoid: '),
    read(UserRow), read(UserCol),
    cliToLogicCoords(UserRow, UserCol, Row, Col),
    createAdaptoid(Color, Row, Col, BoardIn, BoardOut).
    
userCreateOrUpdate(2, Color, BoardIn, BoardOut):-
    write('Enter Coordinates of adaptoid: '),
    read(UserRow), read(UserCol),
    cliToLogicCoords(UserRow, UserCol, Row, Col),
    addPincer(Color, Row, Col, BoardIn, BoardOut).
    
userCreateOrUpdate(3, Color, BoardIn, BoardOut):-
    write('Enter Coordinates of adaptoid: '),
    read(UserRow), read(UserCol),
    cliToLogicCoords(UserRow, UserCol, Row, Col),
    addLeg(Color, Row, Col, BoardIn, BoardOut).
	


