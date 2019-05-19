:-include(read_sentence).
:-include(info_food).

readInputTillQuit:-
	write("Welcome to your personal assistant"),nl,
	res(Q),
	delete(Q,?,Q1),
	delete(Q1,.,Q2),
	
	loop(Q2,[],[]).
	
	loop([quit],PQ,PR):-

		append(PQ,PR,PF),
		helperloop1(PF),
		helperloop2(PF),
		helperloop3(PF),
		
		write("Bye").
	
	helperloop1([]):-
		ws(["You", had, - ,for, breakfast]),nl.
	
	helperloop1([Q|T]):-
		\+Q=[You,can,have,X,for,breakfast],
		\+Q=[i,ate,X,for,breakfast],
		helperloop1(T).
	helperloop1([Q|T]):-
		Q=[i,ate,X,for,breakfast],
		ws(["You",had,X,for,breakfast]),nl.
	helperloop1([Q|T]):-
		Q=[You,can,have,X,for,breakfast],
		ws(["You",had,X,for,breakfast]),nl.
	
	
	helperloop2([]):-
		ws(["You", had, - ,for, lunch]),nl.
	
	helperloop2([Q|T]):-
		\+Q=[You,can,have,X,for,lunch],
		\+Q=[i,ate,X,for,lunch],
		helperloop2(T).
	helperloop2([Q|T]):-
		Q=[i,ate,X,for,lunch],
		ws(["You",had,X,for,lunch]),nl.
	helperloop2([Q|T]):-
		Q=[You,can,have,X,for,lunch],
		ws(["You",had,X,for,lunch]),nl.
		
	helperloop3([]):-
		ws(["You", had, - ,for, dinner]),nl.
	
	helperloop3([Q|T]):-
		\+Q=[You,can,have,X,for,dinner],
		\+Q=[i,ate,X,for,dinner],
		helperloop3(T).
	helperloop3([Q|T]):-
		Q=[i,ate,X,for,dinner],
		ws(["You",had,X,for,dinner]),nl.
	helperloop3([Q|T]):-
		Q=[You,can,have,X,for,dinner],
		ws(["You",had,X,for,dinner]),nl.
		
		
		
	loop(Q,PQ,PR):-
		\+Q=[quit],
		isValid(Q),
		response(Q,PQ,PR,R),
		ws(R),nl,
		res(A),nl,
		delete(A,?,A1),
		delete(A1,.,A2),
		loop(A2,[Q|PQ],[R|PR]).
	loop(Q,PQ,PR):-
		\+isValid(Q),
		write("I can not understand you"),nl,
		res(A),nl,
		delete(A,?,A1),
		delete(A1,.,A2),
		loop(A2,[Q|PQ],PR).
		
totalCal(1800).	
	
isValid([how,many,calories ,does,_ ,contain]).
			
isValid([quit]).		
isValid([what,does,_,contain]).
		
isValid([can,I,have,_,for,_]).
		
isValid([What,is,_]).
		
isValid([how,many,calories,do,I,have,left]).	
isValid([what,kind,of,_,does,_,contain]).
			
isValid([is,_,a,_,in,_]).
		%food_ingredient_check_checkfoodingredientposition
		
			
isValid([what,can,i,have,for,_,that,contains,_]).
	

		
		
isValid([i,ate,_,for,_]).
			
isValid([i,do,not,eat,_]).
			
		


filterProp(R,X):-
	 setof(L,filterProp1(R,L),X).
			
filterProp1(R,(X,Y)):-
	prop(X,R,Y).
	

matchFirst(_,[],[]).	
	
matchFirst(F,[(F,Z)|T1],[Z-1|T2]):-	%F_is_food_type
	matchFirst(F,T1,T2).
	
matchFirst(F,[(X,Z)|T1],[Z-0|T2]):-	%F_is_food_type
	F\=X,
	matchFirst(F,T1,T2).

matchSecond(_,[],[]).	
	
matchSecond(F,[(Z,F)|T1],[Z-1|T2]):-
		matchSecond(F,T1,T2).
		
matchSecond(F,[(X,Z)|T1],[X-0|T2]):-	%F_is_food_type
	F\=Z,
	matchSecond(F,T1,T2).		

	

mergeMatchLists(ML1,ML2,R):-
append(ML1,ML2,R1),
mergeMatchLists0(R1,R).

mergeMatchLists0([],[]).
mergeMatchLists0([Z-X|T],[Z-Y|T2]):-
	mergeMatchLists1(Z-X,T,Y),
	delete(T,Z-_,L2),
	mergeMatchLists0(L2,T2).


mergeMatchLists1(Z-Y,[],Y).
mergeMatchLists1(Z-P,[Z-Y|T],X):-
			mergeMatchLists1(Z-P,T,X1),
			X is Y+X1.
mergeMatchLists1(Z-P,[L-Y|T],X):-	
				Z\=L,
				mergeMatchLists1(Z-P,T,X).
		
bestMatches([],[]).
bestMatches(X,L):-
	getmax(X,Y),
	bestMatchesMin(X,Y,L).

getmax([Z-X],X).
getmax([Z-X|T],X):-
		getmax(T,Y2),
		X>=Y2.
getmax([Z-X|T],Y2):-
		getmax(T,Y2),
		X<Y2.

bestMatchesMin([],_,[]).

bestMatchesMin([Z-X|T],X,[Z|T2]):-
		bestMatchesMin(T,X,T2).
bestMatchesMin([Z-Y|T],X,T2):-
			Y \=X,
			bestMatchesMin(T,X,T2).
	

foodCal(F,C):-
	prop(F,contain ,C ,cal).

foodCal(F,C):-%a11
	setof(X,prop(F,contain,X),L),
	helper(L,C).

helper([],0).
helper([H|T],X):-
		prop(H,contain,Z,cal),
		helper(T,M),
		X is Z+M.
helper([H|T],X):-
		\+prop(H,contain,Z,cal),
		foodCal(H,C),
		helper(T,M),
		X is C+M.


foodCalList([],0).
foodCalList([H|T],C):- %a11
			foodCalList(T,C1),
			foodCal(H,C2),
			C is C1+C2.
			
calcCalories(F,PQ,PR,C):-
		helper1(PQ,L),
		helper2(PR,L0),
		append(L,L0,L2),
		append([F],L2,L3),
		foodCalList(L3,C2),
		totalCal(C3),
		C is C3-C2.
		
		
helper1([],[]).
		
helper1([Q|T],[X|T2]):-
			Q=[i,ate,X,for,_],
			helper1(T,T2).
helper1([Q|T],T2):-
			 \+Q=[i,ate,_,for,_ ],
				helper1(T,T2).
helper2([],[]).
		
helper2([Q|T],[X|T2]):-
			Q=[you,can,have,X,for,_],
			helper1(T,T2).
helper2([Q|T],T2):-
			 \+Q=[you,can,have,X,for,_],
				helper1(T,T2).

getDiffAnswer(Q,[],[],[CR|T2],CR).		
getDiffAnswer(Q,[Q|T1],_,[CR|T2],R):-
				getDiffAnswer(Q,T1,T3,T2,R).
getDiffAnswer(Q,[PQ|T1],_,[CR|T2],R):-
				\+Q=PQ,
				getDiffAnswer(Q,T1,T3,[CR|T2],R).
	

	

	
listOrderDesc(LP,OLP):-
	i_sort(LP,[],OLP).





i_sort([],X,X).
i_sort([H|T],Accumulator,Sorted):-insert(H,Accumulator,N),i_sort(T,N,Sorted).
insert(X-Z,[Y-Z1|T],[Y-Z1|NT]):-Z=<Z1,insert(X-Z,T,NT).
insert(X-Z,[Y-Z1|T],[X-Z,Y-Z1|T]):-Z>Z1.
insert(X,[],[X]).
	

foodFromHistory([],[]).
foodFromHistory([Q|T],[X|T2]):-	
					Q=[i,ate,X,for, _],
					foodFromHistory(T,T2).

foodFromHistory([Q|T],[X|T2]):-	
					Q=[You,can,have,X,for,_],
					foodFromHistory(T,T2).

foodFromHistory([Q|T],T2):-
					\+Q=[i,ate,X,for, _],
					\+Q=[You,can,have,X,for,_],
					foodFromHistory(T,T2).
	
	
getUnlikedIngredients([],[]).
getUnlikedIngredients([Q|T],[X|T2]) :-
					Q=[i ,do ,not ,eat ,X],
					getUnlikedIngredients(T,T2).
					
getUnlikedIngredients([Q|T],T2) :-
						\+Q=[i ,do ,not ,eat ,X],
						getUnlikedIngredients(T,T2).
						
						
						
responseO([what,can ,i ,have, for ,X ,that ,contains ,Y],PQ,PR,R):-
				setof(Z,prop(Z,contain,Y),L),
				resO1(L,L1),
				resO2(X,L1,L2),
				getUnlikedIngredients(PQ,UI),
				resO3(UI,L2,L3),
				resO4(L3,PQ,PR,R).
				
				
				
resO1([],[]).				
resO1([H|T],[H-1|T1]):-
		resO1(T,T1).		
resO2(X,[],[]).
resO2(X,[H-Z|T],[H-Z2|T2]):-
			\+prop(H,not,X),
			Z2 is (Z+1),
			resO2(X,T,T2).
	
resO2(X,[H-Z|T],[H-Z|T2]):-
		prop(H,not,X),
		resO2(X,T,T2).
resO3(_,[],[]).
resO3(UI,[H-Z|T],[H-Z1|T2]):-
	resO31(UI,H,Z2),
	Z1 is Z+Z2,
	resO3(UI,T,T2).
		
resO31(UI,E,1):-
	setof(Y,prop(E,contain,Y),L),
	intersection(UI,L,[]).

resO31(UI,E,0):-
	setof(Y,prop(E,contain,Y),L),
	intersection(UI,L,P),
	P\=[].
	
resO4([],_,_,[]).

resO4([H-Z|T],PQ,PR,[H-Z1|T2]):-
		calcCalories(H,PQ,PR,C),
		C >0,
		Z1 is Z+1,
		resO4(T,PQ,PR,T2).
		
resO4([H-Z|T],PQ,PR,[H-Z|T2]):-
		calcCalories(H,PQ,PR,C),
		C<0,
		resO4(T,PQ,PR,T2).

		
		
					
response(Q,PQ,_,R):-
				Q=[how,many,calories,does,X,contain],
				member(Q,PQ),
				R = ["I",told,you,that,before].
				

					
					
response(Q,PQ,PR,R):-
				Q=[how,many,calories,does,X,contain],
				\+member(Q,PQ),
				isValid(Q),
				foodCal(X,C),
				R=[C,"Calories"].
response(Q,PQ,PR,["I",do,not,know]):-
				Q=[how,many,calories,does,X,contain],
				\+member(Q,PQ),
				\+foodCal(X,C).
				
				
response(Q,PQ,PR,["I",told,you,that,before]):-
			Q=[what,does,X,contain],
			
			filterProp(contain,L1),
			matchFirst(X,L1,R1),
			bestMatchesMin(R1,1,CR),
			length(CR,N),
			N>=1,
			
			\+getDiffAnswer(Q,PQ,PR,CR,_).				

				
response(Q,PQ,PR,[R]):-
			Q=[what,does,X,contain],
			filterProp(contain,L1),
			matchFirst(X,L1,R1),
			bestMatchesMin(R1,1,CR),
			reverse(CR,C1),
			getDiffAnswer(Q,PQ,PR,C1,R).
			
			
response(Q,PQ,PR, ["I", do, not, know]):-
			Q=[what,does,X,contain],
			\+prop(X,contain,_).
			
			
			
			
response(Q,PQ,PR,R):-
			Q=[can ,i ,have ,X, for ,Y],
			\+prop(X,not,Y),
			calcCalories(X,PQ,PR,C),
			C>=0,
			\+member(Q,PQ),
			R= ["You", can, have, X, for, Y] .
			
response(Q,PQ,PR,R):-
			Q=[can ,i ,have ,X, for ,Y],
			prop(X,not,Y),
			\+member(Q,PQ),
			R = [X, is, not, suitable, for, Y] .		


response(Q,PQ,PR,R):-
			Q=[can ,i ,have ,X, for ,Y],
			\+member(Q,PQ),
			\+prop(X,not,Y),
			calcCalories(X,PQ,PR,C),
			C<0,
			R = ["NO"] .	

response(Q,PQ,PR,["I",told,you,that,before]):-
			Q=[can ,i ,have ,X, for ,Y],
			member(Q,PQ).
			
			
response(Q,PQ,_,["I",do,not,know]) :-
Q=[can ,i ,have ,X, for ,Y],
	\+member(Q,PQ),
	\+prop(	X,contain,_).
	



response([what,is,X],PQ,PR,[R]):-
		Q=[what,is,X],
		\+member(Q,PQ),
		prop(X,is,R).
		
response([what,is,X],PQ,PR, ["I", told, you, that, before] ):-
		Q=[what,is,X],
		member(Q,PQ).
			
response([what,is,X],PQ,PR,["I", do, not, know]):-
		Q=[what,is,X],
		\+member(Q,PQ),
		\+prop(X,is,_).
		
		
		
 

 helperHW(Q,R,[PQ|T],[PR|T2]):-
	((Q==PQ,
	R==PR);helperHW(Q,R,T,T2)).
 
 response([how,many,calories,do,i,have,left],PQ,PR,R):-
		foodFromHistory(PQ,FL1),
		foodFromHistory(PR,FL2),
		append(FL1,FL2,FL),
		totalCal(X),
		foodCalList(FL,Y),
		Z is X-Y,
		R=[Z,"Calories"],
		\+helperHW([how,many,calories,do,i,have,left],R,PQ,PR).
		
response([how,many,calories,do,i,have,left],PQ,PR,R):-
		foodFromHistory(PQ,FL1),
		foodFromHistory(PR,FL2),
		append(FL1,FL2,FL),
		totalCal(X),
		\+foodCalList(FL,Y),
		R=["I", do, not, know],
		\+member(R,PR).
		
response([how,many,calories,do,i,have,left],PQ,PR,["I", told, you, that, before]):-
		foodFromHistory(PQ,FL1),
		foodFromHistory(PR,FL2),
		append(FL1,FL2,FL),
		totalCal(X),
		foodCalList(FL,Y),
		Z is X-Y,
		M=[Z,"Calories"],
		helperHW([how,many,calories,do,i,have,left],M,PQ,PR).

response([how,many,calories,do,i,have,left],PQ,PR,["I", told, you, that, before]):-
		foodFromHistory(PQ,FL1),
		foodFromHistory(PR,FL2),
		append(FL1,FL2,FL),
		totalCal(X),
		\+foodCalList(FL,Y),
		M=["I", do, not, know],
		helperHW([how,many,calories,do,i,have,left],M,PQ,PR).
		
		
response(Q,_,_,["I",do,not,know]) :-
Q = [what,kind,of,FC,does,F,contain],
((\+ prop(_,_,FC));
(\+prop(F,_,_))).


response(Q,_,_,["Nothing",from,what,i,know]) :-
Q = [what,kind,of,FC,does,F,contain],
prop(_,_,FC),
prop(F,_,_),
filterProp(contain,L1),
filterProp(is,L2),
matchFirst(F,L1,R1),
matchSecond(FC,L2,R2),
mergeMatchLists(R1,R2,L3),
bestMatchesMin(L3,2,CR),
length(CR,0).

response(Q,PQ,PR,[R]) :-
Q = [what,kind,of,FC,does,F,contain],
prop(_,_,FC),
prop(F,_,_),
filterProp(contain,L1),
filterProp(is,L2),
matchFirst(F,L1,R1),
matchSecond(FC,L2,R2),
mergeMatchLists(R1,R2,L3),
bestMatchesMin(L3,2,CR),
length(CR,N),
N >= 1,
getDiffAnswer(Q,PQ,PR,CR,R).

response(Q,PQ,PR,["I",told,you,that,before]) :-
Q = [what,kind,of,FC,does,F,contain],
prop(_,_,FC),
prop(F,_,_),
filterProp(contain,L1),
filterProp(is,L2),
matchFirst(F,L1,R1),
matchSecond(FC,L2,R2),
mergeMatchLists(R1,R2,L3),
bestMatchesMin(L3,2,CR),
length(CR,N),
N >= 1,
\+getDiffAnswer(Q,PQ,PR,CR,_).


response(Q,PQ,PR,["Yes"]):-
	Q=[is,X,a,Y,in,Z],
	prop(X,is,Y),
	prop(Z,contain,X),
	\+member(Q,PQ).
	
response(Q,PQ,PR,["NO"]):-
	Q=[is,X,a,Y,in,Z],
	prop(X,is,_),
	prop(Z,contain,_),
	prop(_ ,is ,Y),
	((\+prop(X,is,Y));
	(\+prop(Z,contain,X))),
	\+member(Q,PQ).		
response(Q,PQ,PR,["I",do,not,know]):-
	Q=[is,X,a,Y,in,Z],
	(\+prop(X,is,_);
	\+prop(_,is,Y);
	\+prop(Z,contain,_)),
	\+member(Q,PQ).
	
response(Q,PQ,PR,["I",told,you,that,before]):-
	Q=[is,X,a,Y,in,Z],
	member(Q,PQ).
	
	
response(Q,PQ,PR,[R]):-
		Q=[what, can, i, have, for, X, that, contains,Y],
		responseO(Q,PQ,PR,R1),
		bestMatchesMin(R1,4,R2),
		getDiffAnswer(Q,PQ,PR,R2,R).
response(Q,PQ,PR,R):-
		Q=[what, can, i, have, for, X, that,contains, Y],
		responseO(Q,PQ,PR,R1),
		bestMatchesMin(R1,4,R2),
		R2=[],
		R =  ["Nothing", from, what, i, know] .
response(Q,PQ,PR,R):-
		Q=[what, can, i, have, for, X, that,contains, Y],
		responseO(Q,PQ,PR,R1),
		bestMatchesMin(R1,4,R2),
		R2\=[],
		\+getDiffAnswer(Q,PQ,PR,R2,_),
		R =  ["I", told, you, that, before] .
	
response(Q,PQ,PR,R):-
		Q=[what, can, i, have, for, X, that, contains,Y],
		((\+prop(Y,is,_));(\+prop(_,not,X))),
		R =  ["I", do,not,know] .
	
response([i ,ate ,_ ,for ,_],_,_,["Ok"]).

response([I,do, not, eat ,_],_,_,["OK"]).	


	