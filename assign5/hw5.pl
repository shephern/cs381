/*
CS381 HW5
Drake Seifert (seifertd)
Nathan Shepherd (shephern)
Justin Chang (changle)
*/

when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).

where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,dear118).
where(399,cov216).

enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,399).

%1a
schedule(X,Y,Z) :- enroll(X,A), where(A,Y), when(A,Z).

%1b
usage(X,Y) :- where(A,X), when(A,Y).

%1c
conflict(X,Y) :- where(X,A), where(Y,A), when(X,B), when(Y,B), X\=Y.

%1d
meet(X,Y) :- schedule(X,A,B), schedule(Y,A,B), X\=Y;
			 schedule(X,A,C), schedule(Y,A,D), X\=Y, C\==D+1.