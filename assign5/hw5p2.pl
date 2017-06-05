/* Exercise 2 */
%a
rdup([_|L],M) :- rdup(L,M).
rdup([H|_],M) :- not(member(H,M)), append(M,H,M).

%If just member, rdup. If not, append it then rdup.
%b

flat([H|L],F) :- flat(H,F), flat(L,F).
flat(L,F) :- append(F,L,F).
flat([],F) :- F.

%Append if no other list, else move in to the list.
%Hopefully doing this right: First case is list, second is not. If list, just
%Take off the head, otherwise append it. Then hopefully recursion goes next head

%c
suball([H|T]) :- H-1,suball(T).

project(X,[_|Y],L) :- suball(X),project([X,Y,L]).
project([X|T],[H|Y],L) :- X=0,append(L,H,L),project(T,Y,L).
project(_,[],L) :- L.
project([],_,L) :- L.



%Keep subtracting 1 from the list until the head is 0, then remove that
%head. Repeat until it's over.
