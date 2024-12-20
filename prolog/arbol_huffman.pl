mi_arbol(rama(hoja('S',4),rama(hoja('O',3),rama(hoja('E',2),hoja(' ',2))))).

es_arbol(hoja(_,_)).
es_arbol(rama(I,D)) :-
    es_arbol(I),
    es_arbol(D).

peso(hoja(_,P),P).
peso(rama(I,D),P) :-
    peso(I,PI),
    peso(D,PD),
    P is PI+PD.

lista_caracteres(hoja(L,_),[L]).
lista_caracteres(rama(I,D),L) :-
    lista_caracteres(I,LI),
    lista_caracteres(D,LD),
    append(LI,LD,L).
