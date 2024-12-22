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


char_codigohuff(Codigo, Caracter):- mi_arbol(Arbol), 
char_codigohuff_arbol(Codigo, Arbol, Caracter).


char_codigohuff_arbol([],hoja(P,_), P).
char_codigohuff_arbol([0|Xs], rama(X1,_),P):- char_codigohuff_arbol(Xs,X1,P).
char_codigohuff_arbol([1|Xs], rama(_,X1),P):- char_codigohuff_arbol(Xs,X1,P).


texto_a_bits(Texto, StringBits) :-
atom_chars(Texto, ListaChars),
texto_a_bits_lista(ListaChars, Bits),
maplist(number_chars, Bits, ListaBitsChars),
flatten(ListaBitsChars, ListaBitsFlatten),
atomic_list_concat(ListaBitsFlatten, '', AtomicBits),
atom_string(AtomicBits,StringBits).



texto_a_bits_lista([],[]).
texto_a_bits_lista([X|Xs], Zs):- char_codigohuff(Codigo, X), texto_a_bits_lista(Xs,Ys), append(Codigo, Ys, Zs).


bits_a_texto(BitsString, Texto) :-
 string_chars(BitsString, ListaBitsChars),
 maplist(atom_number, ListaBitsChars, Bits),
 bits_a_texto_lista(Bits, ListaChars),
 string_chars(Texto, ListaChars).

bits_a_texto_lista([], []).  
bits_a_texto_lista(Bits, [Char|Chars]) :- mi_arbol(Arbol), char_codigohuff_arbol(Codigo, Arbol, Char), append(Codigo, RestoBits, Bits), bits_a_texto_lista(RestoBits, Chars).

