% TP 2: paradigma logico
% 		- Marcolini, Facundo
% 		- Pacheco Pilan, Federico Ignacio
% 	~ 2C, 2020 ~

% Alaración: para el desarrollo de este tp, se utilizó la pagina web
% https://swish.swi-prolog.org
%_________________________________________________________________________________________________________________
% Predicados a implementar

%------------------------------------------------------------------------
% 5)
cantTestPositivo(Fecha, Cantidad) :-
	obtenerListaPositivos(Fecha, ListaPositivos, []),
	cantidadElementos(ListaPositivos, Cantidad).


obtenerListaPositivos(Fecha, [[Id, Nombre, FechaPositivo] | LPs], LAux) :- 
	persona(Id, Nombre),
    positivo(Id, FechaPositivo),
	not(miembro([Id, Nombre, FechaPositivo], LAux)),
    compara(FechaPositivo, '=<', Fecha), !,
    append(LAux, [[Id, Nombre, FechaPositivo]], LAux2),
    obtenerListaPositivos(Fecha, LPs, LAux2).

obtenerListaPositivos(_Fecha, [], _LAux).

miembro(X, [X | _Ls]) :- !.

miembro(X, [_L | Ls]) :- 
    miembro(X, Ls).


cantidadElementos([_L | Ls], C) :- 
    cantidadElementos(Ls, C1), C is C1 + 1.

cantidadElementos([], 0).

%------------------------------------------------------------------------
% 6) 
cantTestNegativo(Fecha, Cantidad) :-
	obtenerListaNegativos(Fecha, ListaNegativos, []),
    cantidadElementos(ListaNegativos, Cantidad).


obtenerListaNegativos(Fecha, [[Id, Nombre, FechaNegativo] | LPs], LAux) :- 
	persona(Id, Nombre),
    negativo(Id, FechaNegativo),
   	not(miembro([Id, Nombre, FechaNegativo], LAux)),
    compara(FechaNegativo, '=<', Fecha), !,
    append(LAux, [[Id, Nombre, FechaNegativo]], LAux2),
    obtenerListaNegativos(Fecha, LPs, LAux2).

obtenerListaNegativos(_Fecha, [], _LAux).

%------------------------------------------------------------------------
% 7)
cantTestPersona(Id, Cantidad) :- 
    obtenerListaTesteos(Id, ListaTests, []), 
    cantidadElementos(ListaTests, Cantidad).


obtenerListaTesteos(Id, [['(+)', Fecha] | LTs], LAux) :- 
    positivo(Id, Fecha),
    not(miembro(['(+)', Fecha], LAux)), !,
    append(LAux, [['(+)', Fecha]], LAux2),
    obtenerListaTesteos(Id, LTs, LAux2).

obtenerListaTesteos(Id, [['(-)', Fecha] | LTs], LAux) :- 
    negativo(Id, Fecha),
    not(miembro(['(-)', Fecha], LAux)), !,
    append(LAux, [['(-)', Fecha]], LAux2),
    obtenerListaTesteos(Id, LTs, LAux2).

obtenerListaTesteos(_Id, [], _LAux).

    
%------------------------------------------------------------------------
% 8)
noTesteados(Fecha, LNoTesteados) :- 
    obtenerListaPositivos(Fecha, LPositivos, []),
    obtenerListaNegativos(Fecha, LNegativos, []),
    sustraerFechas(LPositivos, LPositivosSinFechas),
    sustraerFechas(LNegativos, LNegativosSinFechas),
    noTesteadosAux(LPositivosSinFechas, LNegativosSinFechas, LAux, []), 
    sustraerIds(LAux, LNoTesteados).

% Se podria guardar directamente el nombre. Esto puede no ser deseable si se repiten los nombres, por lo que se registra el id. 
noTesteadosAux(ListaPositivos, ListaNegativos, [[Id, Nombre] | Ls], LAux) :-   
    persona(Id, Nombre),
    not(miembro([Id, Nombre], LAux)),
    not(miembro([Id, Nombre], ListaPositivos)),
    not(miembro([Id, Nombre], ListaNegativos)), !, 
    append(LAux, [[Id, Nombre]], LAux2),
    noTesteadosAux(ListaPositivos, ListaNegativos, Ls, LAux2).

noTesteadosAux(_ListaPositivos, _ListaNegativos, [], _LAux).


sustraerFechas([[Id, Nombre, _Fecha] | Xs], [[Id, Nombre] | Ys]) :-
    sustraerFechas(Xs, Ys).

sustraerFechas([], []).


sustraerIds([[_Id, Nombre] | Ls], [Nombre | ListaNombres]) :- 
    sustraerIds(Ls, ListaNombres).

sustraerIds([], []).

%------------------------------------------------------------------------
% 9)
casosActivos(Fecha, LCasosActivos) :- 
    casosActivosAux(Fecha, LAux, []), 
    sustraerIds(LAux, LCasosActivos).
    
casosActivosAux(Fecha, [[Id, Nombre] | LCAs], LAux) :- 
    persona(Id, Nombre),
    not(miembro([Id, Nombre], LAux)),
    positivo(Id, FechaPositivo),
    compara(FechaPositivo, '=<', Fecha),
    not(existeTestNegativoAPartirDeFecha(Id, FechaPositivo, Fecha)), !,
    append(LAux, [[Id, Nombre]], LAux2),
    casosActivosAux(Fecha, LCAs, LAux2).

casosActivosAux(_Fecha, [], _LAux).

% No se estan considerando reinfecciones. Si existe dicho test negativo, la persona entra al estado "recuperado" y no se le hacen mas tests.
existeTestNegativoAPartirDeFecha(Id, FechaPositivo, FechaLimite) :- 
	negativo(Id, FechaNegativo),
    compara(FechaNegativo, '>', FechaPositivo),  
    compara(FechaNegativo, '=<', FechaLimite), !.

%------------------------------------------------------------------------
% 10)

% Si testeados = TesteadosNegativos U TesteadosPositivos . Se corresponde con el ejemplo propuesto pero no con el enunciado: los testeados
% contemplan aquellos que han sido evaluados negativamente una cantidad arbitraria de veces pero ninguna vez positivamente.
testeados(Fecha, LTesteados) :-
    obtenerListaPositivos(Fecha, LPositivos, []),
    obtenerListaNegativos(Fecha, LNegativos, []),
    sustraerFechas(LPositivos, LPositivosSinFechas),
    sustraerFechas(LNegativos, LNegativosSinFechas),
    append(LPositivosSinFechas, LNegativosSinFechas, LAux),
    eliminarRepetidos(LAux, LAux2),
    sustraerIds(LAux2, LTesteados).


eliminarRepetidos([X1 | Xs], [X1 | Ys]) :-
    not(miembro(X1, Xs)), !,
    eliminarRepetidos(Xs, Ys).

eliminarRepetidos([X1 | Xs], Y) :-
    miembro(X1, Xs), 
    eliminarRepetidos(Xs, Y).

eliminarRepetidos([], []).


% Alternativamente, para los testeados negativamente pero no positivamente
testeados2(Fecha, LTesteados) :-
    obtenerListaNegativos(Fecha, LNegativos, []),
    sustraerFechas(LNegativos, LNegativosSinFechas),
    eliminarRepetidos(LNegativosSinFechas, LNSFSinRepetidos),
    testeados2Aux(Fecha, LNSFSinRepetidos, LAux), !,
    sustraerIds(LAux, LTesteados).


testeados2Aux(Fecha, [[Id, Nombre] | LNs], [[Id, Nombre] | Ls]) :-
    not(existeTestPositivo(Id, Fecha)),
    testeados2Aux(Fecha, LNs, Ls).

testeados2Aux(Fecha, [[Id, _Nombre] | LNs], L) :-
    existeTestPositivo(Id, Fecha),
    testeados2Aux(Fecha, LNs, L).

testeados2Aux(_Fecha, [], []).


existeTestPositivo(Id, Fecha) :-
    positivo(Id, FechaPositivo),
    compara(FechaPositivo, '=<', Fecha).
      
%------------------------------------------------------------------------
% 11) 
recuperados(Fecha, LRecuperados) :- casosRecuperadosAux(Fecha, LAux, []), sustraerIds(LAux, LRecuperados).
    
casosRecuperadosAux(Fecha, [[Id, Nombre] | LRs], LAux) :- 
	persona(Id, Nombre),
    not(miembro([Id, Nombre], LAux)),
    positivo(Id, FechaPositivo),
    compara(FechaPositivo, '=<', Fecha),
    existeTestNegativoAPartirDeFecha(Id, FechaPositivo, Fecha), !,
    append(LAux, [[Id, Nombre]], LAux2),
    casosRecuperadosAux(Fecha, LRs, LAux2).

casosRecuperadosAux(_Fecha, [], _LAux).

%------------------------------------------------------------------------
% 12) 
periodoActivo(Id, FechaPositivo, FechaNegativo) :-
    obtenerListaTesteos(Id, LTests, []),
    obtenerFechasTestsPositivos(LTests, LAux),
    menorFecha(LAux, FechaPositivo),
    testNegativoAPartirDeFecha(Id, FechaPositivo, FechaNegativo).


% Se hace puesto que los hechos en otras cirscunstancias podrian estar desordenados
obtenerFechasTestsPositivos([['(+)', Fecha] | LTs], [Fecha | Ls]) :-
    obtenerFechasTestsPositivos(LTs, Ls).

obtenerFechasTestsPositivos([['(-)', _Fecha] | LTs], L) :-
    obtenerFechasTestsPositivos(LTs, L).

obtenerFechasTestsPositivos([], []).


menorFecha([F1, F2 | Fs], MF) :- 
    compara(F1, '<', F2), 
    menorFecha([F1 | Fs], MF).

menorFecha([F1, F2 | Fs], MF) :- 
    compara(F1, '>=', F2), 
    menorFecha([F2 | Fs], MF).

menorFecha([MF], MF).


testNegativoAPartirDeFecha(Id, FechaPositivo, FechaNegativo) :-
    negativo(Id, FechaNegativo),
    compara(FechaNegativo, '>', FechaPositivo), !. 
    

%------------------------------------------------------------------------
% 13)
expuestos(Id, LExpuestos) :- periodoActivo(Id, FInferior, FSuperior), expuestosAux(Id, FInferior, FSuperior, LAux, []), sustraerIds(LAux, LExpuestos).


expuestosAux(Id, FInferior, FSuperior, [[IdOtraPersona, Nombre] | LEs], LAux) :-
    persona(IdOtraPersona, Nombre),                % colocar persona() despues de contacto tiene el efecto de mostrar "variables numericas" para el nombre
   	contacto(IdOtraPersona, Id, FechaContacto),
    compara(FechaContacto, '>=', FInferior),
    compara(FechaContacto, '=<', FSuperior),
    not(miembro([IdOtraPersona, Nombre], LAux)), !,
    append(LAux, [[IdOtraPersona, Nombre]], LAux2),
    expuestosAux(Id, FInferior, FSuperior, LEs, LAux2).

expuestosAux(Id, FInferior, FSuperior, [[IdOtraPersona, Nombre] | LEs], LAux) :-
    persona(IdOtraPersona, Nombre),
    contacto(Id, IdOtraPersona, FechaContacto),
    compara(FechaContacto, '>=', FInferior),
    compara(FechaContacto, '=<', FSuperior),
    not(miembro([IdOtraPersona, Nombre], LAux)), !,
    append(LAux, [[IdOtraPersona, Nombre]], LAux2),
    expuestosAux(Id, FInferior, FSuperior, LEs, LAux2).

expuestosAux(_Id, _FInferior, _FSuperior, [], _LAux).

%------------------------------------------------------------------------
% 14)
nexo(Id1, Id2, L) :- nexoAux(Id1, Id2, L, [[]]).
    

nexoAux(Id1, Id2, [LC | LCs], LAux) :-
    persona(Id, Nombre), 
    Id is Id1, 										  		  % Se hace porque en algunos casos salen "variables numericas"
    encontrarCamino(Id2,Id1,-1,fecha(1970,1,1),L),            % La pandemia seguro que empezo despues del 1/1/1970. No hay personas con id = -1. 
    append([Nombre], L, LC),								  % Se evita el uso de otro predicado para el primer nombre en la lista, esto es, el inicio del camino
    not(miembro(LC, LAux)), !,
    append([LC], LAux, LAux2),
    nexoAux(Id1, Id2, LCs, LAux2).

nexoAux(_Id1, _Id2, [], _LAux).


encontrarCamino(Id2, IdActual, IdAnterior, FechaAnterior, [Nombre | LCs]) :- 
    persona(IdOtraPersona, Nombre),
    contacto(IdActual, IdOtraPersona, FechaContacto), 
    IdAnterior \= IdOtraPersona,
    compara(FechaAnterior, '=<', FechaContacto), 
    encontrarCamino(Id2, IdOtraPersona, IdActual, FechaContacto, LCs).

encontrarCamino(Id2, IdActual, IdAnterior, FechaAnterior, [Nombre | LCs]) :- 
    persona(IdOtraPersona, Nombre),
    contacto(IdOtraPersona, IdActual, FechaContacto), 
    IdAnterior \= IdOtraPersona,
    compara(FechaAnterior, '=<', FechaContacto), 
    encontrarCamino(Id2, IdOtraPersona, IdActual, FechaContacto, LCs).

encontrarCamino(Id2, Id2, _IdAnterior, _FechaAnterior, []).


%_________________________________________________________________________________________________________________
% Predicados disponibles

%------------------------------------------------------------------------
% F1 = F2
compara(fecha(AAAA1, MM1, DD1), '=', fecha(AAAA2, MM2, DD2)) :-
	entradaEntera([AAAA1, MM1, DD1, AAAA2, MM2, DD2]),
	AAAA1 = AAAA2,
	MM1 = MM2,
	DD1 = DD2.

%------------------------------------------------------------------------
% F1 > F2
compara(fecha(AAAA1, MM1, DD1), '>', fecha(AAAA2, MM2, DD2)) :-
	entradaEntera([AAAA1, MM1, DD1, AAAA2, MM2, DD2]),
	AAAA1 > AAAA2.

compara(fecha(AAAA, MM1, DD1), '>', fecha(AAAA, MM2, DD2)) :-
	entradaEntera([AAAA, MM1, DD1, MM2, DD2]),
	MM1 > MM2.
                     
compara(fecha(AAAA, MM, DD1), '>', fecha(AAAA, MM, DD2)) :-
	entradaEntera([AAAA, MM, DD1, DD2]),
	DD1 > DD2.

%------------------------------------------------------------------------
% F1 < F2
compara(fecha(AAAA1, MM1, DD1), '<', fecha(AAAA2, MM2, DD2)) :-
	not(compara(fecha(AAAA1, MM1, DD1), '>', fecha(AAAA2, MM2, DD2))).

%------------------------------------------------------------------------
% F1 >= F2
compara(fecha(AAAA1, MM1, DD1), '>=', fecha(AAAA2, MM2, DD2)) :-
	compara(fecha(AAAA1, MM1, DD1), '>', fecha(AAAA2, MM2, DD2)).

compara(fecha(AAAA1, MM1, DD1), '>=', fecha(AAAA2, MM2, DD2)) :-
	compara(fecha(AAAA1, MM1, DD1), '=', fecha(AAAA2, MM2, DD2)).

%------------------------------------------------------------------------
% F1 =< F2
compara(fecha(AAAA1, MM1, DD1), '=<', fecha(AAAA2, MM2, DD2)) :-
	compara(fecha(AAAA1, MM1, DD1), '<', fecha(AAAA2, MM2, DD2)).

compara(fecha(AAAA1, MM1, DD1), '=<', fecha(AAAA2, MM2, DD2)) :-
 	compara(fecha(AAAA1, MM1, DD1), '=', fecha(AAAA2, MM2, DD2)).

%------------------------------------------------------------------------
% Auxiliar
entradaEntera([L1 | Ls]) :- 
    integer(L1), 				% https://www.swi-prolog.org/pldoc/doc_for?object=integer/1
    entradaEntera(Ls).

entradaEntera([]).

%------------------------------------------------------------------------
% Se movieron contacto(), positivo(), negativo() y persona() al apartado "Hechos" por errores con SWISH                   


%_________________________________________________________________________________________________________________
% Hechos (proporcionados por la catedra)

%------------------------------------------------------------------------
persona(Id, _Nombre) :- 
    entradaEntera([Id]), 
    Id > 0.					

persona(1,'Diego').
persona(2,'Camila').
persona(3,'Mauro').
persona(4,'Martin').
persona(5,'Romina').
persona(6,'Melina').
persona(7,'Marcelo').
persona(8,'Fabian').
persona(9,'Belen').
persona(10,'Pablo').
persona(11,'Mariana').

%------------------------------------------------------------------------
contacto(Id1, Id2, fecha(AAAA, MM, DD)) :- 
    entradaEntera([AAAA, MM, DD]),
    persona(Id1_, _Nombre1),				% verificar que los ids existan
    Id1_ is Id1,
    persona(Id2_, _Nombre2),
    Id2_ is Id2.

contacto(10,2,fecha(2020,6,29)).
contacto(2,7,fecha(2020,6,14)).
contacto(4,2,fecha(2020,6,14)).
contacto(7,4,fecha(2020,6,16)).
contacto(5,2,fecha(2020,6,12)).
contacto(5,3,fecha(2020,6,11)).
contacto(3,6,fecha(2020,6,9)).
contacto(1,6,fecha(2020,6,11)).
contacto(6,8,fecha(2020,6,9)).
contacto(11,6,fecha(2020,6,26)).
contacto(6,9,fecha(2020,6,10)).
contacto(8,9,fecha(2020,6,26)).
contacto(11,8,fecha(2020,6,12)).

%------------------------------------------------------------------------
positivo(Id, fecha(AAAA, MM, DD)) :- 
    entradaEntera([Id, AAAA, MM, DD]),
    persona(Id1, _Nombre1),
    Id1 is Id.

positivo(6,fecha(2020,6,7)).
positivo(6,fecha(2020,6,21)).
positivo(1,fecha(2020,6,12)).
positivo(3,fecha(2020,6,12)).
positivo(9,fecha(2020,6,29)).
positivo(9,fecha(2020,7,5)).
positivo(2,fecha(2020,6,16)).
positivo(2,fecha(2020,6,26)).
positivo(4,fecha(2020,6,18)).

%------------------------------------------------------------------------
negativo(Id, fecha(AAAA, MM, DD)) :- 
    entradaEntera([Id, AAAA, MM, DD]),
    persona(Id1, _Nombre1),
    Id1 is Id.

negativo(4,fecha(2020,7,5)).
negativo(6,fecha(2020,6,25)).
negativo(11,fecha(2020,6,9)).
negativo(1,fecha(2020,6,9)).
negativo(1,fecha(2020,6,26)).
negativo(3,fecha(2020,6,7)).
negativo(9,fecha(2020,6,14)).
negativo(2,fecha(2020,7,1)).
negativo(7,fecha(2020,6,20)).
negativo(7,fecha(2020,6,27)).