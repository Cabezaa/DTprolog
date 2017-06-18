%Sin Podar
crearArbol(Atributos,Datos):-
    id3( Atributos, Datos, ArbolSalida ),print("SALI EXITOSO"),!,
    assert(ArbolSalida), print("Se ha creado el arbol correctamente"),nl.

utilizarArbol(DatosTest1):-
    nb_setval(contador, 0),
    nb_setval(acumFilas, 0),
    clasificar(DatosTest1),!,
    length(DatosTest1, TamTest1),
    nb_getval(contador, CAux),
    Exactitud is CAux/TamTest1,
    nl,
    print('###################'),nl,
    print('la exactitud es: '), print(Exactitud), nl,
    print('###################'),nl.

incrementar :- nb_getval(contador, C), CNew is C + 1, nb_setval(contador, CNew).
incrementarFilas :- nb_getval(acumFilas, C), CNew is C + 1, nb_setval(acumFilas, CNew).
imprimirContador:- print("Contador actual: "), nb_getval(contador, C), print(C),nl.
imprimirFilas:- print("##############################################################FILAS!!!: "), nb_getval(acumFilas, C), print(C),nl.

recorrer(Algo):-
    foreach(member(A, Algo), (algo)).

algo:-
    print(2),
    print(3),nl.

printArbol():-
    tree(X,Z),
    print(X),nl,
    nl,nl,nl,nl,nl,nl,
    print(Z).

clasificar([]).
clasificar([ (ValorReal, AtributosValores) | Filas]) :-
    incrementarFilas,
    imprimirFilas,
    foreach(member((Atributo, Valor), AtributosValores ), (
        auxiliar(Atributo,Valor,AtributosValores,ValorReal)
    )),
    clasificar(Filas). %Aca pasar la suma

auxiliar(Atributo,Valor,AtributosValores,ValorReal):-
    print("Entre con el Valor"),nl,print(Valor),nl,
       (tree(raiz(Atributo), RamasDeAtributo) % Ramas del Atributo que estamos recorriendo
       ->
           %Atributo verdadro - Recorrer hermanos.
           (member( (Valor-tree(raiz(R),RamasDeR)), RamasDeAtributo ) % Por cada Rama
           -> print("V"),nl,print(Valor),nl,print("lo logre"),nl,
               clasificarAux(R,RamasDeR, AtributosValores,ValorReal)
               %Aca sumar
           ;  print("la fila a clasificar tiene valores falsos"))
       ;
       print("LLEGUE ACA")).

clasificarAux(_AtributoPadre, [_-hoja(ValorReal)], _AtributosValores, ValorReal):-
    incrementar, imprimirContador,
    print("El valor de la hoja es: "),print(ValorReal),nl.

clasificarAux(_AtributoPadre, [_-hoja(_C)], _AtributosValores, _ValorReal).


% AtributosValores: [(gpa,3.7),(univ,top_10), (publ,yes),(rec,good)]
% (publ, listaSubArbol, )
clasificarAux(AtributoPadre, RamasDePadre, AtributosValores, ValorReal):-
    print("Entre de vuelta"),nl,
    member( (AtributoPadre, ValorPadre), AtributosValores ) %Encontamos el atributo de la fila a clasificar para conocer su valor.
    -> (
        print("Entre con el atributo: "), print(AtributoPadre),print(-),print(ValorPadre),nl,
        (member( (ValorPadre-tree(raiz(R),RamasDeR)), RamasDePadre )
        -> print("encontre la sub rama que buscaba"),nl,print(ValorPadre),nl,print("Raiz "),print(R),nl,
            clasificarAux(R,RamasDeR,AtributosValores,ValorReal)
        ;print("ENTRE AL CASO QUE TENGO DOS HIJOS O MAS"),nl,
        print("AtributoPadre"),print(AtributoPadre),nl,
        print("ValorPadre"),print(ValorPadre),nl,
        print("RamasDePadre"),print(RamasDePadre),nl,
        print("AtributosValores"),print(AtributosValores),nl,
        (member( ValorPadre-hoja(Hoja), RamasDePadre )
        ->
            print("ENTRE AL MEMBER ESPECIAL"),nl,
            clasificarAux(AtributoPadre,[ValorPadre-hoja(Hoja)],AtributosValores,ValorReal),
            print("Volvi del caso especial de varias hojas"),nl,imprimirContador

            ;
                print("#####################################No se puede clasificar esa fila!"),nl
            )
        )
    ).

%Caso todos los datos del SubDatos es tienen la misma clasificacion
id3( _, Data, Tree ) :-
  mismaCategoria( Data, Categ ),!,
  Tree = hoja( Categ).

%Caso sin variables
id3( [], [ (Categ,_) | _Datos ], Tree ):-Tree = hoja( Categ).

id3( Atributos, Datos, ArbolSalida ) :-
print("Entre en ID3"),nl,
  buscarRaiz( Atributos, Datos, BestAttr, BestDataPartition ), print("Busque la raiz"),nl,
	subtract(Atributos,[(BestAttr,_)],NuevaLista),
	print("ESTAMOS QUITANDO A: "),
	print(BestAttr),nl,
  generarSubArboles( NuevaLista, BestDataPartition, ChildrenTrees ),
  ArbolSalida = tree( raiz( BestAttr ), ChildrenTrees ).

%Metodo auxiliar para calcular si todas las tuplas tienen la misma clasificacion
  mismaCategoria( [ ], _ ).
  mismaCategoria( [ (Categ,_) | MoreData ], Categ ) :-
    mismaCategoria( MoreData, Categ ).

% Devuelve el mejor atributo y la mejor particion de datos
buscarRaiz( ListaAtributos, Datos, MejorAtributo, MejorParticion ) :-
  print("ENTRE A BUSCAR RAIZ"),nl,
  print("Mi lista de atributos es "),print(ListaAtributos),nl,
  findall( (Atributo, Particion, Entropia),                                                     % Template utilizado
           (print("Encontre el Atributo :"),print(Atributo),nl, member( ( Atributo, Valores ), ListaAtributos ),
           print("Encontre el Atributo :"),print(Atributo),nl,                                 % For Eeach sobre la lista, creando particiones
           print("Valores :"),print(Valores),nl,                                 % For Eeach sobre la lista, creando particiones
           particionamiento( Datos, Atributo, Valores, Particion, Entropia ),
           print("############################################"),nl,
           print("Atributo "), print(Atributo),print(" Entr: "),print(Entropia),nl ), ParticionesTemplate),  % Asignamos segun el template
           cantYes(Datos, CantidadYes),
           length(Datos,CantDatos),
           EntropiaGeneral is CantidadYes/CantDatos,                                          % Calculamos la Entropia General para calcular la ganancia
           nth0(0, ParticionesTemplate, PrimerParticion),                                       % Asignamos a la primera particion como una auxiliar para el calc
           seleccionarMejorGanancia(EntropiaGeneral, ParticionesTemplate, PrimerParticion, MejorAtributo, MejorParticion).

%Caso base: La entropia de una lista vacia es igual a 0.
particionamiento( _, _, [ ] , [ ], 0 ).
% Devuelve una Particion y su entropia
particionamiento( Datos, Atributo, [ OnePosAttrValue | OtrosValores ], Particion, Entropia ) :-
  filasValorAtributo( Datos, Atributo, OnePosAttrValue, DatosAtributoValor ),
  ( DatosAtributoValor = [ ] -> particionamiento( Datos, Atributo, OtrosValores , Particion, Entropia )
    ;
    entropia( DatosAtributoValor, SubEntropy ), % Sexo masculino
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"),print("SubEntropy de "),print(Atributo),print(" ES "),print(SubEntropy),nl,
    length(Datos,FilasTotales),
    particionamiento( Datos, Atributo, OtrosValores , RestPartition, AcumuladorEntropia ),
    Particion = [ OnePosAttrValue-DatosAtributoValor | RestPartition ],

    %Calculamos la Entropia
    length(DatosAtributoValor,FilasCalculadas),
    NuevaEntropia is -(FilasCalculadas/FilasTotales) * SubEntropy,
    Entropia is NuevaEntropia + AcumuladorEntropia ).


%devuelve todas las FILAS que contienen el par (atributo,valor). Ej: (sexo,masculino)


%Caso base: La particion de una lista vacia y de cualquier atributo, es una particion vacia.
filasValorAtributo( [ ], _, _, [ ] ).

% caso recursivo. V es el valor de la califaicion (positivo o negativo) y datum es la fila (con todos los valores de los atributos digamo)
filasValorAtributo( [ (V,Dat) | OtrosDatos ], Atributo, Valor, SubData ) :-
  member( (Atributo,Valor), Dat ) %ForEach sobre la fila
  -> filasValorAtributo( OtrosDatos, Atributo, Valor, MoreSubData ),
     SubData = [ (V,Dat) | MoreSubData ]
  ;  filasValorAtributo( OtrosDatos, Atributo, Valor, SubData ).

% Ej: 20 filas de sexo masculino.
% Pnum = 14. Pp = 14/20. Pn = 6/20.
% PpLogPn =


entropia( Datos, SubEntropia ) :-
  cantYes( Datos, CantYes ),
  length( Datos, Longitud ),
  Yes is CantYes / Longitud,
  No is 1 - Yes,
  (Yes > 0 ->
  LogYes is Yes*(log10(Yes)/log10(2)) ;
  LogYes = 0 ),
  (No > 0 ->
      LogNo is No*(log10(No)/log10(2)),
      SubEntropia is - ( LogYes + LogNo )
      ;
      LogNo = 0,
      SubEntropia is - ( LogYes + LogNo )).

cantYes( [ ], 0 ).
cantYes( [ (y,_) | More ], Pnum ) :- !,
  cantYes( More, Pnum1 ), Pnum is Pnum1 + 1.
cantYes( [ (n,_) | More ], Pnum ) :- cantYes( More, Pnum ).


%Calculo de cual tiene mejor Ganancia, para elejir el mejor atributo.

%Caso Base
seleccionarMejorGanancia(EntropiaGeneral, [], (AtributoAux, ParticionAux, _EntropiaAux), AtributoAux, ParticionAux).

%Caso Recursivo. Si la Ganancia1 es Mejor, entonces actualizamos nuestro auxiliar y seguimos buscando. Sino, buscamos con el mismo auxiliar.
seleccionarMejorGanancia(EntropiaGeneral, [(Atributo, Particion, Entropia) | ParticionesRestantes],
(AtributoAux, ParticionAux, EntropiaAux), SaleAtributo, SaleParticion):-


    print("Atributo entrante "),print(Atributo),nl,
    print("ENTROPIA GENERAL entrante "),print(EntropiaGeneral),nl,
    %print("EntropiaAux "),print(EntropiaAux),nl,
    Ganancia1 is EntropiaGeneral - Entropia,
    Ganancia2 is EntropiaGeneral - EntropiaAux,
    print("LA Ganancia1 "),print(Ganancia1),nl,
    print("LA Ganancia2 "),print(Ganancia2),nl,

    (Ganancia1 > Ganancia2 -> (print("Ganancia1 "),print(Ganancia1), print(" Atributo "),print(Atributo),nl,
    seleccionarMejorGanancia(EntropiaGeneral, ParticionesRestantes, (Atributo, Particion, Entropia), SaleAtributo, SaleParticion ))
    ; (print("Ganancia2 "),print(Ganancia2), print(" Atributo "),print(AtributoAux),nl,
     seleccionarMejorGanancia(EntropiaGeneral, ParticionesRestantes, (AtributoAux, ParticionAux, EntropiaAux), SaleAtributo, SaleParticion ))).



generarSubArboles( _, [ ], [ ] ).

generarSubArboles(
         ListaAtributos, [ Valor-SubData | OtrosDatos ], Hijos ) :-
  id3( ListaAtributos, SubData, ArbolHijo ),
  generarSubArboles( ListaAtributos, OtrosDatos, OtrosArboles ),
  Hijos = [ Valor-ArbolHijo | OtrosArboles ].
