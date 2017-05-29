%Probando el codigo de PL

%Ejemplos Atributos
% [(gpa, [4.0,3.7,3.5] ),(univ, [top_10,top_20,top_30] ),(publ, [yes,no] ),(rec, [good,normal] )]

%Ejemplos Datos
%[(p, [(gpa,4.0),(univ,top_10), (publ,yes),(rec,good)] ),(n, [(gpa,4.0),(univ,top_10), (publ,no),(rec,good)] )]


%Sin Podar
inicializar(Atributos, Datos, ArbolSalida, DatosTest1):-
    id3( Atributos, Datos, ArbolSalida ),
    print(ArbolSalida),
    assert(ArbolSalida),!,
    nb_setval(contador, 0),
    clasificar(DatosTest1),!,
    print("AL FINAL "), % BORRAR
    imprimirContador, %BORRAR
    length(DatosTest1, TamTest1),
    nb_getval(contador, CAux),
    Exactitud is CAux/TamTest1,
    print("La exactitud es: "), print(Exactitud).

incrementar :- nb_getval(contador, C), CNew is C + 1, nb_setval(contador, CNew). 
imprimirContador:- print("Contador actual: "), nb_getval(contador, C), print(C),nl.

id3( [], [ (Categ,_) | MoreData ] ):-Tree = leaf( Categ).


clasificar([]).   

clasificar([ (ValorReal, AtributosValores) | Filas]) :-
    (member( (Atributo, Valor), AtributosValores ) %ForEach sobre la fila
    -> print("Entre con el Valor"),nl,print(Valor),nl,
        (tree(raiz(Atributo), RamasDeAtributo) % Ramas del Atributo que estamos recorriendo
        -> 
            %Atributo verdadro - Recorrer hermanos.
            (member( (Valor-tree(raiz(R),RamasDeR)), RamasDeAtributo ) % Por cada Rama
            -> print("V"),nl,print(Valor),nl,print("lo logre"),print(R),nl,
                clasificarAux(R,RamasDeR, AtributosValores,ValorReal)
                %Aca sumar
            ;  print("la fila a clasificar tiene valores falsos")))),
        
    clasificar(Filas). %Aca pasar la suma
    


clasificarAux(AtributoPadre, [_-leaf(ValorReal)], AtributosValores, ValorReal):-
    incrementar, imprimirContador,
    print("El valor de la hoja es: "),print(ValorReal),nl.
    
clasificarAux(AtributoPadre, [_-leaf(C)], AtributosValores, ValorReal).


% AtributosValores: [(gpa,3.7),(univ,top_10), (publ,yes),(rec,good)]
% (publ, listaSubArbol, )
clasificarAux(AtributoPadre, RamasDePadre, AtributosValores, ValorReal):- 
    member( (AtributoPadre, ValorPadre), AtributosValores ) %Encontamos el atributo de la fila a clasificar para conocer su valor.
    -> (member( (ValorPadre-tree(raiz(R),RamasDeR)), RamasDePadre )
    -> print("encontre la sub rama que buscaba"),nl,print(ValorPadre),nl,print("Raiz "),print(R),nl,print(RamasDeR),nl),
        clasificarAux(R,RamasDeR,AtributosValores,ValorReal).


clasificar41([ (ValorReal, AtributosValores) | Filas]) :-
    (member( (Atributo, Valor), AtributosValores ) %ForEach sobre la fila
    -> print("Entre con el Valor"),nl,print(Valor),nl,
        (tree(raiz(Atributo), RamasDeAtributo) % Ramas del Atributo que estamos recorriendo
        -> 
            %Atributo verdadro - Recorrer hermanos.
            (member( (Valor-tree(raiz(R),RamasDeR)), RamasDeAtributo ) % Por cada Rama
            -> print("V"),nl,print(Valor),nl,print("lo logre"),print(R),nl,
                clasificarAux(R,RamasDeR, AtributosValores)
                
            ;  print("la fila a clasificar tiene valores falsos")))),
        
    clasificar(Filas).

        
clasificarAux23(AtributoPadre, [_-leaf(C)], AtributosValores):-print("El valor de la hoja es: "),print(C),nl.
        
% AtributosValores: [(gpa,3.7),(univ,top_10), (publ,yes),(rec,good)]
% (publ, listaSubArbol, )
clasificarAux34(AtributoPadre, RamasDePadre, AtributosValores):-  
    member( (AtributoPadre, ValorPadre), AtributosValores ) %Encontamos el atributo de la fila a clasificar para conocer su valor.
    -> (member( (ValorPadre-tree(raiz(R),RamasDeR)), RamasDePadre )
    -> print("encontre la sub rama que buscaba"),nl,print(ValorPadre),nl,print("Raiz "),print(R),nl,print(RamasDeR),nl),
        clasificarAux(R,RamasDeR,AtributosValores).













clasificar1([ (ValorReal, AtributosValores) | Filas]) :-
    member( (Atributo, Valor), AtributosValores ) %ForEach sobre la fila
    -> (tree(raiz(Atributo),[Valor-tree(raiz(A),B) | Algo]),print(Atributo),nl,print(A),nl,print(B),!
    -> print("mache"),nl ;
     print("no match"),nl).

clasificar2([ (ValorReal, AtributosValores) | Filas]) :-
    member( (Atributo, Valor), AtributosValores ) %ForEach sobre la fila
    -> tree(raiz(Atributo),[Valor-tree(raiz(A),B) | Algo]),print(Atributo),nl,print(A),nl,print(B),!.
   
   
id3( [], [ (Categ,_) | MoreData ], Tree ):-Tree = leaf( Categ).


id3( Atributos, Datos, ArbolSalida ) :-
  buscarRaiz( Atributos, Datos, BestAttr, BestDataPartition ),
	subtract(Atributos,[(BestAttr,_)],NuevaLista),
	print("ESTAMOS QUITANDO A: "),
	print(BestAttr),nl,
  generarSubArboles( NuevaLista, BestDataPartition, ChildrenTrees ),
  ArbolSalida = tree( raiz( BestAttr ), ChildrenTrees ).


           
% Devuelve el mejor atributo y la mejor particion de datos
buscarRaiz( ListaAtributos, Datos, MejorAtributo, MejorParticion ) :-
  findall( (Atributo, Particion, Entropia),                                                     % Template utilizado
           ( member( ( Atributo, Valores ), ListaAtributos ),                                   % For Eeach sobre la lista, creando particiones
           partition( Datos, Atributo, Valores, Particion, Entropia ) ), ParticionesTemplate),  % Asignamos segun el template
           cantPos(Datos, CantPositivos),
           length(Datos,CantDatos),
           EntropiaGeneral is CantPositivos/CantDatos,                                          % Calculamos la Entropia General para calcular la ganancia
           nth0(0, ParticionesTemplate, PrimerParticion),                                       % Asignamos a la primera particion como una auxiliar para el calc
           seleccionarMejorGanancia(EntropiaGeneral, ParticionesTemplate, PrimerParticion, MejorAtributo, MejorParticion).

%Caso base: La entropia de una lista vacia es igual a 0.
partition( _, _, [ ] , [ ], 0 ).
% Devuelve una Particion y su entropia
partition( Data, Attr,
           [ OnePosAttrValue | RestValues ] , Partition, Entropy ) :-
  filasValorAtributo( Data, Attr, OnePosAttrValue, SubData ),
  ( SubData = [ ]
    -> partition( Data, Attr, RestValues , Partition, Entropy )
    ;
    entropia( SubData, SubEntropy ),
    length(Data,FilasTotales),
    partition( Data, Attr, RestValues , RestPartition, RestEntropy ),
    Partition = [ OnePosAttrValue-SubData | RestPartition ],
    length(SubData,FilasCalculadas),
    NuevaEntropia is (FilasCalculadas/FilasTotales) * SubEntropy,
    Entropy is NuevaEntropia + RestEntropy ).


%Select devuelve todas las FILAS que contienen el par (atributo,valor). Ej: (sexo,masculino)


%Caso base: La particion de una lista vacia y de cualquier atributo, es una particion vacia.
filasValorAtributo( [ ], _, _, [ ] ).

% caso recursivo. V es el valor de la califaicion (positivo o negativo) y datum es la fila (con todos los valores de los atributos digamo)
filasValorAtributo( [ (V,Datum) | MoreData ], Attr, AttrValue, SubData ) :-
  member( (Attr,AttrValue), Datum ) %ForEach sobre la fila
  -> filasValorAtributo( MoreData, Attr, AttrValue, MoreSubData ),
     SubData = [ (V,Datum) | MoreSubData ]
  ;  filasValorAtributo( MoreData, Attr, AttrValue, SubData ).

% Ej: 20 filas de sexo masculino.
% Pnum = 14. Pp = 14/20. Pn = 6/20.
% PpLogPn =


entropia( Data, Entropy ) :-
  cantPos( Data, Pnum ),
  length( Data, Dnum ),
  Pp is Pnum / Dnum,
  Pn is 1 - Pp,
  (Pp > 0 ->
  PpLogPp is Pp*(log10(Pp)/log10(2)) ;
  PpLogPp = 0 ),
  (Pn > 0 ->
  PnLogPn is Pn*(log10(Pn)/log10(2)),Entropy is - ( PpLogPp + PnLogPn ) ;
  PnLogPn = 0 , Entropy is - ( PpLogPp + PnLogPn )).



cantPos( [ ], 0 ).
cantPos( [ (p,_) | More ], Pnum ) :- !,
  cantPos( More, Pnum1 ), Pnum is Pnum1 + 1.
cantPos( [ (n,_) | More ], Pnum ) :- cantPos( More, Pnum ).



select_minimal_entropy(
    [ (Attr, Partition, Entropy ) | MorePartitions ],
    BestAttr, BestPartition ):-
  select_minimal_entropy_aux( MorePartitions, (Attr, Partition, Entropy),
                              BestAttr, BestPartition ).

%Caso base
select_minimal_entropy_aux( [ ], (Attr, Partition, _), Attr, Partition ).

%Caso si el nuevo es mejor que el anterior: reemplazamos el anterior por el nuevo
select_minimal_entropy_aux(
       [ (Attr1, Partition1, Entropy1) | MorePartitions ],
       ( _, _, Entropy), BestAttr, BestPartition ) :-
  Entropy1 < Entropy , !,
  select_minimal_entropy_aux(
      MorePartitions, (Attr1, Partition1, Entropy1), BestAttr, BestPartition ).

%Caso si el nuevo es peor que el anterior: quitamos el nuevo
select_minimal_entropy_aux(
       [ _ | MorePartitions ],
       (Attr, Partition, Entropy), BestAttr, BestPartition ) :-
  select_minimal_entropy_aux(
       MorePartitions, (Attr, Partition, Entropy), BestAttr, BestPartition ).


%Calculo de cual tiene mejor Ganancia, para elejir el mejor atributo.
       
%Caso Base
seleccionarMejorGanancia(EntropiaGeneral, [], (AtributoAux, ParticionAux, EntropiaAux), AtributoAux, ParticionAux).
       
%Caso Recursivo. Si la Ganancia1 es Mejor, entonces actualizamos nuestro auxiliar y seguimos buscando. Sino, buscamos con el mismo auxiliar.
seleccionarMejorGanancia(EntropiaGeneral, [(Atributo, Particion, Entropia) | ParticionesRestantes],
(AtributoAux, ParticionAux, EntropiaAux), SaleAtributo, SaleParticion):-
    Ganancia1 is EntropiaGeneral - Entropia,
    Ganancia2 is EntropiaGeneral - EntropiaAux,
    Ganancia1 > Ganancia2 -> 
    seleccionarMejorGanancia(EntropiaGeneral, ParticionesRestantes, (Atributo, Particion, Entropia), SaleAtributo, SaleParticion )
    ; seleccionarMejorGanancia(EntropiaGeneral, ParticionesRestantes, (AtributoAux, ParticionAux, EntropiaAux), SaleAtributo, SaleParticion ).
    


generarSubArboles( _, [ ], [ ] ).

generarSubArboles(
         AttrList, [ Value-SubData | MoreData ], ChildrenTrees ) :-
  id3( AttrList, SubData, ChildTree ),
  generarSubArboles( AttrList, MoreData, MoreTrees ),
  ChildrenTrees = [ Value-ChildTree | MoreTrees ].
