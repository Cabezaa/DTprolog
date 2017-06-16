%Sin Podar
crearArbol(Atributos,Datos):-
    id3( Atributos, Datos, ArbolSalida ),print("SALI EXITOSO"),!,
    assert(ArbolSalida), print("Se ha creado el arbol correctamente"),nl.

utilizarArbol(DatosTest1):-
    nb_setval(contador, 0),
    clasificar(DatosTest1),!,
    length(DatosTest1, TamTest1),
    nb_getval(contador, CAux),
    Exactitud is CAux/TamTest1,
    print("La exactitud es: "), print(Exactitud), nl.

incrementar :- nb_getval(contador, C), CNew is C + 1, nb_setval(contador, CNew).
imprimirContador:- print("Contador actual: "), nb_getval(contador, C), print(C),nl.

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



clasificarAux(_AtributoPadre, [_-leaf(ValorReal)], _AtributosValores, ValorReal):-
    incrementar, imprimirContador,
    print("El valor de la hoja es: "),print(ValorReal),nl.

clasificarAux(_AtributoPadre, [_-leaf(_C)], _AtributosValores, _ValorReal).


% AtributosValores: [(gpa,3.7),(univ,top_10), (publ,yes),(rec,good)]
% (publ, listaSubArbol, )
clasificarAux(AtributoPadre, RamasDePadre, AtributosValores, ValorReal):-
    member( (AtributoPadre, ValorPadre), AtributosValores ) %Encontamos el atributo de la fila a clasificar para conocer su valor.
    -> (member( (ValorPadre-tree(raiz(R),RamasDeR)), RamasDePadre )
    -> print("encontre la sub rama que buscaba"),nl,print(ValorPadre),nl,print("Raiz "),print(R),nl,print(RamasDeR),nl),
        clasificarAux(R,RamasDeR,AtributosValores,ValorReal).

id3( [], [ (Categ,_) | _Datos ], Tree ):-Tree = leaf( Categ).


id3( Atributos, Datos, ArbolSalida ) :-
print("Entre en ID3"),nl,
  buscarRaiz( Atributos, Datos, BestAttr, BestDataPartition ), print("Busque la raiz"),nl,
	subtract(Atributos,[(BestAttr,_)],NuevaLista),
	print("ESTAMOS QUITANDO A: "),
	print(BestAttr),nl,
  generarSubArboles( NuevaLista, BestDataPartition, ChildrenTrees ),
  ArbolSalida = tree( raiz( BestAttr ), ChildrenTrees ).



% Devuelve el mejor atributo y la mejor particion de datos
buscarRaiz( ListaAtributos, Datos, MejorAtributo, MejorParticion ) :-
  %print("ENTRE A BUSCAR RAIZ"),nl,
  findall( (Atributo, Particion, Entropia),                                                     % Template utilizado
           ( member( ( Atributo, Valores ), ListaAtributos ),
           %print("Atributo :"),print(Atributo),nl,                                 % For Eeach sobre la lista, creando particiones
           %print("Valores :"),print(Valores),nl,                                 % For Eeach sobre la lista, creando particiones
           partition( Datos, Atributo, Valores, Particion, Entropia ),
           print("############################################"),nl,
           print("Atributo "), print(Atributo),print(" Entr: "),print(Entropia),nl ), ParticionesTemplate),  % Asignamos segun el template

           %print("Sali EXITOSO del find all"),nl,
           %print("Datos: "),print(Datos),nl,
           cantPos(Datos, CantPositivos),
           length(Datos,CantDatos),
          % print("CantPositivos: "),print(CantPositivos),nl,
          % print("CantDatos :"),print(CantDatos),nl,
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
    entropia( SubData, SubEntropy ), % Sexo masculino
    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"),print("SubEntropy de "),print(Attr),print(" ES "),print(SubEntropy),nl,
    length(Data,FilasTotales),
    partition( Data, Attr, RestValues , RestPartition, RestEntropy ),
    Partition = [ OnePosAttrValue-SubData | RestPartition ],
    length(SubData,FilasCalculadas),
    NuevaEntropia is -(FilasCalculadas/FilasTotales) * SubEntropy,
    Entropy is NuevaEntropia + RestEntropy ).


%Select devuelve todas las FILAS que contienen el par (atributo,valor). Ej: (sexo,masculino)


%Caso base: La particion de una lista vacia y de cualquier atributo, es una particion vacia.
filasValorAtributo( [ ], _, _, [ ] ).

% caso recursivo. V es el valor de la califaicion (positivo o negativo) y datum es la fila (con todos los valores de los atributos digamo)
filasValorAtributo( [ (V,Datum) | OtrosDatos ], Attr, AttrValue, SubData ) :-
  member( (Attr,AttrValue), Datum ) %ForEach sobre la fila
  -> filasValorAtributo( OtrosDatos, Attr, AttrValue, MoreSubData ),
     SubData = [ (V,Datum) | MoreSubData ]
  ;  filasValorAtributo( OtrosDatos, Attr, AttrValue, SubData ).

% Ej: 20 filas de sexo masculino.
% Pnum = 14. Pp = 14/20. Pn = 6/20.
% PpLogPn =


entropia( Data, Entropy ) :-
  cantPos( Data, Pnum ),
  length( Data, Dnum ),
  %print("Longitud "), print(Dnum),nl,
  Pp is Pnum / Dnum,
  Pn is 1 - Pp,
  %print("PP "), print(Pp),nl,
  %print("Pn "), print(Pn),nl,
  (Pp > 0 ->
  PpLogPp is Pp*(log10(Pp)/log10(2)) ;
  PpLogPp = 0 ),
  (Pn > 0 ->
      PnLogPn is Pn*(log10(Pn)/log10(2)),
      %print("#### PnLogPn es "),print(PnLogPn),nl,
      %print("#### PpLogPp es "),print(PpLogPp),nl,
      Entropy is - ( PpLogPp + PnLogPn )
      ;
      PnLogPn = 0,
      %print("#### PnLogPn es "),print(PnLogPn),nl,
      %print("#### PpLogPp es "),print(PpLogPp),nl,
      Entropy is - ( PpLogPp + PnLogPn )).



cantPos( [ ], 0 ).
cantPos( [ (y,_) | More ], Pnum ) :- !,
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
         AttrList, [ Value-SubData | OtrosDatos ], ChildrenTrees ) :-
  id3( AttrList, SubData, ChildTree ),
  generarSubArboles( AttrList, OtrosDatos, MoreTrees ),
  ChildrenTrees = [ Value-ChildTree | MoreTrees ].
