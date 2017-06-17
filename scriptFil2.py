
#14 atributos + 1 clasificaicon!
def main():
    nuevaLinea = ""
    i = 0
    archivoEntrada = open("entrada6000lineas.txt", "r")
    archivoSalida = open("filtradoConNuestrosAtributosWEKA.txt","r+")
    print("Antes de entrar al for")
    for linea in archivoEntrada.readlines():
        arreglo = linea.split(',')
        #nuevaLinea = nuevaLinea + '('
        for i in range(0, 14):
            if i==13:
                x = arreglo[i]
                #print(arreglo[i])
                if x == " United-States":
                    nacional = "nacional"
                else:
                    nacional = "extranjero"
                nuevaLinea = nuevaLinea + nacional+', '
            elif i==12:
                x = int(arreglo[i])
                if x < 25:
                    horas = "medio-tiempo"
                elif  x < 40 and x >= 25:
                    horas = "tiempo-completo"
                elif x >= 40:
                    horas = "excesivo"
                nuevaLinea = nuevaLinea +horas+', '
            elif i==11:
                x = int(arreglo[i])
                if x == 0:
                    tieneInversiones = "si"
                else:
                    tieneInversiones = "no"
                nuevaLinea = nuevaLinea +tieneInversiones+', '
            elif i==10:
                x = int(arreglo[i])
                if x == 0:
                    tieneInversiones = "si"
                else:
                    tieneInversiones = "no"
                nuevaLinea = nuevaLinea + tieneInversiones+', '
            elif i==9:
                nuevaLinea = nuevaLinea +arreglo[i]+', '
            elif i==8:
                if(arreglo[i] != " Amer-Indian-Eskimo"):
                    nuevaLinea = nuevaLinea + arreglo[i]+', '
            elif i==7:
                nuevaLinea = nuevaLinea +arreglo[i]+', '
            elif i==6:
                if(arreglo[i] == " Adm-clerical") or (arreglo[i] == " Craft-repair") or (arreglo[i] == " Exec-managerial") or (arreglo[i] == " Machine-op-inspct") or (arreglo[i] == " Other-service") or (arreglo[i] == " Tech-support") or (arreglo[i] == " Prof-specialty") or (arreglo[i] == " Sales") or (arreglo[i] == " Handlers-cleaners"):
                    nuevaLinea = nuevaLinea + arreglo[i]+', '
            elif i==5:
                if(arreglo[i] == " Divorced") or (arreglo[i] == " Married-civ-spouse") or (arreglo[i] == " Never-married") or (arreglo[i] == " Separated"):
                    nuevaLinea = nuevaLinea +arreglo[i]+', '
            elif i==3:
                if(arreglo[i] == " HS-grad") or (arreglo[i] == " Some-college") or (arreglo[i] == " Bachelors") or (arreglo[i] == " Masters") or (arreglo[i] == " Doctorate"):
                    nuevaLinea = nuevaLinea + arreglo[i]+', '

            elif i==1:
                if (arreglo[i] == " Federal-gov") or (arreglo[i] == " Private") or (arreglo[i] == " Self-emp-not-inc") or (arreglo[i] == " State-gov"):
                    nuevaLinea = nuevaLinea + arreglo[i]+', '
            elif i == 0:
                x = int(arreglo[i])
                if x < 25:
                    age = "joven"
                else:
                    if x < 50 and x >= 25:
                        age = "edad-media"
                    elif x >= 50 and x < 65:
                        age = "adulto"
                    else:
                        age = "anciano"
                nuevaLinea = nuevaLinea + age+', '

        if arreglo[14] == " <=50K\n":
            nuevaLinea = nuevaLinea + 'n\n'
        if arreglo[14] == " >50K\n":
            nuevaLinea = nuevaLinea + 'y\n'

    print("antes de escribir")
    archivoSalida.write(nuevaLinea)
    print("despues de escribir")


main()
