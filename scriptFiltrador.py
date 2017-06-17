
#14 atributos + 1 clasificaicon!
def main():
    nuevaLinea = ""
    i = 0
    archivoEntrada = open("entrada6000lineas.txt", "r")
    archivoSalida = open("6000lineasTotalesReducido.txt","r+")
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
                nuevaLinea = nuevaLinea + arreglo[i]+', '
            elif i==7:
                nuevaLinea = nuevaLinea +arreglo[i]+', '
            elif i==6:
                nuevaLinea = nuevaLinea + arreglo[i]+', '
            elif i==5:
                nuevaLinea = nuevaLinea +arreglo[i]+', '
            #elif i==4:
                #nuevaLinea = nuevaLinea + '(education-num,'+arreglo[i]+'),'
            elif i==3:
                nuevaLinea = nuevaLinea + arreglo[i]+', '

            elif i==1:
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
    archivoSalida.write(nuevaLinea)



main()
