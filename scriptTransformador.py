
#14 atributos + 1 clasificaicon!
def main():
    nuevaLinea = ""
    i = 0
    archivoEntrada = open("entrada6000lineas.txt", "r")
    archivoSalida = open("salida600Age.txt","r+")
    for linea in archivoEntrada.readlines():
        arreglo = linea.split(',')
        nuevaLinea = nuevaLinea + '('
        if arreglo[14] == " <=50K\n":
            nuevaLinea = nuevaLinea + 'n,['
        if arreglo[14] == " >50K\n":
            nuevaLinea = nuevaLinea + 'y,['



        for i in range(0, 14):
            if i==13:
                x = arreglo[i]
                #print(arreglo[i])
                if x == " United-States":
                    nacional = "nacional"
                else:
                    nacional = "extranjero"
                nuevaLinea = nuevaLinea + '(pais,'+nacional+')]'
            elif i==12:
                x = int(arreglo[i])
                if x < 25:
                    horas = "medio-tiempo"
                elif  x < 40 and x >= 25:
                    horas = "tiempo-completo"
                elif x >= 40:
                    horas = "excesivo"
                nuevaLinea = nuevaLinea + '(horas-por-semana,'+horas+'),'
            elif i==11:
                x = int(arreglo[i])
                if x == 0:
                    tieneInversiones = "si"
                else:
                    tieneInversiones = "no"
                nuevaLinea = nuevaLinea + '(capital-loss,'+tieneInversiones+'),'
            elif i==10:
                x = int(arreglo[i])
                if x == 0:
                    tieneInversiones = "si"
                else:
                    tieneInversiones = "no"
                nuevaLinea = nuevaLinea + '(capital-gain,'+tieneInversiones+'),'
            elif i==9:
                nuevaLinea = nuevaLinea + '(sexo,'+arreglo[i]+'),'
            elif i==8:
                nuevaLinea = nuevaLinea + '(raza,'+arreglo[i]+'),'
            elif i==7:
                nuevaLinea = nuevaLinea + '(relacion,'+arreglo[i]+'),'
            elif i==6:
                nuevaLinea = nuevaLinea + '(ocupacion,'+arreglo[i]+'),'
            elif i==5:
                nuevaLinea = nuevaLinea + '(marital-status,'+arreglo[i]+'),'
            #elif i==4:
                #nuevaLinea = nuevaLinea + '(education-num,'+arreglo[i]+'),'
            elif i==3:
                nuevaLinea = nuevaLinea + '(educacion,'+arreglo[i]+'),'
            #elif i==2:
                # x = int(arreglo[i])
                # if x < 100000:
                #     weight = "<100000"
                # elif x >= 100000 and x < 200000:
                #     weight = "<200000"
                # elif x >= 200000 and x < 300000:
                #     weight = "<300000"
                # elif x >= 300000 and x < 400000:
                #     weight = "<400000"
                # elif x >= 400000 and x < 500000:
                #     weight = "<500000"
                # elif x >= 500000 and x < 600000:
                #     weight = "<600000"
                # elif x >= 600000 and x < 700000:
                #     weight = "<700000"
                # elif x >= 700000 and x < 800000:
                #     weight = "<800000"
                # elif x >= 800000 and x < 900000:
                #     weight = "<900000"
                # elif x >= 900000 and x < 1000000:
                #     weight = "<1000000"
                # elif x >= 1000000 :
                #     weight = ">=800000"
                # nuevaLinea = nuevaLinea + '(fnlwgt,'+weight+'),'

            elif i==1:
                nuevaLinea = nuevaLinea + '(workclass,'+arreglo[i]+'),'
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
                nuevaLinea = nuevaLinea + '(edad,'+age+'),'
        nuevaLinea = nuevaLinea + '),' + '\n'

    archivoSalida.write(nuevaLinea)


main()
