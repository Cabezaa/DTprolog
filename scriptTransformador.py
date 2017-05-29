
#14 atributos + 1 clasificaicon!
def main():
    nuevaLinea = ""
    i = 0
    archivoEntrada = open("hola2.txt", "r")
    archivoSalida = open("salida2.txt","r+")
    for linea in archivoEntrada.readlines():
        arreglo = linea.split(',')
        nuevaLinea = nuevaLinea + '('
        if arreglo[14] == " <=50K\n":
            nuevaLinea = nuevaLinea + 'n,['
        if arreglo[14] == " >50K\n":
            nuevaLinea = nuevaLinea + 'y,['



        for i in range(0, 14):
            if i==13:
                nuevaLinea = nuevaLinea + '(pais,'+arreglo[i]+')]'
            elif i==12:
                nuevaLinea = nuevaLinea + '(hours-per-week,'+arreglo[i]+'),'
            elif i==11:
                nuevaLinea = nuevaLinea + '(capital-loss,'+arreglo[i]+'),'
            elif i==10:
                nuevaLinea = nuevaLinea + '(capital-gain,'+arreglo[i]+'),'
            elif i==9:
                nuevaLinea = nuevaLinea + '(sex,'+arreglo[i]+'),'
            elif i==8:
                nuevaLinea = nuevaLinea + '(race,'+arreglo[i]+'),'
            elif i==7:
                nuevaLinea = nuevaLinea + '(relationship,'+arreglo[i]+'),'
            elif i==6:
                nuevaLinea = nuevaLinea + '(occupation,'+arreglo[i]+'),'
            elif i==5:
                nuevaLinea = nuevaLinea + '(marital-status,'+arreglo[i]+'),'
            elif i==4:
                nuevaLinea = nuevaLinea + '(education-num,'+arreglo[i]+'),'
            elif i==3:
                nuevaLinea = nuevaLinea + '(education,'+arreglo[i]+'),'
            elif i==2:
                nuevaLinea = nuevaLinea + '(fnlwgt,'+arreglo[i]+'),'

            elif i==1:
                nuevaLinea = nuevaLinea + '(workclass,'+arreglo[i]+'),'
            else:
                nuevaLinea = nuevaLinea + '(age,'+arreglo[i]+'),'
        nuevaLinea = nuevaLinea + '),' + '\n'

    archivoSalida.write(nuevaLinea)


main()
