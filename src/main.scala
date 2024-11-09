import Arbol.*

@main
def main(): Unit = {
  def prueba(): Unit = 
    
    println("Creando un árbol con hojas ('S', 4), ('O', 3), ('E', 2), (' ', 2):")
    val arbol_osos: ArbolHuffman = ramaHuff(hojaHuff('S', 4), ramaHuff(hojaHuff('O', 3), ramaHuff(hojaHuff('E', 2), hojaHuff(' ', 2))))
    println("Peso del árbol: " + arbol_osos.peso)
    println("Codificación del string 'ESO ES OSOS': " + arbol_osos.codificar("ESO ES OSOS"))
    
    val osos: String = "ESO ES OSOS"
    
    println("Intentando crear un árbol a partir del string 'ESO ES OSOS' con la función crearArbolHuffman: ")
    val arbol_funcion: ArbolHuffman = crearArbolHuffman(osos)
    println(arbol_funcion)
    
    println("Intentando crear un árbol a partir del string 'ESO ES OSOS' con el constructor del companion object: ")
    val arbol_companion: ArbolHuffman = ArbolHuffman(osos)
    println(arbol_companion)

    val cadena_vacia: String = ""
    println("Intentando crear un árbol a partir de una cadena vacía: ")
    val arbol_vacio: ArbolHuffman = ArbolHuffman(cadena_vacia)

    println("Codificando el texto 'ESO ES OSOS' a partir de su árbol:")
    val listaBitsOsos: List[Bit] = arbol_osos.codificar(osos)
    println(listaBitsOsos)

    println("Intentando codificar el texto 'HOLA' a partir del árbol anterior:")
    val hola: String = "HOLA"
    println(arbol_osos.codificar(hola))

    println("Intentando codificar una cadena vacía a partir del árbol anterior:")
    println(arbol_osos.codificar(cadena_vacia))

    println("Decodificando la lista de bits del texto anterior a partir de su árbol:")
    println(arbol_osos.decodificar(listaBitsOsos))

    println("Intentando decodificar una lista vacía a partir del árbol anterior:")
    val lista_vacia: List[Bit] = List()
    println(arbol_osos.decodificar(lista_vacia))

    println("Intentando decodificar varias listas de bits arbitrarias que no corresponden a ningún texto a partir del árbol anterior:")
    val bitsUnos: List[Bit] = List(1, 1, 1, 1, 1)
    val bitsCeros: List[Bit] = List(0, 0, 0, 0, 0)
    val bitsArbitrarios: List[Bit] = List(1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1)
    println(arbol_osos.decodificar(bitsUnos))
    println(arbol_osos.decodificar(bitsCeros))
    println(arbol_osos.decodificar(bitsArbitrarios))

    println("Creando una tabla a partir del árbol anterior:")
    val tablaOsos: TablaCodigos = deArbolATabla(arbol_osos)
    println(tablaOsos)

    println("Creando una función que codifique a partir de la tabla anterior:")
    val codificaTabla: String => List[Bit] = codificar(tablaOsos)

    println("Codificando el texto 'ESO ES OSOS' a partir de la función anterior:")
    println(codificaTabla(osos))


    println("Intentando codificar el texto 'HOLA' a partir de la función anterior:")
    println(codificaTabla(hola))


    println("Intentando codificar una cadena vacía a partir de la función anterior:")
    println(codificaTabla(cadena_vacia))


    println("Creando una función que decodifique a partir de la tabla anterior:")
    val decodificaTabla: List[Bit] => String = decodificar(tablaOsos)

    println("Decodificando la lista de bits correspondiente al texto 'ESO ES OSOS' a partir de la función anterior:")
    println(decodificaTabla(listaBitsOsos))

    println("Intentando decodificar una lista vacía a partir de la función anterior:")
    println(decodificaTabla(lista_vacia))

    println("Intentando decodificar varias listas de bits arbitrarias que no corresponden a ningún texto a partir de la función anterior:")
    println(decodificaTabla(bitsUnos))
    println(decodificaTabla(bitsCeros))
    println(decodificaTabla(bitsArbitrarios))
  prueba()
}

