import Arbol.*

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

@main
def main(): Unit = {
  def casos_prueba(): Unit =

    println("Creando un árbol con hojas ('S', 4), ('O', 3), ('E', 2), (' ', 2):")
    val arbol_osos: ArbolHuffman = ramaHuff(hojaHuff('S', 4), ramaHuff(hojaHuff('O', 3), ramaHuff(hojaHuff('E', 2), hojaHuff(' ', 2))))
    println("Peso del árbol: " + arbol_osos.peso)
    println("Codificación del string 'ESO ES OSOS': " + arbol_osos.codificar("ESO ES OSOS"))

    val osos: String = "ESO ES OSOS"

    println("\nIntentando crear un árbol a partir del string 'ESO ES OSOS' con la función crearArbolHuffman: ")
    val arbol_funcion: ArbolHuffman = crearArbolHuffman(osos)
    println(arbol_funcion)

    println("Intentando crear un árbol a partir del string 'ESO ES OSOS' con el constructor del companion object: ")
    val arbol_companion: ArbolHuffman = ArbolHuffman(osos)
    println(arbol_companion)

    val cadena_vacia: String = ""
    println("Intentando crear un árbol a partir de una cadena vacía: ")
    val arbol_vacio: ArbolHuffman = ArbolHuffman(cadena_vacia)

    println("\nCodificando el texto 'ESO ES OSOS' a partir de su árbol:")
    val listaBitsOsos: List[Bit] = arbol_osos.codificar(osos)
    println(listaBitsOsos)

    println("Intentando codificar el texto 'HOLA' a partir del árbol anterior:")
    val hola: String = "HOLA"
    println(arbol_osos.codificar(hola))

    println("Intentando codificar una cadena vacía a partir del árbol anterior:")
    println(arbol_osos.codificar(cadena_vacia))

    println("\nDecodificando la lista de bits del texto anterior a partir de su árbol:")
    println(arbol_osos.decodificar(listaBitsOsos))

    println("Intentando decodificar una lista vacía a partir del árbol anterior:")
    val lista_vacia: List[Bit] = List()
    println(arbol_osos.decodificar(lista_vacia))


    println("\nCreando una tabla a partir del árbol anterior:")
    val tablaOsos: TablaCodigos = deArbolATabla(arbol_osos)
    println(tablaOsos)

    println("\nCreando una función que codifique a partir de la tabla anterior:")
    val codificaTabla: String => List[Bit] = codificar(tablaOsos)

    println("Codificando el texto 'ESO ES OSOS' a partir de la función anterior:")
    println(codificaTabla(osos))


    println("Intentando codificar el texto 'HOLA' a partir de la función anterior:")
    println(codificaTabla(hola))


    println("Intentando codificar una cadena vacía a partir de la función anterior:")
    println(codificaTabla(cadena_vacia))


    println("\nCreando una función que decodifique a partir de la tabla anterior:")
    val decodificaTabla: List[Bit] => String = decodificar(tablaOsos)

    println("Decodificando la lista de bits correspondiente al texto 'ESO ES OSOS' a partir de la función anterior:")
    println(decodificaTabla(listaBitsOsos))

    println("Intentando decodificar una lista vacía a partir de la función anterior:")
    println(decodificaTabla(lista_vacia))
    menu_principal()

  def arbol_personalizado(): Unit =

    def stringToBits(cadena: String): Option[List[Bit]] = {
      val bits: List[Option[Bit]] = cadena.toList.map {
        case '0' => Some(0: Bit)
        case '1' => Some(1: Bit)
        case _ => None
      }

      if (bits.contains(None)) None else Some(bits.flatten)
    }

    def codificar_tabla(tabla: TablaCodigos, arbol: ArbolHuffman): Unit =
      println("Introduce el texto a codificar:")
      val texto: String = scala.io.StdIn.readLine()
      val texto_codificado: String = codificar(tabla)(texto).mkString("[", "", "]")
      println(s"El texto codificado es: $texto_codificado")
      menu_tabla(tabla, arbol)

    def decodificar_tabla(tabla: TablaCodigos, arbol: ArbolHuffman): Unit =
      println("Introduce los bits a decodificar (formato todo junto, solo unos y ceros):")
      val bits: List[Bit] = stringToBits(scala.io.StdIn.readLine()).getOrElse(Nil)

      val texto_decodificado: String = decodificar(tabla)(bits).mkString("[", "", "]")
      println(s"El texto decodificado es: $texto_decodificado")
      menu_tabla(tabla, arbol)

    @tailrec
    def menu_tabla(tabla: TablaCodigos, arbol: ArbolHuffman): Unit =
      println("Elige qué hacer con la tabla:")
      println("1 - Codificar un texto")
      println("2 - Decodificar un texto")
      println("3 - Volver")
      val res = Try(scala.io.StdIn.readLine().toInt)
      res match
        case Failure(exception) => println("Por favor, introduce una opción válida\n"); menu_tabla(tabla, arbol)
        case Success(value) => val opc_tabla: Int = value
          opc_tabla match
            case 1 => codificar_tabla(tabla, arbol)
            case 2 => decodificar_tabla(tabla, arbol)
            case 3 => menu_arbol(arbol)
            case _ => println("Por favor, introduce una opción válida\n"); menu_tabla(tabla, arbol)


    def mostrar_atributos(arbol: ArbolHuffman): Unit =
      println(s"Peso total: ${arbol.peso}   Caracteres: ${arbol.caracteres}\nArbol: $arbol")
      menu_arbol(arbol)

    def codificar_arbol(arbol: ArbolHuffman): Unit =
      println("Introduce el texto a codificar:")
      val texto: String = scala.io.StdIn.readLine()
      val texto_codificado: String = arbol.codificar(texto).mkString("[", "", "]")
      println(s"El texto codificado es: $texto_codificado")
      menu_arbol(arbol)

    def decodificar_arbol(arbol: ArbolHuffman): Unit =
      println("Introduce los bits a decodificar (formato todo junto, solo unos y ceros):")
      val bits: List[Bit] = stringToBits(scala.io.StdIn.readLine()).getOrElse(Nil)

      val texto_decodificado: String = arbol.decodificar(bits).mkString("[", "", "]")
      println(s"El texto decodificado es: $texto_decodificado")
      menu_arbol(arbol)


    @tailrec
    def menu_arbol(arbol: ArbolHuffman): Unit =
      println("Elige qué hacer con el árbol:")
      println("1 - Ver atributos")
      println("2 - Codificar otro texto")
      println("3 - Decodificar una lista de bits con el árbol")
      println("4 - Crear una tabla de datos a partir del árbol")
      println("5 - Volver")
      val res = Try(scala.io.StdIn.readLine().toInt)
      res match
        case Failure(exception) => println("Por favor, introduce una opción válida\n"); menu_arbol(arbol)
        case Success(value) => val opc_arbol: Int = value
          opc_arbol match
            case 1 => mostrar_atributos(arbol)
            case 2 => codificar_arbol(arbol)
            case 3 => decodificar_arbol(arbol)
            case 4 => menu_tabla(deArbolATabla(arbol), arbol)
            case 5 => menu_principal()
            case _ => println("Por favor, introduce una opción válida\n"); menu_arbol(arbol)

    println("Introduce el texto a partir del cual se creará el árbol:")
    val texto: String = scala.io.StdIn.readLine()
    val arbol: ArbolHuffman = crearArbolHuffman(texto)
    val texto_codificado: String = arbol.codificar(texto).mkString("[", "", "]")
    println(s"El texto codificado es: $texto_codificado")
    println(s"El árbol es: $arbol")
    menu_arbol(arbol)


  @tailrec
  def menu_principal(): Unit =
    println("Herramienta para codificación de texto con árboles de Huffman")
    println("Introduce el número correspondiente a la opción deseada:")
    println("1 - Ver casos de prueba\n2 - Crear árbol personalizado\n3 - Salir")

    val res = Try(scala.io.StdIn.readLine().toInt)
    res match
      case Failure(exception) => println("Por favor, introduce una opción válida\n"); menu_principal()
      case Success(value) => val opc_menu_ppal: Int = value
        opc_menu_ppal match
          case 1 => casos_prueba()
          case 2 => arbol_personalizado()
          case 3 => ()
          case _ => println("Por favor, introduce una opción válida\n"); menu_principal()

  menu_principal()
}

