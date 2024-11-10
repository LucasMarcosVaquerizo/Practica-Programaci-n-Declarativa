package Arbol

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Success, Failure}

type Bit = 0 | 1
type TablaCodigos = List[(Char, List[Bit])]

trait ArbolHuffman {

  //Calcula el peso total de las hojas del arbol(La frecuencia de todos los caracteres sumados).
  def peso: Int = this match
    case hojaHuff(caracter: Char, peso: Int) => peso
    case ramaHuff(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) => nodoIzq.peso + nodoDch.peso


  //Retorna una lista con todos los caracteres que contiene el árbol.
  def caracteres: List[Char] =
    def caracteresAux(arbol: ArbolHuffman, lista: List[Char]): List[Char] = arbol match
      case hojaHuff(caracter: Char, peso: Int) => caracter :: lista
      case ramaHuff(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) => caracteresAux(nodoIzq, lista) ::: caracteresAux(nodoDch, lista)

    caracteresAux(this, Nil)


  //Convierte un String en una lista de caracteres.
  private def cadenaAListaChars(cadena: String): List[Char] =
    cadena.toList


  //Convierte una lista de caracteren en un String.
  private def listaCharsACadena(listaCar: List[Char]): String =
    listaCar.mkString


  //Pasa de una lista de bits a un string, pasando por las ramas del arbol para sacar cada caracter.
  def decodificar(bits: List[Bit]): String =
    @tailrec
    def decodificarAux(arbol: ArbolHuffman, bits: List[Bit], listaChar: List[Char]): List[Char] = arbol match
        case hojaHuff(caracter: Char, peso: Int) => decodificarAux(this, bits, listaChar :+ caracter)
        case ramaHuff(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) => bits match
          case Nil => listaChar
          case lista => bits.head match
            case 0 => decodificarAux(nodoIzq, bits.tail, listaChar)
            case 1 => decodificarAux(nodoDch, bits.tail, listaChar)

    listaCharsACadena(decodificarAux(this, bits, Nil))


  //Comprueba si el árbol contiene un caracter, útil para más adelante.
  private def arbolContiene(caracter: Char): Boolean =
    def arbolContieneAux(arbol: ArbolHuffman, caracter: Char, cond: Boolean): Boolean = arbol match
      case hojaHuff(c, _) => c == caracter
      case ramaHuff(nodoIzq, nodoDch) => nodoIzq.arbolContiene(caracter) || nodoDch.arbolContiene(caracter)

    arbolContieneAux(this, caracter, false)


  //Pasa de un String a una lista de bists utilizando el árbol para la conversión.
  def codificar(cadena: String): List[Bit] =
    @tailrec
    def codificarAux(arbol: ArbolHuffman, listaChar: List[Char], listaBits: List[Bit]): List[Bit] = arbol match
      case hojaHuff(c, _) if c != listaChar.head => println(s"Warning: El caracter ${listaChar.head} de la cadena no está en el árbol"); Nil
      case hojaHuff(c, _)
        if c == listaChar.head && listaChar.tail != Nil => codificarAux(this, listaChar.tail, listaBits)
      case hojaHuff(c, _) if c == listaChar.head => listaBits
      case ramaHuff(nodoIzq, nodoDch) if !nodoIzq.arbolContiene(listaChar.head) && !nodoDch.arbolContiene(listaChar.head) => println(s"Warning: El caracter ${listaChar.head} de la cadena no está en el árbol"); Nil
      case ramaHuff(nodoIzq, _) if nodoIzq.arbolContiene(listaChar.head) => codificarAux(nodoIzq, listaChar, 0 :: listaBits)
      case ramaHuff(_, nodoDch) if nodoDch.arbolContiene(listaChar.head) => codificarAux(nodoDch, listaChar, 1 :: listaBits)

    cadena match
      case "" => Nil
      case c => codificarAux(this, cadenaAListaChars(cadena), Nil).reverse
}
end ArbolHuffman

//Constructor del árbol.
object ArbolHuffman {
  def apply(cadena: String): ArbolHuffman = crearArbolHuffman(cadena)
}

case class hojaHuff(caracter: Char, pes: Int) extends ArbolHuffman

case class ramaHuff(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) extends ArbolHuffman


def crearArbolHuffman(cadena: String): ArbolHuffman =

  //Pasa de una lista de caracteres(facil de conseguir con .toList) a una lista de duplas que contiene cada caracter y su frecuencia.
  def ListaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] =
    listaChar.groupBy(identity).map { case (key, caracteres) => //La orden .groupBy(identity).map agrupa cada caracter con una lista de todos los caracteres iguales a el
      (key, caracteres.size) //Despues se cuenta el número de elementos en cada lista asociada a cada caracter en el Map.
    }.toList //Por útimo se pasa el Mapa a una lista de duplas(La deseada)

  //Pasa la lista de duplas(de la función anterior) a una lista de hojas que tienen asociado los caracteres y las frecuencias de la lista original.
  def DistribFrecAListaHojas(frec: List[(Char, Int)]): List[hojaHuff] =
    frec.map { case (char, peso) => hojaHuff(char, peso) }.sortBy(_.peso)

  //Constructor de la ramaHuff, facilita el código más adelante.
  def creaRamaHuff(izq: ArbolHuffman, dch: ArbolHuffman): ramaHuff =
    ramaHuff(izq, dch)

  //Crea la lista de ramas a partir de la lista de hojas creada por la función ya vista.
  def combinar(nodos: List[ArbolHuffman]): List[ArbolHuffman] = nodos match
    case Nil => throw RuntimeException("La lista a combinar no tiene elementos")
    case h :: Nil => nodos
    case h :: t => { //Se ordena la lista de árboles(hojas y ramas que existan hasta ahora) de menor a mayor.
      creaRamaHuff(h, t.head) :: t.tail //Se crea una rama con los dos elementos de menor peso.
    }.sortBy(_.peso) //Se vuelve a ordenar la lista de menor a mayor.

  //Compureba que la lista de árboles(ramas y hojas) solo tiene un elementos(Condición de parada).
  def esListaSingleton(lista: List[ArbolHuffman]): Boolean = lista match
    case Nil => throw RuntimeException("La lista a comprobar no tiene elementos")
    case h :: Nil => true
    case h :: t => false

  //Combina las funciones anteriores para crear la totalidad del arbol.
  def repetirHasta(combinar: List[ArbolHuffman] => List[ArbolHuffman], esListaSingleton: List[ArbolHuffman] => Boolean)(listaHojas: List[hojaHuff]): ArbolHuffman =
    @tailrec
    def repetirHastaAux(combinar: List[ArbolHuffman] => List[ArbolHuffman], esListaSingleton: List[ArbolHuffman] => Boolean)(listaNodos: List[ArbolHuffman]): ArbolHuffman = listaNodos match
      case Nil => throw new NoSuchElementException("La cadena está vacía, no se puede crear el árbol")
      case h :: t if esListaSingleton(listaNodos) => listaNodos.head
      case h :: t if !esListaSingleton(listaNodos) => repetirHastaAux(combinar, esListaSingleton)(combinar(listaNodos))

    repetirHastaAux(combinar, esListaSingleton)(listaHojas)

  try
    repetirHasta(combinar, esListaSingleton)(DistribFrecAListaHojas(ListaCharsADistFrec(cadena.toList)))
  catch
    case e: Exception =>
      println(e)
      hojaHuff('\u0000', 0) //Si se intenta crear un arbol con un caracter vacío(No un espacio) se retorna un arbol con una hoja que contiene el caracter nulo ('\u0000') y peso 0.

//Crea la tabla de caracteres y listas de bits(los que correspondan a cada carcater dependiendo de como esté construido el árbol).
def deArbolATabla(arbol: ArbolHuffman): TablaCodigos = arbol match
  case hojaHuff(char, _) => (char, Nil) :: Nil
  case ramaHuff(izq, dcha) => deArbolATabla(izq).map {
    case (c, bits) => (c, 0 :: bits: List[Bit])
  } ::: deArbolATabla(dcha).map {
    case (c, bits) => (c, 1 :: bits: List[Bit])
  }


//Encuentra la cadena de bits asociados a un caracter en una tabla de códigos.
def codificar(tabla: TablaCodigos)(cadena: String): List[Bit] =

  //Lee los bits que corresponden a cada caracter depeniendo de lo contenido en la tabla.
  @tailrec
  def bitsDeCaracter(tabla: TablaCodigos)(char: Char): List[Bit] = tabla match
    case Nil => Nil
    case (c, bits) :: t if c == char => bits
    case (c, bits) :: t if c != char => bitsDeCaracter(t)(char)

  @tailrec
  def codificarAux(tabla: TablaCodigos, chars_restantes: List[Char], bits_resultado: List[Bit]): List[Bit] = chars_restantes match
    case Nil => bits_resultado
    case h :: t => codificarAux(tabla, t, bits_resultado ::: bitsDeCaracter(tabla)(h))

  val resultado = Try(codificarAux(tabla, cadena.toList, Nil))
  resultado match
    case Success(valor) => valor
    case Failure(exception) => println(exception); Nil

//Convierte una cadena de bits en un string, leyendo cada caracter asiciado a su código de bits en una determinada tabla.
def decodificar(tabla: TablaCodigos)(bits: List[Bit]): String =

  //Comprueba si una lista de bits se corresponde con la lista de bits asociada a algún caracter en la tabla de Códigos.(Esta función será útil más adelante).
  @tailrec
  def caracterDeBits(tabla: TablaCodigos)(bits: List[Bit]): Char = tabla match
    case Nil => '\u0000'
    case (c, b) :: t if b == bits => c
    case (c, b) :: t if b != bits => caracterDeBits(t)(bits)
    
  @tailrec
  def decodificarAux(tabla: TablaCodigos, bits_restantes: List[Bit], chars_resultado: List[Char], bits_palabra: List[Bit]): List[Char] = bits_restantes match
    case Nil => chars_resultado
    case h :: t if caracterDeBits(tabla)(bits_palabra :+ h) == '\u0000' => decodificarAux(tabla, t, chars_resultado, bits_palabra :+ h)
    case h :: t if caracterDeBits(tabla)(bits_palabra :+ h) != '\u0000' => decodificarAux(tabla, t, chars_resultado :+ caracterDeBits(tabla)(bits_palabra :+ h), Nil)

  val resultado = Try(decodificarAux(tabla, bits, Nil, Nil).mkString)
  resultado match
    case Success(valor) => valor
    case Failure(exception) => println(exception); ""

import Arbol.*

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

@main
def main(): Unit = {
  def casos_prueba(): Unit =

    println("Creando un árbol con hojas ('S', 4), ('O', 3), ('E', 2), (' ', 2):")
    val arbol_osos: ArbolHuffman = ramaHuff(hojaHuff('S', 4), ramaHuff(hojaHuff('O', 3), ramaHuff(hojaHuff('E', 2), hojaHuff(' ', 2))))
    println("Peso del árbol: " + arbol_osos.peso)
    println("\nCodificación del string 'ESO ES OSOS': " + arbol_osos.codificar("ESO ES OSOS"))

    val osos: String = "ESO ES OSOS"

    println("\nIntentando crear un árbol a partir del string 'ESO ES OSOS' con la función crearArbolHuffman: ")
    val arbol_funcion: ArbolHuffman = crearArbolHuffman(osos)
    println(arbol_funcion)

    println("\nIntentando crear un árbol a partir del string 'ESO ES OSOS' con el constructor del companion object: ")
    val arbol_companion: ArbolHuffman = ArbolHuffman(osos)
    println(arbol_companion)

    val cadena_vacia: String = ""
    println("\nIntentando crear un árbol a partir de una cadena vacía: ")
    val arbol_vacio: ArbolHuffman = ArbolHuffman(cadena_vacia)

    println("\nCodificando el texto 'ESO ES OSOS' a partir de su árbol:")
    val listaBitsOsos: List[Bit] = arbol_osos.codificar(osos)
    println(listaBitsOsos)

    println("\nIntentando codificar el texto 'HOLA' a partir del árbol anterior:")
    val hola: String = "HOLA"
    println(arbol_osos.codificar(hola))

    println("\nIntentando codificar una cadena vacía a partir del árbol anterior:")
    println(arbol_osos.codificar(cadena_vacia))

    println("\nDecodificando la lista de bits del texto anterior a partir de su árbol:")
    println(arbol_osos.decodificar(listaBitsOsos))

    println("\nIntentando decodificar una lista vacía a partir del árbol anterior:")
    val lista_vacia: List[Bit] = List()
    println(arbol_osos.decodificar(lista_vacia))


    println("\nCreando una tabla a partir del árbol anterior:")
    val tablaOsos: TablaCodigos = deArbolATabla(arbol_osos)
    println(tablaOsos)

    println("\nCreando una función que codifique a partir de la tabla anterior:")
    val codificaTabla: String => List[Bit] = codificar(tablaOsos)

    println("Codificando el texto 'ESO ES OSOS' a partir de la función anterior:")
    println(codificaTabla(osos))


    println("\nIntentando codificar el texto 'HOLA' a partir de la función anterior:")
    println(codificaTabla(hola))


    println("\nIntentando codificar una cadena vacía a partir de la función anterior:")
    println(codificaTabla(cadena_vacia))


    println("\nCreando una función que decodifique a partir de la tabla anterior:")
    val decodificaTabla: List[Bit] => String = decodificar(tablaOsos)

    println("Decodificando la lista de bits correspondiente al texto 'ESO ES OSOS' a partir de la función anterior:")
    println(decodificaTabla(listaBitsOsos))

    println("\nIntentando decodificar una lista vacía a partir de la función anterior:")
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
      bits match
        case Nil => println("Por favor, introduce una lista de bits válida\n")
        case _ =>
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
      bits match
        case Nil => println("Por favor, introduce una lista de bits válida\n")
        case _ =>
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

