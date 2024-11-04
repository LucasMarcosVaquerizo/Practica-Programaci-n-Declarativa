package Arbol

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

type Bit = 0 | 1
type TablaCodigos = List[(Char, List[Bit])]

trait ArbolHuffman {
  def peso: Int = this match
    case hojaHuff(caracter: Char, peso: Int) => peso
    case ramaHuff(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) => nodoIzq.peso + nodoDch.peso

  def caracteres: List[Char] =
    def caracteresAux(arbol: ArbolHuffman, lista: List[Char]): List[Char] = arbol match
      case hojaHuff(caracter: Char, peso: Int) => caracter::lista
      case ramaHuff(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) => caracteresAux(nodoIzq, lista) ::: caracteresAux(nodoDch, lista)
    caracteresAux(this, Nil)

  private def cadenaAListaChars(cadena: String): List[Char] =
    cadena.toList

  private def listaCharsACadena(listaCar: List[Char]): String =
    listaCar.mkString

  def decodificar (bits: List[Bit]): String =
    @tailrec
    def decodificarAux(arbol: ArbolHuffman, bits: List[Bit], listaChar: List[Char]): List[Char] = this match
      case hojaHuff(caracter: Char, peso: Int) => caracter::listaChar
      case ramaHuff(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) => bits.head match
        case 0 => decodificarAux(nodoIzq, bits.tail, listaChar)
        case 1 => decodificarAux(nodoDch, bits.tail, listaChar)

    listaCharsACadena(decodificarAux(this, bits, Nil))

  private def arbolContiene(caracter:Char): Boolean =
    def arbolContieneAux(arbol: ArbolHuffman, caracter: Char, cond: Boolean): Boolean = arbol match
      case hojaHuff(c, _) => c == caracter
      case ramaHuff(nodoIzq, nodoDch) => nodoIzq.arbolContiene(caracter) || nodoDch.arbolContiene(caracter)
      
    arbolContieneAux(this, caracter, false)

  def codificar(cadena: String): List[Bit] =
    @tailrec
    def codificarAux(arbol: ArbolHuffman, listaChar: List[Char], listaBits: List[Bit]): List[Bit] = arbol match
      case hojaHuff(c, _)
        if c == listaChar.head && listaChar.tail != Nil => codificarAux(this, listaChar.tail, listaBits)
      case hojaHuff(c, _) if c == listaChar.head => listaBits
      case ramaHuff(nodoIzq, _) if nodoIzq.arbolContiene(listaChar.head) => codificarAux(nodoIzq, listaChar, 0::listaBits)
      case ramaHuff(_, nodoDch) if nodoDch.arbolContiene(listaChar.head) => codificarAux(nodoDch, listaChar, 1::listaBits)


    codificarAux(this, cadenaAListaChars(cadena), Nil).reverse
}

object ArbolHuffman {
  def apply(cadena: String): ArbolHuffman = crearArbolHuffman(cadena)
}

def crearArbolHuffman(cadena: String): ArbolHuffman =
  def ListaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] =
    listaChar.groupBy(identity).map { case (key, caracteres) =>
      (key, caracteres.size)
    }.toList

  def DistribFrecAListaHojas(frec: List[(Char, Int)]): List[hojaHuff] =
    frec.map { case (char, peso) => hojaHuff(char, peso) }.sortBy(_.peso)

  def creaRamaHuff(izq: ArbolHuffman, dch: ArbolHuffman): ramaHuff =
    ramaHuff(izq, dch)

  def combinar(nodos: List[ArbolHuffman]): List[ArbolHuffman] = nodos match
    case Nil => throw RuntimeException("La lista a combinar no tiene elementos")
    case h :: Nil => nodos
    case h :: t => {
      creaRamaHuff(h, t.head) :: t.tail
    }.sortBy(_.peso)

  def esListaSingleton(lista: List[ArbolHuffman]): Boolean = lista match
    case Nil => throw RuntimeException("La lista a comprobar no tiene elementos")
    case h :: Nil => true
    case h :: t => false

  def repetirHasta(combinar: List[ArbolHuffman] => List[ArbolHuffman], esListaSingleton: List[ArbolHuffman] => Boolean)(listaHojas: List[hojaHuff]): ArbolHuffman =
    @tailrec
    def repetirHastaAux(combinar: List[ArbolHuffman] => List[ArbolHuffman], esListaSingleton: List[ArbolHuffman] => Boolean)(listaNodos: List[ArbolHuffman]): ArbolHuffman = listaNodos match
      case Nil => listaNodos.head
      case h :: t if esListaSingleton(listaNodos) => listaNodos.head
      case h :: t if !esListaSingleton(listaNodos) => repetirHastaAux(combinar, esListaSingleton)(combinar(listaNodos))
    repetirHastaAux(combinar, esListaSingleton)(listaHojas)

  repetirHasta(combinar, esListaSingleton)(DistribFrecAListaHojas(ListaCharsADistFrec(cadena.toList)))


def deArbolATabla(arbol: ArbolHuffman): TablaCodigos = arbol match
  case hojaHuff(char, _) => (char, Nil) :: Nil
  case ramaHuff(izq, dcha) => deArbolATabla(izq).map{
    case (c, bits) => (c, 0 :: bits: List[Bit])
  } ::: deArbolATabla(dcha).map{
    case (c, bits) => (c, 1 :: bits: List[Bit])
  }

@tailrec
def bitsDeCaracter(tabla: TablaCodigos)(char: Char): List[Bit] = tabla match
    case Nil => Nil
    case (c, bits) :: t if c == char => bits: List[Bit]
    case (c, bits) :: t if c != char => bitsDeCaracter(t)(char)

@tailrec
def caracterDeBits(tabla: TablaCodigos)(bits: List[Bit]): Char = tabla match
    case Nil => '\u0000'
    case (c, b) :: t if b == bits => c: Char
    case (c, b) :: t if b != bits => caracterDeBits(t)(bits)


def codificar(tabla: TablaCodigos)(cadena: String): List[Bit] =
  @tailrec
  def codificarAux(tabla: TablaCodigos, chars_restantes: List[Char], bits_resultado: List[Bit]): List[Bit] = chars_restantes match
    case Nil => bits_resultado
    case h :: t => codificarAux(tabla, t, bits_resultado ::: bitsDeCaracter(tabla)(h))

  codificarAux(tabla, cadena.toList, Nil)


def decodificar(tabla: TablaCodigos)(bits: List[Bit]): String =
  @tailrec
  def decodificarAux(tabla: TablaCodigos, bits_restantes: List[Bit], chars_resultado: List[Char], bits_palabra: List[Bit]): List[Char] = bits_restantes match
    case Nil => chars_resultado
    case h :: t if caracterDeBits(tabla)(bits_palabra :+ h) == '\u0000' => decodificarAux(tabla, t, chars_resultado, bits_palabra :+ h)
    case h :: t if caracterDeBits(tabla)(bits_palabra :+ h) != '\u0000' => decodificarAux(tabla, t, chars_resultado :+ caracterDeBits(tabla)(bits_palabra :+ h), Nil)

  decodificarAux(tabla, bits, Nil, Nil).mkString


case class hojaHuff(caracter: Char, pes: Int) extends ArbolHuffman

case class ramaHuff(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) extends ArbolHuffman



@main
def main(): Unit= {
  val arbol: ArbolHuffman = ramaHuff(hojaHuff('S', 4), ramaHuff(hojaHuff('O', 3), ramaHuff(hojaHuff('E', 2), hojaHuff(' ', 2))))
  println(arbol.peso)
  println(arbol.codificar("ESO ES OSOS"))

  val cadena: String = "ESO ES OSOS"
  val tree: ArbolHuffman = crearArbolHuffman(cadena)
  val tree1: ArbolHuffman = ArbolHuffman(cadena)
  println(tree)
  println(tree1)

  val tabla: TablaCodigos = deArbolATabla(tree1)
  println(tabla)

  val bits: List[Bit] = codificar(tabla)(cadena)
  println(bits)

  val osos: String = decodificar(tabla)(bits)
  println(osos)
}

