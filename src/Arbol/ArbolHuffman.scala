package Arbol

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

type Bit = 0 | 1


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

def ListaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] =
  listaChar.groupBy(identity).map { case (key, caracteres) =>
    (key, caracteres.size)
  }.toList

def DistribFrecAListaHojas(frec: List[(Char, Int)]): List[hojaHuff] =
  frec.map { case (char, peso) => hojaHuff(char, peso) }.sortBy(_.peso)

def creaRamaHuff(izq: ArbolHuffman, dch: ArbolHuffman): ramaHuff =
  ramaHuff(izq, dch)

def combinar(nodos: List[ArbolHuffman]): List[ArbolHuffman] = nodos match
  case h :: Nil => nodos
  case h :: t => {
    creaRamaHuff(h, t.head) :: t.tail
  }.sortBy(_.peso)


//def crearArbolHuffman(cadena: String): ArbolHuffman =

case class hojaHuff(caracter: Char, pes: Int) extends ArbolHuffman

case class ramaHuff(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) extends ArbolHuffman



@main
def main(): Unit= {
  val arbol: ArbolHuffman = ramaHuff(hojaHuff('S', 4), ramaHuff(hojaHuff('O', 3), ramaHuff(hojaHuff('E', 2), hojaHuff(' ', 2))))
  println(arbol.peso)
  println(arbol.codificar("ESO ES OSOS"))

  val chars: List[Char] = List('a', 'a', 'b', 'a', 'c', 'b')
  val distFrec: List[(Char, Int)] = ListaCharsADistFrec(chars)
  val listaHojas: List[hojaHuff] = DistribFrecAListaHojas(distFrec)
  val combinado1: List[ArbolHuffman] = combinar(listaHojas)
  val combinado2: List[ArbolHuffman] = combinar(combinado1)
  println(combinado2)
}

