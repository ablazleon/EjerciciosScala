import java.util.NoSuchElementException
// Author: Adrián Blázquez León

object EjsScala{

  /*
    Devuelve true si el primer argumento es igual al primer elemento del segundo argumento
    @returns Boolean
   */
  def firstp[Any](x: Any, y: List[Any]): Boolean =
    (x) match {
      case x if x == y.head => true
      case _ => false
    }



  /*
     @returns Devuelve una lista cuyos elementos son lista de dos elementos
   */

  def duplicar(ls: List[Any]): List[Any] =
    ls match {
      // case ls.isEmpty() => Nil Por qué así se la pega?
      case ls if ls.isEmpty => Nil
      // case head::tail => List(head, head)::duplicar(tail)
      case _ => ls.map(a => List(a,a))
  }

  /*
  @params Toma como argumento un N >0
  @returns Una lista de enteros de N hasta 1
  */
  def countdown(i: Int) : List[Int] =
  i match{
    //case i<=0 => List()
    case i if i<0 => List()
    case _ => i::countdown(i-1)
  }

  /*
  @params Toma como argumento una lista
  @returns devuelve la lista invertida
  */
  def reverso[Any](ls: List[Any]) : List[Any] =
    ls match {
      case Nil => Nil
      case head::tail if tail.isEmpty => ls
      case head::tail => reverso(tail):::List(head)
    }

/*
@params Toma como argumento una lista ls y dos isntances x e y
@returns devuelve una lista en la que todas las apareiciones del elemento y se sustituyen por una x
*/
  def substitute[Any](x: Any, y: Any, ls: List[Any]) : List[Any] =
    (x, y, ls) match {
      case (_, _,Nil) => ls
      case (Nil,_,_) => ls
      case (_,Nil,_) => ls
      case (x, y, head::tail) if (head == y) => List(x):::substitute(x, y, tail)
      case (_, _, head::tail) => List(head):::substitute(x, y, tail)
    }



  /*
 @params toma como argumento dos listas
 @returns devuelve true si todos los elementos de una lista son las del otro y viceversa
 */
  def setequal[Any](A: List[Any], B: List[Any]) : Boolean =
    (A, B) match {
      case (_,Nil) => true
      case (Nil,_) => true
      case (A, B) if isContained(A, B) => isContained(B, A)
      case (_,_) => false // No está contenido A en B
    }

  /*
  @params toma como argumento dos listas
  @returns devuelve true si todos los elementos de una lista son las del otro
  */
  def isContained[Any](A: List[Any], B: List[Any]) : Boolean =
    (A, B) match {
      case (headA::tailA, _) if tailA.isEmpty => B.contains(headA)
      case (_::tailA,_) => isContained(tailA, B)
    }

  /*
  @params toma como argumento una lista
  @returns devuelve una lista formada por los elementos en las posiciones impares de la lista original
  */
  def impares[Any](A: List[Any]) : List[Any] =
    A match {
      case Nil => Nil
      case _::Nil => A
      case head::_::Nil => List(head)
      case head::_::tail => List(head):::impares(tail)
    }


  def main(args: Array[String]): Unit = {
    println("1. firstp")
    println("firstp('a', List('a', 'b', 'c'))")
    println(firstp('a', List('a', 'b', 'c')))
    println("firstp(martes, List(lunes, martes, miercoles))")
    println(firstp("martes", List("lunes", "martes", "miercoles")))

    println("2. duplicar")
    println("duplicar(List('a'))")
    println(duplicar(List('a')))
    println("duplicar(List('a', 'b', 'c'))")
    println(duplicar(List('a', 'b', 'c')))

    println("3. countdown")
    println("countdown(10)")
    println(countdown(10))
    println("countdown(-1)")
    println(countdown(-1))

    println("4. reverse")
    println("reverso(List(1,2,3))")
    println(reverso(List(1,2,3)))

    println("5. substitute")
    println("substitute('a', 'b', List('a', 'b', 'c'))")
    println(substitute('a', 'b', List('a', 'b', 'c')))
    println("substitute('a', 'd', List('a', 'b', 'c'))")
    println(substitute('a', 'd', List('a', 'b', 'c')))

    println("6. setequal")
    println("setequal(List(1, 1, 2), List(1, 2))")
    println(setequal(List(1, 1, 2), List(1, 2)))
    println("setequal(List(1, 2), List(1, 2, 3))")
    println(setequal(List(1, 2), List(1, 2, 3)))
    println("setequal(List(1, 2, 3), List(1, 2))")
    println(setequal(List(1, 2, 3), List(1, 2)))
    println("setequal(List('a', 1), List(1, 'a'))")
    println(setequal(List('a', 1), List(1, 'a')))

    println("7. impares")
    println("impares(List(1))")
    println(impares(List(1)))
    println("impares(List(1, 2, 3, 4, 5, 6))")
    println(impares(List(1, 2, 3, 4, 5, 6)))

  }
}