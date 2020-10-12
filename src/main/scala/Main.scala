import java.util.NoSuchElementException


object EjsScala{

  /*
    Devuelve true si el primer argumento es igual al primer elemento del segundo argumento
    @returns Boolean
   */
  def firstp[Objeto](x: Objeto, y: List[Objeto]): Boolean =
    (x, y) match {
      case (x, z::tail) if (x==z) => true
      case (x, z::Nil) if x==z => true
      case (x, _) if x == Nil => false
      case (_, Nil) => throw new NoSuchElementException()
  }

  /*
     @returns Devuelve una lista cuyos elementos son lista de dos elementos
   */

  def duplicar[Objeto](ls: List[Objeto]): List[Objeto] =
    ls match {
      // case ls.isEmpty() => Nil Por qué así se la pega?
      case ls if ls.isEmpty => Nil
      // case head::tail => List(head, head)::duplicar(tail)
      case head::tail => head::head::duplicar(tail)
  }

  /*
  @params Toma como argumento un N >0
  @returns Una lista de enteros de N hasta 1
  */
  def countdown(i: Int) : List[Int] =
  i match{
    //case i<=0 => List()
    case i if i<=0 => List()
    case _ => i::countdown(i-1)
  }

  /*
  @params Toma como argumento una lista
  @returns devuelve la lista invertida
  */
  def reverso[Objeto](ls: List[Objeto]) : List[Objeto] =
    ls match {
      case Nil => Nil
      case head::tail if tail.isEmpty => ls
      case head::tail => reverso(tail):::List(head)
    }

/*
@params Toma como argumento una lista ls y dos isntances x e y
@returns devuelve una lista en la que todas las apareiciones del elemento y se sustituyen por una x
*/
  def substitute[Objeto](x: Objeto, y: Objeto, ls: List[Objeto]) : List[Objeto] =
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
  def setequal[Objeto](A: List[Objeto], B: List[Objeto]) : Boolean =
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
  def isContained[Objeto](A: List[Objeto], B: List[Objeto]) : Boolean =
    (A, B) match {
      case (headA::tailA, _) if tailA.isEmpty => B.contains(headA)
      case (_::tailA,_) => isContained(tailA, B)
    }



  def main(args: Array[String]): Unit = {
    println("1. firstp")
    println("firstp('a', List('a', 'b', 'c'))")
    println(firstp('a', List('a', 'b', 'c')))
    // println(firstp[String]("martes", List("lunes", "martes", "miercoles")))

    println("2. duplicar")
    println("duplicar[Char](List('a'))")
    println(duplicar[Char](List('a')))
    println("duplicar[Char](List('a', 'b', 'c'))")
    println(duplicar[Char](List('a', 'b', 'c')))

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
  }
}