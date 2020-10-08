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
      case head::tail => reverso(tail)+:head
    }


  def main(args: Array[String]): Unit = {
    println(firstp[Char]('a', List('a', 'b', 'c')))
    // println(firstp[String]("martes", List("lunes", "martes", "miercoles")))

    println(duplicar[Char](List('a')))
    println(duplicar[Char](List('a', 'b', 'c')))

    println(countdown(10))
    println(countdown(-1))

  }
}