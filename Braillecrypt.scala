object BraillCrypt {

  val brailleAlphabet = io.Source
                          .fromFile("alphabet.txt")
                          .getLines
                          .drop(1)
                          .map(_ split "\t")
                          .map{case Array(l,b) => l.head -> binToInt(b)}
                          .toMap

  def binToInt(bs:String) = bs.map(_.toString.toInt).reverse.zipWithIndex.foldLeft(0){case (acc,(e,i)) => acc + (e << i)}

  def toBraille = brailleAlphabet

  def fromBraille = brailleAlphabet.to.map(_.swap).toMap

  def decrypt(s:String) = println(s.map(_.toInt).map(fromBraille).mkString)

  def crypt(s:String) = println(s.toUpperCase.map(toBraille).map(_.toChar).mkString)

  def main(av: Array[String]): Unit = av.toList match {
    case "-c" :: words => words foreach crypt
    case "-d" :: words => words foreach decrypt
    case _ => println("usage: bcrypt <-c|-d> [words...]")
  }

}
