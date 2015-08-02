package exteff

object IntReader extends Reader[Int]
object StrReader extends Reader[String]

object Main {

  import Union._

  def main(args: Array[String]): Unit = {

    val ma = Eff("hello")

    val mb = for {
      v1 <- ma
      _= println("[1]: value="+ v1)    

      v2 <- IntReader.ask
      _= println("[2]: value="+ v2)
      
      v3 <- Eff("<<" + v2 + ":" + v1 + ">>")
      _= println("[3]: value="+ v3)

      v4 <- IntState.get
      _= println("[4]: value="+ v4)

      v5 <- IntReader.ask
      _= println("[5]: value="+ v5)

      _  <- IntState.set(2929)
      
      v6 <- IntReader.ask
      _= println("[6]: value="+ v6)

      v7 <- IntState.get
    } yield v7 * v7

    /*
    val mc = Eff("---")
      .flatMap(b =>
        Eff(println("[3]: b="+ b))
        .flatMap { _ => StrReader.ask }
        .flatMap(v => Eff(println("[4]: v="+ v)))
        .flatMap { _ => IntReader.ask }
        .map(v => v + " + " + b)
      )
    */

    println("done:"+ Eff.run(
      StrReader.run(
        IntState.run(
          IntReader.run(mb, 2525), 213), "(^_^;)")
    ))

    println("done:"+ Eff.run(
      IntReader.run(
        StrReader.run(IntReader.ask, "(^_^)")
      , 8)
    ))
  }

}
