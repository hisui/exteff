package exteff

object Main {

  import Union._

  def main(args: Array[String]): Unit = {

    val ma = Eff("hello")

    val mb = for {
      v1 <- ma
      _= println("[1]: value="+ v1)    

      v2 <- Reader.ask[Int]
      _= println("[2]: value="+ v2)
      
      v3 <- Eff("<<" + v2 + ":" + v1 + ">>")
      _= println("[3]: value="+ v3)

      v4 <- Reader.ask[java.lang.String]
      _= println("[4]: value="+ v4)

      v5 <- State.get[Int]
      _= println("[5]: value="+ v5)

      _  <- State.set(2929)

      v6 <- Reader.ask[java.lang.String]
      _= println("[6]: value="+ v6)

      v7 <- State.get[Int]
    } yield v7 * v7
    
    println("done:"+ Eff.run(
      State.run(
        Reader.run(Reader.run(mb, "wow"), 888), 213)
    ))

    /*
    println("done:"+ Eff.run(
      Reader.run(
        Reader.ask[Int]
      , 8 : Int)
    ))
    */

  }

}
