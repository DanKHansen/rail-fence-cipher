object RailFenceCipher:
   def encode(text: String, key: Int): String =
      val ids = (0 until key) ++ (0 until key).reverse.drop(1).init
      text.sliding(ids.size, ids.size).flatMap(_.zip(ids)).toSeq.groupBy(_._2).values.flatten.map(_._1).mkString

   def decode(text: String, key: Int): String =
      val pattern = (1 until key) ++ (key until 1 by -1)
      val rails = (0 until text.length / key).flatMap(_ => pattern)
      val zipped = (1 until text.length + 1).zip(rails)
      val prevIndex = (1 until key + 1).flatMap(r => zipped.filter(t => t._2 == r).map(_._1))
      prevIndex.zip(text).sorted.map(_._2).mkString