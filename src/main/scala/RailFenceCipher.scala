object RailFenceCipher:
   def encode(s: String, i: Int): String =
      s.sliding(p(i).size, p(i).size).flatMap(_.zip(p(i))).toSeq.groupBy(_._2).values.flatten.map(_._1).mkString

   def decode(s: String, i: Int): String =
      (0 until i).flatMap(r =>
         (0 until s.length).zip(LazyList.continually(p(i)).flatten.take(s.length)).filter(_._2 == r).map(_._1)).zip(s)
        .sortBy(_._1).map(_._2).mkString

   private def p(k: Int): Seq[Int] = (0 until k) ++ (k - 2 until 0 by -1)
