package reptilian


/** Read hard-coded path to data filenames (one file per witness)
 *
 * @return Indexed sequence of lists of strings (token lists)
 */
def read_data(): IndexedSeq[List[String]] =
  val datafiles = os.pwd / "src" / "main" / "data" / "darwin"
  os.walk(datafiles)
    .sorted
    .map(os.read)
    .map(_.split(raw"\s+").toList)

@main def main(): Unit =
  val token_lists = read_data()
  val token_array = token_lists
    .head ++ token_lists
    .tail
    .zipWithIndex
    .flatMap((e, index) => List(s" #$index ") ++ e)
  token_array.foreach(println)
