package reptilian

import os.Path

/** Read data files from supplied path to directory (one file per witness)
 *
 * @param path_to_data os.Path object that points to data directory
 * @return Indexed sequence of lists of strings (token lists)
 */
def read_data(path_to_data: Path): IndexedSeq[List[String]] =
  os.walk(path_to_data)
    .sorted
    .map(os.read)
    .map(_.split(raw"\s+").toList)

def create_token_array(token_lists: IndexedSeq[List[String]]): List[String] =
  token_lists
    .head ++ token_lists
    .tail
    .zipWithIndex
    .flatMap((e, index) => List(s" #$index ") ++ e)

@main def main(): Unit =
  val path_to_darwin = os.pwd / "src" / "main" / "data" / "darwin"
  val token_lists = read_data(path_to_darwin)
  val token_array = create_token_array(token_lists)
  token_array.foreach(println)
