package reptilian

import os.Path

import scala.collection.mutable

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

def vectorize_token_array(token_array: List[String]): mutable.Map[String, Int] =
  token_array.foldLeft(mutable.Map[String, Int]())((term_mapping, next_term) => {
    term_mapping.getOrElseUpdate(next_term, term_mapping.size)
    term_mapping
  })

@main def main(): Unit =
  val path_to_darwin = os.pwd / "src" / "main" / "data" / "darwin"
  val token_lists = read_data(path_to_darwin)
  val token_array = create_token_array(token_lists)
  val token_terms = vectorize_token_array(token_array)
  token_terms.foreach(println)
