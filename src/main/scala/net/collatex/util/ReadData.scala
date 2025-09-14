package net.collatex.util

import os.Path

// Not used in Reptilian, which no longer reads input from directories
object ReadData {
  /** Read data files from supplied path to directory (one file per witness)
    *
    * @param pathToData
    *   os.Path object that points to data directory
    * @return
    *   List of tuples of 1) abbreviated filename and 2) string (token lists)
    */
  def readData(pathToData: Path): List[(String, String)] =
    os.walk(
      path = pathToData,
      skip = _.last.startsWith(".")
    ) // exclude hidden files
      .sorted
      .toList
      .map(e => (e.last, os.read(e)))
}
