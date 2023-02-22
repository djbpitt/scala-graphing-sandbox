package reptilian


def read_data(): Unit =
  val datafiles = os.pwd / "src" / "main" / "data" / "darwin"
  val files = os.walk(datafiles).sorted
  files.foreach(e => println(e))

@main def main(): Unit =
  read_data()