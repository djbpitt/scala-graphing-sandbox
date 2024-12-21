package net.collatex.reptilian

// From witness id to start and until (exclusive "until") offset in global token array
enum TokenRange:
  case LegalTokenRange(start: Int, until: Int)
  case EmptyTokenRange(start: Int, until: Int)
  case IllegalTokenRange(start: Int, until: Int)

  def start: Int
  def until: Int

  def tString(using gTa: Vector[TokenEnum]): String =
    gTa.slice(this.start, this.until).map(_.t).mkString // concatenate t values

  def decreaseStart(): TokenRange = TokenRange(this.start - 1, this.until)

  def contains(pos: Int): Boolean = // position in gTa
    this.start <= pos && this.until > pos
    
object TokenRange:
  def apply(start: Int, until: Int): TokenRange =
    Ordering.Int.compare(start, until) match
      case -1 => LegalTokenRange(start, until)
      case 0  => EmptyTokenRange(start, until)
      case 1  => IllegalTokenRange(start, until)
