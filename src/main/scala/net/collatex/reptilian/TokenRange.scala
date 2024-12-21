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

  def splitTokenRange(rangeToSplitAround: TokenRange): (TokenRange, TokenRange) =
    // Split token range into preblock, block, postblock; ignore block because we already know it
    // Assume resulting ranges are legal or empty; if illegal, the issue is in our block identification
    // TODO: Return Either and let caller manage exceptions
    if rangeToSplitAround.getClass.getSimpleName == "EmptyTokenRange" then
      throw RuntimeException(s"cannot split on empty block range: $rangeToSplitAround")
    
    val rangeToSplit = this
    val pre = TokenRange(rangeToSplit.start, rangeToSplitAround.start)
    val post = TokenRange(rangeToSplitAround.until, rangeToSplit.until)
    
    if pre.getClass.getSimpleName == "IllegalTokenRange" && post.getClass.getSimpleName == "IllegalTokenRange" then
      throw RuntimeException(s"both pre ($pre) and post($post) are illegal")
    if pre.getClass.getSimpleName == "IllegalTokenRange" then throw RuntimeException(s"pre value $pre is illegal")
    if post.getClass.getSimpleName == "IllegalTokenRange" then throw RuntimeException(s"post value $post is illegal")
    
    (pre, post)


object TokenRange:
  def apply(start: Int, until: Int): TokenRange =
    Ordering.Int.compare(start, until) match
      case -1 => LegalTokenRange(start, until)
      case 0  => EmptyTokenRange(start, until)
      case 1  => IllegalTokenRange(start, until)
