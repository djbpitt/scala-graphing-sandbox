package net.collatex.reptilian

enum EdgeLabel:
  case Internal(label: Int)
  case Terminal(label: String)
  override def toString: String = this match
    case Internal(label) => label.toString
    case Terminal(label) => label

object EdgeLabel:
  def apply(label: String): EdgeLabel = EdgeLabel.Terminal(label)
  def apply(label: Int): EdgeLabel = EdgeLabel.Internal(label)
  def apply(label: NodeType): EdgeLabel =
    label match
      case NodeType.Terminal(label) => EdgeLabel.Terminal(label)
      case NodeType.Internal(label) => EdgeLabel.Internal(label)

  given Ordering[EdgeLabel] = // Sort "starts" first, other numerically
    def EdgeLabelToInt(edgeLabel: EdgeLabel): Int =
      edgeLabel match
        case _: EdgeLabel.Terminal     => Int.MinValue
        case EdgeLabel.Internal(label) => label
    (a: EdgeLabel, b: EdgeLabel) => EdgeLabelToInt(a).compare(EdgeLabelToInt(b))

