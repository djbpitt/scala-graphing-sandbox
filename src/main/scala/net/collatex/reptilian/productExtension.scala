package net.collatex.reptilian

extension (p: Product)
  def fields: Iterator[(String, Any)] =
    p.productElementNames.zip(p.productIterator)
