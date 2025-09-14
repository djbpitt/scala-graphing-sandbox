# Suffix array readings

## Spark-based (and non-Spark) implementation of pDC3, a linear-time parallel suffix-array-construction algorithm.

<https://github.com/hammerlab/suffix-arrays> 

This repo contains:

1. an implementation of the sequential algorithm DC3 (paper) under [org.hammerlab.suffixes.dc3](https://github.com/hammerlab/suffix-arrays/blob/master/src/main/scala/org/hammerlab/suffixes/dc3).
1. an Apache-Spark-based implementation of its parallel counterpart, pDC3 (paper), under [org.hammerlab.suffixes.pdc3](https://github.com/hammerlab/suffix-arrays/blob/master/src/main/scala/org/hammerlab/suffixes/pdc3).

The referenced paper is Fabian Kulla and Peter Sanders, [Scalable Parallel Suffix Array Construction](http://algo2.iti.kit.edu/sanders/papers/KulSan06a.pdf). See pp. 13–14 about LCP array.

[Associated Scaladex page](https://index.scala-lang.org/hammerlab/suffix-arrays)

## Stanford lecture about suffix and LCP arrays

[Suffix arrays](Small11.pdf) (local copy)

## UCSD “Algorithms on strings” lecture about LCP array

[Computing the LCP Array](https://www.coursera.org/lecture/algorithms-on-strings/computing-the-lcp-array-HyUlH). (Free) registration required on Coursera site.

## Kasai’s algorithm for computing LCP array in linear time (Youtube video)

Sebastian Wild’s (University of Liverpool) lecture: <https://www.youtube.com/watch?v=NSqD9iHQ_ys>

## C++ code

[Suffix Array and LCP in O(N). Algorithm DC3](https://sites.google.com/site/indy256/algo_cpp/suffix_array_lcp) (part of [Data Structures and Algorithms in C++](https://sites.google.com/site/indy256/algo_cpp/))


