# Huffman Coding

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![Built with Haskell](https://img.shields.io/badge/Built%20with-Haskell-5e5086.svg)](https://www.haskell.org/)
[![CI Status](https://github.com/Rafisto/huffman-coding/actions/workflows/haskell.yml/badge.svg)](https://github.com/Rafisto/huffman-coding/actions/workflows/haskell.yml)

Compresses files using Huffman encoding.

## Usage

Compresses the input file.

```bash
$ huffman [input-file] -o [output-file]
```

Decompresses the input file.

```bash
$ huffman [input-file] -d -o [output-file]
```

## Parameters

- **\[input-file\]**: The file to be processed (required).
- **-o \[output-file\]**: Specifies the output file (required).
- **-d**: Enables decompression mode (default is compression).

## Introduction

In 1952 David A. Huffman created an algorithm used for lossless data
compression [@huffman1952method], building upon the advances of Claude
Shannon's work on information theory. It has been proven to be optimal
for symbol-by-symbol coding with a known input probability distribution.
The algorithm assigns variable-length codes to input characters, and the
shorter codes are assigned to more frequently occurring characters.

## Initial Requirements

The program has been designed to fulfill the following requirements:\
Write a program that uses Huffman coding (classic or dynamic) to
compress files. This project will allow you to practice working with
binary data and using tree-like data structures. To handle binary data,
you can use the *bytestring* package.\
We decided that our take on the implementation will provide a simple
command-line interface.

## Showcase

```bash
$ wget https://pl.wikipedia.org/wiki/Polska -O polska.txt

$ sha256sum polska.txt
645e668ee...9c530  polska.txt

$ stack run -- polska.txt -o polska.txt.huff
polska.txt (1271161 bytes) -> polska.txt.huff (887871 bytes) [69.85%]

$ ls -lah polska.txt polska.txt.huff
-rw-r--r-- 1 rafisto rafisto 1.3M Jun  9 14:14 polska.txt
-rw-r--r-- 1 rafisto rafisto 868K Jun  9 23:27 polska.txt.huff

$ stack run -- polska.txt.huff -d -o polska.out.txt
polska.txt.huff (887871 bytes) -> polska.out.txt (1271161 bytes) [143.17%]

$ sha256sum polska.out.txt 
645e668ee...9c530  polska.out.txt
```

## Huffman Coding Algorithm

As described in [@wikihuffman], can be summarized in the following
steps:

1.  Count the frequency of each symbol in the input data.
2.  Create a leaf node for each symbol and add it to the priority queue.
3.  While there is more than one node in the queue:
    1.  Remove the two nodes of highest priority (lowest probability)
        from the queue.
    2.  Create a new internal node with these two nodes as children and
        with probability equal to the sum of the two nodes'
        probabilities.
    3.  Add the new node to the queue.
4.  The remaining node is the root node, and the tree is complete.

## Prefix Codes

A set of codes $\{C_1, C_2, \ldots, C_n\}$ is a prefix code if:
$$\begin{aligned}
    \forall i, j \in \{1, 2, \ldots, n\}, i \neq j: C_i \not\subseteq C_j
\end{aligned}$$

In any stream of bits, defined as $S = b_1 b_2 \ldots b_m, b\in\{0,1\}$,
a prefix code can be decoded unambiguously. This feature is crucial for
the Huffman coding algorithm as it allows deterministic decoding of the
compressed data in linear time.

## Build

The project uses Stack as a build tool, which allows for simple
dependency management and building.

```bash
$ stack build
```

Afterwards, the executable can be run with the following command:

```bash
$ stack exec huffman -- [input-file] -o [output-file]
```

To make it available globally, we can use `stack install`.

```bash
$ stack install
$ huffman
Usage: huffman <input-file> [-d] -o <output-file>
$ huffman src/Main.hs -o Main.hs.huff
src/Main.hs (1887 bytes) -> Main.hs.huff (1411 bytes) [74.77%]
```

## Conclusions

The Huffman coding algorithm with its presented implementation in
Haskell has shown to be an effective way to compress text files. We can
assume a compression ratio of around 70% for large text files, with the
decompression process being lossless and efficient.

## Future Work

The current implementation can be extended in several ways.
-   Optimization of the priority queue, library **containers** provides
    a more efficient implementation.
-   As proposed by [@knuth1985dynamic], dynamic Huffman coding allows
    data to be encoded without knowing the frequency distribution
    beforehand, by maintaining a dynamic tree structure that adapts to
    the input data as it is processed.

## References

- Huffman, D. A. (1952). "A method for the construction of minimum-redundancy codes." *Proceedings of the IRE*, 40(9), 1098–1101. [@huffman1952method]
- Knuth, D. E. (1985). "Dynamic huffman coding." *Journal of Algorithms*, 6(2), 163–180. [@knuth1985dynamic]
- Wikipedia contributors. "Huffman coding — Wikipedia, The Free Encyclopedia." 2025. [Online; accessed 9-June-2025]. [@wikipedia_huffman]