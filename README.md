## Kodowanie Huffmana

Napisz program wykorzystujący kodowanie Huffmana (klasyczne lub dynamiczne) do kompresji
plików. Ten projekt pozwoli na potrenowanie operowania na danych binarnych oraz
wykorzystania drzewiastych strukturach danych. Do obsługi danych binarnych możesz
wykorzystać pakiet “bytestring”.

## Bazowy pomysł

Zadany plik input file to dowolny zestaw bajtów. Output tak samo.

```bash
# compress
$ huffman [input-file] -o [output-file]
Compressed file from [original-size] to [output-size] <20%>

# decompress
$ huffman [input-file] -d -o [output-file]
```

Parameters:
- positional: input file
- -o output
- -d decompress (default compress)

## Build

Możemy wykonać builda przy pomocy stack:

```bash
stack build
```

A następnie uruchomić program:

```bash
stack exec huffman -- [input-file] -o [output-file]
```

## Testy

Do wymyślenia.