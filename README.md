# bch-codes
An implementation of BCH and Reed-solomon error-correcting codes

## Running a test
This provides a way of
1) Encoding an image
2) Introducing noise (in the form of bursts)
3) Correcting the errors and decoding

To build, you can use dune:  
`dune build src/main.exe`  
`dune exec src/main.exe -- -i <input> -o <output>`  
with `<input>` the path to a (reasonably small) bmp image, and `<output>` the path to the result directory, with a trailing `/`, that must exist.
