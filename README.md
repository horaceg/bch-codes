# bch-codes
An implementation of BCH and Reed-solomon error-correcting codes

## Running a test
This provides a way of
1) Encoding an image
2) Introducing noise (in the form of bursts)
3) Correcting the errors and decoding

Make sure you have removed all pictures from the "resultat" directory.
Then, run :
`ocamlopt frobenius.ml polynome.ml cyclo.ml cantorzass.ml extensions.ml reedsolomon.ml bruitage.ml corr_im.ml -o corr_im`