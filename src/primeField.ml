module type BaseFieldT = PrimeField_base.BaseFieldT
module type TailleT = PrimeField_base.TailleT

module PrimeFieldBase = PrimeField_base.PrimeFieldBase

module type FieldT = sig
  include BaseFieldT

  val alpha : t
end

module PrimeField (Taille : TailleT) : FieldT = struct
  module Base = PrimeFieldBase (Taille)
  module Poly = Polynome.Polynome (Base)
  module Cz = Cantorzass.Cantorzass (Base) (Poly)
  module Cyclo = Cyclo.Cyclo (Poly)
  include Base

  (*
     let poly_irr =
       let poly_cyclo = Cyclo.cyclo (cardinal - 1) in
       Cz.cantor_zassenhaus poly_cyclo dim *)

  let alpha = of_int (if Taille.carac = 2 then 1 else if Taille.carac = 3 then 2 else 3)
end
