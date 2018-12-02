TRIGGER PROCEDURE FOR WRITE OF MedKjop.

  {trg/c_w_trg.i &Type="W" &Fil="MedKjop"}

ASSIGN
  MedKjop.Saldo = MedKjop.KjopsBelop - MedKjop.TildeltBelop.

