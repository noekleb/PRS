/* opprett_Produsent.p */

DEF INPUT PARAMETER cProdusent AS CHAR NO-UNDO.

DEF BUFFER bufProdusent FOR Produsent.

IF NOT CAN-FIND(FIRST Produsent NO-LOCK WHERE 
  Produsent.Beskrivelse = cProdusent) THEN
DO TRANSACTION:
    FIND LAST bufProdusent NO-LOCK NO-ERROR.
    CREATE Produsent.
    ASSIGN
        Produsent.ProdNr      = IF AVAILABLE bufProdusent THEN bufProdusent.ProdNr + 1 ELSE 1
        Produsent.Beskrivelse = cProdusent.  
END.

