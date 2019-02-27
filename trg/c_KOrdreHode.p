TRIGGER PROCEDURE FOR CREATE OF KOrdreHode.

DEF VAR trgKOrdre_Id AS DEC NO-UNDO.

LOOPEN:
DO WHILE TRUE:
    RUN trg/genkordre_id.p (OUTPUT trgKOrdre_Id).
    ASSIGN
        SkoTex.KOrdreHode.KOrdre_Id     = trgKOrdre_Id
        .
    {trg\c_w_trg.i &Fil=SkoTex.KOrdreHode &Type="C"}
    LEAVE LOOPEN.
END.

ASSIGN
  KOrdreHode.Opphav = IF KOrdreHode.Opphav = 0 THEN 1 ELSE KOrdreHode.Opphav /* Manuell registrering - DEFAULT */
  KOrdreHode.DatoTidOpprettet = NOW
  .

