/************************************************************
    Program:  oppr-tick-row.p
    Created:  TN   21 Dec 99
Description:

  Leser alle transaksjoner som ikke er plukket i TransLogg
  og legger disse over i tick-row. Der kjõres de gamle
  programmer for utskrift av plukkliste (ref SkoDej).

  Dette er en midlertidig lõsning for Ü fÜ lõst problemet
  med kontinuerlig pÜfylling i hyllene hos SkoDej.

Last change:  TN    9 Aug 100   10:22 am
************************************************************/


DEF VAR wGrpListe  as CHAR NO-UNDO.
DEF VAR wButikkLst as CHAR NO-UNDO.
DEF VAR wLoop      as INT  NO-UNDO.
DEF VAR wVgListe   as CHAR NO-UNDO.

{plukkliste.i}
{syspara.i 200 1 104 wGrpListe}
{syspara.i 200 1 105 wButikkLst}
{syspara.i 200 1 106 wVgListe}

/* Tõmmer temp-fil */
for each PlukkListe: DELETE PlukkListe. END.

BUTIKKLOOP:
do wLoop = 1 To NUM-ENTRIES(wButikkLst):

  TRANSLOOP:
  FOR EACH TransLogg EXCLUSIVE-LOCK where
    TransLogg.Plukket = FALSE and
    TransLogg.Butik = INT(ENTRY(wLoop,wButikkLst))
    break By TransLogg.Plukket
          by TransLogg.Butik
          by TransLogg.Vg
          by TransLogg.LopNr
          by TransLogg.Storl:

    /* Kun transer med õnsket varegruppe behandles. */
    /* ùvrige flagges bare som oppdatert.           */
    if CAN-DO(wVgListe,STRING(TransLogg.Vg)) then
      DO:
        assign
          TransLogg.Plukket = TRUE.
        NEXT TRANSLOOP.
      END.

    /* Henter artikkelinformasjonen. */
    FIND ArtBas NO-LOCK where
      ArtBas.Vg    = TransLogg.Vg and
      ArtBas.LopNr = Translogg.LopNr NO-ERROR.

    FIND PlukkListe where
      PlukkListe.Butik = TransLogg.Butik and
      PlukkListe.Vg    = TransLogg.Vg    and
      PlukkListe.LopNr = TransLogg.LopNr and
      PlukkListe.Storl = TransLogg.Storl and
      PlukkListe.Pris  = TransLogg.Pris - TransLogg.RabKr NO-ERROR.
    if NOT AVAILABLE PlukkListe then
      DO:
        CREATE PlukkListe.
        assign
          PlukkListe.Butik = TransLogg.Butik
          PlukkListe.Vg    = TransLogg.Vg
          PlukkListe.LopNr = TransLogg.LopNr
          PlukkListe.Storl = TransLogg.Storl
          PlukkListe.Pris  = TransLogg.Pris - TransLogg.RabKr
          PlukkListe.Hg    = if available ArtBAs then ArtBas.Hg ELSE 0.
      END.
    assign
      PlukkListe.Antall  = PlukkListe.Antall + TransLogg.Antall
      PlukkListe.RabKr   = Plukkliste.RabKr  + (TransLogg.RabKr * Translogg.Antall).

    assign
      TransLogg.Plukket = TRUE.
  END. /* TRANSLOOP */
END. /* BUTIKKLOOP */


