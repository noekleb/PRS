/************************************************************
    Program:  byggmedlem.P
    Created:  TN   31 Des 00
Description:

	Last change:  TN   21 Aug 101    9:53 am
************************************************************/

DEF INPUT PARAMETER wListerRecid      as RECID NO-UNDO.
DEF INPUT PARAMETER T-Butikk          as LOG  NO-UNDO.
DEF INPUT PARAMETER wvMedlem          as char no-undo.
DEF INPUT PARAMETER wvButiker         as char no-undo.
def INPUT PARAMETER wvMedlemsGruppe   as char no-undo.
def INPUT PARAMETER wvMedlemsType     as char no-undo.
def INPUT PARAMETER wvBydel           as char no-undo.
def INPUT PARAMETER wvPostNr          as char no-undo.
def INPUT PARAMETER wvKommune         as char no-undo.
def INPUT PARAMETER wvFylke           as char no-undo.
def INPUT PARAMETER wvRegion          as char no-undo.
def INPUT PARAMETER T-Hovedmedlem     as LOG  NO-UNDO.
def INPUT PARAMETER T-HarMedlemsKort  as LOG  NO-UNDO.
def INPUT PARAMETER T-HarOmsetning    as LOG  NO-UNDO.
def INPUT PARAMETER T-HarSaldo        as LOG  NO-UNDO.
def INPUT PARAMETER T-KobletTilKunde  as LOG  NO-UNDO.
def INPUT PARAMETER T-HarOpphort      as LOG  NO-UNDO.
def INPUT PARAMETER FI-FraOpprettet   AS DATE NO-UNDO.
def INPUT PARAMETER FI-TilOpprettet   AS DATE NO-UNDO.
def INPUT PARAMETER FI-FraEndret      AS DATE NO-UNDO.
def INPUT PARAMETER FI-TilEndret      AS DATE NO-UNDO.
def INPUT PARAMETER FI-FraFKjop       AS DATE NO-UNDO.
def INPUT PARAMETER FI-TilFKjop       AS DATE NO-UNDO.
def INPUT PARAMETER FI-FraSKjop       AS DATE NO-UNDO.
def INPUT PARAMETER FI-TilSKjop       AS DATE NO-UNDO.
def INPUT PARAMETER FI-FraOmsDato     AS DATE NO-UNDO.
def INPUT PARAMETER FI-TilOmsDato     AS DATE NO-UNDO.
def INPUT PARAMETER FI-Oms            AS DEC  NO-UNDO.
def INPUT PARAMETER FI-Oms2           AS DEC  NO-UNDO.
def INPUT PARAMETER FI-FraFodtDato    AS DATE NO-UNDO.
def INPUT PARAMETER FI-TilFodtDato    AS DATE NO-UNDO.
def INPUT PARAMETER FI-FraFodtAr      AS INT  NO-UNDO.
def INPUT PARAMETER FI-TilFodtAr      AS INT  NO-UNDO.
def INPUT PARAMETER FI-EtterNavn      AS CHAR NO-UNDO.
def INPUT PARAMETER RS-Filter         AS INT NO-UNDO.
def INPUT PARAMETER RS-Kjonn          AS INT NO-UNDO.

DEF INPUT PARAMETER wParent          as handle NO-UNDO.

DEF VAR FI-Medlem1 AS DEC    NO-UNDO.
DEF VAR FI-Medlem2 AS DEC    NO-UNDO.
def var wAntall    as int    no-undo.
def var wLoop1     as int    no-undo.
def var wLoop2     as int    no-undo.
def var wStatListe as char   no-undo.
DEF VAR FI-Info    as CHAR   NO-UNDO.
DEF VAR wStatLst   as CHAR   NO-UNDO.
DEF VAR wAlle      as CHAR   NO-UNDO.
DEF VAR wCellNr    as INT    NO-UNDO.
DEF VAR wOms       as DEC    NO-UNDO.

find Lister no-lock where
  recid(Lister) = wListerRecid no-error.
if not available Lister then
  return "AVBRYT".

{syspara.i 1 100 1 wAlle}

/*** Dette er for DUMT, men det mangler en indeks. */
assign
  wCellNr = 0.
for each ListeLinje no-lock where
  ListeLinje.ListeType  = Lister.ListeType and
  ListeLinje.ListeNr    = Lister.ListeNr and
  ListeLinje.CellNr     <> ?
  by ListeLinje.ListeType
  by ListeLinje.ListeNr
  by ListeLinje.CellNr:

  wCellNr = ListeLinje.CellNr + 1.
END.

/* Er butikklisten blank, tas alle butikker. */
if wvButiker = wAlle then
  RUN ByggButikkListe.

ASSIGN
    FI-Medlem1 = DEC(ENTRY(1,wvMedlem,"|"))
    FI-Medlem2 = DEC(ENTRY(2,wvMedlem,"|"))
    .

/*
IF wvMedlem <> wAlle THEN
MAIN_LOOP1:
do wLoop1 = 1 to num-entries(wvMedlem):
  FIND Medlem NO-LOCK where
    Medlem.MedlemsNr = INT(ENTRY(wLoop1,wvMedlem)) NO-ERROR.
  if NOT AVAILABLE Medlem then
    NEXT MAIN_LOOP1.

  RUN ByggMedlem.

end. /* MAIN_LOOP1 */
ELSE 
MAIN_LOOP2:
FOR EACH Medlem NO-LOCK:
  RUN ByggMedlem.
END. /* MAIN_LOOP2 */
*/
MAIN_LOOP:
FOR EACH Medlem NO-LOCK WHERE
    (IF FI-Medlem1 = 0
       THEN true
       ELSE Medlem.MedlemsNr >= FI-Medlem1) AND
    (IF FI-Medlem2 = 0
       THEN TRUE 
       ELSE Medlem.MEdlemsNr <= FI-Medlem2)
    /*
    (IF FI-Medlem1 <> 0 THEN
         Medlem.MedlemsNr >= FI-Medlem1
     ELSE
         TRUE) AND
    (IF FI-Medlem2 <> 0 THEN
         Medlem.MedlemsNr <= FI-Medlem2
     ELSE
         TRUE) */:
  RUN ByggMedlem.
END. /* MAIN_LOOP */

if available ListeLinje then
  release ListeLinje.

PROCEDURE ByggButikkListe:
  wvButiker = "".
  FOR EACH Butiker NO-LOCK WHERE
    Butiker.Butik > 0:
    wvButiker = wvButiker +
                (if wvButiker = ""
                   THEN ""
                   ELSE ",") +
                STRING(Butiker.Butik).
  END.
END PROCEDURE.

/* Rutinen forutsetter at det finnes en kundepost tilgjengelig når den kalles. */
PROCEDURE ByggMedlem:
  FIND Post NO-LOCK WHERE
      Post.PostNr = Medlem.PostNr NO-ERROR.
  
  IF wvMedlemsGruppe <> wAlle THEN  
  DO:
    IF NOT CAN-DO(wvMedlemsGruppe,STRING(Medlem.MedGruppe)) THEN
        RETURN "AVBRYT".
  END.
  IF wvMedlemsType <> wAlle THEN
  DO:
      IF NOT CAN-DO(wvMedlemsType,STRING(Medlem.MedType)) THEN
          RETURN "AVBRYT".
  END.
  IF wvBydel <> wAlle THEN
  DO:
      IF NOT CAN-DO(wvBydel,STRING(Medlem.Bydel)) THEN
          RETURN "AVBRYT".
  END.
  IF wvPostNr <> wAlle THEN
  DO:
      IF NOT CAN-DO(wvPostNr,Medlem.PostNr) THEN
          RETURN "AVBRYT".
  END.
  IF wvKommune <> wAlle AND AVAILABLE Post THEN
  DO:
      IF NOT CAN-DO(wvKommune,Post.KommNr) THEN
          RETURN "AVBRYT".
  END.
  IF wvFylke <> wAlle AND AVAILABLE Post THEN
  DO:
      IF NOT CAN-DO(wvFylke,Post.FylkesNr) THEN
          RETURN "AVBRYT".
  END.
  
  IF wvRegion <> wAlle AND AVAILABLE Post THEN
  DO:
      IF NOT CAN-DO(wvRegion,Medlem.RegKode) THEN
          RETURN "AVBRYT".
  END.

  IF T-Hovedmedlem THEN 
  DO:
      IF NOT Medlem.HovedMedlemFlagg  THEN
        RETURN "AVBRYT".
  END.
  IF T-HarMedlemsKort THEN
  DO:
      IF NOT CAN-FIND(FIRST MedlemsKort WHERE
                      MedlemsKort.MedlemsNr = Medlem.MedlemsNr) THEN
        RETURN "AVBRYT".
  END.
  IF T-HarOmsetning THEN
  DO:
      IF NOT CAN-FIND(FIRST MedTrans WHERE
                      MedTrans.MedlemsNr = Medlem.MedlemsNr) THEN
        RETURN "AVBRYT".
  END.
  IF T-HarSaldo THEN
  DO:
      IF NOT CAN-FIND(FIRST MedlemSaldo WHERE
                      MedlemSaldo.MedlemsNr = Medlem.MedlemsNr AND
                      MedlemSaldo.Saldo  <> 0) THEN
        RETURN "AVBRYT".
  END.
  IF T-KobletTilKunde THEN
  DO:
      IF NOT can-find(Kunde where
                      Medlem.MedlemsNr = Medlem.MedlemsNr) THEN
        RETURN "AVBRYT".
  END.
  IF T-HarOpphort THEN
  DO:
      IF Medlem.Opphort <> ? AND 
         Medlem.Opphort >= TODAY THEN. /* Gjør ingenting. */
      ELSE 
        RETURN "AVBRYT".
  END.

  IF FI-FraOpprettet <> ? THEN
  DO:
     IF FI-FraOpprettet < Medlem.RegistrertDato THEN
         RETURN "AVBRYT".
  END.

  IF FI-TilOpprettet <> ? THEN
  DO:
     IF FI-TilOpprettet > Medlem.RegistrertDato THEN
         RETURN "AVBRYT".
  END.

  IF FI-FraEndret <> ? THEN
  DO:
    IF FI-FraEndret < Medlem.EDato THEN
        RETURN "AVBRYT".
  END.
  IF FI-TilEndret <> ? THEN
  DO:
      IF FI-TilEndret > Medlem.EDato THEN
        RETURN "AVBRYT".
  END.
  IF FI-FraFKjop <> ? THEN
  DO:
      if CAN-FIND(FIRST MedlemSaldo WHERE
                    MedlemSaldo.MedlemsNr = Medlem.MedlemsNr AND
                    MedlemSaldo.ForsteDato < FI-FraFKjop) THEN
          RETURN "AVBRYT".
  END.

  IF FI-FraFodtDato <> ? THEN
  DO:
      IF Medlem.FodselsDato = ? THEN
          RETURN "AVBRYT".
      IF Medlem.FodselsDato < FI-FraFodtDato THEN
        RETURN "AVBRYT".
  END.
  
  IF FI-TilFodtDato <> ? THEN
  DO:
      IF Medlem.FodselsDato = ? THEN
          RETURN "AVBRYT".
      IF Medlem.FodselsDato > FI-TilFodtDato THEN
        RETURN "AVBRYT".
  END.
  
  IF FI-FraFodtAr <> ? THEN
  DO:
      IF Medlem.FodtAr = ? THEN
          RETURN "AVBRYT".
      IF Medlem.FodtAr < FI-FraFodtAr THEN
        RETURN "AVBRYT".
  END.
  
  IF FI-TilFodtAr <> ? THEN
  DO:
      IF Medlem.FodtAr = ? THEN
          RETURN "AVBRYT".
      IF Medlem.FodtAr > FI-TilFodtAr THEN
        RETURN "AVBRYT".
  END.

  IF FI-FraFKjop <> ? THEN
  DO:
      if CAN-FIND(FIRST MedlemSaldo WHERE
                    MedlemSaldo.MedlemsNr = Medlem.MedlemsNr AND
                    MEdlemSaldo.ForsteDato < FI-FraFKjop) THEN
          RETURN "AVBRYT".
  END.

  IF FI-FraSKjop <> ? THEN
  DO:
      if CAN-FIND(FIRST MedlemSaldo WHERE
                    MedlemSaldo.MedlemsNr = Medlem.MEdlemsNr AND
                    MedlemSaldo.ForsteDato < FI-FraSKjop) THEN
          RETURN "AVBRYT".
  END.

  IF FI-TilSKjop <> ? THEN
  DO:
      IF CAN-FIND(FIRST MedlemSaldo WHERE
                    MedlemSaldo.MedlemsNr = Medlem.MedlemsNr AND
                    MedlemSaldo.ForsteDato > FI-TilSKjop) THEN
          RETURN "AVBRYT".
  END.
  
  IF FI-FraOmsDato <> ? then
  DO:
      IF FI-TilOmsDato <> ? then
      DO:
          wOms = 0.
          FOR EACH MedTrans NO-LOCK where
            MedTrans.MedlemsNr  = Medlem.MedlemsNr and
            MEdTrans.Dato      >= FI-FraOmsDato and
            MedTrans.Dato      <= FI-TilOmsDato:

            assign
              wOms = wOms + ((IF MedTrans.Antall < 0 THEN -1 ELSE 1) * (MedTrans.Pris - MedTrans.RabKr - MedTrans.SubtotalRab))
              .

          END.
      END.
      ELSE DO:
          wOms = 0.
          FOR EACH MedTrans NO-LOCK where
            MedTrans.MedlemsNr  = Medlem.MedlemsNr and
            MEdTrans.Dato      >= FI-FraOmsDato:

            assign
              wOms = wOms + ((IF MedTrans.Antall < 0 THEN -1 ELSE 1) * (MedTrans.Pris - MedTrans.RabKr - MedTrans.SubtotalRab))
              .

          END.
      END.
      IF (wOms < FI-Oms) OR
         (wOms > FI-Oms2) then
        RETURN "AVBRYT".
  END.
  ELSE IF FI-TilOmsDato <> ? then
  DO:
      wOms = 0.
      FOR EACH MedTrans NO-LOCK where
        MedTrans.MedlemsNr  = Medlem.MedlemsNr and
        MedTrans.Dato      <= FI-TilOmsDato:

        assign
          wOms = wOms + ((IF MedTrans.Antall < 0 THEN -1 ELSE 1) * (MedTrans.Pris - MedTrans.RabKr - MedTrans.SubtotalRab))
          .

      END.
      IF (wOms < FI-Oms) OR
         (wOms > FI-Oms2) then
        RETURN "AVBRYT".
  END.


  IF FI-TilOmsDato <> ? then
  DO:
      IF FI-FraOmsDato <> ? then
      DO:
          IF FI-TilOmsDato < FI-FraOmsDato then
            RETURN "AVBRYT".
      END.
  END.

  IF FI-EtterNavn <> "" THEN
  DO:
    /* Begynner med */
    IF RS-Filter = 1 THEN
    DO:
      IF NOT Medlem.EtterNavn BEGINS FI-EtterNavn THEN
          RETURN "AVBRYT".
    END.
    /* Inneholder */
    ELSE IF RS-Filter = 2 THEN
    DO:
      IF NOT Medlem.EtterNavn MATCHES "*" + FI-EtterNavn + "*" THEN
          RETURN "AVBRYT".
    END.
  END.

  IF FI-Medlem1 <> 0 AND
     FI-Medlem2 <> 0 THEN
  DO:
      IF FI-Medlem2 < FI-Medlem1 THEN
          RETURN "AVBRYT".
  END.

  IF RS-Kjonn < 3 THEN
  DO:
      IF RS-Kjonn = 1 /* Mann */ THEN
      DO:
          IF Medlem.Kjonn = TRUE /* Kvinne */ THEN
              RETURN "AVBRYT".
      END.
      ELSE IF RS-Kjonn = 2 /* Kvinne */ THEN
      DO:
          IF Medlem.Kjonn = FALSE /* Mann */ THEN
              RETURN "AVBRYT".
      END.
  END.

OUTPUT TO VALUE('medlemslogg.csv') append.
     PUT UNFORMATTED Medlem.MedlemsNr ';' Medlem.ForNavn ';' Medlem.Etternavn ';' wOms SKIP.
OUTPUT CLOSE.

  BLOKK-1:
  DO:
    if T-Butikk then
      BUTIKKER:
      do wLoop2 = 1 to num-entries(wvButiker):

        if NOT CAN-FIND(FIRST ListeLinje where
                        ListeLinje.ListeType  = Lister.ListeType and
                        ListeLinje.ListeNr    = Lister.ListeNr and
                        ListeLinje.DataObjekt = string(Medlem.MedlemsNr) + "," +
                                                entry(wLoop2,wvButiker) and
                        ListeLinje.Div1       = entry(wLoop2,wvButiker)) then
          DO:
            create ListeLinje.
            assign
              ListeLinje.ListeType  = Lister.ListeType
              ListeLinje.ListeNr    = Lister.ListeNr
              ListeLinje.DataObjekt = string(Medlem.MedlemsNr) + "," +
                                      entry(wLoop2,wvButiker)
              ListeLinje.Div1       = entry(wLoop2,wvButiker)
              ListeLinje.CellNr     = wCellNr
              wCellNr               = wCellNr + 1
              ListeLinje.DivX[1]    = STRING(Medlem.MedlemsNr,"9999999999999")
              ListeLinje.DivX[2]    = Medlem.ForNavn + " " + Medlem.EtterNavn
              ListeLinje.DivX[3]    = STRING(wCellNr,"999999999").
            assign
              wAntall = wAntall + 1
              FI-Info = "Butikk " + entry(wLoop2,wvButiker) + " - Antall poster opprettet " + string(wAntall).
            IF VALID-HANDLE(wParent) then
              RUN Disp-Info in wParent (FI-Info).
          END.
      end. /* BUTIKKER */
    else do:
      IF NOT CAN-FIND(FIRST ListeLinje where
                      ListeLinje.ListeType  = Lister.ListeType and
                      ListeLinje.ListeNr    = Lister.ListeNr and
                      ListeLinje.DataObjekt = string(Medlem.MedlemsNr) + "," +
                                              "0") /* Allt legges på butikk 0 */ then
        DO:
          create ListeLinje.
          assign
            ListeLinje.ListeType  = Lister.ListeType
            ListeLinje.ListeNr    = Lister.ListeNr
            ListeLinje.DataObjekt = string(Medlem.MedlemsNr) + "," +
                                    "0" /* Allt legges på butikk 0 */
            ListeLinje.CellNr     = wCellNr
            wCellNr               = wCellNr + 1
              ListeLinje.DivX[1]    = STRING(Medlem.MedlemsNr,"9999999999999")
              ListeLinje.DivX[2]    = Medlem.ForNavn + " " + Medlem.EtterNavn
              ListeLinje.DivX[3]    = STRING(wCellNr,"999999999").
          assign
            wAntall = wAntall + 1
            FI-Info = "Antall poster opprettet " + string(wAntall).
          IF VALID-HANDLE(wParent) then
            RUN Disp-Info in wParent (FI-Info).
        END.
    END.
  end. /* BLOKK-1 */

END. /* ByggKunde. */




