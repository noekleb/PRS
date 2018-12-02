/************************************************************
    Program:  byggKunde.P
    Created:  TN   31 Des 00
Description:

	Last change:  TN   21 Aug 101   10:14 am
************************************************************/

DEF INPUT PARAMETER wListerRecid     as RECID NO-UNDO.
DEF INPUT PARAMETER T-Butikk         as LOG  NO-UNDO.
DEF INPUT PARAMETER wvKunde          as char no-undo.
DEF INPUT PARAMETER wvButiker        as char no-undo.
def INPUT PARAMETER wvKundeGruppe    as char no-undo.
def INPUT PARAMETER wvKundeType      as char no-undo.
def INPUT PARAMETER wvBydel          as char no-undo.
def INPUT PARAMETER wvPostNr         as char no-undo.
def INPUT PARAMETER wvKommune        as char no-undo.
def INPUT PARAMETER wvFylke          as char no-undo.
def INPUT PARAMETER T-HarMedlem       as LOG  NO-UNDO.
def INPUT PARAMETER T-HarKundeKort    as LOG  NO-UNDO.
def INPUT PARAMETER T-HarOmsetning    as LOG  NO-UNDO.
def INPUT PARAMETER T-HarSaldo        as LOG  NO-UNDO.
def INPUT PARAMETER T-HarKreditSperre as LOG  NO-UNDO.
def INPUT PARAMETER T-HarOpphort      as LOG  NO-UNDO.
def INPUT PARAMETER FI-FraOpprettet   AS DATE NO-UNDO.
def INPUT PARAMETER FI-TilOpprettet   AS DATE NO-UNDO.
def INPUT PARAMETER FI-FraEndret      AS DATE NO-UNDO.
def INPUT PARAMETER FI-TilEndret      AS DATE NO-UNDO.
def INPUT PARAMETER FI-FraFKjop       AS DATE NO-UNDO.
def INPUT PARAMETER FI-TilFKjop       AS DATE NO-UNDO.
def INPUT PARAMETER FI-FraSKjop       AS DATE NO-UNDO.
def INPUT PARAMETER FI-TilSKjop       AS DATE NO-UNDO.

DEF INPUT PARAMETER FI-FraOmsDato     AS DATE NO-UNDO.
DEF INPUT PARAMETER FI-TilOmsDato     AS DATE NO-UNDO.
DEF INPUT PARAMETER FI-Oms            AS DEC  NO-UNDO.

def INPUT PARAMETER FI-KundeNavn      AS CHAR NO-UNDO.
def INPUT PARAMETER RS-Filter         AS INT NO-UNDO.

DEF INPUT PARAMETER wParent          as handle NO-UNDO.

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

IF wvKunde <> wAlle THEN
MAIN_LOOP1:
do wLoop1 = 1 to num-entries(wvKunde):
  FIND Kunde NO-LOCK where
    Kunde.KundeNr = INT(ENTRY(wLoop1,wvKunde)) NO-ERROR.
  if NOT AVAILABLE Kunde then
    NEXT MAIN_LOOP1.

  RUN ByggKunde.

end. /* MAIN_LOOP1 */
ELSE 
MAIN_LOOP2:
FOR EACH Kunde NO-LOCK:
  RUN ByggKunde.
END. /* MAIN_LOOP2 */

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
PROCEDURE ByggKunde:
  FIND Post NO-LOCK WHERE
      Post.PostNr = Kunde.PostNr NO-ERROR.

  IF wvKundeGruppe <> wAlle THEN  
  DO:
    IF NOT CAN-DO(wvKundeGruppe,STRING(Kunde.GruppeID)) THEN
        RETURN "AVBRYT".
  END.
  IF wvKundeType <> wAlle THEN
  DO:
      IF NOT CAN-DO(wvKundeType,STRING(Kunde.TypeId)) THEN
          RETURN "AVBRYT".
  END.
  IF wvBydel <> wAlle THEN
  DO:
      IF NOT CAN-DO(wvBydel,STRING(Kunde.Bydel)) THEN
          RETURN "AVBRYT".
  END.
  IF wvPostNr <> wAlle THEN
  DO:
      IF NOT CAN-DO(wvPostNr,Kunde.PostNr) THEN
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

  IF T-HarMedlem THEN 
  DO:
      IF NOT CAN-FIND(FIRST Medlem WHERE
                      Medlem.KundeNr = Kunde.KundeNr) THEN
        RETURN "AVBRYT".
  END.
  IF T-HarKundeKort THEN
  DO:
      IF NOT CAN-FIND(FIRST KundeKort WHERE
                      KundeKort.KundeNr = Kunde.KundeNr) THEN
        RETURN "AVBRYT".
  END.
  IF T-HarOmsetning THEN
  DO:
      IF NOT CAN-FIND(FIRST KundeTrans WHERE
                      KundeTrans.KundeNr = Kunde.KundeNr) THEN
        RETURN "AVBRYT".
  END.
  IF T-HarSaldo THEN
  DO:
      IF NOT CAN-FIND(FIRST KundeSaldo WHERE
                      KundeSaldo.KundeNr = Kunde.KundeNr AND
                      KundeSaldo.Saldo  <> 0) THEN
        RETURN "AVBRYT".
  END.
  IF T-HarKreditSperre THEN
  DO:
      IF Kunde.KreditSperre = FALSE THEN
        RETURN "AVBRYT".
  END.
  IF T-HarOpphort THEN
  DO:
      IF Kunde.Opphort <> ? AND 
         Kunde.Opphort >= TODAY THEN. /* Gjør ingenting. */
      ELSE 
        RETURN "AVBRYT".
  END.

  IF FI-FraOpprettet <> ? THEN
  DO:
     IF FI-FraOpprettet < Kunde.RegistrertDato THEN
         RETURN "AVBRYT".
  END.

  IF FI-TilOpprettet <> ? THEN
  DO:
     IF FI-TilOpprettet > Kunde.RegistrertDato THEN
         RETURN "AVBRYT".
  END.

  IF FI-FraEndret <> ? THEN
  DO:
    IF FI-FraEndret < Kunde.EDato THEN
        RETURN "AVBRYT".
  END.
  IF FI-TilEndret <> ? THEN
  DO:
      IF FI-TilEndret > Kunde.EDato THEN
        RETURN "AVBRYT".
  END.
  IF FI-FraFKjop <> ? THEN
  DO:
      if CAN-FIND(FIRST KundeSaldo WHERE
                    KundeSaldo.KundeNr = Kunde.KundeNr AND
                    KundeSaldo.ForsteDato < FI-FraFKjop) THEN
          RETURN "AVBRYT".
  END.

  IF FI-FraFKjop <> ? THEN
  DO:
      if CAN-FIND(FIRST KundeSaldo WHERE
                    KundeSaldo.KundeNr = Kunde.KundeNr AND
                    KundeSaldo.ForsteDato < FI-FraFKjop) THEN
          RETURN "AVBRYT".
  END.

  IF FI-FraSKjop <> ? THEN
  DO:
      if CAN-FIND(FIRST KundeSaldo WHERE
                    KundeSaldo.KundeNr = Kunde.KundeNr AND
                    KundeSaldo.ForsteDato < FI-FraSKjop) THEN
          RETURN "AVBRYT".
  END.

  IF FI-TilSKjop <> ? THEN
  DO:
      IF CAN-FIND(FIRST KundeSaldo WHERE
                    KundeSaldo.KundeNr = Kunde.KundeNr AND
                    KundeSaldo.ForsteDato > FI-TilSKjop) THEN
          RETURN "AVBRYT".
  END.

  IF FI-FraOmsDato <> ? then
  DO:
      IF FI-TilOmsDato <> ? then
      DO:
          wOms = 0.
          FOR EACH KundeTrans NO-LOCK where
            KundeTrans.KundeNr    = Kunde.KundeNr and
            KundeTrans.Dato      >= FI-FraOmsDato and
            KundeTrans.Dato      <= FI-TilOmsDato:

            assign
              wOms = wOms + (KundeTrans.Antall * (KundeTrans.Pris - KundeTrans.Mva))
              .

          END.
      END.
      ELSE DO:
          wOms = 0.
          FOR EACH KundeTrans NO-LOCK where
            KundeTrans.KundeNr    = Kunde.KundeNr and
            KundeTrans.Dato      >= FI-FraOmsDato:

            assign
              wOms = wOms + (KundeTrans.Antall * (KundeTrans.Pris - KundeTrans.Mva))
              .

          END.
      END.
      IF wOms < FI-Oms then
        RETURN "AVBRYT".
  END.
  ELSE IF FI-TilOmsDato <> ? then
  DO:
      wOms = 0.
      FOR EACH KundeTrans NO-LOCK where
        KundeTrans.KundeNr  = Kunde.KundeNr and
        KundeTrans.Dato      <= FI-TilOmsDato:

        assign
          wOms = wOms + (KundeTrans.Antall * (KundeTrans.Pris - KundeTrans.Mva))
          .

      END.
      IF wOms < FI-Oms then
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

  IF FI-KundeNavn <> "" THEN
  DO:
    /* Begynner med */
    IF RS-Filter = 1 THEN
    DO:
      IF NOT Kunde.Navn BEGINS FI-KundeNavn THEN
          RETURN "AVBRYT".
    END.
    /* Inneholder */
    ELSE IF RS-Filter = 2 THEN
    DO:
      IF NOT Kunde.Navn MATCHES "*" + FI-KundeNavn + "*" THEN
          RETURN "AVBRYT".
    END.
  END.

  BLOKK-1:
  DO:

    if T-Butikk then
      BUTIKKER:
      do wLoop2 = 1 to num-entries(wvButiker):

        if NOT CAN-FIND(FIRST ListeLinje where
                        ListeLinje.ListeType  = Lister.ListeType and
                        ListeLinje.ListeNr    = Lister.ListeNr and
                        ListeLinje.DataObjekt = string(Kunde.KundeNr) + "," +
                                                entry(wLoop2,wvButiker) and
                        ListeLinje.Div1       = entry(wLoop2,wvButiker)) then
          DO:
            create ListeLinje.
            assign
              ListeLinje.ListeType  = Lister.ListeType
              ListeLinje.ListeNr    = Lister.ListeNr
              ListeLinje.DataObjekt = string(Kunde.KundeNr) + "," +
                                      entry(wLoop2,wvButiker)
              ListeLinje.Div1       = entry(wLoop2,wvButiker)
              ListeLinje.CellNr     = wCellNr
              wCellNr               = wCellNr + 1
              ListeLinje.DivX[1]    = STRING(Kunde.KundeNr,"9999999999999")
              ListeLinje.DivX[2]    = Kunde.Navn
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
                      ListeLinje.DataObjekt = string(Kunde.KundeNr) + "," +
                                              "0") /* Allt legges på butikk 0 */ then
        DO:
          create ListeLinje.
          assign
            ListeLinje.ListeType  = Lister.ListeType
            ListeLinje.ListeNr    = Lister.ListeNr
            ListeLinje.DataObjekt = string(Kunde.KundeNr) + "," +
                                    "0" /* Allt legges på butikk 0 */
            ListeLinje.CellNr     = wCellNr
            wCellNr               = wCellNr + 1
              ListeLinje.DivX[1]    = STRING(Kunde.KundeNr,"9999999999999")
              ListeLinje.DivX[2]    = Kunde.Navn
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




