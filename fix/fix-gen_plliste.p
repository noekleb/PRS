DEF VAR cVgLst      AS CHAR NO-UNDO.
DEF VAR lSumAnt     AS DEC  NO-UNDO.
DEF VAR lPlListeId  AS DEC  NO-UNDO.
DEF VAR iCL         AS INT  NO-UNDO.
DEF VAR ibutNr      AS INT NO-UNDO.
DEF VAR iLoop       AS INT NO-UNDO.
DEF VAR iVg         AS INT NO-UNDO.

ASSIGN
    iCL    = 20
    iButNr = 2
    cVgLst = '60000,60010,60020,60030,60040'
    .

DEF BUFFER bufPlListeLinje FOR plListeLinje.
DEF BUFFER clButiker       FOR Butiker.


/* Temp-Tabell som holder på lagerantall på sentrallager. */
DEF TEMP-TABLE tmpArtLag
    FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr
    FIELD StrKode    LIKE Strekkode.StrKode
    FIELD Antall     AS DEC FORMAT "->>,>>9.999"
    INDEX Lager ArtikkelNr StrKode.

/* Temp-Tabell som holder på antall solgt pr. artikkel, størrelse og butikk */
DEF TEMP-TABLE tmpPlukk
    FIELD ArtikkelNr   LIKE Artlag.ArtikkelNr
    FIELD StrKode      LIKE ArtLag.StrKode
    FIELD ButikkNr     AS INT FORMAT ">>>>>9"
    FIELD PrioPlukket  AS INT FORMAT ">>>9"
    FIELD Antall       AS DEC FORMAT "->>,>>9.999"
    INDEX Plukk PrioPlukket ButikkNr ArtikkelNr StrKode.

FIND Butiker NO-LOCK WHERE
    Butiker.Butik = ibutNr NO-ERROR.
FIND clButiker NO-LOCK WHERE
    clButiker.butik = iCL NO-ERROR.

/* Oppretter plukklistehode. */
IF (NOT AVAILABLE PlListeHode OR lPlListeId = 0) THEN
OPPSTANDELSEN:
DO:
  FIND LAST PlListeHode NO-LOCK USE-INDEX PlListeHode NO-ERROR.
  IF AVAILABLE PlListeHode THEN
      lPlListeId  = PlListeHode.PlListeId + 1.
  ELSE lPlListeId = 1.
  /* Er nummerserie full, får vi lete etter hull */
  IF lPlListeId > 99999999 THEN
  LOOPEN:
  DO lPlListeId = 1 TO 99999999:
    IF NOT CAN-FIND(PlListeHode WHERE
                    PlListeHode.PlListeId = lPlListeId) THEN
        LEAVE LOOPEN.
  END. /* LOOPEN */
  /* Oppretter listehode. */
  CREATE PlListeHode.
  ASSIGN
      PlListeHode.PlListeId   = lPlListeId
      PlListeHode.FraButikkNr = iCL
      PlListeHode.TilButikkNr = iButNr
      PlListeHode.DatoPlukket = ?
      PlListeHode.TidPlukket  = 0
      PlListeHode.PrioPlukket = Butiker.PrioPlukket
      PlListeHode.PlNavn      = "Overf. til " + Butiker.ButNamn
      PlListeHode.PlMerknad   = "" /*"Plukkliste til " + Butiker.ButNamn*/
      PlListeHode.PlLType     = 1 /* Plukkliste */
      .
END. /* OPPSTANDELSEN */

MAT_LOOP:
DO iLoop = 1 TO NUM-ENTRIES(cVgLst):
    iVg = INT(ENTRY(iLoop,cVgLst)).
 
    FOR EACH ArtBas NO-LOCK WHERE
        ArtBas.Vg = iVg,
        EACH Strekkode NO-LOCK WHERE
            Strekkode.ArtikkelNr = ArtBas.ArtikkelNr:
        FIND StrKonv OF Strekkode NO-LOCK NO-ERROR.
        IF NOT AVAILABLE StrKonv THEN
            NEXT.

        CREATE tmpPlukk.
        ASSIGN
            tmpPlukk.ArtikkelNr  = ArtBas.ArtikkelNr
            tmpPlukk.StrKode     = StrKonv.StrKode
            tmpPlukk.ButikkNr    = iButNr
            tmpPlukk.PrioPlukket = Butiker.PrioPlukket
            tmpPlukk.antall      = 2
            lSumant = lSumAnt + tmpPlukk.antall
            .
    END.

END. /* MAT_LOOP */ 

RUN PlukkBehandling(lPlListeId, lSumant).

PROCEDURE PlukkBehandling:
    DEF INPUT PARAMETER plPlListeId AS DEC NO-UNDO.
    DEF INPUT PARAMETER plSumAnt    AS DEC NO-UNDO.
    
    /* Leser plukkanmodning i prioritert rekkefølge. */
    /* Her posteres plikkliste linjene.              */
    PLUKKBEHANDLING:
    FOR EACH tmpPlukk WHERE tmpPlukk.Antall > 0
        BREAK BY tmpPlukk.PrioPlukket
              BY tmpPlukk.ButikkNr
              BY tmpPlukk.ArtikkelNr
              BY tmpPlukk.StrKode:

      /* Må finnes */
      FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = tmpPlukk.ArtikkelNr NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
          NEXT PLUKKBEHANDLING.

      /* Skaper plukklistelinjen hvis den ikke finnes fra før */
      FIND PlListeLinje EXCLUSIVE-LOCK WHERE
          PlListeLinje.PlListeId  = plPlListeId AND
          PlListeLinje.ArtikkelNr = tmpPlukk.ArtikkelNr AND
          PlListeLinje.StrKode    = tmpPlukk.StrKode NO-ERROR.
      IF NOT AVAILABLE PlListeLinje THEN
      DO:
          FIND LAST bufPlListeLinje NO-LOCK WHERE
              bufPlListeLinje.PlListeId = plPlListeId USE-INDEX PlListeLinje NO-ERROR.
          CREATE PlListeLinje.
          ASSIGN
              PlListeLinje.PlListeId  = plPlListeId 
              PlListeLinje.PlLinjeNr  = IF AVAILABLE bufPlListeLinje
                                          THEN bufPlListeLinje.PlLinjeNr + 1
                                          ELSE 1
              PlListeLinje.ArtikkelNr = tmpPlukk.ArtikkelNr 
              PlListeLinje.StrKode    = tmpPlukk.StrKode 
              .
      END.

      /* Akkumulerer opp antall på plukke for artikkel, størrelse og butikk */
      IF AVAILABLE PlListeLinje THEN
          ASSIGN
            PlListeLinje.VarGr  = ArtBas.Vg
            plListeLinje.LopNr  = ArtBas.LopNr
            PlListeLinje.Antall = PlListeLinje.Antall + tmpPlukk.Antall.
    END. /* PLUKKBEHANDLING */

    HODESUM:
    DO TRANSACTION:
        FIND PlListeHode EXCLUSIVE-LOCK WHERE
            PlListeHode.PlListeId = plPlListeId NO-ERROR.
        IF AVAILABLE PlListeHode THEN 
        DO:
            ASSIGN
              PlListeHode.Antall = lSumAnt.
            /* Tomme lister fjernes (Linjer tas eventuelt i trigger) */
            IF PlListeHode.Antall = 0 OR
                NOT CAN-FIND(FIRST PlListeLinje OF PlListeHode) THEN
                DELETE PlListeHode.
        END.
    END. /* HODESUM */
END PROCEDURE.
