/* kordrelinje_byttvare.p
    Mottar ROWID på varelinje det skal legges inn ny vare på.
    Mottar buffer på ArtLag record som peker på vare som skal legges inn på ny varelinje.
    Kopierer varelinje til ny linje.
    Legger inn vare fra Artlag på den nye varelinjen.
    
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRowId AS CHARACTER NO-UNDO.
DEFINE VARIABLE iKOrdreLinjeNr AS INTEGER NO-UNDO.

DEFINE BUFFER bufKOrdreLinje FOR KOrdreLinje.

/* RowId på kordrelinje som skal kopieres og deaktiveres. */
cRowId = ENTRY(1,icParam,'|').

DO TRANSACTION:
  FIND bufKOrdreLinje EXCLUSIVE-LOCK WHERE
    bufKOrdreLinje.KOrdre_Id = DEC(ENTRY(1,icParam)) AND 
    bufKOrdreLinje.KOrdrELinjeNr = INT(ENTRY(2,icParam)) NO-ERROR.
  IF NOT AVAILABLE bufKOrdreLinje THEN 
  DO:
    ASSIGN 
      obOk     = FALSE 
      ocReturn = "** Ukjent KOrdreLinje mottatt i 'kordrelinje_byttvare.p' ( " + cRowId + " ): " + ERROR-STATUS:GET-MESSAGE(1)
      . 
  END. 

  FINN_LEDIG_NR:
  DO iKOrdreLinjeNr = 1 TO 10000000:
    IF NOT CAN-FIND(KOrdreLinje WHERE 
                    KOrdreLinje.KOrdre_Id = bufKOrdreLinje.KOrdre_Id AND 
                    KOrdreLinje.KOrdreLinjeNr = iKOrdreLinjeNr) THEN 
      LEAVE FINN_LEDIG_NR.
  END. /* FINN_LEDIG_NR */
  IF iKOrdreLinjeNr > 9999999 THEN
  DO:
    iKOrdreLinjeNr = 0. 
    ASSIGN 
      obOk     = FALSE 
      ocReturn = "** Ingen ledige linjenr."
      . 
  END.
  ELSE DO:  
    /* Deaktiverer rad. og legger inn peker til ny linje. */
    ASSIGN 
      bufKOrdreLinje.Aktiv             = FALSE
      bufKOrdreLinje.KopiKOrdreLinjeNr = iKOrdreLinjeNr  
      .
      
    CREATE KOrdreLinje.
    BUFFER-COPY bufKOrdreLinje
      EXCEPT KOrdreLinjeNr
      TO KOrdreLinje
      ASSIGN 
        KOrdreLinje.KOrdreLinjeNr     = iKOrdreLinjeNr
        KOrdreLinje.Aktiv             = TRUE
        KOrdreLinje.KopiKOrdreLinjeNr = bufKOrdreLinje.KOrdreLinjeNr
        .  
      
    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(ihBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " EXCLUSIVE-LOCK").
    hQuery:QUERY-OPEN().
    
    hQuery:GET-FIRST().
    BLOKKEN:
    REPEAT WHILE NOT hQuery:QUERY-OFF-END:
      ASSIGN
        ocReturn = ''
        obOk     = TRUE
        .
    
      FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE NO-ERROR.
      FIND StrKonv NO-LOCK WHERE 
        StrKonv.Storl = STRING(ihBuffer:BUFFER-FIELD("Storl"):BUFFER-VALUE) NO-ERROR.
      IF AVAILABLE StrKonv THEN 
        FIND LAST StrekKode NO-LOCK WHERE 
          StrekKode.ArtikkelNr = ArtBas.ArtikkelNr AND 
          StrekKode.StrKode    = StrKonv.StrKode NO-ERROR.

      IF NOT AVAILABLE ArtBas THEN
      DO: 
        ASSIGN
          ocReturn = '** Ukjent artikkel ' + STRING(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) + '. Gml artikkel ' + STRING(bufKORdreLinje.VareNr) + 
                     'Artlag_Kode:  ' + STRING(ihBuffer:BUFFER-FIELD("ArtLag_Kode"):BUFFER-VALUE)
          obOk     = FALSE
          .
      END.

      /* Legger inn ny vareinfo på den nye linjen. */
      IF AVAILABLE ArtBas THEN 
      ASSIGN 
        KOrdreLinje.VareNr     = STRING(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) 
        KOrdreLinje.Varetekst  = ArtBas.Beskr
        KOrdreLinje.Storl      = STRING(ihBuffer:BUFFER-FIELD("Storl"):BUFFER-VALUE) 
/*        KOrdreLinje.StrKode = ihBuffer:BUFFER-FIELD("StrKode":BUFFER-VALUE) ??? Kompileringsfeil */
        KOrdreLinje.StrKode    = StrKonv.StrKode
        KOrdreLinje.Kode       = IF AVAILABLE StrekKode THEN StrekKode.Kode ELSE '' 
        KOrdreLinje.LevFargKod = ArtBas.LevFargKod
        .
      LEAVE BLOKKEN.     
/*      hQuery:GET-NEXT().*/
    END.
  END. /* BLOKKEN */

  IF AVAIL KOrdreLinje THEN RELEASE KOrdreLinje.
  IF AVAIL bufKOrdreLinje THEN RELEASE bufKOrdreLinje.

END. /* TRANSACTION */
