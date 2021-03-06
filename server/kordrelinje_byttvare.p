/* kordrelinje_byttvare.p
    Mottar ROWID p� varelinje det skal legges inn ny vare p�.
    Mottar buffer p� ArtLag record som peker p� vare som skal legges inn p� ny varelinje.
    Kopierer varelinje til ny linje.
    Legger inn vare fra Artlag p� den nye varelinjen.
    
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

/* RowId p� kordrelinje som skal kopieres og deaktiveres. */
cRowId = ENTRY(1,icParam,'|').

DO TRANSACTION:
  FIND KOrdreHode NO-LOCK WHERE 
    KOrdrEHode.KOrdre_Id = DEC(ENTRY(1,icParam,'|')) NO-ERROR.
    
  FIND bufKOrdreLinje EXCLUSIVE-LOCK WHERE
    bufKOrdreLinje.KOrdre_Id = DEC(ENTRY(1,icParam,'|')) AND 
    bufKOrdreLinje.KOrdreLinjeNr = INT(ENTRY(2,icParam,'|')) NO-ERROR.
  IF NOT AVAILABLE bufKOrdreLinje THEN 
  DO:
    ASSIGN 
      obOk     = FALSE 
      ocReturn = "** Ukjent KOrdreLinje mottatt i 'kordrelinje_byttvare.p' ( " + cRowId + "/" + ENTRY(2,icParam,'|')
      . 
    RETURN.
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
    RETURN.
  END.
  ELSE 
  BLOKKEN:
  DO:  

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
    /* Er det en retur ordre det byttes varelinje p�, skal antallet og bel�pene p� den nye linjen settes til positivt antall. */
    /* Den motposterer den opprinnelige linjen slik at totalen p� de to linjene p� ordren blir 0.                             */
    IF KOrdreHode.SendingsNr = 'RETUR' THEN 
    DO:
      ASSIGN 
      KOrdreLinje.Antall        = KOrdreLinje.Antall * -1
      KOrdreLinje.Mva%          = ABS(KOrdreLinje.Mva%)
      KOrdreLinje.BruttoPris    = KOrdreLinje.BruttoPris * -1
      KOrdreLinje.Pris          = KOrdreLinje.Pris * -1
      KOrdreLinje.MvaKr         = KOrdreLinje.MvaKr * -1
      KOrdreLinje.Linjesum      = KOrdreLinje.Linjesum * -1
      KOrdreLinje.LinjeRab%     = ABS(KOrdreLinje.LinjeRab%)
      KOrdreLinje.LinjeRabattKr = KOrdreLinje.LinjeRabattKr * -1
      KOrdreLinje.NettoLinjesum = KOrdreLinje.NettoLinjesum * -1
      KOrdreLinje.NettoPris     = KOrdreLinje.NettoPris * -1    
      . 
    END.  
      
    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(ihBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " EXCLUSIVE-LOCK").
    hQuery:QUERY-OPEN().
    
    hQuery:GET-FIRST().
    BLOKKEN:
    REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    
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

      /* Legger inn ny vareinfo p� den nye linjen. */
      IF AVAILABLE ArtBas THEN 
      ASSIGN 
        KOrdreLinje.VareNr     = STRING(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) 
        KOrdreLinje.Varetekst  = ArtBas.Beskr
        KOrdreLinje.Storl      = STRING(ihBuffer:BUFFER-FIELD("Storl"):BUFFER-VALUE) 
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

  ASSIGN
    ocReturn = ''
    obOk     = TRUE
    .

END. /* TRANSACTION */
