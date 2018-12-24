/* Levering av kundeordre
   Parametere: <KOrdre_id>;<evt liste over linjenr|antall>
   
   Opprettet: 30.06.06 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix           AS INT    NO-UNDO.
DEF VAR iLnr         AS INT    NO-UNDO.
DEF VAR cLevVareList AS CHAR   NO-UNDO.
DEF VAR fKOrdre_id   AS DEC    NO-UNDO.
DEF VAR bDelLev      AS LOG    NO-UNDO.
DEF VAR fLevAnt      AS DEC    NO-UNDO.
DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

ASSIGN
    bTest = TRUE 
    cLogg = 'KOrdreUtlever' + REPLACE(STRING(TODAY),'/','')
    .

DEF BUFFER bKOrdreLinje FOR KOrdreLinje.

/* Sjekker om det finnes alfanumeriske tegn i artiklenes varenummer. */
FUNCTION SjekkVarenr RETURNS LOGICAL (INPUT icVarenr AS CHAR):
  DEF VAR fArtNr       AS DEC    NO-UNDO.

  fArtNr = DEC(icVarenr) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO.
  FIND FIRST ArtBas NO-LOCK
       WHERE ArtBas.ArtikkelNr = fArtNr 
       NO-ERROR.
  IF NOT AVAIL ArtBas THEN RETURN NO.
  ELSE RETURN YES.

END FUNCTION.

fKOrdre_id = DEC(ENTRY(1,icParam,";")).

FIND KOrdreHode EXCLUSIVE-LOCK
     WHERE KOrdreHode.KOrdre_id = fKOrdre_id
     NO-ERROR.

IF bTest THEN 
DO:
    RUN Bibl_LoggDbFri.p(cLogg,'Start kordre_levering.p').
    RUN Bibl_LoggDbFri.p(cLogg,'    fKOrdre_id: ' + STRING(fKOrdre_id) + '.').
    RUN Bibl_LoggDbFri.p(cLogg,'    AVAIL KOrdreHode: ' + STRING(AVAILABLE KOrdreHode) + '.').
    RUN Bibl_LoggDbFri.p(cLogg,'    icParam: ' + icParam + '.').
END.

IF AVAIL KOrdreHode THEN 
LEVERING: 
DO ON ERROR UNDO, LEAVE:

  IF NOT CAN-FIND(FIRST Kunde OF KOrdreHode) THEN DO:
    ocReturn = "Ugyldig kundenr: " + STRING(KOrdreHode.KundeNr) + " for ordre: " + STRING(KOrdreHode.Kordre_id).
    UNDO,LEAVE Levering.
  END.
  IF KOrdreHode.SendingsNr = '' THEN DO:
    ocReturn = "Sendingsnr er ikke angitt for ordre: " + STRING(KOrdreHode.Kordre_id).
    UNDO,LEAVE Levering.
  END.

  IF NUM-ENTRIES(icParam,";") > 1 THEN DO:
    cLevVareList = ENTRY(2,icParam,";").
  
    DO ix = 1 TO NUM-ENTRIES(cLevVareList,"|") BY 2:
      FIND FIRST KOrdreLinje EXCLUSIVE-LOCK
           WHERE KOrdreLinje.KOrdre_id     = fKOrdre_id
             AND KOrdreLinje.KOrdreLinjeNr = INT(ENTRY(ix,cLevVareList,"|"))
          NO-ERROR.
      ASSIGN lDec = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
      IF NOT ERROR-STATUS:ERROR THEN
      VARELINJER: 
      DO:
        fLevAnt = DEC(ENTRY(ix + 1,cLevVareList,"|")).
        IF AVAIL KOrdreLinje THEN DO:
          IF NOT SjekkVarenr (KOrdreLinje.VareNr) THEN DO:
            ocReturn = "Artikkelnr for linje " + ENTRY(ix,cLevVareList,"|") + " (" + KOrdreLinje.Varenr + ") er ikke gyldig." + CHR(10) +
                       "Velg f.eks en PLU artikkel - ta evt. først vare på varetekst og pris slik at dette kan legges inn igjen som overstyring".
            UNDO Levering,LEAVE Levering.
          END. 

          IF fLevAnt NE KOrdreLinje.Antall THEN DO:
            FIND LAST bKOrdreLinje WHERE bKOrdreLinje.KOrdre_id = fKOrdre_id
                 NO-LOCK NO-ERROR.
            iLnr = bKOrdreLinje.KOrdreLinjeNr + 10.
            CREATE bKOrdreLinje.
            BUFFER-COPY KOrdreLinje EXCEPT KOrdre_id KOrdreLinjeNr Antall TO bKOrdreLinje.
            ASSIGN bKOrdreLinje.KOrdre_id     = fKOrdre_id
                   bKOrdreLinje.KOrdreLinjeNr = iLnr
                   bKOrdreLinje.Antall        = KOrdreLinje.Antall - fLevAnt
                   KOrdreLinje.Antall         = fLevAnt 
                   bDelLev                    = YES
                   .
          END.
          KOrdreLinje.Leveringsdato  = TODAY.
        END.
        ELSE ocReturn = "Varelinje for kundeordre ikke tilgjengelig for oppdatering".
      END. /* VARELINJER */
      ELSE KOrdreLinje.Leveringsdato  = TODAY.
    END.
  END.
  ELSE FOR EACH KOrdreLinje OF KOrdreHode EXCLUSIVE-LOCK:
    ASSIGN lDec = DECIMAL(KOrdreLinje.VareNr) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
    VARELINJER: 
    DO:
      IF NOT SjekkVarenr (KOrdreLinje.VareNr) THEN DO:
        ocReturn = "Artikkelnr for linje " + STRING(KOrdreLinje.KOrdreLinjeNr) + " (" + KOrdreLinje.Varenr + ") er ikke gyldig." + CHR(10) +
                   "Velg f.eks en PLU artikkel - ta evt. først vare på varetekst og pris slik at dette kan legges inn igjen som overstyring".
        UNDO Levering,LEAVE Levering.
      END.
    END.           
    KOrdreLinje.Leveringsdato  = TODAY.
  END.

  ASSIGN KOrdreHode.Utsendelsesdato = TODAY
         KOrdreHode.LevStatus = IF CAN-FIND(FIRST KOrdreLinje OF KOrdreHode WHERE KOrdreLinje.Leveringsdato = ?) THEN "40" ELSE "50"
         .

  FIND FIRST SysPara NO-LOCK
       WHERE SysPara.SysHId = 19 
         AND SysPara.SysGr  = 1
         AND SysPara.ParaNr = INT(KOrdreHode.LevStatus)
       NO-ERROR.
  IF AVAIL SysPara THEN DO:
    ASSIGN ocReturn = SysPara.Parameter1
           obOk     = YES.
  END.
  ELSE ocReturn = "Ordrestatus for leveranser er ikke definert (sys.param 19,1,40/50)".
END. /* LEVERING */
ELSE ocReturn = "Kundeordre ikke tilgjengelig for oppdatering".

IF bTest THEN 
DO:
    RUN Bibl_LoggDbFri.p(cLogg,'    ocReturn' + ocReturn + '.').
    RUN Bibl_LoggDbFri.p(cLogg,'Slutt kordre_levering.p').
END.

IF NOT obOk THEN
  obOk = ocReturn = "".

  