DEFINE VARIABLE cSenast AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dDatum  AS DATE        NO-UNDO.
DEFINE VARIABLE iAarPerlinNr AS INTEGER     NO-UNDO.

FOR EACH butiker NO-LOCK:
    /* Hämta senast körda */
    cSenast = "20170203".
    /* om inte finns i listan så hoppa ur */
    /* Sätt nästa dag */
    dDatum = DATE(INT(SUBSTR(cSenast,5,2)),INT(SUBSTR(cSenast,7,2)),INT(SUBSTR(cSenast,1,4))) + 1.
    /* kontrollera att godkänd dagsrapport finns */
    /* om inte hoppa över */
    /* kör alla datum från sist körda, men hoppa ur när en inte godkänd hittas inte finns */
    /* för varje körd dag uppdatera syspara */
iAarPerLinNr = YEAR(dDatum) * 1000.
iAarPerlinNr = iAarPerLinNr + (dDatum - DATE(12,31,YEAR(dDatum) - 1)).
MESSAGE iAarPerLinNr dDatum
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
FOR  EACH stlinje WHERE stlinje.butik = butiker.butik AND stlinje.sttypeid = "BUTSTAT" AND perid = "DAG" AND aarperlinnr = iAarperlinnr NO-LOCK.
DISP perid aarperlinnr StLinje.EDato.
END.
END.

