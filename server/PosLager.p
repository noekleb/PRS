DEFINE INPUT  PARAMETER cParameter AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER cLager AS CHARACTER   NO-UNDO.

/* DEFINE INPUT  PARAMETER cData AS CHARACTER   NO-UNDO. */
DEFINE VARIABLE iButikkNr AS INTEGER     NO-UNDO.
DEFINE VARIABLE cStrekkode    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dArtikkelnr   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cStorlekar AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cStorlekarMEU AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cReturStr  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cHarLager  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ii         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iBestLoop AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLU        AS INTEGER     NO-UNDO.
DEFINE VARIABLE lLagerFinns AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cButs AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE cType       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lFunnet AS LOGICAL INIT TRUE  NO-UNDO.
DEFINE VARIABLE cStrekStorl AS CHARACTER   NO-UNDO.

DEFINE VARIABLE dDate AS DATE FORMAT "99/99/99"       NO-UNDO.
DEFINE VARIABLE iBestNr AS INTEGER     NO-UNDO. 
DEFINE VARIABLE cBestNr AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iLevBut AS INTEGER     NO-UNDO.
DEFINE TEMP-TABLE tt_lager NO-UNDO
    FIELD butik AS INTEGER
    FIELD cBLager AS CHAR
    FIELD cBLagersend AS CHAR
    FIELD AntLager AS DECI
    .
/* tt för nästa inleverans */
DEFINE TEMP-TABLE tt_inlev NO-UNDO
    FIELD butik AS INTEGER
    FIELD cBLager AS CHAR
    FIELD cBLagersend AS CHAR
    FIELD AntLager AS DECI
    .
    
   FOR EACH butiker NO-LOCK:
        cButs = cButs + (IF cbuts <> "" THEN "," ELSE "") + STRING(butiker.butik).
    END.

   cLager = "FEL,Inget lager".
   RELEASE Artbas.
   IF NUM-ENTRIES(cParameter,CHR(2)) = 3 THEN DO:
       cType       = ENTRY(1,cParameter,CHR(2)).
       IF cType = "E" THEN
           cStrekkode = ENTRY(2,cParameter,CHR(2)).
       ELSE
           dArtikkelnr = DECI(ENTRY(2,cParameter,CHR(2))) NO-ERROR.
       iButikkNr = INT(ENTRY(3,cParameter,CHR(2))).
       IF NOT ERROR-STATUS:ERROR THEN DO:
           IF cType = "E" THEN DO:
               FIND strekkode WHERE strekkode.kode = cStrekkode NO-LOCK NO-ERROR.
               IF AVAIL Strekkode THEN
                   FIND Artbas OF strekkode NO-LOCK NO-ERROR.
           END.
           IF NOT AVAIL ArtBas AND cType = "A" THEN
               FIND Artbas WHERE artbas.artikkelnr = dArtikkelnr NO-LOCK NO-ERROR.
       END.
   END.

EMPTY TEMP-TABLE TT_lager.
IF NOT AVAIL artbas THEN
    RETURN.

IF artbas.utgatt = TRUE THEN DO:
    cLager = "FEL,UTGÅTT".
    FIND FIRST Konvreg WHERE KonvReg.EDB-System = "RESTPAR" AND
                             Konvreg.Tabell     = ""        AND
                             KonvReg.EkstId     = STRING(artbas.artikkelnr) NO-LOCK NO-ERROR.
    IF AVAIL Konvreg THEN 
        FIND artbas WHERE artbas.artikkelnr = DECI(KonvReg.InterntId) NO-LOCK NO-ERROR.
    ELSE
        RELEASE Artbas.
    IF AVAIL Artbas THEN
        cLager = cLager + "," + "NY art: " + STRING(artbas.vg) + "/" + STRING(artbas.lopnr).
    ELSE
        cLager = cLager + "," + "NY art: OKÄND".
    RETURN.
END.
FOR EACH strtstr WHERE strtstr.strtypeid = artbas.strtypeid NO-LOCK:
    cStorlekar = cstorlekar + (IF cStorlekar <> "" THEN "," ELSE "") + TRIM(strtstr.sostorl).
    cStorlekarMEU = cStorlekarMEU + (IF cStorlekarMEU <> "" THEN "," ELSE "") + TRIM(strtstr.sostorl) + (IF strtstr.eustorl <> "" AND TRIM(strtstr.sostorl) <> TRIM(strtstr.eustorl) THEN "/" + strtstr.eustorl ELSE "").
END.

FOR EACH strekkode OF artbas NO-LOCK:
    FIND strkonv OF strekkode NO-LOCK.
    IF NOT CAN-DO(cStorlekar,TRIM(strkonv.storl)) THEN DO:
        cStorlekar = cStorlekar + (IF cStorlekar <> "" THEN "," ELSE "") + TRIM(strkonv.storl).
        cStorlekarMEU = cStorlekarMEU + (IF cStorlekarMEU <> "" THEN "," ELSE "") + TRIM(strkonv.storl).
    END.
END.
/* IF NOT lFunnet THEN DO:       */
/*     cStorlekar = cStrekStorl. */
/* END.                          */

cStorlekar = "Totfsg," + cStorlekar.
cStorlekarMEU = "Totfsg," + cStorlekarMEU.
cHarLager = FILL(",",NUM-ENTRIES(cStorlekar) - 1).
ENTRY(1,cHarLager) = "x".
 
/*  dDate = DATE(1,1,2100). */

 FOR EACH besthode WHERE besthode.artikkelnr = Artbas.artikkelnr NO-LOCK:
     IF besthode.beststat < 2 OR besthode.beststat > 4 THEN
         NEXT.
     IF besthode.levdato = ? /* OR besthode.levdato <= TODAY */ THEN
         NEXT.
/*      IF besthode.levdato < dDate THEN */
     DO:
         cBestNr = cBestNr + (IF cBestNr <> "" THEN "," ELSE "") + STRING(besthode.bestnr).
/*          iBestNr = besthode.bestnr. */
         dDate   = IF dDate = ? THEN besthode.levdato ELSE MAX(dDate,besthode.levdato).

     END.
 END.
IF cBestnr <> "" THEN DO: 
    iLevBut = 9990.
    DO iBestLoop = 1 TO NUM-ENTRIES(cBestNr):
        FIND besthode NO-LOCK WHERE besthode.bestnr = INT(ENTRY(iBestLoop,cBestnr)).
        iLevBut = iLevBut + 1.
        CREATE tt_inlev.
        ASSIGN tt_inlev.butik = iLevBut
               tt_inlev.cBLager = FILL(",",NUM-ENTRIES(cStorlekar) - 1).
        ENTRY(1,tt_inlev.cBLager) = STRING(MONTH(besthode.levdato),"99") + "-" + STRING(DAY(besthode.levdato),"99").
        FOR EACH beststr OF besthode WHERE beststr.beststat = besthode.beststat NO-LOCK:
            iLU = LOOKUP(TRIM(beststr.storl),cStorlekar).
            IF iLU > 0 THEN DO:
                tt_inlev.antlager = tt_inlev.antlager + beststr.Bestilt.
                ENTRY(iLU,tt_inlev.cBLager) = STRING(INT(ENTRY(iLU,tt_inlev.cBLager)) + beststr.Bestilt).
                ENTRY(iLU,cHarLager) = "x".
            END.
        END.
    END.
END.
 
/* DEFINE TEMP-TABLE tt_inlev NO-UNDO */
/*     FIELD butik AS INTEGER         */
/*     FIELD cDatum  AS CHAR          */
/*     FIELD cBLager AS CHAR          */
/*     FIELD cBLagersend AS CHAR      */
/*     FIELD AntLager AS DECI        */
 
 
DO ii = 1 TO NUM-ENTRIES(cButs):
    FOR EACH artlag WHERE artlag.artikkelnr = artbas.artikkelnr and
                          artlag.butik = INT(ENTRY(ii,cButs)) AND 
/*                           artlag.lagant > 0 AND */
                          CAN-DO(cStorlekar,TRIM(artlag.storl)) NO-LOCK:
        FIND tt_lager WHERE tt_lager.butik = INT(ENTRY(ii,cButs)) NO-ERROR.
        IF NOT AVAIL tt_lager THEN DO:
            CREATE tt_lager.
            ASSIGN tt_lager.butik  = artlag.butik
                   tt_lager.cBLager = FILL(",",NUM-ENTRIES(cStorlekar) - 1).
        END.
        IF artlag.lagant > 0 THEN
            tt_lager.antlager = tt_lager.antlager + artlag.lagant.
        iLU = LOOKUP(TRIM(artlag.storl),cStorlekar).
        IF artlag.lagant > 0 THEN DO:
            ENTRY(iLU,tt_lager.cBLager) = STRING(artlag.lagant).
            ENTRY(iLU,cHarLager) = "x".
        END.
        /* entry 1 håller på antsolgt */
        ENTRY(1,tt_lager.cBLager) = STRING(INT(ENTRY(1,tt_lager.cBLager)) + ArtLag.AntSolgt).
    END.
END.
DO ii = 1 TO NUM-ENTRIES(cHarLager):
    IF ENTRY(ii,cHarLager) <> "" THEN DO:
/*         cReturStr = cReturStr + (IF cReturStr <> "" THEN "," ELSE "") + ENTRY(ii,cStorlekar). */
        cReturStr = cReturStr + (IF cReturStr <> "" THEN "," ELSE "") + ENTRY(ii,cStorlekarMEU). /* Om vi inte har eustorlekar så är detta samma som cStorlekar */
/*             FILL(" ",iMax - LENGTH(ENTRY(ii,cStorlekar))) + ENTRY(ii,cStorlekar). */
    END.
END.
/*  */
FOR EACH tt_lager WHERE tt_lager.butik <> iButikkNr AND tt_lager.antlager = 0:
    DELETE tt_lager.
END.
IF NOT CAN-FIND(FIRST tt_lager WHERE tt_lager.butik <> iButikkNr) THEN DO:
    /* egna lagret endast kvar, on man inte har lag så skall den tas bort */
    FIND FIRST tt_lager NO-ERROR.
    IF AVAIL tt_lager AND tt_lager.antlager = 0 THEN
        DELETE tt_lager.

END.
FOR EACH tt_inlev:
    CREATE tt_lager.
    BUFFER-COPY tt_inlev TO tt_lager.
END.
/* nu har vi minst 1 butik med lager, */

FOR EACH tt_lager:
    DO ii = 1 TO NUM-ENTRIES(cHarLager):
        IF ENTRY(ii,cHarLager) <> "" THEN DO:
            tt_lager.cBLagersend = tt_lager.cBLagersend + (IF tt_lager.cBLagersend <> "" THEN "," ELSE "") + 
                         (IF ENTRY(ii,tt_lager.cBLager) <> "" THEN ENTRY(ii,tt_lager.cBLager) ELSE "").
        END.
    END.
    tt_lager.cBLagersend = REPLACE(tt_lager.cBLagersend," ","").
END.
IF NOT CAN-FIND(FIRST tt_lager WHERE ) THEN
    cLager = "FEL,Inget lager".
ELSE DO:
        cLager = cReturStr.
    FOR EACH tt_lager:
        cLager = cLager + ";" + string(tt_lager.butik) + "," + 
                         tt_lager.cBlagersend.

    END.
END.
