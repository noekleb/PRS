&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE dDato AS DATE    NO-UNDO.
DEFINE VARIABLE dFrom AS DATE    NO-UNDO.
DEFINE VARIABLE dTo AS DATE    NO-UNDO.
DEFINE VARIABLE cButListe AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iAntBong AS INTEGER     NO-UNDO.
DEFINE VARIABLE iAntUtensilier AS INTEGER     NO-UNDO.
DEFINE VARIABLE cDataObjekt AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DEFINE VARIABLE i2 AS INTEGER     NO-UNDO.
DEFINE VARIABLE lOkBilag AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iPerlinnr AS INTEGER     NO-UNDO.
dFrom = TODAY.
DEFINE VARIABLE cButiker AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cVgList AS CHARACTER   NO-UNDO.
DEFINE VARIABLE eMailTitle AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cExcelfil AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRapportfil AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSumTagExcel AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMailto AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cErrorfil AS CHARACTER   NO-UNDO.
DEFINE TEMP-TABLE tt_hitrate NO-UNDO
    FIELD selgernr AS INTE
    FIELD namn     AS CHAR
    FIELD antutens AS INTE
    FIELD utensbrutto AS DECI
    FIELD antsolgt    AS INTE
    FIELD bruttosolgt    AS DECI.
DEFINE TEMP-TABLE tt_NoEOD
    FIELD butik AS INTE.

{methodexcel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{syspara.i 210 275 1 cButiker}

{syspara.i 210 50 5 cSumTagExcel}
/* cButiker = "1,2,3,4,5,6,7,8,9,10". */
FOR EACH vargr WHERE vargr.hg = 5 NO-LOCK.
    IF NOT CAN-DO(cVgList,STRING(vargr.vg)) THEN
        cVgList = cVgList + (IF cVgList <> "" THEN "," ELSE "") + STRING(vargr.vg).
END.
IF WEEKDAY(dFrom) = 1 THEN
    dFrom = dFrom - 13.
ELSE 
    dFrom = dFrom - WEEKDAY(dFrom) - 5.
dTo = dFrom + 6.

lOkBilag = TRUE.
DO ii = 1 TO NUM-ENTRIES(cbutiker):
    FIND FIRST Bokforingsbilag WHERE Bokforingsbilag.Omsetningsdato = dTo AND Bokforingsbilag.ButikkNr = INT(ENTRY(ii,cButiker)) NO-LOCK NO-ERROR.
    IF NOT AVAIL Bokforingsbilag OR Bokforingsbilag.EODMottatt = FALSE THEN DO:
         lOkBilag = FALSE.
         CREATE tt_NoEOD.
         tt_NoEOD.butik = INT(ENTRY(ii,cButiker)).
    END.
END.

IF lOkBilag THEN DO:
    cRapportFil = SESSION:TEMP-DIR + "Hitrate_" + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") .
    cExcelFil   = cRapportFil + ".xlsx".
    cRapportFil = cRapportFil + ".txt".
    RUN getData.
    IF CAN-FIND(FIRST tt_hitrate WHERE tt_hitrate.antsolgt > 0) THEN DO:
        RUN data2Excel.
        /* cExcelfil */
    END.
    ELSE DO:
        cErrorfil  = SESSION:TEMP-DIR + "Rapportinfo.txt".
        OUTPUT TO VALUE(cErrorfil).
        PUT UNFORMATTED "Hitrate saknas för perioden: " dFrom " - " dTo SKIP.
        OUTPUT CLOSE.
        
        eMailTitle = "Hitrate saknas".
    END.
END.
ELSE DO:
    cErrorfil  = SESSION:TEMP-DIR + "Rapportinfo.txt".
    OUTPUT TO VALUE(cErrorfil).
    PUT UNFORMATTED "EOD saknas för butik: " dTo SKIP.
    FOR EACH tt_NoEOD:
        PUT UNFORMATTED tt_NoEOD.butik SKIP.
    END.
    OUTPUT CLOSE.
    eMailTitle = "EOD saknas för hitrate".
END.
IF cErrorfil <> "" THEN DO:

    RUN sendmail_tsl.p ("HITRATE",eMailTitle + " " + STRING(dTo),cErrorfil,"","","") NO-ERROR.
    OS-DELETE VALUE(cErrorfil).
END.
IF SESSION:PARAMETER = "B" THEN
    QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-data2Excel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE data2Excel Procedure 
PROCEDURE data2Excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iRad AS INTEGER INIT 1    NO-UNDO.
    OUTPUT TO VALUE(cRapportfil).
    PUT UNFORMATTED 
        "Säljare;Beskrivning;Sålt;Sålt brutto; ; ;Säljare;Beskrivning;Sålt;Sålt brutto" SKIP.
    FOR EACH tt_hitrate WHERE tt_hitrate.antsolgt > 0 BY tt_hitrate.selgernr:
        iRad = iRad + 1.
        PUT UNFORMATTED tt_hitrate.selgernr ";"
                        tt_hitrate.namn     ";"
                        tt_hitrate.antutens ";"
                        ROUND(tt_hitrate.utensbrutto,0) ";"
                        "=" cSumTagExcel "(C" STRING(iRad) "/I" STRING(iRad) ")" ";"
                        "=" cSumTagExcel "(D" STRING(iRad) "/J" STRING(iRad) ")" ";"
/*                         " ;" */
/*                         " ;" */
                        tt_hitrate.selgernr ";"
                        tt_hitrate.namn     ";"
                        tt_hitrate.antsolgt ";"
                        ROUND(tt_hitrate.bruttosolgt,0) SKIP.
    END.
    OUTPUT CLOSE.
    CREATE "Excel.Application" chExcelApplication.  
    chExcelApplication:Visible = FALSE.                                     
    chWorkbooks = chExcelApplication:Workbooks:OpenText(cRapportFil,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).

    /*   STATUS DEFAULT "Setter aktivt ark...". */
    chWorkSheets = chExcelApplication:Sheets:Item(1).

    chWorkSheets:Range("A1:J1"):Font:Bold = TRUE.

    chWorkSheets:Range("E:F"):NumberFormat = "0%".
/*         chWorkSheets:Range("A:A"):NumberFormat = "#0". */

    chWorkSheets:Range("C:D"):NumberFormat = "# ##0".
    chWorkSheets:Range("I:J"):NumberFormat = "# ##0".


/*     chWorkSheets:Range(cSumBold):Font:Bold = TRUE. */
    chWorkSheets:Columns("A:J"):AutoFit().
/* !!!    chWorkSheets:Columns("A:A"):HorizontalAlignment = -4108. */
    chExcelApplication:ActiveWorkbook:SaveAs(cExcelFil,51,,,,,,,,,,).  
    chExcelApplication:QUIT.
    RELEASE OBJECT chWorksheets NO-ERROR.            /* release com-handles */
    RELEASE OBJECT chWorkbooks NO-ERROR.             /* release com-handles */
    RELEASE OBJECT chExcelApplication NO-ERROR.      /* release com-handles */
    ASSIGN chWorksheets       = ?
         chWorkbooks        = ?
         chExcelApplication = ?.

    IF SEARCH(cRapportFil) <> ? THEN
        OS-DELETE VALUE(cRapportFil).
    RUN sendmail_tsl.p ("HITRATE","Hitrate " +  STRING(dFrom) + " " + STRING(dTo),cExcelFil,"","","") NO-ERROR.
    OS-DELETE VALUE(cExcelFil).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getData Procedure 
PROCEDURE getData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH selger NO-LOCK.
        CREATE tt_hitrate.
        ASSIGN tt_hitrate.selgernr = selger.selgernr
               tt_hitrate.namn     = Selger.Navn.
        DO dDato = dFrom TO dTo:
            iPerlinnr = dDato - DATE(12,31,YEAR(dDato) - 1).
            cDataObjekt = STRING(selger.selgernr).
            cDataObjekt = FILL("0",13 - LENGTH(cDataObjekt)) + cDataObjekt.
            FOR EACH stlinje WHERE stlinje.dataobjekt = cDataobjekt AND
                                   stlinje.sttypeid   = "SELGER"    AND
                                   stlinje.perid      = "DAG"       AND
                                   stlinje.aar        = YEAR(dDato) AND
                                   stlinje.perlinnr   = iPerlinnr  NO-LOCK.
                IF NOT CAN-DO(cButiker,STRING(stlinje.butik)) THEN
                    NEXT.
                ASSIGN tt_hitrate.antsolgt    = tt_hitrate.antsolgt    + stlinje.antsolgt
                       tt_hitrate.bruttosolgt = tt_hitrate.bruttosolgt + StLinje.VerdiSolgt + StLinje.MvaVerdi.
            END.
            DO i2 = 1 TO NUM-ENTRIES(cVgList):
                FOR EACH stlinje WHERE stlinje.dataobjekt = cDataobjekt + CHR(1) + STRING(INT(ENTRY(i2,cVgList)),"999999") AND
                                       stlinje.sttypeid   = "SELGER-VG"    AND
                                       stlinje.perid      = "DAG"       AND
                                       stlinje.aar        = YEAR(dDato) AND
                                       stlinje.perlinnr   = iPerlinnr  NO-LOCK.
                    ASSIGN tt_hitrate.antutens    = tt_hitrate.antutens    + stlinje.antsolgt
                           tt_hitrate.utensbrutto = tt_hitrate.utensbrutto + StLinje.VerdiSolgt + StLinje.MvaVerdi.
                END.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

