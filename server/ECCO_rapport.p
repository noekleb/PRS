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

DEFINE VARIABLE ii       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iButik   AS INTEGER     NO-UNDO.
DEFINE VARIABLE cButiker AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cKasser  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLevs    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cHgShoes AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cHgAA    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iWeeknum AS INTEGER     NO-UNDO.

DEFINE VARIABLE dAntShoes AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dAntAA    AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dSumShoes AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dSumAA    AS DECIMAL     NO-UNDO.

DEFINE VARIABLE dMonday AS DATE        NO-UNDO.
DEFINE VARIABLE dDatum  AS DATE        NO-UNDO.
DEFINE VARIABLE dToday  AS DATE        NO-UNDO.
DEFINE VARIABLE lKvitto AS LOGICAL     NO-UNDO.
DEFINE VARIABLE dLastday  AS DATE        NO-UNDO.
DEFINE VARIABLE dFirstday AS DATE        NO-UNDO.
DEFINE VARIABLE iLastWeekNum  AS INTEGER     NO-UNDO.
DEFINE VARIABLE cExcelMailFiler AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSumTagExcel AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cVariant AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cArtlistaAA AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE tt_ecco
    FIELD vecka AS CHAR
    FIELD butik AS INTE
    FIELD AntShoes AS DECI
    FIELD SumShoes AS DECI
    FIELD AntAA    AS DECI
    FIELD sumAA    AS DECI
    FIELD visitors AS INTE
    FIELD sumKvitton AS INTE
    INDEX vb IS PRIMARY UNIQUE vecka butik
    INDEX butik butik.

DEF STREAM sExportFile.

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
/* hämta syspara */
{syspara.i 210 50 1 cButiker}
{syspar2.i 210 50 1 cKasser}
{syspara.i 210 50 2 cLevs}
{syspara.i 210 50 3 cHgShoes}
{syspara.i 210 50 4 cHgAA}
{syspara.i 210 50 5 cSumTagExcel}
{syspara.i 210 50 6 cVariant}
{syspara.i 210 50 7 cArtlistaAA}

IF cSumTagExcel = "" THEN
    cSumTagExcel = "SUM".
IF cButiker = "" OR cLevs = "" THEN
    RETURN.
RUN setDates.
CASE cVariant:
    WHEN "1" THEN DO:
        IF cHgShoes = "" OR cHgAA = "" THEN
            RETURN.
        RUN RapportJF.
    END.
    WHEN "2" THEN
        RUN RapportRyns.
    WHEN "3" THEN
        RUN RapportAugust.
    WHEN "4" THEN
        RUN RapportSkohorn.
    OTHERWISE
        RETURN.
END CASE.
/* RUN TabortTomma. */
RUN ExporteraExcel.
/* RUN Exportera. */
/* IF cExcelMailFiler <> "" THEN DO:                                                                              */
/* /*     RUN sendmail_tsl.p ("ECCORAPPORT","Eccorapport",REPLACE(cExcelMailFiler,","," "),"","","") NO-ERROR. */ */
/* END.                                                                                                           */
QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Exportera) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Exportera Procedure 
PROCEDURE Exportera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    OUTPUT TO "C:\tmp\ecco.txt".
    FOR EACH tt_ecco:
        EXPORT DELIMITER ";" tt_ecco.
    END.
    OUTPUT CLOSE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExporteraExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExporteraExcel Procedure 
PROCEDURE ExporteraExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iButik AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cRapportFil AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cExcelFil    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iVecka AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cSumBold AS CHARACTER   NO-UNDO.
/*     DEFINE VARIABLE cLastRowCol AS CHARACTER   NO-UNDO. */
    iVecka = INT(SUBSTR(STRING(iLastWeekNum),5,2)).
    cSumBold = "A" + STRING(iVecka + 4) + ":" + "G" + STRING(iVecka + 4).
/*     cLastRowCol = "G" + STRING(iVecka + 4).             */
    DO ii = 1 TO NUM-ENTRIES(cButiker):
        iButik = INT(ENTRY(ii,cButiker)).
        FIND butiker WHERE butiker.butik = iButik NO-LOCK NO-ERROR.
        IF NOT AVAIL butiker THEN
            NEXT.
        IF NOT CAN-FIND(FIRST tt_ecco WHERE tt_ecco.butik = iButik) THEN
            NEXT.
        cRapportFil = SESSION:TEMP-DIR + "Ecco_" + string(iLastWeekNum) + "_" + STRING(iButik).
        cExcelFil   = cRapportFil + ".xlsx".
        cRapportFil = cRapportFil  + ".tmp".
        IF SEARCH(cRapportFil) <> ? THEN
            OS-DELETE VALUE(cRapportFil).
        IF SEARCH(cExcelFil) <> ? THEN
            OS-DELETE VALUE(cExcelFil).
          
        /* Åpner stream */
        OUTPUT STREAM sExportFile TO VALUE(cRapportFil) NO-ECHO.
          
         /* Legger ut overskrifter. */
        EXPORT STREAM sExportFile DELIMITER ";"
           "STORE " + Butiker.butnamn + " " + SUBSTR(STRING(iLastWeekNum),1,4) SKIP.
        EXPORT STREAM sExportFile DELIMITER ";" " " SKIP.
        EXPORT STREAM sExportFile DELIMITER ";"
            /* A  */ "Week"          
            /* B  */ "Sales Shoes"              
            /* C  */ "Sales AA"                 
            /* D  */ "Units Shoes"        
            /* E  */ "Unit AA"
            /* F  */ "Visitors"               
            /* G  */ "Antal Ecco Kvitton" SKIP.
/*         EXPORT STREAM sExportFile DELIMITER ";" " " " " " " " " " " " " " " SKIP. */
        FOR EACH tt_ecco WHERE tt_ecco.butik = iButik BY tt_ecco.vecka:
            EXPORT STREAM sExportFile DELIMITER ";"
                   INT(SUBSTR(vecka,5))
                   SumShoes
                   sumAA
                   AntShoes
                   AntAA
                   visitors
                   sumKvitton SKIP.
        END.
        EXPORT STREAM sExportFile DELIMITER ";"
            " "
            "=" + cSumTagExcel + "(B4:B" + STRING(iVecka + 3) + ")"
            "=" + cSumTagExcel + "(C4:C" + STRING(iVecka + 3) + ")"
            "=" + cSumTagExcel + "(D4:D" + STRING(iVecka + 3) + ")"
            "=" + cSumTagExcel + "(E4:E" + STRING(iVecka + 3) + ")"
            "=" + cSumTagExcel + "(F4:F" + STRING(iVecka + 3) + ")"
            "=" + cSumTagExcel + "(G4:G" + STRING(iVecka + 3) + ")".
        OUTPUT STREAM sExportFile CLOSE.


        /*   STATUS DEFAULT "Importerer data i Excel...". */
        CREATE "Excel.Application" chExcelApplication.  
        chExcelApplication:Visible = FALSE.                                     
        chWorkbooks = chExcelApplication:Workbooks:OpenText(cRapportFil,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).
        
        /*   STATUS DEFAULT "Setter aktivt ark...". */
        chWorkSheets = chExcelApplication:Sheets:Item(1).
        
        chWorkSheets:Range("A1:G1"):Merge().
        chWorkSheets:Range("A1:D1"):Font:Bold = TRUE.
        
        /*   chWorkSheets:Range("A1:G1"):Font:Italic = TRUE. */
        chWorkSheets:Range("A3:G3"):Font:Bold = TRUE.
        
        chWorkSheets:Range("A:A"):NumberFormat = "#0".
        
        chWorkSheets:Range("B:G"):NumberFormat = "# ##0".
        
        /*   STATUS DEFAULT "Setter overskrift...". */
/*         chWorkSheets:Range("A3:A4"):Merge().         */
/*         chWorkSheets:Range("A3:A4"):WrapText = TRUE. */
/*         chWorkSheets:Range("A3:A4"):Merge(). */
/*         chWorkSheets:Range("B3:B4"):Merge(). */
/*         chWorkSheets:Range("C3:C4"):Merge(). */
/*         chWorkSheets:Range("D3:D4"):Merge(). */
/*         chWorkSheets:Range("E3:E4"):Merge(). */
/*         chWorkSheets:Range("F3:F4"):Merge(). */
/*         chWorkSheets:Range("G3:G4"):Merge(). */
        
/*         chWorkSheets:Range("B5:" + cLastRowCol):Pattern = 1. */
/*         chWorkSheets:Range("B5:G27"):Pattern = 1.                          */
/*         chWorkSheets:Range("B5:" + cLastRowCol):PatternColorIndex = -4105. */
/*         chWorkSheets:Range("B5:" + cLastRowCol):Color = 65535.             */

        chWorkSheets:Range(cSumBold):Font:Bold = TRUE.
        chWorkSheets:Columns("A:G"):AutoFit().
        chWorkSheets:Columns("A:A"):HorizontalAlignment = -4108.
/*         chWorkSheets:PageSetup:Orientation    = 1. */
/*         chWorkSheets:PageSetup:FitToPagesWide = 1. */
        /*   chExcelApplication:Visible = TRUE. */
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
        cExcelMailFiler = cExcelMailFiler + (IF cExcelMailFiler <> "" THEN "," ELSE "") + cExcelFil.
        RUN sendmail_tsl.p ("ECCORAPPORT","Eccorapport " +  STRING(iLastWeekNum) + " " + Butiker.butnamn,cExcelFil,"","","") NO-ERROR.
        OS-DELETE VALUE(cExcelFil).
/*         IF NUM-ENTRIES(cExcelMailFiler) = 2 THEN */
/*             LEAVE.                               */
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RapportAugust) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportAugust Procedure 
PROCEDURE RapportAugust :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cWeekNum AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iWeekNum AS INTEGER     NO-UNDO.
DEFINE VARIABLE lOK      AS LOGICAL     NO-UNDO.
/* dMonday = IF WEEKDAY(TODAY) = 1 THEN TODAY - 13 ELSE TODAY - (WEEKDAY(TODAY)  + 5). */

DO ii = 1 TO NUM-ENTRIES(cButiker):
    iButik = INT(ENTRY(ii,cButiker)).
    DO dDatum = dFirstday TO dLastday.
        RUN weeknum.p(dDatum,OUTPUT cWeekNum).
        FIND tt_ecco WHERE tt_ecco.vecka = cWeeknum AND
                           tt_ecco.butik = ibutik NO-ERROR.
        IF NOT AVAIL tt_ecco THEN DO:
            CREATE tt_ecco.
            ASSIGN tt_ecco.vecka = cWeekNum
                   tt_ecco.butik = iButik.
        END.
        FOR EACH Bonghode WHERE bonghode.butikknr = iButik AND
                                bonghode.dato     = dDatum NO-LOCK:
            lKvitto = FALSE.
            IF Bonghode.Makulert = 2 THEN
                NEXT.
/*             lOK = FALSE.                        */
/*             CASE Bonghode.butikknr:             */
/*                 WHEN 1 THEN                     */
/*                     lOK = Bonghode.kassenr = 4. */
/*                 WHEN 2 THEN                     */
/*                     lOK = Bonghode.kassenr = 2. */
/*             END CASE.                           */
/*             IF NOT lOK THEN                     */
/*                 NEXT.                           */
            FOR EACH bonglinje WHERE bonglinje.b_id = bonghode.b_id AND bonglinje.makulert = FALSE NO-LOCK:
                IF NOT CAN-DO("1,3,10", STRING(bonglinje.ttid))  THEN
                    NEXT.
/*                 IF NOT CAN-DO(cLevs,STRING(bonglinje.levnr)) AND bonglinje.varegr <> 50 THEN */
/*                     NEXT.                                                                    */
                IF NOT CAN-DO("17,18,19,50,90",STRING(bonglinje.varegr)) THEN
                    NEXT.
                IF CAN-DO("17,18,19",string(bonglinje.varegr)) THEN DO:
                    lKvitto = TRUE.      /* levnr 160 */
                    RUN SummeraTT(1).
                END.
                ELSE IF bonglinje.varegr = 50 AND bonglinje.lopenr >= 9000 THEN DO:
/*                     IF NOT bonglinje.bongtekst BEGINS "ECCO" THEN DO:                                      */
/*                         FIND artbas WHERE artbas.artikkelnr = DECI(bonglinje.artikkelnr) NO-LOCK NO-ERROR. */
/*                         IF NOT artbas.beskr BEGINS "ECCO" THEN                                             */
/*                             NEXT.                                                                          */
/*                     END.                                                                                   */
                    lKvitto = TRUE.
                    RUN SummeraTT(2).
                END.
                ELSE IF bonglinje.varegr = 90 THEN DO:
                    lKvitto = TRUE.
                    RUN SummeraTT(2).
                END.
            END.
            tt_ecco.sumKvitton = tt_ecco.sumKvitton + IF lKvitto THEN 1 ELSE 0.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RapportJF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportJF Procedure 
PROCEDURE RapportJF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cWeekNum AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iWeekNum AS INTEGER     NO-UNDO.
/* dMonday = IF WEEKDAY(TODAY) = 1 THEN TODAY - 13 ELSE TODAY - (WEEKDAY(TODAY)  + 5). */

DO ii = 1 TO NUM-ENTRIES(cButiker):
    iButik = INT(ENTRY(ii,cButiker)).
    DO dDatum = dFirstday TO dLastday.
        RUN weeknum.p(dDatum,OUTPUT cWeekNum).
        FIND tt_ecco WHERE tt_ecco.vecka = cWeeknum AND
                           tt_ecco.butik = ibutik NO-ERROR.
        IF NOT AVAIL tt_ecco THEN DO:
            CREATE tt_ecco.
            ASSIGN tt_ecco.vecka = cWeekNum
                   tt_ecco.butik = iButik.
        END.
        FOR EACH Bonghode WHERE bonghode.butikknr = iButik AND
                                bonghode.dato     = dDatum NO-LOCK:
            lKvitto = FALSE.
            IF Bonghode.Makulert = 2 THEN
                NEXT.
            FOR EACH bonglinje WHERE bonglinje.b_id = bonghode.b_id AND bonglinje.makulert = FALSE NO-LOCK:
                IF NOT CAN-DO("1,3,10", STRING(bonglinje.ttid))  THEN
                    NEXT.
                IF NOT CAN-DO(cLevs,STRING(bonglinje.levnr)) THEN
                    NEXT.
                IF CAN-DO(cHgShoes,STRING(bonglinje.hovedgr)) THEN DO:
                    lKvitto = TRUE.
                    RUN SummeraTT(1).
                END.
                ELSE IF CAN-DO(cHgAA,STRING(bonglinje.hovedgr)) THEN DO:
                    lKvitto = TRUE.
                    RUN SummeraTT(2).
                END.
            END.
            tt_ecco.sumKvitton = tt_ecco.sumKvitton + IF lKvitto THEN 1 ELSE 0.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RapportRyns) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportRyns Procedure 
PROCEDURE RapportRyns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cWeekNum AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iWeekNum AS INTEGER     NO-UNDO.
/* dMonday = IF WEEKDAY(TODAY) = 1 THEN TODAY - 13 ELSE TODAY - (WEEKDAY(TODAY)  + 5). */

DO ii = 1 TO NUM-ENTRIES(cButiker):
    iButik = INT(ENTRY(ii,cButiker)).
    DO dDatum = dFirstday TO dLastday.
        RUN weeknum.p(dDatum,OUTPUT cWeekNum).
        FIND tt_ecco WHERE tt_ecco.vecka = cWeeknum AND
                           tt_ecco.butik = ibutik NO-ERROR.
        IF NOT AVAIL tt_ecco THEN DO:
            CREATE tt_ecco.
            ASSIGN tt_ecco.vecka = cWeekNum
                   tt_ecco.butik = iButik.
        END.
        FOR EACH Bonghode WHERE bonghode.butikknr = iButik AND
                                bonghode.dato     = dDatum NO-LOCK:
            lKvitto = FALSE.
            IF Bonghode.Makulert = 2 THEN
                NEXT.
            FOR EACH bonglinje WHERE bonglinje.b_id = bonghode.b_id AND bonglinje.makulert = FALSE NO-LOCK:
                IF NOT CAN-DO("1,3,10", STRING(bonglinje.ttid))  THEN
                    NEXT.
                IF NOT CAN-DO(cLevs,STRING(bonglinje.levnr)) AND bonglinje.varegr <> 98 THEN
                    NEXT.
                IF bonglinje.varegr <> 98 THEN DO:
                    lKvitto = TRUE.
                    RUN SummeraTT(1).
                END.
                ELSE DO:
                    lKvitto = TRUE.
                    RUN SummeraTT(2).
                END.
            END.
            tt_ecco.sumKvitton = tt_ecco.sumKvitton + IF lKvitto THEN 1 ELSE 0.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RapportSkohorn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportSkohorn Procedure 
PROCEDURE RapportSkohorn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cWeekNum AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iWeekNum AS INTEGER     NO-UNDO.
DEFINE VARIABLE cButKasser AS CHARACTER   NO-UNDO.
/* dMonday = IF WEEKDAY(TODAY) = 1 THEN TODAY - 13 ELSE TODAY - (WEEKDAY(TODAY)  + 5). */

DO ii = 1 TO NUM-ENTRIES(cButiker):
    iButik = INT(ENTRY(ii,cButiker)).
    cButKasser = ENTRY(ii,cKasser).
    DO dDatum = dFirstday TO dLastday.
        RUN weeknum.p(dDatum,OUTPUT cWeekNum).
        FIND tt_ecco WHERE tt_ecco.vecka = cWeeknum AND
                           tt_ecco.butik = ibutik NO-ERROR.
        IF NOT AVAIL tt_ecco THEN DO:
            CREATE tt_ecco.
            ASSIGN tt_ecco.vecka = cWeekNum
                   tt_ecco.butik = iButik.
        END.
        FOR EACH Bonghode WHERE bonghode.butikknr = iButik AND
                                bonghode.dato     = dDatum NO-LOCK:
            IF NOT CAN-DO(cButKasser,STRING(Bonghode.kassenr)) THEN
                NEXT.
            IF Bonghode.Makulert = 2 THEN
                NEXT.
            lKvitto = FALSE.
            FOR EACH bonglinje WHERE bonglinje.b_id = bonghode.b_id AND bonglinje.makulert = FALSE NO-LOCK:
                IF NOT CAN-DO("1,3,10", STRING(bonglinje.ttid))  THEN
                    NEXT.

                IF NOT CAN-DO(cLevs,STRING(bonglinje.levnr)) AND NOT CAN-DO(cArtlistaAA,bonglinje.artikkelnr) THEN
                    NEXT.
                IF CAN-DO(cArtlistaAA,bonglinje.artikkelnr) THEN DO:
                    lKvitto = TRUE.
                    RUN SummeraTT(2).
                END.
                ELSE DO:
                    lKvitto = TRUE.
                    RUN SummeraTT(1).
                END.
            END.
            tt_ecco.sumKvitton = tt_ecco.sumKvitton + IF lKvitto THEN 1 ELSE 0.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setDates) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setDates Procedure 
PROCEDURE setDates :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE dTst          AS DATE        NO-UNDO.
DEFINE VARIABLE cWeeknum      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iFirstweeknum AS INTEGER     NO-UNDO.
DEFINE VARIABLE iAAr AS INTEGER     NO-UNDO.

dLastday = IF WEEKDAY(TODAY) = 1 THEN TODAY - 7 ELSE TODAY - WEEKDAY(TODAY) + 1.

RUN weeknum.p (dLastday,OUTPUT cWeeknum).
iLastWeeknum = INT(cWeeknum).
iAar = INT(SUBSTR(cWeeknum,1,4)).
iFirstweeknum = INT(STRING(iAAr) + "01").
dTst = DATE(1,5,iaar).

REPEAT:
    RUN weeknum.p(dTst,OUTPUT cWeeknum).
    IF INT(cWeeknum) >= iFirstweeknum THEN
        dTst = dTst - 1.
    ELSE DO:
        dFirstDay = dTst + 1.
        LEAVE.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SummeraTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SummeraTT Procedure 
PROCEDURE SummeraTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iHgTyp AS INTEGER     NO-UNDO.
    DEFINE VARIABLE dAntal AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dSum   AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE iKoeff AS INTEGER     NO-UNDO.

    dAntal = bonglinje.antal.
    iKoeff = IF dAntal < 1 THEN -1 ELSE 1.
    dSum   = iKoeff * (bonglinje.linjesum - bonglinje.linjerab - bonglinje.subtotalrab - BongLinje.MvaKr) .
    CASE iHgTyp:
        WHEN 1 THEN
            ASSIGN tt_ecco.AntShoes = tt_ecco.AntShoes + dAntal
                   tt_ecco.SumShoes = tt_ecco.SumShoes + dSum.
        WHEN 2 THEN
            ASSIGN tt_ecco.AntAA = tt_ecco.AntAA + dAntal
                   tt_ecco.sumAA = tt_ecco.sumAA + dSum. 
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TabortTomma) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TabortTomma Procedure 
PROCEDURE TabortTomma :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iButik AS INTEGER     NO-UNDO.
    DO ii = 1 TO NUM-ENTRIES(cButiker):
        iButik = INT(ENTRY(ii,cButiker)).
        IF NOT CAN-FIND(FIRST tt_ecco WHERE tt_ecco.butik = iButik AND (tt_ecco.SumShoes > 0 OR tt_ecco.sumAA > 0)) THEN DO:
            FOR EACH tt_ecco WHERE tt_ecco.butik = iButik:
                 DELETE tt_ecco.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

