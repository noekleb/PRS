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
DEFINE VARIABLE cHgLaeder    AS CHARACTER   NO-UNDO.
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

DEFINE VARIABLE cLevlista AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE tt_RiekerMnd NO-UNDO
    FIELD mnd   AS INTE
    FIELD artikkelnr AS DECI
    FIELD levartnr AS CHAR
    FIELD inglager AS INTE
    FIELD sold     AS INTE
    FIELD utglager AS INTE
    FIELD ttid1    AS INTE
    FIELD ttid2    AS INTE
    FIELD ttid3    AS INTE
    FIELD ttid4    AS INTE
    FIELD ttid5    AS INTE
    FIELD ttid6    AS INTE
    FIELD ttid7    AS INTE
    FIELD ttid8    AS INTE
    FIELD ttid9    AS INTE
    FIELD ttid10   AS INTE
    FIELD ttid11   AS INTE
    INDEX artikkelnr IS PRIMARY UNIQUE artikkelnr
    INDEX levartnr levartnr.


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


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getkoeff) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getkoeff Procedure 
FUNCTION getkoeff RETURNS INTEGER
  ( INPUT iType AS INTE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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
{syspar2.i 210 50 4 cHgLaeder}
{syspara.i 210 50 5 cSumTagExcel}
/* {syspara.i 210 50 6 cVariant} */
{syspara.i 210 50 7 cArtlistaAA}

/* cVariant = SESSION:PARAMETER. */
cVariant = "1".

IF cSumTagExcel = "" THEN
    cSumTagExcel = "SUM".
IF cButiker = "" OR cLevs = "" THEN
    RETURN.
RUN setDates.
CASE cVariant:
    WHEN "1" THEN DO:
        cLevlista = "1041,2041,3041,4041,5041".
        RUN RapportJF.
    END.
    OTHERWISE
        RETURN.
END CASE.

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
    OUTPUT TO "C:\tmp\RiekerMnd.txt".
    FOR EACH tt_RiekerMnd:
        EXPORT DELIMITER ";" tt_RiekerMnd.
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
    DEFINE VARIABLE iDag AS INTEGER     NO-UNDO.
FIND FIRST butiker NO-LOCK.
/* DEFINE TEMP-TABLE tt_RiekerMnd                    */
/*     FIELD mnd   AS INTE                           */
/*     FIELD artikkelnr AS DECI                      */
/*     FIELD levartnr AS CHAR                        */
/*     FIELD inglager AS INTE                        */
/*     FIELD sold AS INTE                            */
/*     FIELD utglager AS INTE                        */
/*     INDEX artikkelnr IS PRIMARY UNIQUE artikkelnr */
/*     INDEX levartnr levartnr.                      */

/*     DEFINE VARIABLE cLastRowCol AS CHARACTER   NO-UNDO. */
    iVecka = INT(SUBSTR(STRING(iLastWeekNum),5,2)).
    cSumBold = "A" + STRING(iVecka + 4) + ":" + "G" + STRING(iVecka + 4).
/*     cLastRowCol = "G" + STRING(iVecka + 4).             */
    cRapportFil = SESSION:TEMP-DIR + "Rieker_" + string(YEAR(dLastday)) + "_" + STRING(MONTH(dLastday)).
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
        /* A  */ "Style no"          
        /* B  */ "quantity"              
        /* C  */ "Sold pair" 
        /* D  */ "Instock"    
                 "TTId1 "
                 "TTId2 "
                 "TTId3 "
                 "TTId4 "
                 "TTId5 "
                 "TTId6 "
                 "TTId7 "
                 "TTId8 "
                 "TTId9 "
                 "TTId10"
                 "TTId11"
                 SKIP.
        
/*         EXPORT STREAM sExportFile DELIMITER ";" " " " " " " " " " " " " " " SKIP. */
    iDag = 0.
    FOR EACH tt_RiekerMnd:
            EXPORT STREAM sExportFile DELIMITER ";"
/*                    INT(SUBSTR(vecka,5)) */
                levartnr
                utglager
                sold
                inglager 
                ttid1 
                ttid2 
                ttid3 
                ttid4 
                ttid5 
                ttid6 
                ttid7 
                ttid8 
                ttid9 
                ttid10
                ttid11
                SKIP.
    END.
    OUTPUT STREAM sExportFile CLOSE.


        /*   STATUS DEFAULT "Importerer data i Excel...". */
        CREATE "Excel.Application" chExcelApplication.  
        chExcelApplication:Visible = FALSE.                                     
        chWorkbooks = chExcelApplication:Workbooks:OpenText(cRapportFil,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).
        
        /*   STATUS DEFAULT "Setter aktivt ark...". */
        chWorkSheets = chExcelApplication:Sheets:Item(1).
        
        chWorkSheets:Range("A1:I1"):Merge().
        chWorkSheets:Range("A1:F1"):Font:Bold = TRUE.
        
        /*   chWorkSheets:Range("A1:G1"):Font:Italic = TRUE. */
        chWorkSheets:Range("A3:I3"):Font:Bold = TRUE.
        chWorkSheets:Range("A:A"):NumberFormat = "åååå-mm-dd".
/*         chWorkSheets:Range("A:A"):NumberFormat = "#0". */
        
        chWorkSheets:Range("B:O"):NumberFormat = "# ##0".
        

        chWorkSheets:Range(cSumBold):Font:Bold = TRUE.
        chWorkSheets:Columns("A:O"):AutoFit().
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
/*         RUN sendmail_tsl.p ("RIEKERRAPPORT","Eccorapport " +  STRING(iLastWeekNum) + " " + Butiker.butnamn,cExcelFil,"","","") NO-ERROR.  */
/*         OS-DELETE VALUE(cExcelFil).                                                                                                     */
        /*         IF NUM-ENTRIES(cExcelMailFiler) = 2 THEN */
/*             LEAVE.                               */
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
DEFINE VARIABLE iLagantUtg AS INTE     NO-UNDO.
DEFINE VARIABLE iKorrant   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSalgAnt   AS INTEGER     NO-UNDO.
DEFINE VARIABLE dDateLoop AS DATE        NO-UNDO.
DEFINE VARIABLE dTTId1    AS INTE NO-UNDO.
DEFINE VARIABLE dTTId2    AS INTE NO-UNDO.
DEFINE VARIABLE dTTId3    AS INTE NO-UNDO.
DEFINE VARIABLE dTTId4    AS INTE NO-UNDO.
DEFINE VARIABLE dTTId5    AS INTE NO-UNDO.
DEFINE VARIABLE dTTId6    AS INTE NO-UNDO.
DEFINE VARIABLE dTTId7    AS INTE NO-UNDO.
DEFINE VARIABLE dTTId8    AS INTE NO-UNDO.
DEFINE VARIABLE dTTId9    AS INTE NO-UNDO.
DEFINE VARIABLE dTTId10   AS INTE NO-UNDO.
DEFINE VARIABLE dTTId11   AS INTE NO-UNDO.


/* dLastday  = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1. */
/* /* leta efter första dagen i förra månaden */     */
/* dFirstday                                         */


/* DEFINE TEMP-TABLE tt_RiekerMnd                    */
/*     FIELD mnd   AS INTE                           */
/*     FIELD artikkelnr AS DECI                      */
/*     FIELD levartnr AS CHAR                        */
/*     FIELD inglager AS INTE                        */
/*     FIELD sold AS INTE                            */
/*     FIELD utglager AS INTE                        */
/*     INDEX artikkelnr IS PRIMARY UNIQUE artikkelnr */
/*     INDEX levartnr levartnr.                      */

DO ii = 1 TO NUM-ENTRIES(cLevlista):
    FOR EACH artbas WHERE artbas.levnr = INT(ENTRY(ii,cLevlista)) NO-LOCK.
        IF NOT CAN-FIND(FIRST lager WHERE lager.lagant > 0) AND NOT CAN-FIND(FIRST translogg WHERE translogg.artikkelnr = artbas.artikkelnr AND translogg.dato > dFirstday - 1) THEN
            NEXT.
        iLagantUtg = 0.
        iSalgAnt   = 0.
        iKorrant   = 0.
        dTTId1  = 0.
        dTTId2  = 0.
        dTTId3  = 0.
        dTTId4  = 0.
        dTTId5  = 0.
        dTTId6  = 0.
        dTTId7  = 0.
        dTTId8  = 0.
        dTTId9  = 0.
        dTTId10 = 0.
        dTTId11 = 0.

        FOR EACH lager WHERE lager.artikkelnr = artbas.artikkelnr NO-LOCK. /* lager per idag */
            iLagantUtg = iLagantUtg + (IF lager.lagant > 0 THEN lager.lagant ELSE 0).
        END.
        FOR EACH translogg WHERE translogg.artikkelnr = artbas.artikkelnr AND translogg.dato > dLastday NO-LOCK: /* lager per sista i perioden */
             iLagantUtg = iLagantUtg + (getkoeff(1) * translogg.antall).
        END.
        DO dDateLoop = dFirstday TO dLastday:
            FOR EACH translogg WHERE translogg.artikkelnr = artbas.artikkelnr AND translogg.dato = dDateLoop NO-LOCK: /* lager per sista i perioden */
                 iKorrAnt = iKorrant + (getkoeff(1) * translogg.antall).
                 IF translogg.ttid = 1 OR translogg.ttid = 10 THEN
                     iSalgAnt = iSalgAnt + translogg.antall.
                 CASE translogg.ttid:
                     WHEN  1 THEN dTTId1  = dTTId1  + translogg.antall.
                     WHEN  2 THEN dTTId2  = dTTId2  + translogg.antall.
                     WHEN  3 THEN dTTId3  = dTTId3  + translogg.antall.
                     WHEN  4 THEN dTTId4  = dTTId4  + translogg.antall.
                     WHEN  5 THEN dTTId5  = dTTId5  + translogg.antall.
                     WHEN  6 THEN dTTId6  = dTTId6  + translogg.antall.
                     WHEN  7 THEN dTTId7  = dTTId7  + translogg.antall.
                     WHEN  8 THEN dTTId8  = dTTId8  + translogg.antall.
                     WHEN  9 THEN dTTId9  = dTTId9  + translogg.antall.
                     WHEN 10 THEN dTTId10 = dTTId10 + translogg.antall.
                     WHEN 11 THEN dTTId11 = dTTId11 + translogg.antall.
                 END CASE.
            END.
        END.
        CREATE tt_RiekerMnd.
        ASSIGN tt_RiekerMnd.artikkelnr = artbas.artikkelnr
               tt_RiekerMnd.levartnr   = REPLACE(artbas.levkod,"-","/")
               tt_RiekerMnd.utglager   = iLagantUtg
               tt_RiekerMnd.sold       = iSalgAnt
               tt_RiekerMnd.inglager   = iLagantUtg + iKorrant
               tt_RiekerMnd.ttid1      = dTTId1 
               tt_RiekerMnd.ttid2      = dTTId2 
               tt_RiekerMnd.ttid3      = dTTId3 
               tt_RiekerMnd.ttid4      = dTTId4 
               tt_RiekerMnd.ttid5      = dTTId5 
               tt_RiekerMnd.ttid6      = dTTId6 
               tt_RiekerMnd.ttid7      = dTTId7 
               tt_RiekerMnd.ttid8      = dTTId8 
               tt_RiekerMnd.ttid9      = dTTId9 
               tt_RiekerMnd.ttid10     = dTTId10
               tt_RiekerMnd.ttid11     = dTTId11.
            
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

/* leta efter sista dagen i förra månaden */
dLastday  = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1.
/* leta efter första dagen i förra månaden */
dFirstday = DATE(MONTH(dLastday),1,YEAR(dLastday)).

/* RUN weeknum.p (dLastday,OUTPUT cWeeknum).   */
/* iLastWeeknum = INT(cWeeknum).               */
/* iAar = INT(SUBSTR(cWeeknum,1,4)).           */
/* iFirstweeknum = INT(STRING(iAAr) + "01").   */
/* dTst = DATE(1,5,iaar).                      */
/*                                             */
/* REPEAT:                                     */
/*     RUN weeknum.p(dTst,OUTPUT cWeeknum).    */
/*     IF INT(cWeeknum) >= iFirstweeknum THEN  */
/*         dTst = dTst - 1.                    */
/*     ELSE DO:                                */
/*         dFirstDay = dTst + 1.               */
/*         LEAVE.                              */
/*     END.                                    */
/* END.                                        */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getkoeff) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getkoeff Procedure 
FUNCTION getkoeff RETURNS INTEGER
  ( INPUT iType AS INTE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iKoeff AS INTEGER     NO-UNDO.
  CASE translogg.ttid:
      WHEN 1 THEN DO: /* fsg */                    /* TransTypeId- Beskrivelse--------------------- */ 
          iKoeff = 1.                              /*                                               */ 
      END.                                         /*  1       1     Varesalg                       */ 
      WHEN 4 THEN DO: /* lagerrekla */             /*          2     Brekkasje                      */ 
          iKoeff = 1.                              /*  1       4     Lagerreklamasjon               */ 
      END.                                         /* -1       5     Varekjøp                       */ 
      WHEN 5 THEN DO: /* varek */                  /*          6     Overføringer                   */ 
          iKoeff = -1.                             /*  1       7     Lagerjustering                 */ 
      END.                                         /*  1       9     Svinn                          */ 
      WHEN 7 THEN DO: /* lagerj */                 /*  1      10     Gjenkjøp                       */ 
          iKoeff = 1.                              /*  1      11     Internt forbruk                */ 
      END.
      WHEN 9 THEN DO: /* svinn */
          iKoeff = 1.
      END.
      WHEN 10 THEN DO: /* retur */
          iKoeff = 1.
      END.
      WHEN 11 THEN DO: /* internt f */
          iKoeff = 1.
      END.
  END CASE.
  RETURN iKoeff.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

