&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
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
/* Parameters Definitions ---                                           */
  DEFINE INPUT  PARAMETER cListeType           AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cListe               AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE         iCl                  AS INTEGER    NO-UNDO.
  DEFINE VARIABLE         cLogo                AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE         cTitel               AS CHARACTER  FORMAT "x(25)" NO-UNDO.
  DEFINE VARIABLE         cClInfo              AS CHARACTER  FORMAT "x(70)" NO-UNDO.
  DEFINE VARIABLE         cText                AS CHARACTER  FORMAT "x(50)" NO-UNDO.
  DEFINE VARIABLE cLng AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iPos AS INTEGER EXTENT 8    NO-UNDO.
  DEFINE VARIABLE iY AS INTEGER     NO-UNDO.

  DEFINE FRAME PageHeader
     HEADER
        "<ALIGN=BASE><FArial><R3><P18><B><C2><CENTER=C80>" cTitel "</B><P8>"SKIP
/*         "<R4><C6><FROM><R4><C78><LINE>" SKIP */
        WITH PAGE-TOP STREAM-IO WIDTH 255.
  
  DEFINE VARIABLE iRad   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.

  DEF VARIABLE thRLogg   AS HANDLE NO-UNDO.
  DEF VARIABLE thRLinje  AS HANDLE NO-UNDO.
  DEF VARIABLE thLev     AS HANDLE NO-UNDO.
                         
  DEF VARIABLE bhRLogg   AS HANDLE NO-UNDO.
  DEF VARIABLE bhRLinje  AS HANDLE NO-UNDO.
  DEF VARIABLE bhLev     AS HANDLE NO-UNDO.
                         
  DEF VARIABLE qhLinje   AS HANDLE NO-UNDO.
  DEF VARIABLE qhLogg    AS HANDLE NO-UNDO.

  DEF VARIABLE iProfilNr AS INTEGER NO-UNDO.
  
 { pdf_inc.i "THIS-PROCEDURE"}.
/* {xPrint.i} */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-bredd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD bredd Procedure 
FUNCTION bredd RETURNS DECIMAL
    ( INPUT cText AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doButikkInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD doButikkInfo Procedure 
FUNCTION doButikkInfo RETURNS LOGICAL
  (INPUT ipfY AS DEC,
   INPUT ipfX AS DEC,
   INPUT ipfY2 AS DEC,
   INPUT ipfX2 AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doKundeInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD doKundeInfo Procedure 
FUNCTION doKundeInfo RETURNS LOGICAL
  (INPUT ipfY AS DEC,
   INPUT ipfX AS DEC,
   INPUT ipfY2 AS DEC,
   INPUT ipfX2 AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLevInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD doLevInfo Procedure 
FUNCTION doLevInfo RETURNS LOGICAL
  (INPUT ipfY AS DEC,
   INPUT ipfX AS DEC,
   INPUT ipfY2 AS DEC,
   INPUT ipfX2 AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doReklamasjonsInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD doReklamasjonsInfo Procedure 
FUNCTION doReklamasjonsInfo RETURNS LOGICAL
  (INPUT ipfY AS DEC,
   INPUT ipfX AS DEC,
   INPUT ipfY2 AS DEC,
   INPUT ipfX2 AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doReklamasjonsLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD doReklamasjonsLinje Procedure 
FUNCTION doReklamasjonsLinje RETURNS LOGICAL
  (INPUT ipiStartRad AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFeil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFeil Procedure 
FUNCTION getFeil RETURNS CHARACTER
  ( INPUT iFeilKode AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLinje Procedure 
FUNCTION getLinje RETURNS LOGICAL
  (INPUT iType AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLinjeHode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLinjeHode Procedure 
FUNCTION getLinjeHode RETURNS LOGICAL
  (INPUT ipiType AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFdoButikkInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PDFdoButikkInfo Procedure 
FUNCTION PDFdoButikkInfo RETURNS LOGICAL
  (INPUT ipY AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFdoKundeInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PDFdoKundeInfo Procedure 
FUNCTION PDFdoKundeInfo RETURNS LOGICAL
  (INPUT ipY AS DEC,
   INPUT ipX AS DEC)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFdoLevInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PDFdoLevInfo Procedure 
FUNCTION PDFdoLevInfo RETURNS LOGICAL
  (INPUT ipY AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFdoReklamasjonsInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PDFdoReklamasjonsInfo Procedure 
FUNCTION PDFdoReklamasjonsInfo RETURNS LOGICAL
  (INPUT ipY AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFdoReklamasjonsLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PDFdoReklamasjonsLinje Procedure 
FUNCTION PDFdoReklamasjonsLinje RETURNS LOGICAL
  (INPUT ipY AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFdotline) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PDFdotline Procedure 
FUNCTION PDFdotline RETURNS LOGICAL
  ( INPUT iY AS INTE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFgetLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PDFgetLinje Procedure 
FUNCTION PDFgetLinje RETURNS LOGICAL
  (INPUT iY AS INTE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFgetLinjeHode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PDFgetLinjeHode Procedure 
FUNCTION PDFgetLinjeHode RETURNS LOGICAL
  (INPUT iY AS INT)  FORWARD.

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
         HEIGHT             = 28.71
         WIDTH              = 60.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
iCl   = DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1").  
cText = DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 1 and SysGr = 1 and ParaNr = 101","Parameter1").  
cLogo = DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 5 and SysGr = 4 and ParaNr = 101","Parameter1").  

/* {syspara.i 5 1 1 iCl INT} */
/* {syspara.i 1 1 101 cText} */
/* {syspara.i 5 4 30 cLogo}  */
FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
IF AVAIL bruker THEN
    cLng = Bruker.Lng.
IF cLogo = "" THEN
    cLogo = "icon\orderlogo.bmp".
IF cListe NE '' THEN
    RUN PDFSkrivUt IN THIS-PROCEDURE.
/*   RUN SkrivUt IN THIS-PROCEDURE. */
ELSE RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-PageFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PageFooter Procedure 
PROCEDURE PageFooter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
PUT UNFORMATTED
      '<R66><C6><FROM><R66><C78><LINE>'
      '<R66><C6>' STRING(TODAY,'99.99.9999') ' ' STRING(TIME,'HH:MM') '<C6><CENTER=C78>' cText '<C75>' PAGE-NUMBER FORMAT ">>" SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFNotat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFNotat Procedure 
PROCEDURE PDFNotat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cNotatTxt AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER dYL AS DECIMAL     NO-UNDO. 
DEFINE INPUT  PARAMETER dLeftCol  AS DECIMAL     NO-UNDO.
DEFINE INPUT  PARAMETER dRightCol AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER iAntal AS INTEGER     NO-UNDO.
DEFINE VARIABLE iRadCount AS INTEGER     NO-UNDO.
DEFINE VARIABLE cc AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNy AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c2 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DEFINE VARIABLE iGodkand AS INTEGER     NO-UNDO.
DEFINE VARIABLE i2 AS INTEGER     NO-UNDO.
iGodkand = dRightCol - dLeftCol.
iAntal = iAntal + 1.

DO ii = 1 TO NUM-ENTRIES(cNotatTxt,CHR(10)):
    cc = ENTRY(ii,cNotatTxt,CHR(10)).
    cc = TRIM(REPLACE(cc,"~\r","")).
    IF bredd(cc) < iGodkand THEN DO:
        RUN pdf_text_xy_dec ("Spdf",cc,dLeftCol,dYL - iRadCount).
        iRadCount = iRadCount + 12.
        iAntal = iAntal + 1.
    END.
    ELSE DO:
         c2 = ENTRY(1,cc," ").
         DO i2 = 2 TO NUM-ENTRIES(cc," "):
             IF bredd(c2 + " " + ENTRY(i2,cc," ")) < iGodkand THEN DO:
                 c2 = c2 + " " + ENTRY(i2,cc," ").
                 IF i2 = NUM-ENTRIES(cc," ") THEN DO:
                     RUN pdf_text_xy_dec ("Spdf",c2,dLeftCol,dYL - iRadCount).
                     iRadCount = iRadCount + 12.
                     iAntal = iAntal + 1.
                 END.
             END.
             ELSE DO:
                 RUN pdf_text_xy_dec ("Spdf",c2,dLeftCol,dYL - iRadCount).
                 iRadCount = iRadCount + 12.
                 iAntal = iAntal + 1.
                 c2 = ENTRY(i2,cc," ").
                 IF i2 = NUM-ENTRIES(cc," ") THEN DO:
                     RUN pdf_text_xy_dec ("Spdf",c2,dLeftCol,dYL - iRadCount).
                     iRadCount = iRadCount + 12.
                     iAntal = iAntal + 1.
                 END.
             END.
         END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFPageFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFPageFooter Procedure 
PROCEDURE PDFPageFooter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cSidTxt AS CHARACTER   NO-UNDO.

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
/*  RUN pdf_set_dash IN h_PDFinc ("Spdf",1,0).*/
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), pdf_BottomMargin ("Spdf"), pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , pdf_BottomMargin ("Spdf"), 0.5).

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).  
  RUN pdf_text_xy_dec ("Spdf",STRING(TODAY),pdf_LeftMargin ("Spdf"),pdf_BottomMargin ("Spdf") - 14).
  RUN pdf_text_xy_dec ("Spdf",cText,pdf_Pagewidth ("Spdf") - pdf_LeftMargin ("Spdf") - 350,pdf_BottomMargin ("Spdf") - 14).
  cSidTxt = TRIM("Sida: " + STRING(pdf_page("Spdf")) + " (" + pdf_TotalPages("Spdf") + ")").

  RUN pdf_text_xy_dec ("Spdf",cSidTxt,pdf_Pagewidth ("Spdf") - pdf_LeftMargin ("Spdf") - 50,pdf_BottomMargin ("Spdf") - 14).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFPageHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFPageHeader Procedure 
PROCEDURE PDFPageHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEFINE VARIABLE cTxt AS CHARACTER   NO-UNDO.

   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",18).



   RUN pdf_text_xy_dec ("Spdf",cTitel, pdf_PageWidth("Spdf") / 2 - (bredd(cTitel) / 2),pdf_PageHeight("Spdf") - 30).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFSkrivUt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFSkrivUt Procedure 
PROCEDURE PDFSkrivUt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount  AS INTEGER    NO-UNDO.
    DEFINE VARIABLE pcRappFil AS CHARACTER  NO-UNDO.
    
    ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "SErek2.pdf".
    
    /* tillfällig tömning av fil */
    OUTPUT TO VALUE(pcRappFil).
    OUTPUT CLOSE.


    ASSIGN iPos[1] = 30
           iPos[2] = 90
           iPos[3] = 150
           iPos[4] = 252
           iPos[5] = 320
           iPos[6] = 360
           iPos[7] = 395
           iPos[8] = 470.

    thRLogg  = DYNAMIC-FUNCTION("getTempTable","","Reklamasjonslogg|WHERE FALSE",bhRLogg).
    bhRLogg  = thRLogg:DEFAULT-BUFFER-HANDLE.
    thRLinje = DYNAMIC-FUNCTION("getTempTable","","ReklamasjonsLinje|WHERE FALSE",bhRLinje).
    bhRLinje = thRLinje:DEFAULT-BUFFER-HANDLE.
    
    CREATE QUERY qhLinje.
    qhLinje:SET-BUFFERS(bhRLinje).

    thLev = DYNAMIC-FUNCTION("getTempTable","","LevBas|WHERE FALSE",bhLev).
    bhLev = thLev:DEFAULT-BUFFER-HANDLE.

/*     DO iCount = 1 TO NUM-ENTRIES(cListe): */
    DO iCount = 1 TO 1:  /* Bara ett nr */
      DYNAMIC-FUNCTION("getTempTable","","Reklamasjonslogg|WHERE reklamasjonslogg.reklamasjonsnr = " + ENTRY(iCount,cListe),bhRLogg).
      IF NOT bhRlogg:AVAIL THEN
        NEXT.
  RUN pdf_new ("Spdf",pcRappFil).
  pdf_PageHeader ("Spdf",THIS-PROCEDURE:HANDLE,"PDFPageHeader").
  pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PDFPageFooter").
  RUN pdf_set_PaperType ("Spdf","A4").
  RUN pdf_set_LeftMargin ("Spdf", 30).
  RUN pdf_set_BottomMargin ("Spdf", 40).
  RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf", 13).
      /* Varje reklamation skall starta från sidonummer 1 */
      CASE cListeType:
          WHEN "1" THEN DO:
              ASSIGN cTitel = STRING(cLng = "SE","Reklamation/Reklamasjon").
              RUN pdf_new_page ("Spdf").

              PDFdoButikkInfo(60).
              PDFdoReklamasjonsInfo(60).
              PDFdoKundeInfo(160,30).
              PDFdoReklamasjonslinje(290).
          END.
          WHEN "2" THEN DO:
              ASSIGN cTitel = STRING(cLng = "SE","Reklamation/Reklamasjon").
              RUN pdf_new_page ("Spdf").
              PDFdoLevInfo(60).
              PDFdoButikkInfo(160).
              PDFdoKundeInfo(160,380).
              PDFdoReklamasjonsInfo(60).
              PDFdoReklamasjonslinje(290).
          END.
          WHEN "3" THEN DO:
              ASSIGN cTitel = STRING(cLng = "SE","Tillgodo/Tilgode").
              RUN pdf_new_page ("Spdf").
              PDFdoButikkInfo(60).
              PDFdoReklamasjonsInfo(60).
              PDFdoKundeInfo(160,30).
              PDFdoReklamasjonslinje(290).
          END.
          WHEN "4" THEN DO:
            ASSIGN cTitel = STRING(cLng = "SE","Lista/Liste").
            RUN pdf_new_page ("Spdf").
/*             PDFdoButikkInfo(6.0,5.0,10.0,30.0). */
/*             PDFdoKundeInfo(12.0,5.0,19.0,30.0). */
            PDFdoReklamasjonsInfo(60).
            PDFdoReklamasjonslinje(160).
        END.
      END CASE.
      RUN pdf_close ("Spdf").
/*       OUTPUT CLOSE. */
    END.
/*     OUTPUT TO TERMINAL. */
    IF VALID-HANDLE(qhLogg)   THEN DELETE OBJECT qhLogg.
    IF VALID-HANDLE(qhLinje)  THEN DELETE OBJECT qhLinje.
    IF VALID-HANDLE(thRLogg)  THEN DELETE OBJECT thRLogg.
    IF VALID-HANDLE(thRLinje) THEN DELETE OBJECT thRLinje.
    IF VALID-HANDLE(thLev)    THEN DELETE OBJECT thLev.
/*     IF VALID-HANDLE(bhRLogg)  THEN DELETE OBJECT bhRLogg.  */
/*     IF VALID-HANDLE(bhRLinje) THEN DELETE OBJECT bhRLinje. */
/*     IF VALID-HANDLE(bhLev)    THEN DELETE OBJECT bhLev.    */

/* Klargjør rapportfilnavnet */
ASSIGN FILE-INFO:FILE-NAME = pcRappFil.

/* Sender filen til visning og utskrift. */
  RUN browse2pdf\viewxmldialog.w (pcRappFil,"Polygon Retail Solutions").

/*  RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivUt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivUt Procedure 
PROCEDURE SkrivUt PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount  AS INTEGER    NO-UNDO.
    DEFINE VARIABLE pcRappFil AS CHARACTER  NO-UNDO.
    
    ASSIGN pcRappFil = SESSION:TEMP-DIRECTORY + "SErek2.xpr".
    
    /* tillfällig tömning av fil */
    OUTPUT TO VALUE(pcRappFil).
    OUTPUT CLOSE.

    thRLogg  = DYNAMIC-FUNCTION("getTempTable","","Reklamasjonslogg|WHERE FALSE",bhRLogg).
    bhRLogg  = thRLogg:DEFAULT-BUFFER-HANDLE.
    thRLinje = DYNAMIC-FUNCTION("getTempTable","","ReklamasjonsLinje|WHERE FALSE",bhRLinje).
    bhRLinje = thRLinje:DEFAULT-BUFFER-HANDLE.
    
    CREATE QUERY qhLinje.
    qhLinje:SET-BUFFERS(bhRLinje).

    thLev = DYNAMIC-FUNCTION("getTempTable","","LevBas|WHERE FALSE",bhLev).
    bhLev = thLev:DEFAULT-BUFFER-HANDLE.

    DO iCount = 1 TO NUM-ENTRIES(cListe):
      DYNAMIC-FUNCTION("getTempTable","","Reklamasjonslogg|WHERE reklamasjonslogg.reklamasjonsnr = " + ENTRY(iCount,cListe),bhRLogg).
      IF NOT bhRlogg:AVAIL THEN
        NEXT.

      /* Varje reklamation skall starta från sidonummer 1 */
      OUTPUT TO VALUE(pcRappFil) APPEND PAGED PAGE-SIZE VALUE(80).
      PUT CONTROL '<PREVIEW=ZoomToWidth>'.
      CASE cListeType:
          WHEN "1" THEN DO:
              ASSIGN cTitel = "Reklamasjon".
              VIEW FRAME PageHeader.
              doButikkInfo(6.0,5.0,10.0,30.0).
              doReklamasjonsInfo(6.0,55.0,10.0,78.0).
              doKundeInfo(13.0,5.0,19.0,30.0).
              doReklamasjonslinje(25).
          END.
          WHEN "2" THEN DO:
              ASSIGN cTitel = "Reklamasjon".
              VIEW FRAME PageHeader.
              doLevInfo(6.0,5.0,10.0,30.0).
              doButikkInfo(12.0,5.0,19.0,30.0).
              doKundeInfo(12.0,55.0,10.0,78.0).
              doReklamasjonsInfo(6.0,55.0,10.0,78.0).
              doReklamasjonslinje(25).
          END.
          WHEN "3" THEN DO:
              ASSIGN cTitel = "Tilgode".
              VIEW FRAME PageHeader.
              doButikkInfo(6.0,5.0,10.0,30.0).
              doKundeInfo(12.0,5.0,19.0,30.0).
              doReklamasjonsInfo(6.0,55.0,10.0,78.0).
              doReklamasjonslinje(25).
          END.
          WHEN "4" THEN DO:
            ASSIGN cTitel = "Liste".
            VIEW FRAME PageHeader.
/*             doButikkInfo(6.0,5.0,10.0,30.0). */
/*             doKundeInfo(12.0,5.0,19.0,30.0). */
/*             doReklamasjonsInfo(6.0,55.0,10.0,78.0). */
            doReklamasjonslinje(8).
        END.
      END CASE.
      OUTPUT CLOSE.
    END.
    OUTPUT TO TERMINAL.
    IF VALID-HANDLE(qhLogg)   THEN DELETE OBJECT qhLogg.
    IF VALID-HANDLE(qhLinje)  THEN DELETE OBJECT qhLinje.
    IF VALID-HANDLE(thRLogg)  THEN DELETE OBJECT thRLogg.
    IF VALID-HANDLE(thRLinje) THEN DELETE OBJECT thRLinje.
    IF VALID-HANDLE(thLev)    THEN DELETE OBJECT thLev.
/*     IF VALID-HANDLE(bhRLogg)  THEN DELETE OBJECT bhRLogg.  */
/*     IF VALID-HANDLE(bhRLinje) THEN DELETE OBJECT bhRLinje. */
/*     IF VALID-HANDLE(bhLev)    THEN DELETE OBJECT bhLev.    */

/* Klargjør rapportfilnavnet */
ASSIGN FILE-INFO:FILE-NAME = pcRappFil.

/* Sender filen til visning og utskrift. */
 RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-bredd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION bredd Procedure 
FUNCTION bredd RETURNS DECIMAL
    ( INPUT cText AS CHARACTER ):
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/

    RETURN pdf_text_widthdec ("Spdf",cText).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doButikkInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION doButikkInfo Procedure 
FUNCTION doButikkInfo RETURNS LOGICAL
  (INPUT ipfY AS DEC,
   INPUT ipfX AS DEC,
   INPUT ipfY2 AS DEC,
   INPUT ipfX2 AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cNavn         AS CHAR NO-UNDO.
  DEF VAR cAdr          AS CHAR NO-UNDO.
  DEF VAR cPnr          AS CHAR NO-UNDO.
  DEF VAR cTelefon      AS CHAR NO-UNDO.
  DEF VAR cPSted        AS CHAR NO-UNDO.
  DEF VAR cReturnValue  AS CHAR NO-UNDO.
  DEFINE VARIABLE cKontakt AS CHARACTER NO-UNDO.
  
  /* Henter linjen for å få tak i riktig butikknummer. */
  FIND FIRST Reklamasjonslinje NO-LOCK WHERE
    Reklamasjonslinje.ReklamasjonsNr = DECIMAL(bhRLogg:BUFFER-FIELD('reklamasjonsnr'):BUFFER-VALUE) NO-ERROR.

  ASSIGN
    cReturnValue = DYNAMIC-FUNCTION("getFieldValues","butiker","WHERE butiker.butik = " + 
                                     (IF AVAILABLE Reklamasjonslinje THEN STRING(ReklamasjonsLinje.Butik) ELSE STRING(iCL)),
                                     "butnamn,LevAdresse1,LevPostNr,LevTelefon,ProfilNr,LevKontakt")
    cNavn        = ENTRY(1,cReturnValue,'|')
    cAdr         = ENTRY(2,cReturnValue,'|')
    cPnr         = ENTRY(3,cReturnValue,'|')
    cTelefon     = ENTRY(4,cReturnValue,'|')
    iProfilNr    = INT(ENTRY(5,cReturnValue,'|'))
    cKontakt     = ENTRY(6,cReturnValue,'|')
  .
  
  ASSIGN 
    cReturnValue = DYNAMIC-FUNCTION("getFieldValues","post","WHERE post.postnr = " + QUOTER(cPnr),"beskrivelse").
    cPSted      = ENTRY(1,cReturnValue,'|')
  .

  IF cNavn NE '' THEN
  DO:
    PUT UNFORMATTED '<R' ipfY      '><C' ipfX  '><#Butikk>' SKIP.
    PUT UNFORMATTED '<R' ipfY + 0.5 '><C55><#Logo>' SKIP.
    PUT UNFORMATTED '<R' ipfY - 1  '><C' ipfX + 1  '><B>Butikk:</B>' SKIP.
/*     PUT UNFORMATTED '<R' ipfY2 '><C' ipfX2 '><RECT#Butikk>' SKIP. */
    PUT UNFORMATTED '<FArial><R' ipfY '><C' ipfX + 1 '>' TRIM(cNavn)    SKIP.
    PUT UNFORMATTED '<FArial><R' ipfY + 1 '><C' ipfX + 1 '>' TRIM(cAdr) SKIP.
    PUT UNFORMATTED '<FArial><R' ipfY + 2 '><C' ipfX + 1 '>' cPnr + ' ' + cPSted SKIP.
    PUT UNFORMATTED '<FArial><R' ipfY + 3 '><C' ipfX + 1 '>' cKontakt SKIP.
    PUT UNFORMATTED '<FArial><R' ipfY + 4 '><C' ipfX + 1 '>' cTelefon SKIP.
  END.
  ELSE RETURN FALSE.
  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doKundeInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION doKundeInfo Procedure 
FUNCTION doKundeInfo RETURNS LOGICAL
  (INPUT ipfY AS DEC,
   INPUT ipfX AS DEC,
   INPUT ipfY2 AS DEC,
   INPUT ipfX2 AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cPSted        AS CHARACTER NO-UNDO.
  DEF VAR cReturnValue  AS CHARACTER NO-UNDO.
  DEF VAR cTelefon      AS CHAR NO-UNDO.
  DEF VAR cMobilTlf     AS CHAR NO-UNDO.
  DEF VAR cePostAdresse AS CHAR NO-UNDO.

  ASSIGN 
    cReturnValue  = DYNAMIC-FUNCTION("getFieldValues","post","WHERE post.postnr = " + QUOTER(bhRLogg:BUFFER-FIELD('postnr'):BUFFER-VALUE),"beskrivelse").
    cTelefon      = STRING(bhRLogg:BUFFER-FIELD('KundeTelefon'):BUFFER-VALUE). /*DYNAMIC-FUNCTION("getFieldValues","Kunde","WHERE Kunde.KundeNr = " + QUOTER(bhRLogg:BUFFER-FIELD('KundeNr'):BUFFER-VALUE),"Telefon").*/
    cMobilTlf     = STRING(bhRLogg:BUFFER-FIELD('KundeMobil'):BUFFER-VALUE). /* DYNAMIC-FUNCTION("getFieldValues","Kunde","WHERE Kunde.KundeNr = " + QUOTER(bhRLogg:BUFFER-FIELD('KundeNr'):BUFFER-VALUE),"MobilTlf"). */
    cePostAdresse = DYNAMIC-FUNCTION("getFieldValues","Kunde","WHERE Kunde.KundeNr = " + QUOTER(bhRLogg:BUFFER-FIELD('KundeNr'):BUFFER-VALUE),"ePostAdresse").
    cPSted        = ENTRY(1,cReturnValue,'|')
    .
  ASSIGN 
    cTelefon      = IF (cTelefon = ? OR TRIM(cTelefon) = '') THEN '' ELSE cTelefon
    cMobilTlf     = IF (cMobilTlf = ? OR trim(cMobilTlf) = '') THEN '' ELSE cMobilTlf
    cePostAdresse = IF (cePostAdresse = ? OR trim(cePostAdresse) = '') THEN '' ELSE cePostAdresse
    cPSted        = IF (cPSted = ? OR trim(cPSted) = '') THEN '' ELSE cPSted
  .

  PUT UNFORMATTED '<R' ipfY      '><C' ipfX  '><#Kunde>' SKIP.
  PUT UNFORMATTED '<R' ipfY - 1  '><C' ipfX + 1  '><B>Kunde:</B>' SKIP.
/*   PUT UNFORMATTED '<R' ipfY2 '><C' ipfX2 '><RECT#Kunde>' SKIP. */
  PUT UNFORMATTED '<FArial><R' ipfY     '><C' ipfX + 1 '>' bhRLogg:BUFFER-FIELD('KundeNavn'):BUFFER-VALUE    SKIP.
  PUT UNFORMATTED '<FArial><R' ipfY + 1 '><C' ipfX + 1 '>' bhRLogg:BUFFER-FIELD('KundeAdresse'):BUFFER-VALUE SKIP.
  PUT UNFORMATTED '<FArial><R' ipfY + 2 '><C' ipfX + 1 '>' bhRLogg:BUFFER-FIELD('postnr'):STRING-VALUE + ' ' + cPSted SKIP.
  PUT UNFORMATTED '<FArial><R' ipfY + 3 '><C' ipfX + 1 '>' cTelefon SKIP.
  PUT UNFORMATTED '<FArial><R' ipfY + 4 '><C' ipfX + 1 '>' cMobilTlf SKIP.
  PUT UNFORMATTED '<FArial><R' ipfY + 5 '><C' ipfX + 1 '>' cePostAdresse SKIP.
  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doLevInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION doLevInfo Procedure 
FUNCTION doLevInfo RETURNS LOGICAL
  (INPUT ipfY AS DEC,
   INPUT ipfX AS DEC,
   INPUT ipfY2 AS DEC,
   INPUT ipfX2 AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/ 
  DEF VAR cReturnValue AS CHAR NO-UNDO.
  DEF VAR cPSted       AS CHAR NO-UNDO.
  
  ASSIGN 
    cReturnValue = DYNAMIC-FUNCTION("getFieldValues","post","WHERE post.postnr = " + QUOTER(bhRLogg:BUFFER-FIELD('postnr'):BUFFER-VALUE),"beskrivelse").
    cPSted      = ENTRY(1,cReturnValue,'|')
  .


/*   IF VALID-HANDLE(thLev) THEN DELETE OBJECT thLev. */
  thLev = DYNAMIC-FUNCTION("getTempTable","","LevBas|WHERE LevBas.LevNr = " + STRING(bhRLogg:BUFFER-FIELD('LevNr'):BUFFER-VALUE),bhLev).
  IF bhLev:AVAIL THEN
  DO:
    PUT UNFORMATTED '<R' ipfY      '><C' ipfX  '><#Lev>' SKIP.
    PUT UNFORMATTED '<R' ipfY - 1  '><C' ipfX + 1  '><B>Leverandør:</B>' SKIP.
/*     PUT UNFORMATTED '<R' ipfY2 '><C' ipfX2 '><RECT#Lev>' SKIP. */
    PUT UNFORMATTED '<FArial><R' ipfY     '><C' ipfX + 1  '>' bhLev:BUFFER-FIELD('Levnamn'):BUFFER-VALUE SKIP.
    PUT UNFORMATTED '<FArial><R' ipfY + 1 '><C' ipfX + 1  '>' bhLev:BUFFER-FIELD('LevAdr'):BUFFER-VALUE SKIP.
    PUT UNFORMATTED '<FArial><R' ipfY + 2 '><C' ipfX + 1  '>' bhLev:BUFFER-FIELD('Levponr'):BUFFER-VALUE ' ' cPSted SKIP.
    PUT UNFORMATTED '<P5>' SKIP.
    PUT UNFORMATTED '<FArial><R' ipfY + 3 '><C' ipfX + 1 '>Tel:' bhLev:BUFFER-FIELD('LevTel'):BUFFER-VALUE  '   Faks:' bhLev:BUFFER-FIELD('telefax'):BUFFER-VALUE  '   ePost:' bhLev:BUFFER-FIELD('e_maillev'):BUFFER-VALUE   SKIP.
/*     PUT UNFORMATTED '<FArial><R' ipfY + 4 '><C' ipfX + 1 '>Faks:' LevBas.telefax    SKIP.  */
/*     PUT UNFORMATTED '<FArial><R' ipfY + 5 '><C' ipfX + 1 '>ePost:' LevBas.e_maillev  SKIP. */
    PUT UNFORMATTED '<P8>' SKIP.
  END.
  ELSE 
    RETURN FALSE.

  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doReklamasjonsInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION doReklamasjonsInfo Procedure 
FUNCTION doReklamasjonsInfo RETURNS LOGICAL
  (INPUT ipfY AS DEC,
   INPUT ipfX AS DEC,
   INPUT ipfY2 AS DEC,
   INPUT ipfX2 AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cReturnValue AS CHARACTER NO-UNDO.
  
  PUT UNFORMATTED '<R' ipfY      '><C' ipfX     '><#Rek>' SKIP.
  PUT UNFORMATTED '<R' ipfY - 1  '><C' ipfX + 1 '><B>Reklamasjon:</B>' SKIP.
/*   PUT UNFORMATTED '<R' ipfY2     '><C' ipfX2    '><RECT#Rek>' SKIP. */

  PUT UNFORMATTED '<FArial><R' ipfY     '><C' ipfX + 1 '>Nr:<C'     ipfX + 10 '>' bhRLogg:BUFFER-FIELD('ReklamasjonsNr'):BUFFER-VALUE    SKIP.
  PUT UNFORMATTED '<FArial><R' ipfY + 1 '><C' ipfX + 1 '>Dato:<C'   ipfX + 10 '>' bhRLogg:BUFFER-FIELD('RegistrertDato'):BUFFER-VALUE SKIP.
  /*
  cReturnValue = DYNAMIC-FUNCTION("getFieldValues","sysPara","where sysHId = 15 and sysGr = 1","Parameter1").
  PUT UNFORMATTED '<FArial><R' ipfY + 2 '><C' ipfX + 1 '>Status:<C'           ipfX + 10 '>' bhRLogg:BUFFER-FIELD('ReklamStatus'):BUFFER-VALUE ' ' cReturnValue  SKIP.
  */
  cReturnValue = DYNAMIC-FUNCTION("getFieldValues","Forsalj","WHERE forsalj.forsnr = INT(" + QUOTER(bhRLogg:BUFFER-FIELD('forsnr'):BUFFER-VALUE) + ')',"FoNamn").
  PUT UNFORMATTED '<FArial><R' ipfY + 2 '><C' ipfX + 1 '>Kasserer:<C'         ipfX + 10 '>' bhRLogg:BUFFER-FIELD('forsnr'):BUFFER-VALUE ' ' cReturnValue SKIP.
  /*
  IF bhRLogg:BUFFER-FIELD('sluttfortbesluttet'):BUFFER-VALUE THEN 
    PUT UNFORMATTED '<FArial><R' ipfY + 3 '><C' ipfX + 1 '>Sluttført:<C'      ipfX + 10 '>' bhRLogg:BUFFER-FIELD('sluttfortAv'):BUFFER-VALUE ' ' bhRLogg:BUFFER-FIELD('sluttfortdato'):BUFFER-VALUE SKIP.

  IF cListeType = '3' THEN
    PUT UNFORMATTED '<FArial><R' ipfY + 4 '><C' ipfX + 1 '><B>Tilgode:</B><C' ipfX + 10 '>' bhRLogg:BUFFER-FIELD('ReklamVerdi'):BUFFER-VALUE SKIP.
  */

  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doReklamasjonsLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION doReklamasjonsLinje Procedure 
FUNCTION doReklamasjonsLinje RETURNS LOGICAL
  (INPUT ipiStartRad AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
    
------------------------------------------------------------------------------*/

  ASSIGN 
    iRad    = ipiStartRad
    iCount  = 1
  .
  getLinjeHode(INT(cListeType)). 
  bhRLinje:EMPTY-TEMP-TABLE().
  DYNAMIC-FUNCTION("getTempTable","","ReklamasjonsLinje|WHERE reklamasjonsLinje.reklamasjonsnr = " + bhRLogg:BUFFER-FIELD('reklamasjonsnr'):BUFFER-VALUE,bhRLinje).
  qhLinje:QUERY-PREPARE('FOR EACH ' + bhRLinje:NAME + ' BY reklamasjonslinje.artikkelnr BY reklamasjonslinje.Storl').
  qhLinje:QUERY-OPEN().
  qhLinje:GET-FIRST().

  DO WHILE bhRLinje:AVAILABLE:
    IF (iRad + 6) GT 60 THEN
    DO:
        ASSIGN 
          iCount = 0
          iRad   = 5
        .
        RUN PageFooter.
        PAGE.
        VIEW FRAME PageHeader.
        getLinjeHode(INT(cListeType)).
    END.

    iRad = iRad + 3.
    getLinje(INT(cListeType)).
    PUT UNFORMATTED '<P12>'.
    IF INTEGER(bhRLinje:BUFFER-FIELD('Feilkode'):BUFFER-VALUE) > 0 THEN 
      PUT UNFORMATTED '<R' STRING(iRad + 1) '><C6>Feilkode: ' bhRLinje:BUFFER-FIELD('Feilkode'):BUFFER-VALUE '-' getFeil(bhRLinje:BUFFER-FIELD('Feilkode'):BUFFER-VALUE) SKIP.
    PUT UNFORMATTED '<R' STRING(iRad + 2) '><C6><#Notat2><R' STRING(iRad + 6) '><C78><FRAME#Notat2><USE#Notat2>' bhRLinje:BUFFER-FIELD('FeilNotat'):BUFFER-VALUE  '</USE>'  SKIP.
    PUT UNFORMATTED '<R' STRING(iRad + 1) '><C6><FROM><R' STRING(iRad + 1) '><C78><LINE>' SKIP.
    PUT UNFORMATTED '<P8>' SKIP.
    ASSIGN 
      iRad    = iRad + 6
      iCount  = iCount + 1
    .
  
    qhLinje:GET-NEXT().
  END.
  RUN PageFooter.
  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getFeil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFeil Procedure 
FUNCTION getFeil RETURNS CHARACTER
  ( INPUT iFeilKode AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cReturnValue AS CHAR NO-UNDO.
  
  ASSIGN 
    cReturnValue = DYNAMIC-FUNCTION("getFieldValues","Feilkode","WHERE Feilkode.Feilkode = " + STRING(iFeilkode),"beskrivelse").
  .

  RETURN cReturnValue.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLinje Procedure 
FUNCTION getLinje RETURNS LOGICAL
  (INPUT iType AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cReturnValue  AS CHAR NO-UNDO.
  DEF VAR cReturnValue2 AS CHAR NO-UNDO.
  DEF VAR cLevNamn      AS CHAR NO-UNDO.
  
  ASSIGN 
    cReturnValue = DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtBas.artikkelnr = " + bhRLinje:BUFFER-FIELD('Artikkelnr'):BUFFER-VALUE,"levkod,levfargkod,LevNr").
    cLevNamn = DYNAMIC-FUNCTION("getFieldValues","LevBas","WHERE LevBas.LevNr = " + ENTRY(3,cReturnValue,'|'),"LevNamn").
  .

  CASE iType:
    WHEN 1 THEN
    DO:
      PUT UNFORMATTED '<R' STRING(iRad) 
        '><C6>' bhRLinje:BUFFER-FIELD('artikkelnr'):BUFFER-VALUE 
        '<C10>' ENTRY(1,cReturnValue,'|') 
        '<C18><width=20>' bhRLinje:BUFFER-FIELD('varetekst'):BUFFER-VALUE 
        '<C32>' ENTRY(2,cReturnValue,'|') 
        '<C41>' bhRLinje:BUFFER-FIELD('storl'):BUFFER-VALUE 
        '<C46>' bhRLinje:BUFFER-FIELD('antall'):BUFFER-VALUE 
        /* Kunde skal ikke se summen
        '<C51>' (bhRLinje:BUFFER-FIELD('pris'):BUFFER-VALUE * bhRLinje:BUFFER-FIELD('antall'):BUFFER-VALUE - bhRLinje:BUFFER-FIELD('RabKr'):BUFFER-VALUE) SKIP
        */
        '<C51>' cLevNamn 
        .
    END.
    WHEN 2 THEN
    DO:
      cReturnValue2 = DYNAMIC-FUNCTION("getFieldValues","ArtPris","WHERE ArtPris.artikkelnr = " + QUOTER(bhRLinje:BUFFER-FIELD('Artikkelnr'):BUFFER-VALUE) 
                                       + 'AND ArtPris.ProfilNr = ' + STRING(iProfilNr),"Varekost[1]").
      PUT UNFORMATTED '<R' STRING(iRad) 
        '><C6>' bhRLinje:BUFFER-FIELD('artikkelnr'):BUFFER-VALUE 
        '<C10>' ENTRY(1,cReturnValue,'|') 
        '<C18><width=20>' bhRLinje:BUFFER-FIELD('varetekst'):BUFFER-VALUE 
        '<C32>' ENTRY(2,cReturnValue,'|') 
        '<C41>' bhRLinje:BUFFER-FIELD('storl'):BUFFER-VALUE 
        '<C46>' bhRLinje:BUFFER-FIELD('antall'):BUFFER-VALUE 
        '<C51>' (bhRLinje:BUFFER-FIELD('pris'):BUFFER-VALUE * bhRLinje:BUFFER-FIELD('antall'):BUFFER-VALUE - bhRLinje:BUFFER-FIELD('RabKr'):BUFFER-VALUE) 
        '<C55>' cReturnValue2 
        SKIP.
    END.
    WHEN 3 THEN
    DO:
      PUT UNFORMATTED '<R' STRING(iRad) 
        '><C6>' bhRLinje:BUFFER-FIELD('artikkelnr'):BUFFER-VALUE 
        '<C10>' ENTRY(1,cReturnValue,'|') 
        '<C18><width=20>' bhRLinje:BUFFER-FIELD('varetekst'):BUFFER-VALUE 
        '<C32>' ENTRY(2,cReturnValue,'|') 
        '<C41>' bhRLinje:BUFFER-FIELD('storl'):BUFFER-VALUE 
        '<C46>' bhRLinje:BUFFER-FIELD('antall'):BUFFER-VALUE 
        '<C51>' (bhRLinje:BUFFER-FIELD('pris'):BUFFER-VALUE * bhRLinje:BUFFER-FIELD('antall'):BUFFER-VALUE - bhRLinje:BUFFER-FIELD('RabKr'):BUFFER-VALUE) SKIP.
    END.
    WHEN 4 THEN
    DO:
      PUT UNFORMATTED '<R' STRING(iRad) 
        '><C6>' bhRLinje:BUFFER-FIELD('artikkelnr'):BUFFER-VALUE 
        '<C10>' ENTRY(1,cReturnValue,'|') 
        '<C18><width=20>' bhRLinje:BUFFER-FIELD('varetekst'):BUFFER-VALUE 
        '<C32>' ENTRY(2,cReturnValue,'|')
        '<C41>' bhRLinje:BUFFER-FIELD('storl'):BUFFER-VALUE 
        '<C46>' bhRLinje:BUFFER-FIELD('antall'):BUFFER-VALUE 
        '<C51>' (bhRLinje:BUFFER-FIELD('pris'):BUFFER-VALUE * bhRLinje:BUFFER-FIELD('antall'):BUFFER-VALUE - bhRLinje:BUFFER-FIELD('RabKr'):BUFFER-VALUE) SKIP.
    END.
  END CASE.

  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLinjeHode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLinjeHode Procedure 
FUNCTION getLinjeHode RETURNS LOGICAL
  (INPUT ipiType AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  CASE ipiType:
    WHEN 1 THEN
    DO:
      PUT UNFORMATTED '<FArial><P7>' SKIP.
      PUT UNFORMATTED '<R' STRING(iRad) '><C6><FROM><R' STRING(iRad) '><C78><LINE>' SKIP.
      PUT UNFORMATTED '<R' STRING(iRad)'><B><C6>Art.nr<C10>Lev.kod<C18>Beskrivelse<C32>Lev.Fargekod<C41>Størrelse<C46>Antall<C51>Leverandør</B>' SKIP.
      /*PUT UNFORMATTED '<R' STRING(iRad + 1) '><C6><FROM><R' STRING(iRad + 1) '><C78><LINE>' SKIP.*/
    END.
    WHEN 2 THEN
    DO:
      PUT UNFORMATTED '<FArial><P7>' SKIP.
      PUT UNFORMATTED '<R' STRING(iRad) '><C6><FROM><R' STRING(iRad) '><C78><LINE>' SKIP.
      PUT UNFORMATTED '<R' STRING(iRad)'><B><C6>Art.nr<C10>Lev.kod<C18>Beskrivelse<C32>Lev.Fargekod<C41>Størrelse<C46>Antall<C51>Sum<C55>InnkjøpsSum</B>' SKIP.
      PUT UNFORMATTED '<R' STRING(iRad + 1) '><C6><FROM><R' STRING(iRad + 1) '><C78><LINE>' SKIP.
    END.
    WHEN 3 THEN
    DO:
      PUT UNFORMATTED '<FArial><P7>' SKIP.
      PUT UNFORMATTED '<R' STRING(iRad) '><C6><FROM><R' STRING(iRad) '><C78><LINE>' SKIP.
      PUT UNFORMATTED '<R' STRING(iRad)'><B><C6>Art.nr<C10>Lev.kod<C18>Beskrivelse<C32>Lev.Fargekod<C41>Størrelse<C46>Antall<C51>Sum</B>' SKIP.
      PUT UNFORMATTED '<R' STRING(iRad + 1) '><C6><FROM><R' STRING(iRad + 1) '><C78><LINE>' SKIP.
    END.
    WHEN 4 THEN
    DO:
      PUT UNFORMATTED '<FArial><P7>' SKIP.
      PUT UNFORMATTED '<R' STRING(iRad) '><C6><FROM><R' STRING(iRad) '><C78><LINE>' SKIP.
      PUT UNFORMATTED '<R' STRING(iRad)'><B><C6>Art.nr<C10>Lev.kod<C18>Beskrivelse<C32>Lev.Fargekod<C41>Størrelse<C46>Antall<C51>Sum</B>' SKIP.
      PUT UNFORMATTED '<R' STRING(iRad + 1) '><C6><FROM><R' STRING(iRad + 1) '><C78><LINE>' SKIP.
    END.
  END CASE.
  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFdoButikkInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PDFdoButikkInfo Procedure 
FUNCTION PDFdoButikkInfo RETURNS LOGICAL
  (INPUT ipY AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cNavn         AS CHAR NO-UNDO.
  DEF VAR cAdr          AS CHAR NO-UNDO.
  DEF VAR cPnr          AS CHAR NO-UNDO.
  DEF VAR cTelefon      AS CHAR NO-UNDO.
  DEF VAR cPSted        AS CHAR NO-UNDO.
  DEF VAR cReturnValue  AS CHAR NO-UNDO.
  DEFINE VARIABLE cButFirmanavn AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cKontakt AS CHARACTER NO-UNDO.
  
  /* Henter linjen for å få tak i riktig butikknummer. */
  FIND FIRST Reklamasjonslinje NO-LOCK WHERE
    Reklamasjonslinje.ReklamasjonsNr = DECIMAL(bhRLogg:BUFFER-FIELD('reklamasjonsnr'):BUFFER-VALUE) NO-ERROR.

  ASSIGN
    cReturnValue = DYNAMIC-FUNCTION("getFieldValues","butiker","WHERE butiker.butik = " + 
                                     (IF AVAILABLE Reklamasjonslinje THEN STRING(ReklamasjonsLinje.Butik) ELSE STRING(iCL)),
                                     "butnamn,BuAdr,BuPonr,Butel,ProfilNr,LevKontakt,ButFirmanavn")
    cNavn        = ENTRY(1,cReturnValue,'|')
    cAdr         = ENTRY(2,cReturnValue,'|')
    cPnr         = ENTRY(3,cReturnValue,'|')
    cTelefon     = ENTRY(4,cReturnValue,'|')
    iProfilNr    = INT(ENTRY(5,cReturnValue,'|'))
    cKontakt     = ENTRY(6,cReturnValue,'|')
    cButFirmanavn = ENTRY(7,cReturnValue,'|')
  .
  IF cButFirmanavn <> "" THEN
      cNavn = cButFirmanavn.
  
  ASSIGN 
    cReturnValue = DYNAMIC-FUNCTION("getFieldValues","post","WHERE post.postnr = " + QUOTER(cPnr),"beskrivelse").
    cPSted      = ENTRY(1,cReturnValue,'|')
  .

  IF cNavn NE '' THEN
  DO:

      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
      RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Butik:/Butikk:"), pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - ipY).
                           
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",12).
      RUN pdf_text_xy_dec ("Spdf",TRIM(cNavn), pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - ipY - 14).
      RUN pdf_text_xy_dec ("Spdf",TRIM(cAdr), pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - ipY - 28).
      RUN pdf_text_xy_dec ("Spdf",cPnr + ' ' + cPSted, pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - ipY - 42).
      RUN pdf_text_xy_dec ("Spdf",cKontakt, pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - ipY - 56).
      RUN pdf_text_xy_dec ("Spdf",cTelefon, pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - ipY - 70).

  END.
  ELSE RETURN FALSE.
  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFdoKundeInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PDFdoKundeInfo Procedure 
FUNCTION PDFdoKundeInfo RETURNS LOGICAL
  (INPUT ipY AS DEC,
   INPUT ipX AS DEC) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cPSted        AS CHARACTER NO-UNDO.
  DEF VAR cReturnValue  AS CHARACTER NO-UNDO.
  DEF VAR cTelefon      AS CHAR NO-UNDO.
  DEF VAR cMobilTlf     AS CHAR NO-UNDO.
  DEF VAR cePostAdresse AS CHAR NO-UNDO.

  ASSIGN 
    cReturnValue  = DYNAMIC-FUNCTION("getFieldValues","post","WHERE post.postnr = " + QUOTER(bhRLogg:BUFFER-FIELD('postnr'):BUFFER-VALUE),"beskrivelse").
    cTelefon      = STRING(bhRLogg:BUFFER-FIELD('KundeTelefon'):BUFFER-VALUE). /*DYNAMIC-FUNCTION("getFieldValues","Kunde","WHERE Kunde.KundeNr = " + QUOTER(bhRLogg:BUFFER-FIELD('KundeNr'):BUFFER-VALUE),"Telefon").*/
    cMobilTlf     = STRING(bhRLogg:BUFFER-FIELD('KundeMobil'):BUFFER-VALUE). /* DYNAMIC-FUNCTION("getFieldValues","Kunde","WHERE Kunde.KundeNr = " + QUOTER(bhRLogg:BUFFER-FIELD('KundeNr'):BUFFER-VALUE),"MobilTlf"). */
    cePostAdresse = DYNAMIC-FUNCTION("getFieldValues","Kunde","WHERE Kunde.KundeNr = " + QUOTER(bhRLogg:BUFFER-FIELD('KundeNr'):BUFFER-VALUE),"ePostAdresse").
    cPSted        = ENTRY(1,cReturnValue,'|')
    .
  ASSIGN 
    cTelefon      = IF (cTelefon = ? OR TRIM(cTelefon) = '') THEN '' ELSE cTelefon
    cMobilTlf     = IF (cMobilTlf = ? OR trim(cMobilTlf) = '') THEN '' ELSE cMobilTlf
    cePostAdresse = IF (cePostAdresse = ? OR trim(cePostAdresse) = '') THEN '' ELSE cePostAdresse
    cPSted        = IF (cPSted = ? OR trim(cPSted) = '') THEN '' ELSE cPSted.


  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Kund:/Kunde:"), ipX,pdf_PageHeight("Spdf") - ipY).

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",12).
  RUN pdf_text_xy_dec ("Spdf",bhRLogg:BUFFER-FIELD('KundeNavn'):BUFFER-VALUE, ipX,pdf_PageHeight("Spdf") - ipY - 14).
  RUN pdf_text_xy_dec ("Spdf",bhRLogg:BUFFER-FIELD('KundeAdresse'):BUFFER-VALUE, ipX,pdf_PageHeight("Spdf") - ipY - 28).
  RUN pdf_text_xy_dec ("Spdf",bhRLogg:BUFFER-FIELD('postnr'):STRING-VALUE + ' ' + cPSted, ipX,pdf_PageHeight("Spdf") - ipY - 42).
  RUN pdf_text_xy_dec ("Spdf",cTelefon, ipX,pdf_PageHeight("Spdf") - ipY - 56).
  RUN pdf_text_xy_dec ("Spdf",cMobilTlf, ipX,pdf_PageHeight("Spdf") - ipY - 70).
  RUN pdf_text_xy_dec ("Spdf",cePostAdresse, ipX,pdf_PageHeight("Spdf") - ipY - 84).


  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFdoLevInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PDFdoLevInfo Procedure 
FUNCTION PDFdoLevInfo RETURNS LOGICAL
  (INPUT ipY AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/ 
  DEF VAR cReturnValue AS CHAR NO-UNDO.
  DEF VAR cPSted       AS CHAR NO-UNDO.
  
  ASSIGN 
    cReturnValue = DYNAMIC-FUNCTION("getFieldValues","post","WHERE post.postnr = " + QUOTER(bhRLogg:BUFFER-FIELD('postnr'):BUFFER-VALUE),"beskrivelse").
    cPSted      = ENTRY(1,cReturnValue,'|')
  .


/*   IF VALID-HANDLE(thLev) THEN DELETE OBJECT thLev. */
  thLev = DYNAMIC-FUNCTION("getTempTable","","LevBas|WHERE LevBas.LevNr = " + STRING(bhRLogg:BUFFER-FIELD('LevNr'):BUFFER-VALUE),bhLev).
  IF bhLev:AVAIL THEN
  DO:
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
      RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE", "Leverantör:/Leverandør:"), pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - ipY).
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",12).
      RUN pdf_text_xy_dec ("Spdf",bhLev:BUFFER-FIELD('Levnamn'):BUFFER-VALUE, pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - ipY - 14).
      RUN pdf_text_xy_dec ("Spdf",bhLev:BUFFER-FIELD('LevAdr'):BUFFER-VALUE, pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - ipY - 28).
      RUN pdf_text_xy_dec ("Spdf",bhLev:BUFFER-FIELD('Levponr'):BUFFER-VALUE + ' ' + cPSted, pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - ipY - 42).
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
      RUN pdf_text_xy_dec ("Spdf",bhLev:BUFFER-FIELD('LevTel'):BUFFER-VALUE + '   Faks:' + bhLev:BUFFER-FIELD('telefax'):BUFFER-VALUE + '   ePost:' + bhLev:BUFFER-FIELD('e_maillev'):BUFFER-VALUE, pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - ipY - 56).
  END.
  ELSE 
    RETURN FALSE.

  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFdoReklamasjonsInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PDFdoReklamasjonsInfo Procedure 
FUNCTION PDFdoReklamasjonsInfo RETURNS LOGICAL
  (INPUT ipY AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cReturnValue AS CHARACTER NO-UNDO.
  
  cReturnValue = DYNAMIC-FUNCTION("getFieldValues","Forsalj","WHERE forsalj.forsnr = INT(" + QUOTER(bhRLogg:BUFFER-FIELD('forsnr'):BUFFER-VALUE) + ')',"FoNamn").
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Reklamation:/Reklamasjon:"), pdf_LeftMargin ("Spdf") + 350,pdf_PageHeight("Spdf") - ipY).

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",12).
  RUN pdf_text_xy_dec ("Spdf","Nr:", pdf_LeftMargin ("Spdf") + 350,pdf_PageHeight("Spdf") - ipY - 14).
  RUN pdf_text_xy_dec ("Spdf",bhRLogg:BUFFER-FIELD('ReklamasjonsNr'):BUFFER-VALUE, pdf_LeftMargin("Spdf") + 440,pdf_PageHeight("Spdf") - ipY - 14).

  RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Datum:/Dato:"), pdf_LeftMargin ("Spdf") + 350,pdf_PageHeight("Spdf") - ipY - 28).
  RUN pdf_text_xy_dec ("Spdf",bhRLogg:BUFFER-FIELD('RegistrertDato'):BUFFER-VALUE, pdf_LeftMargin("Spdf") + 440,pdf_PageHeight("Spdf") - ipY - 28).

  RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Kassör:/Kasserer:"), pdf_LeftMargin ("Spdf") + 350,pdf_PageHeight("Spdf") - ipY - 42).
  RUN pdf_text_xy_dec ("Spdf",bhRLogg:BUFFER-FIELD('forsnr'):BUFFER-VALUE + ' ' + cReturnValue, pdf_LeftMargin("Spdf") + 440,pdf_PageHeight("Spdf") - ipY - 42).
  

  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFdoReklamasjonsLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PDFdoReklamasjonsLinje Procedure 
FUNCTION PDFdoReklamasjonsLinje RETURNS LOGICAL
  (INPUT ipY AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
    
------------------------------------------------------------------------------*/
/*   PDFdotline(ipY - 5). */
  DEFINE VARIABLE iAntal AS INTEGER     NO-UNDO.
  PDFgetLinjeHode(INT(ipY)). 
  bhRLinje:EMPTY-TEMP-TABLE().
  ipY = ipY + 25.
  DYNAMIC-FUNCTION("getTempTable","","ReklamasjonsLinje|WHERE reklamasjonsLinje.reklamasjonsnr = " + bhRLogg:BUFFER-FIELD('reklamasjonsnr'):BUFFER-VALUE,bhRLinje).
  qhLinje:QUERY-PREPARE('FOR EACH ' + bhRLinje:NAME + ' BY reklamasjonslinje.artikkelnr BY reklamasjonslinje.Storl').
  qhLinje:QUERY-OPEN().
  qhLinje:GET-FIRST().

  DO WHILE bhRLinje:AVAILABLE:
      IF ipY > 720 THEN DO:
          RUN pdf_new_page ("Spdf").
          PDFdoReklamasjonsInfo(60).
          ipY = 160.
          PDFgetLinjeHode(ipY). 
          ipY = ipY + 25.
      END.
/*     IF (iRad + 6) GT 60 THEN              */
/*     DO:                                   */
/*         ASSIGN                            */
/*           iCount = 0                      */
/*           iRad   = 5                      */
/*         .                                 */
/*         RUN PageFooter.                   */
/*         PAGE.                             */
/*         VIEW FRAME PageHeader.            */
/*         PDFgetLinjeHode(INT(cListeType)). */
/*     END.                                  */

    iRad = iRad + 3.
    PDFgetLinje(ipY).
    ipY = ipY + 20.

    IF INTEGER(bhRLinje:BUFFER-FIELD('Feilkode'):BUFFER-VALUE) > 0 THEN DO: 
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
      RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Felkod: /Feilkode: ") + bhRLinje:BUFFER-FIELD('Feilkode'):BUFFER-VALUE + '-' + getFeil(bhRLinje:BUFFER-FIELD('Feilkode'):BUFFER-VALUE), pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - ipY).
    END.
    ipY = ipY + 15.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
    RUN PDFNotat(bhRLinje:BUFFER-FIELD('FeilNotat'):BUFFER-VALUE,pdf_PageHeight("Spdf") - ipY,pdf_LeftMargin ("Spdf") + 10,480, OUTPUT iAntal).

/*     PUT UNFORMATTED '<R' STRING(iRad + 2) '><C6><#Notat2><R' STRING(iRad + 6) '><C78><FRAME#Notat2><USE#Notat2>' bhRLinje:BUFFER-FIELD('FeilNotat'):BUFFER-VALUE  '</USE>'  SKIP. */
/*     PUT UNFORMATTED '<R' STRING(iRad + 1) '><C6><FROM><R' STRING(iRad + 1) '><C78><LINE>' SKIP.                                                                                   */
/*     PUT UNFORMATTED '<P8>' SKIP.                                                                                                                                                  */
       IF iAntal < 3 THEN
          ipY = ipY + 30.
       ELSE
           ipY = ipY + (iAntal * 15).
    ASSIGN 
      iRad    = iRad + 6
      iCount  = iCount + 1
    .
  
    qhLinje:GET-NEXT().
  END.
  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFdotline) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PDFdotline Procedure 
FUNCTION PDFdotline RETURNS LOGICAL
  ( INPUT iY AS INTE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ii AS INTEGER     NO-UNDO.

  DO ii = 30 TO 500:
      RUN pdf_text_xy_dec ("Spdf",IF ii MOD 10 = 0 THEN "|" ELSE ".", ii,pdf_PageHeight("Spdf") - iY).
  END.
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFgetLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PDFgetLinje Procedure 
FUNCTION PDFgetLinje RETURNS LOGICAL
  (INPUT iY AS INTE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR cReturnValue  AS CHAR NO-UNDO.
  DEF VAR cReturnValue2 AS CHAR NO-UNDO.
  DEF VAR cLevNamn      AS CHAR NO-UNDO.
  DEF VAR cVaretekst    AS CHAR NO-UNDO.
  
  ASSIGN 
    cReturnValue = DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtBas.artikkelnr = " + bhRLinje:BUFFER-FIELD('Artikkelnr'):BUFFER-VALUE,"levkod,levfargkod,LevNr").
    ENTRY(1,cReturnValue,"|") = SUBSTR(ENTRY(1,cReturnValue,"|"),1,10).
    cLevNamn = DYNAMIC-FUNCTION("getFieldValues","LevBas","WHERE LevBas.LevNr = " + ENTRY(3,cReturnValue,'|'),"LevNamn").
  .
  cVaretekst = SUBSTR(bhRLinje:BUFFER-FIELD('varetekst'):BUFFER-VALUE,1,20).
  CASE INT(cListeType):
    WHEN 1 THEN
    DO:
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
      RUN pdf_text_xy_dec ("Spdf",bhRLinje:BUFFER-FIELD('artikkelnr'):BUFFER-VALUE, iPos[1],pdf_PageHeight("Spdf") - iY).
      RUN pdf_text_xy_dec ("Spdf",ENTRY(1,cReturnValue,'|'), iPos[2],pdf_PageHeight("Spdf") - iY).
      RUN pdf_text_xy_dec ("Spdf", cVaretekst, iPos[3],pdf_PageHeight("Spdf") - iY).
      RUN pdf_text_xy_dec ("Spdf",ENTRY(2,cReturnValue,'|'), iPos[4],pdf_PageHeight("Spdf") - iY).
      RUN pdf_text_xy_dec ("Spdf",bhRLinje:BUFFER-FIELD('storl'):BUFFER-VALUE, iPos[5],pdf_PageHeight("Spdf") - iY).
      RUN pdf_text_xy_dec ("Spdf",bhRLinje:BUFFER-FIELD('antall'):BUFFER-VALUE, iPos[6],pdf_PageHeight("Spdf") - iY).
      RUN pdf_text_xy_dec ("Spdf",cLevNamn, iPos[7],pdf_PageHeight("Spdf") - iY).
      RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), pdf_PageHeight("Spdf") - iY - 5, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , pdf_PageHeight("Spdf") - iY - 5, 0.5).   
      
    END.
    WHEN 2 THEN
    DO:
      cReturnValue2 = DYNAMIC-FUNCTION("getFieldValues","ArtPris","WHERE ArtPris.artikkelnr = " + QUOTER(bhRLinje:BUFFER-FIELD('Artikkelnr'):BUFFER-VALUE) 
                                       + 'AND ArtPris.ProfilNr = ' + STRING(iProfilNr),"Varekost[1]").
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
      RUN pdf_text_xy_dec ("Spdf",bhRLinje:BUFFER-FIELD('artikkelnr'):BUFFER-VALUE, iPos[1],pdf_PageHeight("Spdf") - iY).
      RUN pdf_text_xy_dec ("Spdf",ENTRY(1,cReturnValue,'|'), iPos[2],pdf_PageHeight("Spdf") - iY).
      RUN pdf_text_xy_dec ("Spdf", cVaretekst, iPos[3],pdf_PageHeight("Spdf") - iY).
      RUN pdf_text_xy_dec ("Spdf",ENTRY(2,cReturnValue,'|'), iPos[4],pdf_PageHeight("Spdf") - iY).
      RUN pdf_text_xy_dec ("Spdf",bhRLinje:BUFFER-FIELD('storl'):BUFFER-VALUE, iPos[5],pdf_PageHeight("Spdf") - iY).
      RUN pdf_text_xy_dec ("Spdf",bhRLinje:BUFFER-FIELD('antall'):BUFFER-VALUE, iPos[6],pdf_PageHeight("Spdf") - iY).
      RUN pdf_text_xy_dec ("Spdf",(bhRLinje:BUFFER-FIELD('pris'):BUFFER-VALUE * bhRLinje:BUFFER-FIELD('antall'):BUFFER-VALUE - bhRLinje:BUFFER-FIELD('RabKr'):BUFFER-VALUE), iPos[7],pdf_PageHeight("Spdf") - iY).
      RUN pdf_text_xy_dec ("Spdf",cReturnValue2, iPos[8],pdf_PageHeight("Spdf") - iY).
      RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), pdf_PageHeight("Spdf") - iY - 5, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , pdf_PageHeight("Spdf") - iY - 5, 0.5).   
    END.
    WHEN 3 THEN
    DO:
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
        RUN pdf_text_xy_dec ("Spdf",bhRLinje:BUFFER-FIELD('artikkelnr'):BUFFER-VALUE, iPos[1],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf",ENTRY(1,cReturnValue,'|'), iPos[2],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf", cVaretekst, iPos[3],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf",ENTRY(2,cReturnValue,'|'), iPos[4],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf",bhRLinje:BUFFER-FIELD('storl'):BUFFER-VALUE, iPos[5],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf",bhRLinje:BUFFER-FIELD('antall'):BUFFER-VALUE, iPos[6],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf",(bhRLinje:BUFFER-FIELD('pris'):BUFFER-VALUE * bhRLinje:BUFFER-FIELD('antall'):BUFFER-VALUE - bhRLinje:BUFFER-FIELD('RabKr'):BUFFER-VALUE), iPos[7],pdf_PageHeight("Spdf") - iY).
        RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), pdf_PageHeight("Spdf") - iY - 5, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , pdf_PageHeight("Spdf") - iY - 5, 0.5).   
      
    END.
    WHEN 4 THEN
    DO:
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
        RUN pdf_text_xy_dec ("Spdf",bhRLinje:BUFFER-FIELD('artikkelnr'):BUFFER-VALUE, iPos[1],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf",ENTRY(1,cReturnValue,'|'), iPos[2],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf", cVaretekst, iPos[3],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf",ENTRY(2,cReturnValue,'|'), iPos[4],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf",bhRLinje:BUFFER-FIELD('storl'):BUFFER-VALUE, iPos[5],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf",bhRLinje:BUFFER-FIELD('antall'):BUFFER-VALUE, iPos[6],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf",(bhRLinje:BUFFER-FIELD('pris'):BUFFER-VALUE * bhRLinje:BUFFER-FIELD('antall'):BUFFER-VALUE - bhRLinje:BUFFER-FIELD('RabKr'):BUFFER-VALUE), iPos[7],pdf_PageHeight("Spdf") - iY).
        RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), pdf_PageHeight("Spdf") - iY - 5, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , pdf_PageHeight("Spdf") - iY - 5, 0.5).   
    END.
  END CASE.

  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFgetLinjeHode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PDFgetLinjeHode Procedure 
FUNCTION PDFgetLinjeHode RETURNS LOGICAL
  (INPUT iY AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  CASE INT(cListeType):
    WHEN 1 THEN 
    DO:
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
      RUN pdf_text_xy_dec ("Spdf","Art.nr", iPos[1],pdf_PageHeight("Spdf") - iY).
      RUN pdf_text_xy_dec ("Spdf","Lev.kod", iPos[2],pdf_PageHeight("Spdf") - iY).
      RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Beskrivning/Beskrivelse"), iPos[3],pdf_PageHeight("Spdf") - iY).
      RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Lev.Färgkod/Lev.Fargekod"), iPos[4],pdf_PageHeight("Spdf") - iY).
      RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Storlek/Størrelse"), iPos[5],pdf_PageHeight("Spdf") - iY).
      RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Antal/Antall"), iPos[6],pdf_PageHeight("Spdf") - iY).
      RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Leverantör/Leverandør"), iPos[7],pdf_PageHeight("Spdf") - iY).

    END.
      WHEN 2 THEN
      DO:
          RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
          RUN pdf_text_xy_dec ("Spdf","Art.nr", iPos[1],pdf_PageHeight("Spdf") - iY).
          RUN pdf_text_xy_dec ("Spdf","Lev.kod", iPos[2],pdf_PageHeight("Spdf") - iY).
          RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Beskrivning/Beskrivelse"), iPos[3],pdf_PageHeight("Spdf") - iY).
          RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Lev.Färgkod/Lev.Fargekod"), iPos[4],pdf_PageHeight("Spdf") - iY).
          RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Storlek/Størrelse"), iPos[5],pdf_PageHeight("Spdf") - iY).
          RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Antal/Antall"), iPos[6],pdf_PageHeight("Spdf") - iY).
          RUN pdf_text_xy_dec ("Spdf","Sum", iPos[7],pdf_PageHeight("Spdf") - iY).
          RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","InköpsSum/InnkjøpsSum"), iPos[8],pdf_PageHeight("Spdf") - iY).
    END.
    WHEN 3 THEN
    DO:
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
        RUN pdf_text_xy_dec ("Spdf","Art.nr", iPos[1],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf","Lev.kod", iPos[2],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Beskrivning/Beskrivelse"), iPos[3],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Lev.Färgkod/Lev.Fargekod"), iPos[4],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Storlek/Størrelse"), iPos[5],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Antal/Antall"), iPos[6],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf","Sum", iPos[7],pdf_PageHeight("Spdf") - iY).
    END.
    WHEN 4 THEN
    DO:
        RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
        RUN pdf_text_xy_dec ("Spdf","Art.nr", iPos[1],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf","Lev.kod", iPos[2],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Beskrivning/Beskrivelse"), iPos[3],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Lev.Färgkod/Lev.Fargekod"), iPos[4],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Storlek/Størrelse"), iPos[5],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf",STRING(cLng = "SE","Antal/Antall"), iPos[6],pdf_PageHeight("Spdf") - iY).
        RUN pdf_text_xy_dec ("Spdf","Sum", iPos[7],pdf_PageHeight("Spdf") - iY).
    END.
  END CASE.
  RETURN TRUE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

