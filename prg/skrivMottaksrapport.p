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

DEFINE INPUT  PARAMETER ipOrdreNr   AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER ipcBestNr   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcButikker AS INTEGER    NO-UNDO.
DEFINE VARIABLE iBestNrTst  AS INTEGER  NO-UNDO.
DEFINE VARIABLE iOrdreNr    AS INTEGER    NO-UNDO.
DEFINE VARIABLE iBestNr     AS INTEGER    NO-UNDO.
DEFINE VARIABLE ii          AS INTEGER    NO-UNDO.
DEFINE VARIABLE cOrdreListe AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cStatusList AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iY AS INTEGER     NO-UNDO.
DEFINE VARIABLE iAntSida AS INTEGER     NO-UNDO.

DEFINE TEMP-TABLE TT_Ordre NO-UNDO
    FIELD OrdreNr      AS INTEGER.

DEFINE TEMP-TABLE TT_Bestnr NO-UNDO
    FIELD Bestnr      AS INTEGER
    FIELD Storrelser  AS CHARACTER
    FIELD DirekteLev  AS LOGICAL
    INDEX bestnr bestnr.

DEFINE TEMP-TABLE TT_BestButikk NO-UNDO
    FIELD Bestnr       AS INTEGER
    FIELD ButikkNr     AS INTEGER
    FIELD RadAntal     AS INTEGER
    INDEX idx1 bestnr butikknr.

DEFINE TEMP-TABLE TT_Bestilt NO-UNDO
    FIELD Bestnr       AS INTEGER
    FIELD ButikkNr     AS INTEGER
    FIELD cBestiltantal  AS CHARACTER
    FIELD bestUser     AS CHARACTER
    FIELD cRest        AS CHARACTER
    INDEX idx1 bestnr butikknr.

DEFINE TEMP-TABLE TT_Inlevert NO-UNDO
    FIELD Bestnr       AS INTEGER
    FIELD ButikkNr     AS INTEGER
    FIELD LeveringsNr  AS INTEGER
    FIELD Dato         AS DATE
    FIELD cInlevantal  AS CHARACTER
    FIELD LevertAv     AS CHARACTER
    FIELD cMakulert    AS CHARACTER
    INDEX idx1 bestnr butikknr leveringsnr.

DEFINE TEMP-TABLE TT_Rest NO-UNDO
    FIELD Bestnr       AS INTEGER
    FIELD ButikkNr     AS INTEGER
    FIELD LeveringsNr AS INTEGER
    FIELD cRest        AS CHARACTER
    INDEX idx1 bestnr butikknr leveringsnr.

DEFINE VARIABLE cLevNamn     LIKE levbas.levnamn  NO-UNDO.
DEFINE VARIABLE cInlevLabel AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE cOrdreNr    AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cHeaderTxt  AS CHARACTER FORMAT "x(30)"   NO-UNDO.
DEFINE VARIABLE iMin AS INTEGER NO-UNDO.
DEFINE VARIABLE iMax AS INTEGER NO-UNDO.
DEFINE VARIABLE cKundenavn    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPolygon      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikkNr     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iInfoCol      AS INTEGER INIT 6   NO-UNDO.
DEFINE VARIABLE iSignCol      AS INTEGER INIT 12   NO-UNDO.
DEFINE VARIABLE iStrCol       AS INTEGER INIT 26   NO-UNDO.
DEFINE VARIABLE iSidNr AS INTEGER     NO-UNDO.
DEFINE VARIABLE iRColFirstStr AS INTEGER  INIT 210   NO-UNDO.
DEFINE VARIABLE iPlusXstr AS INTEGER  INIT 30   NO-UNDO.
DEFINE FRAME PageHeader
   HEADER
      "<ALIGN=BASE><FArial><R2><P12><B><C6>" STRING(TODAY) "<C30>" cHeaderTxt "</B><C60>Butikk:" cButikkNr "<C75><P10>" PAGE-NUMBER FORMAT ">>" SKIP
      "<R3><C6><FROM><R3><C78><LINE>" SKIP
      WITH PAGE-TOP STREAM-IO WIDTH 255.

{ pdf_inc.i "THIS-PROCEDURE"}
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

&IF DEFINED(EXCLUDE-getString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getString Procedure 
FUNCTION getString RETURNS CHARACTER
  ( INPUT cEntryList AS CHARACTER )  FORWARD.

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
         HEIGHT             = 26.43
         WIDTH              = 95.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

IF ipOrdreNr <> 0 THEN DO:
    FIND ordre WHERE ordre.ordrenr = ipOrdreNr NO-LOCK NO-ERROR.
    IF NOT AVAIL Ordre THEN
        RETURN "FEIL, finner ikke ordre".
    CREATE TT_Ordre.
    ASSIGN TT_Ordre.OrdreNr = ipOrdreNr.
    IF TRIM(ipcBestNr) = "" THEN DO:
        ASSIGN ipcBestNr = "".
        FOR EACH BestHode WHERE BestHode.OrdreNr = ipOrdreNr NO-LOCK:
            ASSIGN ipcBestNr = ipcBestNr + (IF ipcBestNr <> "" THEN "," ELSE "") + STRING(BestHode.BestNr).
        END.
    END.
END.
ELSE IF TRIM(ipcBestNr) <> "" THEN DO:
    DO ii = 1 TO NUM-ENTRIES(ipcBestNr):
        ASSIGN iBestNrTst = INT(ENTRY(ii,ipcBestNr)) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RETURN "FEIL i Bestnrliste".
        FIND BestHode WHERE BestHode.Bestnr = iBestNrTst NO-LOCK NO-ERROR.
        IF NOT AVAIL BestHode THEN
            RETURN "FEIL, finner ikke besthode".
/*         FIND FIRST BestSort OF Besthode WHERE BestSort.Fri = TRUE NO-LOCK NO-ERROR. */
        IF NOT CAN-DO(cOrdreListe,STRING(BestHode.OrdreNr)) THEN
            ASSIGN cOrdreListe = cOrdreListe + (IF cOrdreListe <> "" THEN "," ELSE "") + STRING(BestHode.OrdreNr).
    END.
    IF NUM-ENTRIES(cOrdreListe) <> 1 THEN
        RETURN "Flere Ordre " + cOrdreListe.
    CREATE TT_Ordre.
    ASSIGN TT_Ordre.OrdreNr = INT(cOrdreListe).
END.
ELSE
    RETURN.
    {syspara.i 5 2 99 cStatusList}
RUN ByggTT_Inleveranser.

IF NOT CAN-FIND(FIRST TT_Bestnr) THEN
    RETURN "FEIL".
  {syspara.i 1 1 100 cKundenavn}
  {syspara.i 1 1 101 cPolygon}

RUN PDFSkrivRapport.
/*   RUN SkrivRapport. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggTT_Inleveranser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTT_Inleveranser Procedure 
PROCEDURE ByggTT_Inleveranser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iIdx AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iMin AS INTEGER INIT 1000 NO-UNDO.
    DEFINE VARIABLE iMax AS INTEGER INIT 0    NO-UNDO.
    DEFINE VARIABLE cEntryList  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cTmpString AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cTmpEntry  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lFinnsRest AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    DO ii = 1 TO NUM-ENTRIES(ipcBestNr): /* ii definearad i definitions */
        FIND BestHode WHERE BestHode.BestNr = INT(ENTRY(ii,ipcBestNr)) NO-LOCK.
        CREATE TT_Bestnr.
        ASSIGN TT_Bestnr.BestNr     = BestHode.Bestnr
               TT_BestNr.DirekteLev = BestHode.DirekteLev.

        FIND FIRST BestSort OF Besthode WHERE BestSort.Fri = TRUE NO-LOCK NO-ERROR.
        IF AVAIL BestSort THEN
            ASSIGN TT_BestNr.Storrelser = REPLACE(TRIM(BestSort.Storrelser)," ",",")
                   cEntryList  = FILL(",",NUM-ENTRIES(TT_BestNr.Storrelser) - 1).

/*     FIELD cBestiltantal  AS CHARACTER */
        ASSIGN iMin = 1000
               iMax = 0.
        FOR EACH BestStr WHERE BestStr.BestNr = BestHode.BestNr AND BestStr.Beststat = BestHode.BestStat AND BestStr.Bestilt <> 0 NO-LOCK.
            FIND TT_BestButikk WHERE TT_BestButikk.BestNr = BestStr.BestNr AND TT_BestButikk.ButikkNr = BestStr.Butik NO-ERROR.
            IF NOT AVAIL TT_BestButikk THEN DO:
                CREATE TT_BestButikk.
                ASSIGN TT_BestButikk.BestNr        = BestStr.BestNr
                       TT_BestButikk.ButikkNr      = BestStr.Butik
                       TT_BestButikk.RadAntal      = 8.
            END.
            FIND TT_Bestilt WHERE TT_Bestilt.BestNr = BestStr.BestNr AND TT_Bestilt.ButikkNr = BestStr.Butik NO-ERROR.
            IF NOT AVAIL TT_Bestilt THEN DO:
                CREATE TT_Bestilt.
                ASSIGN TT_Bestilt.BestNr        = BestStr.BestNr
                       TT_Bestilt.ButikkNr      = BestStr.Butik
                       TT_Bestilt.cBestiltantal = cEntryList.
            END.
            ASSIGN iIdx = LOOKUP(TRIM(BestStr.Storl),TT_BestNr.Storrelser)
                   ENTRY(iIdx,TT_Bestilt.cBestiltantal) = STRING(BestStr.Bestilt)
                   iMin = IF iIdx < iMin THEN iIdx ELSE iMin
                   iMax = IF iIdx > iMax THEN iIdx ELSE iMax.
        END.
        FOR EACH TT_Bestilt WHERE TT_Bestilt.BestNr = BestHode.BestNr.
            ASSIGN TT_Bestilt.cRest = TT_Bestilt.cBestiltAntal.
        END.

        IF CAN-FIND(FIRST BestLevert WHERE BestLevert.BestNr = BestHode.BestNr) THEN DO:
            /* finn störrelser på best */

            FOR EACH BestLevert OF BestHode NO-LOCK BREAK BY BestLevert.Butik BY BestLevert.STORL BY BestLevert.LeveringsNr:
                FIND TT_Inlevert WHERE TT_Inlevert.BestNr      = BestLevert.BestNr AND
                                       TT_Inlevert.ButikkNr    = BestLevert.Butik AND
                                       TT_Inlevert.LeveringsNr = BestLevert.LeveringsNr NO-ERROR.
                IF NOT AVAIL TT_Inlevert THEN DO:
                    FIND TT_BestButikk WHERE TT_BestButikk.BestNr = BestLevert.BestNr AND TT_BestButikk.ButikkNr = BestLevert.Butik NO-ERROR.
                    CREATE TT_Inlevert.
                    ASSIGN TT_Inlevert.BestNr      = BestLevert.BestNr
                           TT_Inlevert.ButikkNr    = BestLevert.Butik
                           TT_Inlevert.LeveringsNr = BestLevert.LeveringsNr
                           TT_Inlevert.Dato        = BestLevert.levertdato
                           TT_Inlevert.LevertAv    = BestLevert.LevertAv.
                    ASSIGN TT_Inlevert.cInlevAntal = cEntryList
                           TT_Inlevert.cMakulert   = TT_Inlevert.cInlevAntal
                           TT_BestButikk.RadAntal  = TT_BestButikk.RadAntal + 1.
                END.
                ASSIGN iIdx = LOOKUP(TRIM(BestLevert.Storl),TT_BestNr.Storrelser)
                       iMin = IF iIdx < iMin THEN iIdx ELSE iMin
                       iMax = IF iIdx > iMax THEN iIdx ELSE iMax.
                IF iIdx > 0 THEN DO:
                    IF BestLevert.Levert > 0 THEN DO:
                        ASSIGN ENTRY(iIdx,TT_Inlevert.cInlevAntal) = STRING(DECI(ENTRY(iIdx,TT_Inlevert.cInlevAntal)) + BestLevert.Levert).
                        FIND TT_Bestilt WHERE TT_Bestilt.BestNr = TT_Inlevert.BestNr AND TT_Bestilt.ButikkNr = TT_Inlevert.ButikkNr.
                        ASSIGN ENTRY(iIdx,TT_Bestilt.cRest) = STRING(DECI(ENTRY(iIdx,TT_Bestilt.cRest)) - BestLevert.Levert)
                               ENTRY(iIdx,TT_Bestilt.cRest) = IF ENTRY(iIdx,TT_Bestilt.cRest) = "0" THEN "" ELSE ENTRY(iIdx,TT_Bestilt.cRest).
                    END.
                    IF LAST-OF(BestLevert.Storl) THEN DO:
                        IF BestLevert.Avskrevet = TRUE AND Bestlevert.Rest > 0 THEN
                            ASSIGN ENTRY(iIdx,TT_Inlevert.cMakulert) = "M/" + STRING(Bestlevert.Rest).
                        ELSE IF BestLevert.Rest > 0 THEN DO:
/*                             FIND TT_Rest WHERE TT_Rest.BestNr      = BestLevert.BestNr AND                                */
/*                                                TT_Rest.ButikkNr    = BestLevert.Butik AND                                 */
/*                                                TT_Rest.LeveringsNr = 0 NO-ERROR.                                          */
/*                             IF NOT AVAIL TT_Rest THEN DO:                                                                 */
/*                                 CREATE TT_Rest.                                                                           */
/*                                 ASSIGN TT_Rest.BestNr      = BestLevert.BestNr                                            */
/*                                        TT_Rest.ButikkNr    = BestLevert.Butik                                             */
/*                                        TT_Rest.LeveringsNr = 0                                                            */
/*                                        TT_Rest.cRest = cEntryList                                                         */
/*                                        TT_BestButikk.RadAntal  = TT_BestButikk.RadAntal + 1.                              */
/*                             END.                                                                                          */
/*                             ASSIGN ENTRY(iIdx,TT_Rest.cRest) = STRING(DECI(ENTRY(iIdx,TT_Rest.cRest)) + BestLevert.Rest). */
                        END.
                    END.
                END.
            END.
            FOR EACH TT_Bestilt WHERE TT_Bestilt.BestNr = BestHode.BestNr:
                IF TT_Bestilt.cRest = TT_Bestilt.cBestiltAntal THEN
                    ASSIGN TT_Bestilt.cRest = "".
                ELSE DO:
                    ASSIGN lFinnsRest = FALSE.
                    DO iIdx = 1 TO NUM-ENTRIES(TT_Bestilt.cRest):
                        IF INT(ENTRY(iIdx,TT_Bestilt.cRest)) <> 0 THEN DO:
                            ASSIGN lFinnsRest = TRUE.
                            LEAVE.
                        END.
                    END.
                    IF lFinnsRest = TRUE THEN DO:
                        FIND TT_BestButikk WHERE TT_BestButikk.BestNr = TT_Bestilt.BestNr AND TT_BestButikk.ButikkNr = TT_Bestilt.Butik NO-ERROR.
                        ASSIGN TT_BestButikk.RadAntal  = TT_BestButikk.RadAntal + 1.
                    END.
                    ELSE
                        ASSIGN TT_Bestilt.cRest = "".
                END.
            END.
        END.
        ELSE DO:
        END.
        ASSIGN cTmpEntry = FILL(",",iMax - iMin).
               cTmpString = cTmpEntry.
        DO iCount = iMin TO iMax:
            ENTRY(iCount - iMin + 1,cTmpString) = ENTRY(iCount,TT_BestNr.Storrelser).
        END.
        ASSIGN TT_BestNr.Storrelser = cTmpString.
        FOR EACH TT_Bestilt WHERE TT_Bestilt.BestNr = TT_BestNr.BestNr:
            ASSIGN cTmpString = cTmpEntry.
            DO iCount = iMin TO iMax:
                 ENTRY(iCount - iMin + 1,cTmpString) = ENTRY(iCount,TT_Bestilt.cBestiltantal).
            END.
            ASSIGN TT_Bestilt.cBestiltantal = cTmpString.
        END.
        FOR EACH TT_Inlevert WHERE TT_Inlevert.BestNr = TT_BestNr.BestNr:
            DO iCount = iMin TO iMax:
                 ASSIGN ENTRY(iCount - iMin + 1,cTmpString) = ENTRY(iCount,TT_Inlevert.cInlevantal).
            END.
            ASSIGN TT_Inlevert.cInlevantal = cTmpString.
            ASSIGN cTmpString = cTmpEntry.
            DO iCount = iMin TO iMax:
                 ASSIGN ENTRY(iCount - iMin + 1,cTmpString) = ENTRY(iCount,TT_Inlevert.cMakulert).
            END.
            ASSIGN TT_Inlevert.cMakulert = cTmpString.
        END.
        FOR EACH TT_Rest WHERE TT_Rest.BestNr = TT_BestNr.BestNr:
            ASSIGN cTmpString = cTmpEntry.
            DO iCount = iMin TO iMax:
                 ENTRY(iCount - iMin + 1,cTmpString) = ENTRY(iCount,TT_Rest.cRest).
            END.
            ASSIGN TT_Rest.cRest = cTmpString.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFNewPage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFNewPage Procedure 
PROCEDURE PDFNewPage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN cButikkNr = STRING(TT_BestButikk.ButikkNr).
    iSidNr = iSidNr + 1.
    RUN pdf_new_page ("Spdf").
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
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",12).
   RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), 35, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , 35, 0.5).   
   RUN pdf_text_xy_dec ("Spdf",cKundenavn, pdf_LeftMargin ("Spdf"),20).
   RUN pdf_text_xy_dec ("Spdf",cPolygon, pdf_LeftMargin ("Spdf") + 180,20).
   RUN pdf_text_xy_dec ("Spdf",String(TODAY), pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") - bredd(String(TODAY)) ,20).

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

   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
   RUN pdf_text_xy_dec ("Spdf",STRING(TODAY), pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 30).
   RUN pdf_text_xy_dec ("Spdf",cHeaderTxt, pdf_LeftMargin ("Spdf") + 190,pdf_PageHeight("Spdf") - 30).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
   RUN pdf_text_xy_dec ("Spdf","Butikk: " + cButikkNr, pdf_LeftMargin ("Spdf") + 400,pdf_PageHeight("Spdf") - 30).
   RUN pdf_text_xy_dec ("Spdf",String(iSidNr), pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") - bredd(String(iSidNr)) ,pdf_PageHeight("Spdf") - 30).
   RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), pdf_PageHeight ("Spdf") - 32, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , pdf_PageHeight ("Spdf") - 32, 0.5).   
  IF iSidNr = 1 THEN
      iY = 90.
  ELSE
      iY = 50.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFSkrivBestillInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFSkrivBestillInfo Procedure 
PROCEDURE PDFSkrivBestillInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iRCol AS INTEGER     NO-UNDO.
    FIND farg OF ArtBas NO-LOCK NO-ERROR.
    FIND BestPris WHERE BestPris.Bestnr = BestHode.BestNr AND BestPris.BestStat = BestHode.BestStat NO-LOCK NO-ERROR.

    
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
    RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - iY, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") ,pdf_PageHeight("Spdf") - iY, 0.5).   
    iY = iY + 14.

    RUN pdf_text_xy_dec ("Spdf","Bestillingsnr:", pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - iY).
    RUN pdf_text_xy_dec ("Spdf",STRING(BestHode.BestNr), pdf_LeftMargin ("Spdf") + 80,pdf_PageHeight("Spdf") - iY).
    RUN pdf_text_xy_dec ("Spdf","Artikkel: " + STRING(Artbas.ArtikkelNr), pdf_LeftMargin ("Spdf") + 150,pdf_PageHeight("Spdf") - iY).
    RUN pdf_text_xy_dec ("Spdf","Direktelevert: " + STRING(BestHode.direktelev,"JA/NEI"), pdf_LeftMargin ("Spdf") + 330,pdf_PageHeight("Spdf") - iY).
    iY = iY + 14.
    RUN pdf_text_xy_dec ("Spdf","Levart:", pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - iY).
    RUN pdf_text_xy_dec ("Spdf",ArtBas.Levkod + (IF Artbas.beskr <> "" THEN " (" + SUBSTR(ArtBas.beskr,1,20) + ")" ELSE ""), pdf_LeftMargin ("Spdf") + 80,pdf_PageHeight("Spdf") - iY).
    RUN pdf_text_xy_dec ("Spdf","Varekost: ", pdf_LeftMargin ("Spdf") + 330,pdf_PageHeight("Spdf") - iY).
    RUN pdf_text_xy_dec ("Spdf",IF AVAIL BestPris THEN STRING(BestPris.Varekost,"->>,>>9.99") ELSE STRING(0,"9.99"), pdf_LeftMargin ("Spdf") + 440 - bredd(IF AVAIL BestPris THEN STRING(BestPris.Varekost,"->>,>>9.99") ELSE STRING(0,"9.99")),pdf_PageHeight("Spdf") - iY).
    iY = iY + 14.
    RUN pdf_text_xy_dec ("Spdf","Levfarge:", pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - iY).
    RUN pdf_text_xy_dec ("Spdf", ArtBas.LevFargKod + (IF AVAIL Farg AND TRIM(Farg.farbeskr) <> "" THEN " (" + Farg.farbeskr + ")" ELSE ""), pdf_LeftMargin ("Spdf") + 80,pdf_PageHeight("Spdf") - iY).
    RUN pdf_text_xy_dec ("Spdf","Pris: ", pdf_LeftMargin ("Spdf") + 330,pdf_PageHeight("Spdf") - iY).
    RUN pdf_text_xy_dec ("Spdf",IF AVAIL BestPris THEN STRING(BestPris.Pris,"->>,>>9.99") ELSE STRING(0,"9.99"), pdf_LeftMargin ("Spdf") + 440 - bredd(IF AVAIL BestPris THEN STRING(BestPris.Pris,"->>,>>9.99") ELSE STRING(0,"9.99")),pdf_PageHeight("Spdf") - iY).
    iY = iY + 14.
    RUN pdf_text_xy_dec ("Spdf","Status:     " + (IF NUM-ENTRIES(cStatusList) >= BestHode.BestStat THEN ENTRY(BestHode.BestStat,cStatusList) ELSE ""), pdf_LeftMargin ("Spdf") + 330,pdf_PageHeight("Spdf") - iY).
    iY = iY + 14.
    
    RUN pdf_text_xy_dec ("Spdf","Størrelser:", pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - iY).

    iRCol = iRColFirstStr.
    DO ii = 1 TO NUM-ENTRIES(TT_BestNr.Storrelser):
         RUN pdf_text_xy_dec ("Spdf",ENTRY(ii,TT_BestNr.Storrelser), iRcol - bredd(ENTRY(ii,TT_BestNr.Storrelser)),pdf_PageHeight("Spdf") - iY).
         RUN pdf_line IN h_PDFinc  ("Spdf", iRcol - bredd(ENTRY(ii,TT_BestNr.Storrelser)),pdf_PageHeight("Spdf") - iY - 1, iRCol ,pdf_PageHeight("Spdf") - iY - 1, 0.5).   
         iRcol = iRcol + iPlusXstr.
    END.
    iY = iY + 20.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFSkrivInleveranser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFSkrivInleveranser Procedure 
PROCEDURE PDFSkrivInleveranser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iRcol AS INTEGER     NO-UNDO.
    FIND BestHode WHERE bestHode.Bestnr = TT_BestNr.Bestnr NO-LOCK.
    FIND ArtBas OF BestHode.
    IF (iSidnr = 1 AND iAntSida = 5) OR (iSidnr > 1 AND iAntSida = 6) THEN DO:
        RUN PDFNewPage.
        iAntSida = 0.
    END.
    iAntSida = iAntSida + 1.
    RUN PDFSkrivBestillInfo.


    FOR EACH TT_Bestilt WHERE TT_Bestilt.ButikkNr = TT_BestButikk.ButikkNr AND TT_Bestilt.BestNr = TT_BestButikk.BestNr BY TT_Bestilt.BestNr:

        RUN pdf_text_xy_dec ("Spdf","Bestillt:", pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - iY).
        iRCol = iRColFirstStr.
        DO ii = 1 TO NUM-ENTRIES(TT_Bestilt.cBestiltantal):
             RUN pdf_text_xy_dec ("Spdf",ENTRY(ii,TT_Bestilt.cBestiltantal), iRcol - bredd(ENTRY(ii,TT_Bestilt.cBestiltantal)),pdf_PageHeight("Spdf") - iY).
             iRcol = iRcol + iPlusXstr.
        END.
        iY = iY + 14.

        PUT UNFORMATTED "<C" iInfoCol ">Bestillt:<C" STRING(iStrCol) ">" REPLACE(getString(TT_Bestilt.cBestiltantal),","," ") SKIP.
        FIND FIRST TT_Inlevert WHERE TT_Inlevert.Bestnr = TT_BestNr.bestnr AND TT_Inlevert.ButikkNr = TT_Bestilt.ButikkNr NO-ERROR.
        IF NOT AVAIL TT_Inlevert THEN
            .
/*             PUT UNFORMATTED "<C6>" TT_Bestilt.ButikkNr "<C9>" "Ingen innleveranse" SKIP(2). */
        ELSE DO:
            FOR EACH TT_Inlevert WHERE TT_Inlevert.Bestnr = TT_Bestnr.Bestnr AND TT_Inlevert.ButikkNr = TT_Bestilt.ButikkNr BREAK BY TT_Inlevert.leveringsnr:

                RUN pdf_text_xy_dec ("Spdf",STRING(TT_Inlevert.dato) + " " + TT_Inlevert.LevertAv, pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - iY).
                iRCol = iRColFirstStr.
                DO ii = 1 TO NUM-ENTRIES(TT_Bestilt.cBestiltantal):
                     RUN pdf_text_xy_dec ("Spdf",ENTRY(ii,TT_Bestilt.cBestiltantal), iRcol - bredd(ENTRY(ii,TT_Bestilt.cBestiltantal)),pdf_PageHeight("Spdf") - iY).
                     iRcol = iRcol + iPlusXstr.
                END.
                iY = iY + 14.

                IF LAST-OF(TT_Inlevert.leveringsnr) AND TT_Inlevert.cMakulert <> FILL(",",NUM-ENTRIES(TT_Bestnr.storrelser) - 1) THEN DO:
                    RUN pdf_text_xy_dec ("Spdf",STRING(TT_Inlevert.dato) + " " + TT_Inlevert.LevertAv, pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - iY).
                    iRCol = iRColFirstStr.
                    DO ii = 1 TO NUM-ENTRIES(TT_Inlevert.cMakulert):
                         RUN pdf_text_xy_dec ("Spdf",ENTRY(ii,TT_Inlevert.cMakulert), iRcol - bredd(ENTRY(ii,TT_Inlevert.cMakulert)),pdf_PageHeight("Spdf") - iY).
                         iRcol = iRcol + iPlusXstr.
                    END.
                    iY = iY + 14.
                END.
            END.
            IF TT_Bestilt.cRest <> "" THEN DO:
                iRCol = iRColFirstStr.
                RUN pdf_text_xy_dec ("Spdf","REST:", pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - iY).
                DO ii = 1 TO NUM-ENTRIES(TT_Bestilt.cRest):
                     RUN pdf_text_xy_dec ("Spdf",ENTRY(ii,TT_Bestilt.cRest), iRcol - bredd(ENTRY(ii,TT_Bestilt.cRest)),pdf_PageHeight("Spdf") - iY).
                     iRcol = iRcol + iPlusXstr.
                END.
            END.
                PUT UNFORMATTED "<C" iInfoCol ">" "REST" "<C" iStrCol ">" REPLACE(getString(TT_Bestilt.cRest),","," ") SKIP.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFSkrivOrdreHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFSkrivOrdreHeader Procedure 
PROCEDURE PDFSkrivOrdreHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cOrdreStatus AS CHARACTER  NO-UNDO.
    FIND syspara WHERE SysPara.SysHId = 5 AND 
                       SysPara.SysGr  = 3 and
                       SysPara.ParaNr = Ordre.Ordrestatus NO-LOCK NO-ERROR.

    IF AVAIL SysPara THEN
        ASSIGN cOrdreStatus = SysPara.Beskrivelse.
    FIND Levbas OF Ordre NO-LOCK NO-ERROR.
    ASSIGN cLevNamn    = IF AVAIL LevBas THEN LevBas.Levnamn ELSE "Ukjent".

   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
   RUN pdf_text_xy_dec ("Spdf","Ordre:", pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 50).
   RUN pdf_text_xy_dec ("Spdf",STRING(Ordre.OrdreNr), pdf_LeftMargin("Spdf") + 80,pdf_PageHeight("Spdf") - 50).
   RUN pdf_text_xy_dec ("Spdf","Ordrestatus:", pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 64).
   RUN pdf_text_xy_dec ("Spdf",cOrdreStatus, pdf_LeftMargin("Spdf") + 80,pdf_PageHeight("Spdf") - 64).
   RUN pdf_text_xy_dec ("Spdf","Leverandør:", pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 78).
   RUN pdf_text_xy_dec ("Spdf",STRING(Ordre.Levnr) + " " + cLevNamn, pdf_LeftMargin("Spdf") + 80,pdf_PageHeight("Spdf") - 78).
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFSkrivRapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFSkrivRapport Procedure 
PROCEDURE PDFSkrivRapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR pcRappFil      AS CHAR NO-UNDO.
    DEF VAR iRad           AS INTE NO-UNDO.
    DEF VAR cSendtDato     AS CHAR NO-UNDO.
    DEF VAR cUser          AS CHAR NO-UNDO.

/*     FIND Ordre WHERE ordre.ordrenr = ipOrdreNr NO-LOCK. */
    ASSIGN cUser = USERID("skotex")
           cUser = IF cUser = "" THEN "xx" ELSE cUser
           pcRappFil = SESSION:TEMP-DIRECTORY + "Mottak-" + cUser + "-" + STRING(TT_Ordre.OrdreNr) + ".pdf"
           cHeaderTxt = "Innleveranserapport"
               .
  RUN pdf_new ("Spdf",pcRappFil).
  pdf_PageHeader ("Spdf",THIS-PROCEDURE:HANDLE,"PDFPageHeader").
  pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PDFPageFooter").
  RUN pdf_set_PaperType ("Spdf","A4").
  RUN pdf_set_LeftMargin ("Spdf", 30).
  RUN pdf_set_BottomMargin ("Spdf", 40).
  RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf", 13).
/*   RUN pdf_set_Orientation ("Spdf", "landscape"). */
    
    FOR EACH TT_BestButikk BREAK BY TT_BestButikk.ButikkNr BY TT_BestButikk.BestNr.
        FIND TT_BestNr WHERE TT_BestNr.BestNr = TT_BestButikk.BestNr.
        IF FIRST-OF(TT_BestButikk.ButikkNr) AND NOT FIRST(TT_BestButikk.ButikkNr) THEN DO:
            RUN PDFNewPage.
            RUN PDFSkrivOrdreHeader.
        END.
        ELSE IF FIRST-OF(TT_BestButikk.ButikkNr) THEN DO:
            RUN PDFNewPage.
            IF TT_Ordre.OrdreNr > 0 THEN
                RUN PDFSkrivOrdreHeader.
        END.
        RUN PDFSkrivInleveranser.
    END.
  RUN pdf_close ("Spdf").
  RUN browse2pdf\viewxmldialog.w (pcRappFil,"Polygon Retail Solutions").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PFooter Procedure 
PROCEDURE PFooter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      PUT UNFORMATTED "<R66><C6><FROM><R66><C80><LINE>" SKIP
                      "<C6>" cKundenavn "<C35>" cPolygon "<C74>" STRING(TODAY) SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivBestillInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivBestillInfo Procedure 
PROCEDURE SkrivBestillInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND farg OF ArtBas NO-LOCK NO-ERROR.
    FIND BestPris WHERE BestPris.Bestnr = BestHode.BestNr AND BestPris.BestStat = BestHode.BestStat NO-LOCK NO-ERROR.
    IF LINE-COUNTER > 5 THEN PUT UNFORMATTED "<C6><FROM><C78><LINE>" SKIP.
    PUT UNFORMATTED 
/*         "<C6><FROM><C78><LINE>" SKIP */
                    "<C6>Bestillingsnr: <C15>" BestHode.BestNr "<C25>Artikkel: " Artbas.ArtikkelNr "<C50>Direktelevert: " STRING(BestHode.direktelev,"JA/NEI") SKIP
                    "<C6>Levart: <C15>"      ArtBas.Levkod (IF Artbas.beskr <> "" THEN " (" + SUBSTR(ArtBas.beskr,1,20) + ")" ELSE "")   "<C50>Varekost: <C57><RIGHT=C+7>" IF AVAIL BestPris THEN STRING(BestPris.Varekost,"->>,>>9.99") ELSE "0" SKIP 
                    "<C6>Levfarge: <C15>"  ArtBas.LevFargKod (IF AVAIL Farg AND TRIM(Farg.farbeskr) <> "" THEN " (" + Farg.farbeskr + ")" ELSE "") "<C50>Pris: <C57><RIGHT=C+7>" IF AVAIL BestPris THEN STRING(BestPris.Pris,"->>,>>9.99") ELSE "0" SKIP
                    "<C50>Status: <C57>" (IF NUM-ENTRIES(cStatusList) >= BestHode.BestStat THEN ENTRY(BestHode.BestStat,cStatusList) ELSE " ") SKIP
                    "<C6>Størrelser:<C" iStrCol "><U>" REPLACE(getString(TT_BestNr.Storrelser),","," ") "</U>" SKIP(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivInleveranser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivInleveranser Procedure 
PROCEDURE SkrivInleveranser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    FIND BestHode WHERE bestHode.Bestnr = TT_BestNr.Bestnr NO-LOCK.
    FIND ArtBas OF BestHode.
    IF LINE-COUNTER + TT_BestButikk.RadAntal > 65 THEN DO:
        RUN PFooter.
        PAGE.
        VIEW FRAME PageHeader.
    END.
    RUN SkrivBestillInfo.
    FOR EACH TT_Bestilt WHERE TT_Bestilt.ButikkNr = TT_BestButikk.ButikkNr AND TT_Bestilt.BestNr = TT_BestButikk.BestNr BY TT_Bestilt.BestNr:
        PUT UNFORMATTED "<C" iInfoCol ">Bestillt<C" STRING(iStrCol) ">" REPLACE(getString(TT_Bestilt.cBestiltantal),","," ") SKIP.
        FIND FIRST TT_Inlevert WHERE TT_Inlevert.Bestnr = TT_BestNr.bestnr AND TT_Inlevert.ButikkNr = TT_Bestilt.ButikkNr NO-ERROR.
        IF NOT AVAIL TT_Inlevert THEN
            .
/*             PUT UNFORMATTED "<C6>" TT_Bestilt.ButikkNr "<C9>" "Ingen innleveranse" SKIP(2). */
        ELSE DO:
            FOR EACH TT_Inlevert WHERE TT_Inlevert.Bestnr = TT_Bestnr.Bestnr AND TT_Inlevert.ButikkNr = TT_Bestilt.ButikkNr BREAK BY TT_Inlevert.leveringsnr:
                PUT UNFORMATTED "<C" iInfoCol ">" TT_Inlevert.dato "<C" iSignCol ">" TT_Inlevert.LevertAv /* " " TT_Inlevert.leveringsnr */ "<C" iStrCol ">" REPLACE(getString(TT_Inlevert.cInlevantal),","," ") SKIP.
                IF LAST-OF(TT_Inlevert.leveringsnr) AND TT_Inlevert.cMakulert <> FILL(",",NUM-ENTRIES(TT_Bestnr.storrelser) - 1) THEN
                    PUT UNFORMATTED "<C" iInfoCol ">" TT_Inlevert.dato "<C" iSignCol ">" TT_Inlevert.LevertAv /* " " TT_Inlevert.leveringsnr */ "<C" iStrCol ">" REPLACE(getString(TT_Inlevert.cMakulert),","," ") SKIP.
            END.
/*             FIND TT_Rest WHERE TT_Rest.Bestnr = TT_Bestnr.Bestnr AND TT_Rest.ButikkNr = TT_Bestilt.ButikkNr NO-ERROR. */
/*             IF AVAIL TT_Rest THEN                                                                                     */
            IF TT_Bestilt.cRest <> "" THEN
                PUT UNFORMATTED "<C" iInfoCol ">" "REST" "<C" iStrCol ">" REPLACE(getString(TT_Bestilt.cRest),","," ") SKIP.
        END.
        PUT SKIP(1).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivOrdreHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivOrdreHeader Procedure 
PROCEDURE SkrivOrdreHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cOrdreStatus AS CHARACTER  NO-UNDO.
    FIND syspara WHERE SysPara.SysHId = 5 AND 
                       SysPara.SysGr  = 3 and
                       SysPara.ParaNr = Ordre.Ordrestatus NO-LOCK NO-ERROR.

    IF AVAIL SysPara THEN
        ASSIGN cOrdreStatus = SysPara.Beskrivelse.
    FIND Levbas OF Ordre NO-LOCK NO-ERROR.
    ASSIGN cLevNamn    = IF AVAIL LevBas THEN LevBas.Levnamn ELSE "Ukjent".

    PUT UNFORMATTED "<C6>Ordre: <C15>"       Ordre.OrdreNr SKIP
                    "<C6>Ordrestatus: <C15>" cOrdreStatus SKIP
                    "<C6>Leverandør: <C15>" Ordre.Levnr " " cLevNamn SKIP(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivRapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivRapport Procedure 
PROCEDURE SkrivRapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR pcRappFil      AS CHAR NO-UNDO.
    DEF VAR iRad           AS INTE NO-UNDO.
    DEF VAR cSendtDato     AS CHAR NO-UNDO.
    DEF VAR cUser          AS CHAR NO-UNDO.

/*     FIND Ordre WHERE ordre.ordrenr = ipOrdreNr NO-LOCK. */
    ASSIGN cUser = USERID("skotex")
           cUser = IF cUser = "" THEN "xx" ELSE cUser
           pcRappFil = SESSION:TEMP-DIRECTORY + "Mottak-" + cUser + "-" + STRING(TT_Ordre.OrdreNr) + ".xpr"
           cHeaderTxt = "Innleveranserapport"
               .

    OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(68).
    PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
  /*   PUT CONTROL '<SILENT=TRUE>'. */
  /*   PUT CONTROL '<PRINT=NO>'.    */
    PUT CONTROL '<PREVIEW=ZoomToWidth>'.
  /*   PUT CONTROL '<PREVIEW=PDF>'. */
    /*put control "<PrinterSetup>". */
    FOR EACH TT_BestButikk BREAK BY TT_BestButikk.ButikkNr BY TT_BestButikk.BestNr.
        FIND TT_BestNr WHERE TT_BestNr.BestNr = TT_BestButikk.BestNr.
        IF FIRST-OF(TT_BestButikk.ButikkNr) AND NOT FIRST(TT_BestButikk.ButikkNr) THEN DO:
            RUN PFooter.
            PAGE.
            ASSIGN cButikkNr = STRING(TT_BestButikk.ButikkNr).
            VIEW FRAME PageHeader.
            RUN SkrivOrdreHeader.
        END.
        ELSE IF FIRST-OF(TT_BestButikk.ButikkNr) THEN DO:
            ASSIGN cButikkNr = STRING(TT_BestButikk.ButikkNr).
            VIEW FRAME PageHeader.
            IF TT_Ordre.OrdreNr > 0 THEN
                RUN SkrivOrdreHeader.
        END.
        RUN SkrivInleveranser.
    END.
    RUN PFooter.
    OUTPUT CLOSE.
   RUN VisXprint.p (pcRappFil).
/* DEFINE TEMP-TABLE TT_Bestnr NO-UNDO   */
/*     FIELD Bestnr       AS INTEGER     */
/*     FIELD Storrelser  AS CHARACTER.   */
/*                                       */
/* DEFINE TEMP-TABLE TT_Inlevert NO-UNDO */
/*     FIELD Bestnr       AS INTEGER     */
/*     FIELD ButikkNr     AS INTEGER     */
/*     FIELD LeveringsNr AS INTEGER      */
/*     FIELD Dato        AS DATE         */
/*     FIELD cInlevantal  AS CHARACTER   */
/*     FIELD LevertAv      AS CHARACTER  */
/*     FIELD cRest        AS CHARACTER.  */

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

&IF DEFINED(EXCLUDE-getString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getString Procedure 
FUNCTION getString RETURNS CHARACTER
  ( INPUT cEntryList AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iStart AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iStep  AS INTEGER    NO-UNDO.
  ASSIGN iStart = iStrCol
         iStep  = 4.
  DO iCount = 1 TO NUM-ENTRIES(cEntryList):
      ENTRY(iCount,cEntryList) = "<C" + STRING(iStart + ((iCount - 1) * iStep)) + "><RIGHT=C+3>" + ENTRY(iCount,cEntryList).
  END.
  RETURN cEntryList.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

