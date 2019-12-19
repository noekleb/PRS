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


DEFINE INPUT  PARAMETER lplListeId AS DECIMAL FORMAT ">>>>>>>9" NO-UNDO. 
DEFINE INPUT  PARAMETER piButNr    AS INTEGER                  NO-UNDO.
DEFINE INPUT  PARAMETER iLevType   AS INTEGER                  NO-UNDO. /* 1-Skjerm, 2-Batch, 3-eMail (Butikkens kontaktperson) */
DEFINE OUTPUT PARAMETER pcFilNavn  AS CHARACTER                NO-UNDO.

DEFINE VARIABLE lBatch             AS LOG                      NO-UNDO.
DEFINE VARIABLE cSprak             AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cBelopp            AS CHARACTER                NO-UNDO.
DEFINE VARIABLE dColPosBF          AS DECIMAL EXTENT 5         NO-UNDO.
DEFINE VARIABLE dColPosFR          AS DECIMAL EXTENT 6         NO-UNDO.
DEFINE VARIABLE dULstartFR         AS DECIMAL EXTENT 6         NO-UNDO.
DEFINE VARIABLE iLineSpace         AS INTEGER INITIAL 11       NO-UNDO.
DEFINE VARIABLE cTekst             AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cButBatchPrinter   AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cLogo              AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cTittel            AS CHARACTER FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE cFirma             AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE cDato              AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cButikkTxt AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cparToADDRESS AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOk AS LOG NO-UNDO.

DEFINE VARIABLE cOverskr AS CHARACTER EXTENT 7 NO-UNDO.
DEFINE VARIABLE cLabel   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dColPos_S4  AS DECIMAL  EXTENT 6   NO-UNDO.
DEFINE VARIABLE dULstart_S4 AS DECIMAL  EXTENT 6   NO-UNDO.

DEFINE VARIABLE rSendEMail AS cls.SendEMail.SendEMail NO-UNDO.

DEFINE TEMP-TABLE ttPlukk 
  FIELD ButNr AS INTEGER FORMAT ">>>>>9"
  FIELD ArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>9"
  FIELD Beskr AS CHARACTER FORMAT "x(40)"
  FIELD LevKod AS CHARACTER FORMAT "x(20)"
  FIELD LevFargKod AS CHARACTER FORMAT "x(30)"
  FIELD Sasong AS CHARACTER FORMAT "x(20)"
  FIELD Storl AS CHARACTER FORMAT "x(10)"
  FIELD StrKode AS INTEGER FORMAT ">>>>>>>9"
  FIELD Antall AS DECIMAL FORMAT "->>>>>>>>9"
  FIELD Lagant AS DECIMAL FORMAT "->>>>>>>>9"
  INDEX idxPlukk ArtikkelNr StrKode
  INDEX idxSkriv LevKod LevFargKod Sasong Storl.        

{tmpKas_Rap.i &NEW = NEW &SHARED = SHARED}
DEFINE BUFFER btmpKas_rap FOR tmpKas_rap.

{ pdf_inc.i "THIS-PROCEDURE"}

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
  ( INPUT cText AS CHARACTER )  FORWARD.

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
         HEIGHT             = 27.81
         WIDTH              = 82.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

rSendEMail  = NEW cls.SendEMail.SendEMail( ) NO-ERROR.

ASSIGN 
  lBatch = IF iLevType = 2 THEN TRUE ELSE FALSE
  . 

FIND Butiker WHERE Butiker.Butik = piButNr NO-LOCK NO-ERROR.
IF NOT AVAIL Butiker THEN
    RETURN "FEIL".

ASSIGN 
  cparToADDRESS = IF NUM-ENTRIES(Butiker.ePostAdresse,'@') = 2 THEN Butiker.ePostAdresse ELSE ''  
  .

FIND bruker WHERE 
  bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.

FIND PlListeHode NO-LOCK WHERE 
  PlListeHode.PlListeId = lPlListeId NO-ERROR.
IF NOT AVAIL PlListeHode THEN
    RETURN "FEIL".

RUN ByggListe.

RUN PDFUtskrift.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ButikRubrik) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButikRubrik Procedure 
PROCEDURE ButikRubrik :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cString AS CHARACTER  NO-UNDO.
    
    cButikkTxt = "Butikk".
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
    IF AVAIL Butiker THEN
    DO:
        RUN pdf_text_xy_dec ("Spdf",cButikkTxt + " " + STRING(Butiker.Butik) + " " + Butiker.ButNamn + " " + cString,pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 88).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-ByggListe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggListe Procedure 
PROCEDURE ByggListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pdDato       AS DATE NO-UNDO.
  DEFINE VARIABLE lFlereDar AS LOGICAL    NO-UNDO.

  /* Tømmer temp-table */
  EMPTY TEMP-TABLE ttPlukk NO-ERROR.

  FOR EACH PlListeLinje OF PlListeHode NO-LOCK:
    FIND ArtBas OF PlListeLinje NO-LOCK NO-ERROR.
    FIND FIRST ArtLag NO-LOCK WHERE
      ArtLag.ArtikkelNr = PlListeLinje.ArtikkelNr AND 
      ArtLag.butik      = piButNr AND 
      ArtLag.StrKode    = PlListeLinje.StrKode NO-ERROR. 
    FIND StrKonv NO-LOCK WHERE 
      StrKonv.StrKode = ArtLag.StrKode NO-ERROR.
     
    CREATE ttPlukk.
    ASSIGN
      ttPlukk.ButNr      = piButNr
      ttPlukk.ArtikkelNr = PlListeLinje.ArtikkelNr
      ttPlukk.Beskr      = IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE ''
      ttPlukk.LevKod     = IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE ''
      ttPlukk.LevFargKod = IF AVAILABLE ArtBAs THEN ArtBas.LevFargKod ELSE ''
      ttPlukk.Sasong     = IF AVAILABLE ArtBas THEN STRING(ArtBas.Sasong) ELSE ''
      ttPlukk.Storl      = IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE ''
      ttPlukk.StrKode    = PlListeLinje.StrKode
      ttPlukk.Antall     = PlListeLinje.Antall
      ttPlukk.Lagant     = IF AVAILABLE ArtLag THEN ArtLag.LagAnt ELSE 0
      .
  END. 

  STATUS DEFAULT "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-PageFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PageFooter Procedure 
PROCEDURE PageFooter :
/*------------------------------------------------------------------------------
  Purpose:  Procedure to Print Page Footer -- on all pages.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cSidTxt AS CHARACTER   NO-UNDO.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_set_dash IN h_PDFinc ("Spdf",1,0).
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), pdf_BottomMargin ("Spdf"), pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , pdf_BottomMargin ("Spdf"), 0.5).

  RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(TODAY)) + " " + STRING(TIME,"HH:MM:SS"),pdf_LeftMargin ("Spdf"),pdf_BottomMargin ("Spdf") - 14).
  cSidTxt = TRIM("Sida: " + STRING(pdf_page("Spdf")) + " (" + pdf_TotalPages("Spdf") + ")").
  RUN pdf_text_xy_dec ("Spdf",cSidTxt,pdf_Pagewidth ("Spdf") - pdf_LeftMargin ("Spdf") - 50,pdf_BottomMargin ("Spdf") - 14).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PageHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PageHeader Procedure 
PROCEDURE PageHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",20).
  RUN pdf_text_xy_dec ("Spdf",cTittel,pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 45).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
  RUN pdf_text_xy_dec ("Spdf",cFirma,pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 61).
  
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_text_xy_dec ("Spdf",cDato,pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 72).
  
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), pdf_PageHeight ("Spdf") - 74, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , pdf_PageHeight ("Spdf") - 74, 0.5).
/*   RUN xx. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFUtskrift) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFUtskrift Procedure 
PROCEDURE PDFUtskrift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLoop               AS INT    NO-UNDO.
  DEF VAR pcOverskr            AS CHAR    NO-UNDO.
  DEFINE VARIABLE dY           AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE wOK          AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cPrinter     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dYspara      AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cEmail       AS CHARACTER NO-UNDO.
  
  {syspara.i  1 1 100 cFirma}
  cDato = "Dato: " + STRING(PlListeHode.RegistrertDato).
 
  ASSIGN 
    pcFilNavn =  "Plukkliste" + "_" + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME),':','') + ".pdf".

  IF lBatch = TRUE THEN 
    ASSIGN cButBatchPrinter = TRIM(Butiker.RAPPrinter).
  
  RUN pdf_new ("Spdf",pcFilNavn).
  pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PageFooter").
  RUN pdf_set_PaperType ("Spdf","A4").
  RUN pdf_set_LeftMargin ("Spdf", 40).
  RUN pdf_set_BottomMargin ("Spdf", 60).
  RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
  RUN pdf_set_Orientation ("Spdf","portrait").

  RUN SetPositioner.

  RUN pdf_new_page ("Spdf").
  RUN ButikRubrik.
  cTittel     = "Plukkliste".
  RUN PageHeader.
  dY = pdf_PageHeight ("Spdf") - 110.
  
  RUN SideUtskrift(INPUT-OUTPUT dY).

  RUN pdf_close ("Spdf").
  
  IF cButBatchPrinter <> "" THEN 
  DO:
      IF SEARCH("cmd\PrintPdf.cmd") <> ? THEN DO:
          OS-COMMAND SILENT VALUE(SEARCH("cmd\PrintPdf.cmd") + " " + pcFilnavn + " " + '"' + cButBatchPrinter + '"').
      END.
  END.
  ELSE IF iLevType = 3 AND cparToADDRESS <> '' THEN 
    RUN sendEMail( pcFilnavn, piButNr ).

  STATUS DEFAULT " ".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sendEMail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendEMail Procedure
PROCEDURE sendEMail:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER icFil AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iButNr AS INTEGER NO-UNDO.

IF cparToADDRESS = '' THEN 
DO:
  RETURN.
END.

FILE-INFO:FILE-NAME = icFil.

rSendEMail:parToADDRESS = cparToADDRESS.
rSendEMail:parMailType = 'PLUKKLISTE'.
rSendEMail:parSUBJECT  = 'Plukkliste for butikk: ' + STRING(Butiker.butik) + ' ' + Butiker.ButNamn + ' (Dato/Tid: ' + STRING(NOW,"99/99/9999 HH:MM:SS") + ').'.
rSendEMail:parMESSAGE  = "Plukkliste: " + icFil + '.' + CHR(10) + 
                         "Listen inneholder varer solgt siden forrige gang den bla tatt ut.".
rSendEMail:parFILE     = FILE-INFO:FULL-PATHNAME.  

bOk = rSendEMail:send( ).

END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-SideUtskrift) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SideUtskrift Procedure 
PROCEDURE SideUtskrift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
  
  DEFINE VARIABLE cTxt     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dSumSolgt AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dSumLagAnt AS DECIMAL     NO-UNDO.

  RUN tblHeader(INPUT-OUTPUT dY).
      
  FOR EACH ttPlukk
    BREAK BY ttPlukk.LevKod
          BY ttPlukk.LevFargKod
          BY ttPlukk.Storl:
    ASSIGN 
      dSumSolgt = dSumSolgt + ttPlukk.Antall
      dSumLagAnt = dSumLagant + ttPlukk.Lagant
      .
    dY = dY - iLineSpace.
    
    cTxt = SUBSTRING(ttPlukk.Beskr,1,45).
    RUN pdf_text_xy_dec ("Spdf",cTxt,dColPos_S4[1] - bredd(ctxt),dY).
    
    cTxt = STRING(ttPlukk.LevKod).
    RUN pdf_text_xy_dec ("Spdf",cTxt,dColPos_S4[2] - bredd(ctxt),dY).

    cTxt = STRING(ttPlukk.LevFargKod).
    RUN pdf_text_xy_dec ("Spdf",cTxt,dColPos_S4[3] - bredd(ctxt),dY).

    cTxt = STRING(ttPlukk.Storl).
    RUN pdf_text_xy_dec ("Spdf",cTxt,dColPos_S4[4] - bredd(ctxt),dY).

    cBelopp = STRING(ROUND(ttPlukk.Antall,0),"->>>,>>>,>>9").
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S4[5] - bredd(cBelopp),dY).

    cBelopp = TRIM(STRING(ttPlukk.Lagant,"->>>,>>>,>>9")).
    RUN pdf_text_xy_dec ("Spdf",cBelopp,dColPos_S4[6] - bredd(cBelopp),dY).

    IF dY <= 100 THEN 
    DO:      
      RUN pdf_new_page ("Spdf").
      cTittel     = "Plukkliste".
      RUN PageHeader.
      dY = pdf_PageHeight ("Spdf") - 110.
      RUN tblHeader(INPUT-OUTPUT dY).
    END.
  END.
  
  /* Sumlinje og total */  
  dY = dY - 4.
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S4[5], dY, dULstart_S4[4] + 10, dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S4[6], dY, dULstart_S4[5] + 10, dY, 0.5).
  dY = dY - iLineSpace.

  cBelopp = TRIM(STRING(dSumSolgt,"->>>,>>>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dULStart_S4[5] - bredd(cBelopp),dY).
  cBelopp = TRIM(STRING(dSumLagant,"->>>,>>>,>>9")).
  RUN pdf_text_xy_dec ("Spdf",cBelopp,dULStart_S4[6] - bredd(cBelopp),dY).
  dY = dY - 4.
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S4[5], dY, dULstart_S4[4] + 10, dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S4[6], dY, dULstart_S4[5] + 10, dY, 0.5).
  dY = dY - 2.
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S4[5], dY, dULstart_S4[4] + 10, dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S4[6], dY, dULstart_S4[5] + 10, dY, 0.5).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-SetPositioner) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetPositioner Procedure 
PROCEDURE SetPositioner :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN dColPosBF[1] = pdf_LeftMargin ("Spdf")
         dColPosBF[2] = 210
         dColPosBF[3] = 370
         dColPosBF[4] = 470
         dColPosBF[5] = pdf_PageWidth ("Spdf") - pdf_LeftMargin ("Spdf").
  ASSIGN dColPosFR[1] = pdf_LeftMargin ("Spdf")
         dColPosFR[2] = 160
         dColPosFR[3] = 250
         dColPosFR[4] = 315
         dColPosFR[5] = 455
         dColPosFR[6] = pdf_PageWidth ("Spdf") - pdf_LeftMargin ("Spdf").
  ASSIGN dULstartFR[1] = 0
         dULstartFR[2] = 110
         dULstartFR[3] = 170
         dULstartFR[4] = 0
         dULstartFR[5] = 410
         dULstartFR[6] = 470.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-tblHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tblHeader Procedure
PROCEDURE tblHeader:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
 DEFINE INPUT-OUTPUT PARAMETER dY AS DECIMAL     NO-UNDO.
 
  ASSIGN cOverskr[1] = "Varer å plukke:"
         cOverskr[2] = "Varetekst"
         cOverskr[3] = "Artikkelnr"
         cOverskr[4] = "Fargekode"
         cOverskr[5] = "Størrelse"
         cOverskr[6] = "Solgt"
         cOverskr[7] = "Lager"
         clabel      = "Totalt".

  ASSIGN dColPos_S4[1] = pdf_LeftMargin ("Spdf") + 190
         dColPos_S4[2] = dColPos_S4[1] + 100
         dColPos_S4[3] = dColPos_S4[2] + 70
         dColPos_S4[4] = dColPos_S4[3] + 60
         dColPos_S4[5] = dColPos_S4[4] + 40
         dColPos_S4[6] = dColPos_S4[5] + 40
         .
         
  ASSIGN dULstart_S4[1] = pdf_LeftMargin ("Spdf") + 190
         dULstart_S4[2] = dColPos_S4[1] + 100
         dULstart_S4[3] = dColPos_S4[2] + 70
         dULstart_S4[4] = dColPos_S4[3] + 60
         dULstart_S4[5] = dColPos_S4[4] + 40
         dULstart_S4[6] = dColPos_S4[5] + 40
         .
         
  RUN ButikRubrik.
         
  /* RUBRIK */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[1],pdf_LeftMargin ("Spdf"),dY).
  dY = dY - iLineSpace.

  /* Kolonnrubrik */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[2],dULStart_S4[1] - bredd(cOverskr[2]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[3],dULStart_S4[2] - bredd(cOverskr[3]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[4],dULStart_S4[3] - bredd(cOverskr[4]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[5],dULStart_S4[4] - bredd(REPLACE(cOverskr[5],'ø','o')),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[6],dULStart_S4[5] - bredd(cOverskr[6]),dY).
  RUN pdf_text_xy_dec ("Spdf",cOverskr[7],dULStart_S4[6] - bredd(cOverskr[7]),dY).
  dY = dY - 4.
  
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S4[1], dY, pdf_LeftMargin ("Spdf"), dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S4[2], dY, dULstart_S4[1] + 10, dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S4[3], dY, dULstart_S4[2] + 10, dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S4[4], dY, dULstart_S4[3] + 10, dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S4[5], dY, dULstart_S4[4] + 10, dY, 0.5).
  RUN pdf_line IN h_PDFinc  ("Spdf", dULstart_S4[6], dY, dULstart_S4[5] + 10, dY, 0.5).

END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-bredd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION bredd Procedure 
FUNCTION bredd RETURNS DECIMAL
  ( INPUT cText AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN pdf_text_widthdec ("Spdf",cText).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
