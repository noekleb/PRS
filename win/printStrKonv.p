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
/* DEFINE VAR ipBuntNr LIKE ovBunt.BuntNr  NO-UNDO. */

DEFINE INPUT  PARAMETER cRapportTyp AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER cRapLabels  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRubrik       AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE iColStrKode   AS INTEGER INIT 6  NO-UNDO.
DEFINE VARIABLE iColStorl     AS INTEGER INIT 12 NO-UNDO.
DEFINE VARIABLE iColIbruk     AS INTEGER INIT 18 NO-UNDO.
DEFINE VARIABLE iColMerknad   AS INTEGER INIT 23 NO-UNDO.
DEFINE VARIABLE iStrekrad     AS INTEGER INIT 5  NO-UNDO.
DEFINE VARIABLE iRad          AS INTEGER         NO-UNDO.
DEFINE VARIABLE cRubCol1      AS CHARACTER FORMAT "X(15)" NO-UNDO.
DEFINE VARIABLE cRubCol2      AS CHARACTER FORMAT "X(15)" NO-UNDO.
DEFINE VARIABLE cRubCol3      AS CHARACTER FORMAT "X(15)" NO-UNDO.
DEFINE VARIABLE cRubCol4      AS CHARACTER FORMAT "X(15)" NO-UNDO.
  DEFINE VARIABLE cFilNavn       AS CHARACTER   NO-UNDO.

  DEFINE VARIABLE iCols          AS INTEGER EXTENT 15  NO-UNDO.
  DEFINE VARIABLE cColLabels     AS CHARACTER EXTENT 15  NO-UNDO.


DEF STREAM Eksport.
DEFINE FRAME PageHeader
   HEADER
      "<ALIGN=BASE><FArial><R3><B><C6><P14>" TODAY "<C20><P24>" cRubrik
      "<P12></B><C75><P10>" PAGE-NUMBER FORMAT ">>" SKIP
      "<R" STRING(iStrekrad) "><C6><FROM><R" STRING(iStrekrad) "><C78><LINE>" SKIP
      "<R" STRING(iStrekrad) "><C" STRING(iColStrkode) "><B>" cRubCol1 "<C" + STRING(iColStorl) ">" cRubCol2 "<C"
                             STRING(iColiBruk - 1) ">" cRubCol3 "<C" STRING(iColMerknad - 1) ">" cRubCol4 "</B>" SKIP
      "<R" STRING(iStrekrad + 1) "><C6><FROM><R" STRING(iStrekrad + 1) "><C78><LINE>" SKIP
      WITH PAGE-TOP STREAM-IO WIDTH 255.

/* {xPrint.i} */
{runlib.i}
{ pdf_inc.i "NOT SUPER"}

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
/* RUN InitLabels för språkhantering */ 

IF cRapportTyp = "StrKonvXPrint" THEN DO:
    /*    RUN StrKonvXPrint. */
    {sww.i}
    RUN StrKonvPDFinclude.
    {swn.i}
    IF SEARCH(cFilnavn) <> ? THEN DO:
        RUN visPDF.w (cFilnavn,"Strkonv").
/*         OS-DELETE VALUE(SEARCH(cFilnavn)). */
    END.
END.
ELSE IF cRapportTyp = "StrKonvExcel" THEN DO:
    {sww.i}
        RUN StrKonvExcel.
    {swn.i}
END.
RETURN RETURN-VALUE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ColLabels) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ColLabels Procedure 
PROCEDURE ColLabels :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
/*   RUN pdf_text_at IN h_PDFinc ("Spdf",FILL("_",85),iCols[1]). */
  RUN pdf_skip    IN h_PDFinc ("Spdf").
  RUN pdf_text_at IN h_PDFinc ("Spdf",cColLabels[1],iCols[1]).
  RUN pdf_text_at IN h_PDFinc ("Spdf",cColLabels[2],iCols[2]).
  RUN pdf_text_at IN h_PDFinc ("Spdf",cColLabels[3],iCols[3]).
  RUN pdf_text_at IN h_PDFinc ("Spdf",cColLabels[4],iCols[4]).
  RUN pdf_text_at IN h_PDFinc ("Spdf",FILL("_",95),iCols[1]).
  
/*   RUN pdf_line IN h_PDFinc  ("Spdf", iX, 720, pdf_PageWidth("Spdf") - 61 , 720, 1). */
  RUN pdf_skip    IN h_PDFinc ("Spdf").
  RUN pdf_skip    IN h_PDFinc ("Spdf").
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).

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

    /* Collabels */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",18).
  RUN pdf_text_at IN h_PDFinc ("Spdf",STRING(TODAY),6).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",20).
  RUN pdf_text_at IN h_PDFinc ("Spdf",cRubrik,20).
  RUN pdf_text_to IN h_PDFinc ("Spdf",pdf_Page ("Spdf"),42).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).

  RUN pdf_skip IN h_PDFinc ("Spdf").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StrKonvExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StrKonvExcel Procedure 
PROCEDURE StrKonvExcel :
def var wtmpFileName as char no-undo.
  def var lAck         as log INIT TRUE no-undo.
  DEF VAR wCount       AS INTE NO-UNDO.
  def var wExcEkstent  as char no-undo.
  
  {syspara.i 1 4 1 wExcEkstent}
  ASSIGN wExcEkstent = IF wExcEkstent = "" THEN "sdv" ELSE wExcEkstent.    
  /* Henter temporært filnavn. */
  IF VALID-HANDLE(wLibHandle) then
    RUN GetTempFileName IN wLibHandle (INPUT "StrKonv", INPUT wExcEkstent, OUTPUT wtmpFileName).
  OUTPUT STREAM Eksport TO VALUE(wtmpFileName).
  {sww.i}
 FOR EACH StrKonv NO-LOCK:
/*    EXPORT STREAM Eksport DELIMITER ";"                                           */
/*    "Dagsrapport "                                                                */
/*    STRING(FI-FraDat,"99/99/99") + IF lAck = TRUE AND FI-FraDat <> FI-TilDat THEN */
/*                        " - " + STRING(FI-TilDat,"99/99/99") ELSE "".             */
    EXPORT STREAM Eksport DELIMITER ";"
        StrKonv.StrKode
        StrKonv.Storl
        StrKonv.Merknad.
  END.
  {swn.i}
  OUTPUT STREAM Eksport CLOSE.
  IF VALID-HANDLE(wLibHandle) THEN
    RUN OpenExcelDocument IN wLibHandle (wtmpFileName, " ").
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StrKonvPDFinclude) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StrKonvPDFinclude Procedure 
PROCEDURE StrKonvPDFinclude :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ii              AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cOrdrenr        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iAntall         AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iColLabelPage   AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iSumAntall      AS INTEGER     NO-UNDO.
  DEFINE VARIABLE dSumVk          AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dSumPris        AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dSumReklamtotal AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE iOldPage        AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iToRight       AS INTEGER EXTENT 15  NO-UNDO.
  DEFINE VARIABLE iPageHeight    AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iPageWidth     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iLeftMargin    AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iRMarginPos    AS INTEGER     NO-UNDO.

  ASSIGN cRubrik     = ENTRY(1,cRapLabels)
         iRad        = 6
         cRubCol1    = TRIM(REPLACE(ENTRY(2,cRapLabels),"*",""))
         cRubCol2    = TRIM(REPLACE(ENTRY(3,cRapLabels),"*",""))
         cRubCol3    = TRIM(REPLACE(ENTRY(4,cRapLabels),"*",""))
         cRubCol4    = TRIM(REPLACE(ENTRY(5,cRapLabels),"*",""))
         iColStorl   = iColStrKode + MAX(LENGTH(TRIM(cRubCol1)),3)
         iColIbruk   = iColStorl   + MAX(LENGTH(TRIM(cRubCol2)),4)
         iColMerknad = iColIbruk   + MAX(LENGTH(TRIM(cRubCol3)),1)
         .


  ASSIGN iCols[1] = 10 
         iCols[2] = 30 
         iCols[3] = 50 
         iCols[4] = 62.

  ASSIGN cColLabels[1]  = TRIM(REPLACE(ENTRY(2,cRapLabels),"*",""))
         cColLabels[2]  = TRIM(REPLACE(ENTRY(3,cRapLabels),"*",""))
         cColLabels[3]  = TRIM(REPLACE(ENTRY(4,cRapLabels),"*",""))
         cColLabels[4]  = TRIM(REPLACE(ENTRY(5,cRapLabels),"*","")).
  ASSIGN iToRight[1]  = 10
         .

  cFilNavn = SESSION:TEMP-DIR + "Strkonv" + "_" + STRING(TIME) + ".pdf".
  RUN pdf_new IN h_PDFinc ("Spdf",cFilNavn).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
/*   MESSAGE pdf_text_width ("Spdf",cColLabels[1])  */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.         */
/*   pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PageFooter"). */
  RUN pdf_set_BottomMargin IN h_PDFinc ("Spdf", 60).
  RUN pdf_set_PaperType IN h_PDFinc ("Spdf","A4").
    /*   RUN LoadFonts. */
  RUN pdf_set_Orientation IN h_PDFinc ("Spdf","Portrait").

  iPageHeight = pdf_PageHeight ("Spdf").
  iPageWidth  = pdf_PageWidth ("Spdf").

  RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
/*   RUN new_page. */
RUN pdf_new_page IN h_PDFinc ("Spdf").
  iColLabelPage = 1.
  ASSIGN iLeftMargin = pdf_LeftMargin ("Spdf")
         iRMarginPos =  pdf_PageWidth ("Spdf") - iLeftMargin.
iColLabelPage = 1.
FOR EACH strkonv NO-LOCK:
/*       RUN ColLabels. */
  IF pdf_Page ("Spdf") > iOldPage THEN DO:
/*       iColLabelPage = iColLabelPage + 1. */
      RUN PageHeader.
      RUN ColLabels.
      iOldPage = pdf_Page ("Spdf").
  END.

/*   RUN pdf_text_at IN h_PDFinc ("Spdf",STRING(StrKonv.StrKode,"z999"),iCols[1]). */
  RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(StrKonv.StrKode,"z999"),iToRight[1]).
  RUN pdf_text_at IN h_PDFinc ("Spdf",TRIM(StrKonv.Storl),iCols[2]).
  RUN pdf_text_at IN h_PDFinc ("Spdf", STRING(CAN-FIND(FIRST StrTStr 
                           WHERE StrTStr.SoStorl = StrKonv.Storl USE-INDEX Storl),"J/N"),iCols[3]).
  RUN pdf_text_at IN h_PDFinc ("Spdf",StrKonv.Merknad,iCols[4]).

/*         RUN pdf_text_at IN h_PDFinc ("Spdf",ABS(Reklamasjonslinje.antall),iCols[7]). */
  RUN pdf_skip    IN h_PDFinc ("Spdf").
END.

RUN pdf_close IN h_PDFinc ("Spdf").
/*  RUN SendEmail IN THIS-PROCEDURE. */
/*  RUN browse2pdf\viewxmldialog.w (cFilNavn,"WEBORDRE"). */
  /* Sender filen til visning og utskrift. */
/*   RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). */
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StrKonvXPrint) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StrKonvXPrint Procedure 
PROCEDURE StrKonvXPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcRappFil      AS CHAR NO-UNDO.

  DEFINE BUFFER bufovBuffer FOR ovBuffer.

  ASSIGN cRubrik     = ENTRY(1,cRapLabels)
         iRad        = 6
         cRubCol1    = ENTRY(2,cRapLabels)
         cRubCol2    = ENTRY(3,cRapLabels)
         cRubCol3    = ENTRY(4,cRapLabels)
         cRubCol4    = ENTRY(5,cRapLabels)
         iColStorl   = iColStrKode + MAX(LENGTH(TRIM(cRubCol1)),3)
         iColIbruk   = iColStorl   + MAX(LENGTH(TRIM(cRubCol2)),4)
         iColMerknad = iColIbruk   + MAX(LENGTH(TRIM(cRubCol3)),1)
         .
IF VALID-HANDLE(wLibHandle) THEN
     RUN GetTempFileName IN wLibHandle ("StrKonv", "xpr", OUTPUT pcRappFil). 
  /* Åpner stream til skriverfil. */
  OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(65).


  VIEW FRAME PageHeader.
  PUT CONTROL '<PREVIEW=ZoomToWidth><PrinterSetup>'.
  FOR EACH StrKonv NO-LOCK:
      PUT UNFORMATTED "<FCourier NEW><B><R" STRING(iRad) 
         "><C" STRING(iColStrKode) ">" STRING(StrKonv.StrKode,"z999")
          "<C" STRING(iColStorl)   ">" StrKonv.Storl 
          "<C" STRING(iColIbruk)   ">" STRING(CAN-FIND(FIRST StrTStr 
                                  WHERE StrTStr.SoStorl = StrKonv.Storl USE-INDEX Storl),"J/N")
          "<C" STRING(iColMerknad) ">" StrKonv.Merknad SKIP.
      IF LINE-COUNTER = PAGE-SIZE THEN DO:
          PAGE.
          ASSIGN iRad = 6.
      END.
      ELSE
          ASSIGN iRad = iRad + 1.
  END.
  OUTPUT CLOSE.
  OUTPUT TO TERMINAL.

  /* Klargjør rapportfilnavnet */
  ASSIGN FILE-INFO:File-NAME = pcRappFil.
    
  /* Sender filen til visning og utskrift. */
 RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

