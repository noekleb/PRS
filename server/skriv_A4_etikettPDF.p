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

DEF INPUT  PARAM icParam     AS CHAR  NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
/* DEF INPUT  PARAM icSessionId AS CHAR  NO-UNDO.  */
/* DEF OUTPUT PARAM ocReturn    AS CHAR  NO-UNDO.  */
/* DEF OUTPUT PARAM obOk        AS LOG NO-UNDO.    */

/* DEF VAR icParam     AS CHAR  NO-UNDO.  */
/* DEF VAR ihBuffer    AS HANDLE NO-UNDO. */
DEF VAR icSessionId AS CHAR  NO-UNDO.
DEF VAR ocReturn    AS CHAR  NO-UNDO.
DEF VAR obOk        AS LOG NO-UNDO. 


DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR fReklamasjonsNr AS INT    NO-UNDO.
DEF VAR iStatus         AS INT    NO-UNDO.

DEFINE VARIABLE cFilNavn       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCols          AS INTEGER EXTENT 3  NO-UNDO.
/* DEFINE VARIABLE cColLabels     AS CHARACTER EXTENT 15  NO-UNDO. */
/* DEFINE VARIABLE iToRight       AS INTEGER EXTENT 15  NO-UNDO.   */
DEFINE VARIABLE iLeftMargin    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iRMarginPos    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPageHeight    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPageWidth     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCL            AS INTEGER     NO-UNDO.
DEFINE VARIABLE cFirmanavn     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAdress        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPostadress AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE tt_Etikett NO-UNDO
    FIELD Rad1 AS CHAR
    FIELD Rad2 AS CHAR
    FIELD Rad3 AS CHAR
    FIELD Rad4 AS CHAR.


{ pdf_inc.i "THIS-PROCEDURE"}
/* { pdf_inc.i "NOT SUPER"} */

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

/* För utveckling */
/*     RUN ByggTT. */

/* IF CAN-FIND(FIRST tt_Etikett) THEN DO: */
    RUN RapportPDF.
    ASSIGN 
      ocReturn = cFilnavn
      obOk     = TRUE
    .
/* END. */
IF VALID-HANDLE(h_PDFinc) THEN DO:
        DELETE PROCEDURE h_PDFinc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTT Procedure 
PROCEDURE ByggTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
  DO ii = 1 TO 33:
      CREATE tt_etikett.
      ASSIGN tt_etikett.Rad1 = "Rad "  + STRING(ii,"99")
             tt_etikett.Rad2 = "Namn " + STRING(ii)
             tt_etikett.Rad3 = "Adr "  + STRING(ii)
             tt_etikett.Rad4 = "Padr " + STRING(ii)
             .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RapportPDF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportPDF Procedure 
PROCEDURE RapportPDF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
Font      Storlek Sidlayout Första TO_Vänsterjusterat Sista_TO Sista_AT
Helvetika      10 Landscape      6                    121      285
               11                6                    110      259
               12                5                    100      237
               10 Portrait       6                     82      192
               11                6                     75      174
               12                5                     68      160
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cOrdrenr AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iAntall AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iColLabelPage AS INTEGER     NO-UNDO.

  DEFINE VARIABLE iAntEti AS INTEGER     NO-UNDO.
  DEFINE VARIABLE i#Col   AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iNumEti AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iAktCol AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iIdx    AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iRowY    AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iRowYPos AS INTEGER EXTENT 8  NO-UNDO.

  DEF VAR hQuery          AS HANDLE NO-UNDO.
  
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(ihBuffer).
  hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
  hQuery:QUERY-OPEN().

  iAntEti = INT(icParam). /* alt 24 hämtas från icParam */

  ASSIGN i#Col = IF iAntEti = 16 THEN 2 ELSE 3.
  ASSIGN iCols[1] = 6
         iCols[2] = IF i#Col = 2 THEN 92 ELSE 60 
         iCols[3] = IF i#Col = 2 THEN   ? ELSE 120.
  ASSIGN iRowYPos[1] = 826
         iRowYPos[2] = 716
         iRowYPos[3] = 609
         iRowYPos[4] = 500
         iRowYPos[5] = 392
         iRowYPos[6] = 288
         iRowYPos[7] = 180
         iRowYPos[8] = 72.

  ASSIGN cFilNavn = SESSION:TEMP-DIR + "A4_etikett" + "_" + STRING(TIME) + ".pdf".
    /* skapa ett utlägg pr butik */
  RUN pdf_new ("Spdf",cFilNavn).
  /* för att pagefooter skall komma automatiskt */
  
  RUN pdf_set_BottomMargin ("Spdf", 10).
  RUN pdf_set_PaperType ("Spdf","A4").
    /*   RUN LoadFonts. */
  RUN pdf_set_Orientation ("Spdf","Portrait").
  RUN pdf_set_font ("Spdf","Helvetica",12).

  RUN pdf_set_VerticalSpace ("Spdf",15).
  RUN pdf_new_page ("Spdf").
/*   RUN new_page. */
  ASSIGN iLeftMargin = pdf_LeftMargin ("Spdf") 
         iRMarginPos =  pdf_PageWidth ("Spdf") - iLeftMargin.
iRowY = 1.
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
/* FOR EACH tt_etikett BY tt_etikett.Rad1: */
  iNumEti = iNumEti + 1.
  iIdx = iNumEti MOD i#Col.
  iIdx = IF iIdx = 0 THEN i#Col ELSE iIdx.
  RUN pdf_set_TextY("Spdf",iRowYPos[iRowY]).
  RUN pdf_text_at ("Spdf",ihBuffer:BUFFER-FIELD('Rad1'):BUFFER-VALUE,iCols[iIdx]).
  RUN pdf_skip    ("Spdf").
  RUN pdf_text_at ("Spdf",ihBuffer:BUFFER-FIELD('Rad2'):BUFFER-VALUE,iCols[iIdx]).
  RUN pdf_skip    ("Spdf").
  RUN pdf_text_at ("Spdf",ihBuffer:BUFFER-FIELD('Rad3'):BUFFER-VALUE,iCols[iIdx]).
  RUN pdf_skip    ("Spdf").
  RUN pdf_text_at ("Spdf",ihBuffer:BUFFER-FIELD('Rad4'):BUFFER-VALUE,iCols[iIdx]).
  RUN pdf_skip    ("Spdf").
  IF iIdx = i#Col THEN DO:
      iRowY = iRowY + 1.
      IF iRowY MOD 8 = 1 THEN
          iRowY = 1.
  END.
  IF iNumEti MOD iAntEti = 0 THEN
      RUN pdf_new_page ("Spdf").
  hQuery:GET-NEXT().
END.
hQuery:QUERY-CLOSE().
DELETE OBJECT hQuery.
 RUN pdf_close ("Spdf").
/*  RUN SendEmail IN THIS-PROCEDURE. */
 RUN browse2pdf\viewxmldialog.w (cFilNavn,"PDF Template").
  /* Sender filen til visning og utskrift. */
/*   RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). */
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

