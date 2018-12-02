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

DEFINE INPUT  PARAMETER cBildeFil AS CHARACTER FORMAT "x(20)" NO-UNDO.
/* DEFINE INPUT  PARAMETER cRapLabels  AS CHARACTER  NO-UNDO. */
DEFINE VARIABLE cKundenavn AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPolygon AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cHdrBildeFil AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEF STREAM Eksport.
DEFINE FRAME PageHeader
   HEADER
      "<OLANDSCAPE><ALIGN=BASE><FArial><R3><B><C5><P16>Skjermutskrift <P10>" cHdrBildeFil "<P16><C100>" TODAY
      "<P10></B>" SKIP
      "<R4><C5><FROM><R4><C110><LINE>" SKIP
      WITH PAGE-TOP STREAM-IO WIDTH 255.

{xPrint.i}
{runlib.i}

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
{sww.i}
/* RUN InitLabels för språkhantering */ 
  {syspara.i 1 1 100 cKundenavn}
  {syspara.i 1 1 101 cPolygon}
   ASSIGN cHdrBildeFil = "(" + TRIM(cBildeFil) + ")".
   RUN printScreenDump.
{swn.i}
RETURN RETURN-VALUE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-printScreenDump) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printScreenDump Procedure 
PROCEDURE printScreenDump :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcRappFil      AS CHAR NO-UNDO.

IF VALID-HANDLE(wLibHandle) THEN
     RUN GetTempFileName IN wLibHandle ("S_Dump", "xpr", OUTPUT pcRappFil). 
  /* Åpner stream til skriverfil. */
  OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(65).
  PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.


  VIEW FRAME PageHeader.
  PUT CONTROL '<PREVIEW=ZoomToWidth>'.
  PUT UNFORMATTED
  "<TRANSPARENT=false><R5><C5><#3><R43><C110><IMAGE#3=" cBildeFil  ">" SKIP
  "<R44><C5><FROM><R44><C110><LINE>" SKIP
  "<R45><C5>" cKundenavn "<C40>" cPolygon
  "<TRANSPARENT=false><C94><#4><R49><C119><IMAGE#4=icon\skotex_ligg.bmp>" SKIP.
                      /* <R45> */
  OUTPUT CLOSE.
  OUTPUT TO TERMINAL.

  /* Klargjør rapportfilnavnet */
  ASSIGN FILE-INFO:File-NAME = pcRappFil.
    
  /* Sender filen til visning og utskrift. */
/*  RUN PrintFile(FILE-INFO:FULL-PATHNAME). */
 RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

