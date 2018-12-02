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

DEFINE INPUT  PARAMETER cFilekstent AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cEksportDir AS CHARACTER  NO-UNDO.

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

ASSIGN cEksportDir = IF cEksportdir = "" THEN "\home\lindbak\sendes\" ELSE RIGHT-TRIM(cEksportDir,"\") + "\".

RUN StartEksport.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-StartEksport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartEksport Procedure 
PROCEDURE StartEksport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cDummy  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFiler1 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFiler2 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFiler3 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iAnt1   AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAnt2   AS INTEGER    NO-UNDO.
DEFINE VARIABLE cOutputString AS CHARACTER  NO-UNDO.
/* 
DEFINE INPUT  PARAMETER cFilekstent AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cEksportDir AS CHARACTER  NO-UNDO. 
 */
  ASSIGN cOutputString = "Export " + STRING(TODAY) + " " + STRING(TIME,"HH:MM:SS") + CHR(10).
/*   IF CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn = "ArtBas" AND ELogg.EksterntSystem = "POS") THEN VARE: */
  DO:
      RUN eksportRigalArtBas.p (INPUT cFilekstent,INPUT cDummy,INPUT cEksportDir,OUTPUT cFiler1, OUTPUT cFiler2,OUTPUT iAnt1, OUTPUT iAnt2).
          /* cFiler2 = Mixfiler (iAnt2), för närvarande får vi inga mix från MD" */
          ASSIGN cOutputString = cOutputString + "Antal varor: " + STRING(iAnt1) + " Fil: " + cFiler1 + CHR(10).
  END.
/*   IF CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn = "VarGr" AND ELogg.EksterntSystem = "POS") THEN GRUPPE: */
  DO:
      RUN eksportRigalGruppe.p (INPUT cFilekstent,INPUT cDummy,INPUT cEksportDir, OUTPUT cFiler1, OUTPUT iAnt1).
      ASSIGN cOutputString = cOutputString + "Antal gruppe: " + STRING(iAnt1) + " Fil: " + cFiler1 + CHR(10).
  END.
/*   IF CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn = "LevBas" AND ELogg.EksterntSystem = "POS") THEN LEVBAS: */
  DO:
      RUN eksportRigalLevBas.p (INPUT cFilekstent,INPUT cDummy,INPUT cEksportDir, OUTPUT cFiler1, OUTPUT iAnt1).
      ASSIGN cOutputString = cOutputString + "Antal lev: " + STRING(iAnt1) + " Fil: " + cFiler1 + CHR(10).
  END.
/*   IF CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn = "Produsent" AND ELogg.EksterntSystem = "POS") THEN PRODUSENT: */
  DO:
    RUN eksportRigalProdusent.p (INPUT cFilekstent,INPUT cDummy,INPUT cEksportDir, OUTPUT cFiler1, OUTPUT iAnt1).
    ASSIGN cOutputString = cOutputString + "Antal produsent: " + STRING(iAnt1) + " Fil: " + cFiler1 + CHR(10).
  END.
  OUTPUT TO VALUE(cEksportDir + "ExportLog.txt").
  PUT UNFORMATTED cOutputString.
  OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

