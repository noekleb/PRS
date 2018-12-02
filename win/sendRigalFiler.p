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

DEFINE INPUT  PARAMETER cExportDir        AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cRunBatFil        AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cButikkListe      AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cVareFiler        AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cLevBasFiler      AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cProdusentFiler   AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cGruppeFiler      AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cDummy        AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  iAntvarer     AS INTEGER    NO-UNDO.
DEFINE        VARIABLE  iAntLevBas    AS INTEGER    NO-UNDO.
DEFINE        VARIABLE  iAntProdusent AS INTEGER    NO-UNDO.
DEFINE        VARIABLE  iAntGruppe    AS INTEGER    NO-UNDO.
DEFINE        VARIABLE  iDummy        AS INTEGER    NO-UNDO.

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

/*  cExportDir  = "\home\lindbak\sendes\" */
IF cExportDir = "" THEN
    ASSIGN cExportDir = "\home\lindbak\sendes\".
ELSE
    ASSIGN cExportDir = RIGHT-TRIM(cExportDir,"\") + "\".

RUN initButikker.

RUN eksportRigalArtBas.p (cButikkListe,"",cExportDir,OUTPUT cVareFiler,
                          OUTPUT cDummy,OUTPUT iAntvarer,OUTPUT iDummy).
RUN eksportRigalGruppe.p (cButikkListe,"",cExportDir,OUTPUT cGruppeFiler,OUTPUT iAntGruppe).
RUN eksportRigalLevBas.p (cButikkListe,"",cExportDir,OUTPUT cLevBasFiler,OUTPUT iAntLevBas).
RUN eksportRigalProdusent.p (cButikkListe,"",cExportDir,OUTPUT cProdusentFiler,OUTPUT iAntProdusent).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-initButikker) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initButikker Procedure 
PROCEDURE initButikker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.
  FOR EACH Butiker NO-LOCK WHERE 
      CAN-FIND(FIRST Kasse WHERE kasse.butik = butiker.butik AND Kasse.GruppeNr = 1 AND kasse.aktiv = TRUE AND
                                       kasse.modellnr = 10):
      ASSIGN cButikkListe = cButikkListe + (IF cButikkListe = "" THEN "" ELSE ",") + 
          STRING(Butiker.Butik).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

