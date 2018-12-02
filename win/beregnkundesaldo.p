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
DEF INPUT PARAMETER lKundeNr  AS DEC NO-UNDO.
DEF INPUT PARAMETER iButikkNr AS INT NO-UNDO.

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

DEF VAR pdForsteDato AS DATE NO-UNDO.
DEF VAR piForsteTid  AS INT  NO-UNDO.
DEF VAR pdDatoSiste  AS DATE NO-UNDO.
DEF VAR piSisteTid   AS INT  NO-UNDO.
DEF VAR plSaldo      AS DEC  NO-UNDO.
DEF VAR plTotKjop    AS DEC  NO-UNDO.
DEF VAR plBetaling   AS DEC  NO-UNDO.
DEF VAR plPris       AS DEC  NO-UNDO.
DEFINE VARIABLE ocReturn    AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk        AS LOG       NO-UNDO.
DEFINE VARIABLE cKundeNrLst AS CHARACTER NO-UNDO.

DEFINE BUFFER bKunde FOR Kunde.

DO TRANSACTION:
  FIND Kunde EXCLUSIVE-LOCK WHERE
    Kunde.KundeNr = lKundeNr NO-ERROR.
  IF NOT AVAILABLE Kunde THEN
    RETURN "AVBRYT".

  ASSIGN
    cKundeNrLst = STRING(Kunde.KundeNr).
  RUN beregn_kunde_saldo.p ("idlist|" + TRIM(cKundeNrLst,","),?,'',OUTPUT ocReturn,OUTPUT obOk).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


