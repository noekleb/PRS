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

DEF INPUT PARAMETER wProgram-Handle AS HANDLE NO-UNDO.

DEF VAR wTotAntall AS DEC FORMAT "zzz,zzz,zz9" NO-UNDO.
DEF VAR wWork            AS DEC  NO-UNDO.
DEF VAR wWork2           AS DEC  NO-UNDO.
DEF VAR wWork3           AS DEC  NO-UNDO.
DEF VAR wStop            AS LOG  INITIAL FALSE NO-UNDO.
DEF VAR wDato            AS DATE NO-UNDO.
DEF VAR wTid             AS INT  NO-UNDO.
DEF VAR wSkjerm          AS CHAR NO-UNDO.
DEF VAR wTilbud          AS LOG  NO-UNDO.
DEF VAR wOk              AS INT  NO-UNDO.
DEF VAR bEtiTvang        AS LOG  NO-UNDO.
DEF VAR cTekst           AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */
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

{runlib.i}

/* Sjekker om det er tvang på etikett. */
{syspara.i 2 4 41 cTekst}
IF CAN-DO('1,J,Ja,Y,YES,True',cTekst) THEN 
  bEtiTvang = TRUE.
ELSE
  bEtiTvang = FALSE.

/* Klargjørings dato og tid. */
ASSIGN
  wDato = TODAY
  wTid  = TIME.

/* Klargjør priskøen. */
RUN KlargjorPrisKo.

/* Sletter procedyrebibloteket hvis det kjøres i batch modus. */
IF TERMINAL = "" THEN
  DO:
    IF VALID-HANDLE(wLibHandle) THEN
      DELETE PROCEDURE wLibHandle NO-ERROR.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-KlargjorPrisKo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KlargjorPrisKo Procedure 
PROCEDURE KlargjorPrisKo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wOppdatertAntall AS INT    NO-UNDO.
  DEF VAR wStartTid        AS INT    NO-UNDO.
  DEF VAR wFerdigTid       AS INT    NO-UNDO.
  DEF VAR wBruktTid        AS INT    NO-UNDO.
  DEF VAR h_PrisKo         AS HANDLE NO-UNDO.

/*Er det tvang på etikett, skal ikke denne rutinen kjøres. */
/*
IF bEtiTvang THEN
    RETURN.
*/

DO WITH FRAME DEFAULT-FRAME:
  /* Startet info */
  ASSIGN
    wStartTid = TIME.
  IF VALID-HANDLE(wProgram-Handle) THEN
    RUN StartInfo IN wProgram-Handle (INPUT TODAY, INPUT wStartTid).

  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN prisko.p PERSISTENT SET h_PrisKo.

  IF VALID-HANDLE(h_PrisKo) THEN
    RUN KlargjorPriskoAlle IN h_PrisKo.

  IF VALID-HANDLE(h_PrisKo) THEN
      DELETE PROCEDURE h_Prisko.

  /* Brukt info */
  ASSIGN
    wFerdigTid = TIME
    wBruktTid  = wFerdigTid - wStartTid.
  IF VALID-HANDLE(wProgram-Handle) THEN
    RUN BruktInfo IN wProgram-Handle (INPUT TODAY, INPUT wFerdigTid, INPUT wBruktTid).

END. /* FRAME */  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
/* ************************  Function Implementations ***************** */
