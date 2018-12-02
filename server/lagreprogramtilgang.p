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
DEF INPUT PARAMETER  cParameter        AS CHAR   NO-UNDO. /* Input parametre til programmet.           */
DEF INPUT PARAMETER  htmpHandle        AS HANDLE NO-UNDO. /* Handle til temptable - hvis det benyttes. */
DEF INPUT  PARAMETER icSessionId       AS CHAR   NO-UNDO.
DEF OUTPUT PARAMETER cReturn           AS CHAR   NO-UNDO. /* Retur veri til kallende program.          */
DEF OUTPUT PARAMETER lOk               AS LOG    NO-UNDO. /* Vellykket utføring.                       */

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

IF NUM-ENTRIES(cParameter,"|") <> 2 THEN
DO:
    ASSIGN
        lOk     = FALSE
        cReturn = "Feil antall entries i variabel send til lagreprogramtilgang.p."
        .
    RETURN.
END.
ELSE
    RUN lagreprogramtilgang.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-lagreprogramtilgang) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lagreprogramtilgang Procedure 
PROCEDURE lagreprogramtilgang :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piBrGrpNr         AS INT  NO-UNDO.
  DEF VAR cProgramRowIdList AS CHAR NO-UNDO.
  DEF VAR piLoop            AS INT  NO-UNDO.

  ASSIGN
      piBrGrpNr         = int(ENTRY(1,cParameter,"|"))
      cProgramRowIdList = ENTRY(2,cParameter,"|")
      .

  KOBLING:
  DO TRANSACTION:
      FOR EACH ProgBrGrp EXCLUSIVE-LOCK WHERE
          ProgBrGrp.BrGrpNr = piBrGrpNr:
          DELETE ProgBrGrp.
      END.
      DO piLoop = 1 TO NUM-ENTRIES(cProgramRowIdList):
          FIND ProgramListe NO-LOCK WHERE
              ROWID(ProgramListe) = TO-ROWID(ENTRY(piLoop,cProgramRowIdList)) NO-ERROR.
          IF AVAILABLE ProgramListe THEN
          DO:
              CREATE ProgBrGrp.
              ASSIGN
                  ProgBrGrp.BrGrpNr  = piBrGrpNr
                  ProgBrGrp.ProgNavn = ProgramListe.ProgNavn
                  .
          END.
      END.
      lOk = TRUE.
  END. /* KOBLING */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

