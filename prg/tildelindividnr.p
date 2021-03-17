&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :  run tildelindividnr.p (<ButikkNr inn>,<IndividNr retur>).

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEF VAR iButikkNr AS INT NO-UNDO.
  ASSIGN
      iButikkNr = 1
      .
  DEF VAR fIndividNr AS DEC NO-UNDO.
  DEF VAR iSeqNr     AS INT NO-UNDO.
&ELSE
  DEF INPUT  PARAM iButikkNr  AS INT NO-UNDO.
  DEF OUTPUT PARAM fIndividNr AS DEC NO-UNDO.
  DEF OUTPUT PARAM iSeqNr     AS INT NO-UNDO.
&ENDIF

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

DEF VAR iTeller AS INT NO-UNDO.

iTeller = 1.
EVIGHETEN:
DO WHILE TRUE:
    FIND LAST Individ NO-LOCK WHERE 
        Individ.butnr = iButikkNr 
        USE-INDEX SeqNr  NO-ERROR.

    ASSIGN 
        iSeqNr     = (IF NOT AVAIL Individ 
                        THEN iTeller 
                        ELSE Individ.SeqNr + iTeller)
        fIndividNr = dec(STRING(iButikkNr) + STRING(iSeqNr))
        iTeller    = iTeller + 1
        .
    /* Ledig nummer - da er vi ferdige. */
    IF NOT CAN-FIND(Individ WHERE
                    Individ.IndividNr = fIndividNr) THEN
        LEAVE EVIGHETEN.

    /* Stopper LOOP ved feil.*/
    IF iTeller > 200 THEN
    DO:
        fIndividNr = ?.
        LEAVE EVIGHETEN.
    END.

END. /* EVIGHETEN */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


