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
  DEFINE INPUT  PARAMETER iButikknr AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER dDato     AS DATE    NO-UNDO.
  DEFINE INPUT  PARAMETER iRappType AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER lOK       AS LOGICAL NO-UNDO.
  DEFINE OUTPUT PARAMETER cMelding  AS CHAR    NO-UNDO.

  DEFINE VARIABLE lTranslate AS LOGICAL     NO-UNDO.

  DEFINE VARIABLE ii         AS INTEGER     NO-UNDO.


DEFINE VARIABLE cDummyfilnamn AS CHARACTER   NO-UNDO.

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

FIND bruker WHERE bruker.brukerid = "kasse" NO-LOCK NO-ERROR.
/*             dDato = DATE(6,16,2010).  */
/*             iButikkNr = 1.            */

IF bruker.lng = "SE" OR bruker.lng = "SVE" THEN
    ASSIGN lTranslate = TRUE.
    
IF NOT CAN-FIND(FIRST dags_rap WHERE dags_rap.butikk = iButikkNr AND dags_rap.dato = dDato) THEN DO:
    lOK = FALSE.
    cMelding = STRING(lTranslate,"Data saknas/Data mangler").
END.
ELSE DO:
    CASE iRappType:
        WHEN 1 OR WHEN 2 THEN DO:
            lOK = TRUE.
            RUN dagsrapp_utskrift.p (iRapptype,iButikkNr,dDato,dDato,TRUE,OUTPUT cDummyfilnamn) NO-ERROR.
        END.
        WHEN 3 THEN DO:
            RUN skrivbongrap.p (iButikkNr,dDato,TRUE,TRUE) NO-ERROR.
        END.
        OTHERWISE DO:
            lOK = FALSE.
            cMelding = STRING(lTranslate,"Rapport saknas/Rapport mangler").
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


