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

DEFINE INPUT  PARAMETER lFinansPro       AS LOGICAL    NO-UNDO.
DEFINE INPUT  PARAMETER lFinansPreem     AS LOGICAL    NO-UNDO.
DEFINE INPUT  PARAMETER cExportDir       AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER lBehandlet       AS LOGICAL    NO-UNDO.

DEFINE TEMP-TABLE TT_Datasett NO-UNDO
    FIELD ButikkNr AS INTE
    FIELD Dato     AS DATE
    FIELD Status2  AS LOGICAL
    INDEX datobut Dato ButikkNr.

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

RUN FinnDatasett.

RUN BehandlaTT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BehandlaTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BehandlaTT Procedure 
PROCEDURE BehandlaTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF CAN-FIND(FIRST TT_DataSett) THEN
        ASSIGN lBehandlet = TRUE.
    FOR EACH TT_Datasett:
/*         IF TT_DataSett.Status2 = TRUE THEN */
/*             NEXT.                          */
        FOR EACH Datasett WHERE Datasett.ButikkNr = TT_DataSett.ButikkNr AND
                                Datasett.Dato     = TT_Datasett.Dato BREAK BY Datasett.Dato:
            IF FIRST-OF(Datasett.dato) THEN
                RUN oppdaterPro.p (TT_DataSett.ButikkNr,TT_Datasett.Dato,lFinansPro,lFinansPreem,cExportDir).
            Datasett.pfFlagg = 3.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FinnDatasett) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FinnDatasett Procedure 
PROCEDURE FinnDatasett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH Datasett WHERE Datasett.pfFlagg = 1 NO-LOCK.
       IF Datasett.SettStatus > 1 THEN DO:
           FIND TT_Datasett WHERE TT_Datasett.ButikkNr = Datasett.ButikkNr AND
                                  TT_DataSett.Dato     = Datasett.dato NO-ERROR.

           IF NOT AVAIL TT_Datasett THEN DO:
               CREATE TT_Datasett.
               ASSIGN TT_Datasett.ButikkNr = Datasett.ButikkNr
                      TT_DataSett.Dato     = Datasett.dato.
           END.
           /* Vi hoppar över denna dagen om status2 = true */
/*            IF TT_Datasett.Status2 = TRUE THEN     */
/*                NEXT.                              */
/*            IF Datasett.SettStatus = 2 THEN        */
/*                ASSIGN TT_Datasett.Status2 = TRUE. */
       END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

