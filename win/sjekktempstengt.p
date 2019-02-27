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

DEFINE INPUT  PARAMETER ipButikkNr LIKE Butiker.Butik NO-UNDO.
DEFINE INPUT  PARAMETER ipDato     AS DATE       NO-UNDO.
DEFINE OUTPUT PARAMETER opFra      AS DATE       NO-UNDO.
DEFINE OUTPUT PARAMETER opTil      AS DATE       NO-UNDO.

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

RUN KontrollerStengt.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-KontrollerStengt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerStengt Procedure 
PROCEDURE KontrollerStengt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dFg31Dec       AS DATE       NO-UNDO.
  DEFINE VARIABLE iCount         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFirstDt       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLastDt        AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFgClosedDt    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cAapenVeckodag AS CHARACTER  NO-UNDO.
    FIND ApnSkjema WHERE ApnSkjema.ButikkNr = ipButikkNr AND
                         ApnSkjema.Ar       = YEAR(ipDato) NO-LOCK NO-ERROR.
    IF NOT AVAIL ApnSkjema THEN DO:
        ASSIGN opFra = ?
               opTil = ?.
        RETURN.
    END.
    ASSIGN cAapenVeckodag = IF ApnSkjema.Ukelengde = 7 THEN "1,2,3,4,5,6,7" ELSE
        IF ApnSkjema.Ukelengde = 6 THEN "2,3,4,5,6,7" ELSE "2,3,4,5,6".
    IF AVAIL ApnSkjema THEN DO:
        ASSIGN dFg31Dec = DATE(12,31,ApnSkjema.Ar - 1).
        DO iCount = ipDato - dFg31Dec TO NUM-ENTRIES(ApnSkjema.OpenClosed):
            IF CAN-DO(cAapenVeckodag,STRING(WEEKDAY(dFg31Dec + iCount))) THEN DO:
                IF ENTRY(iCount,OpenClosed) <> "0" AND iFirstDt = 0 THEN
                     NEXT.
                ELSE IF ENTRY(iCount,OpenClosed) = "1" AND iFirstDt <> 0 THEN DO:
                    ASSIGN iLastDt = iFgClosedDt.
                    LEAVE.
                END.
                ELSE IF ENTRY(iCount,OpenClosed) = "0" AND iFirstDt = 0 THEN 
                    ASSIGN iFirstDt = iCount.
                ELSE IF ENTRY(iCount,OpenClosed) = "0" THEN
                    ASSIGN iFgClosedDt = iCount.
                ELSE IF iFirstDt <> 0 THEN DO:
                    ASSIGN iLastDt = iFgClosedDt.
                    LEAVE.
                END.
            END.
            ELSE IF iFirstDt <> 0 AND ENTRY(iCount,OpenClosed) <> "0" THEN DO:
                ASSIGN iLastDt = iFgClosedDt.
                LEAVE.
            END.
        END.
    END.
    IF iFirstDt <> 0 AND iLastDt = 0 THEN
        ASSIGN iLastDt = iFgClosedDt.
    ASSIGN opFra = IF iFirstDt = 0 THEN ? ELSE dFg31Dec + iFirstDt
           opTil = IF iLastDt  = 0 THEN ? ELSE dFg31Dec + iLastDt.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

