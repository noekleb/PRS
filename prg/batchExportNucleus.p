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
DEFINE VARIABLE cOutputDir AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRetur1 AS CHAR       NO-UNDO.
DEFINE VARIABLE cRetur2 AS CHAR       NO-UNDO.
DEFINE VARIABLE iRetur3 AS INTEGER    NO-UNDO.
DEFINE VARIABLE iRetur4 AS INTEGER    NO-UNDO.

DEFINE TEMP-TABLE TT_N9Butiker NO-UNDO
    FIELD ButikkNr AS INTEGER.

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
RUN InitButiker.
IF NOT CAN-FIND(FIRST TT_N9Butiker) THEN
    RETURN.
ASSIGN cOutputDir = "d:\home\lindbak\sendes".
                     /* 1 = 1 -> detta är gjort för att ev i en framtid få in ett villkor */
RUN ArtBas2Nucleus.p (IF 1 = 1 THEN BUFFER TT_N9Butiker:HANDLE ELSE ?,INPUT "",INPUT cOutputDir,0,FALSE,'POS',OUTPUT cRetur1,OUTPUT cRetur2,OUTPUT iRetur3,OUTPUT iRetur4).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-InitButiker) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitButiker Procedure 
PROCEDURE InitButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     FOR EACH butiker WHERE CAN-FIND(FIRST Kasse WHERE kasse.butik = butiker.butik AND Kasse.GruppeNr = 1 AND kasse.aktiv = TRUE AND kasse.modell = 51): */
    FOR EACH butiker WHERE butiker.vpi = 1:
        CREATE TT_N9Butiker.
        ASSIGN TT_N9Butiker.ButikkNr = Butiker.Butik.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

