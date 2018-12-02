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

DEFINE INPUT  PARAMETER ipButikkNr AS INTEGER    NO-UNDO.

DEFINE TEMP-TABLE TT_Kasse NO-UNDO LIKE Skotex.Kasse.

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
         HEIGHT             = 16.29
         WIDTH              = 122.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

RUN FixGruppe.
RUN FixNyaKassor.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-FixGruppe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixGruppe Procedure 
PROCEDURE FixGruppe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF NOT CAN-FIND(Skotex.Gruppe WHERE Skotex.Gruppe.butik = ipButikkNr AND Skotex.Gruppe.Gruppenr = 1) THEN DO:
        CREATE Skotex.Gruppe.
        ASSIGN Skotex.Gruppe.ButikkNr = ipButikkNr
               Skotex.Gruppe.GruppeNr = 1
               Skotex.Gruppe.Navn     = "Skotex.Gruppe 1".
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixNyaKassor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixNyaKassor Procedure 
PROCEDURE FixNyaKassor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iButikkNr AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cKasseNr  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE ii        AS INTEGER    NO-UNDO.
    
    DEFINE BUFFER bufKasse FOR Skotex.kasse.
    DEFINE BUFFER bKasse   FOR Skotex.Kasse.
    
    ASSIGN 
      cKasseNr = "1,2,3,4,99".
    
    /* InfoPOS 8.0 */
    FIND FIRST bufKasse WHERE bufKasse.ModellNr = 10 NO-LOCK NO-ERROR.
    IF NOT AVAIL bufKasse THEN
        RETURN.
    
    DO ii = 1 TO NUM-ENTRIES(cKasseNr):
        FIND Kasse WHERE Kasse.butikknr = ipButikkNr AND
                         Kasse.GruppeNr = 1          AND
                         Kasse.KasseNr  = INT(ENTRY(ii,cKasseNr)) NO-LOCK NO-ERROR.
        IF AVAIL Kasse THEN
            NEXT.
        CREATE TT_Kasse.
        BUFFER-COPY bufKasse EXCEPT butikknr TO TT_Kasse.
        ASSIGN TT_Kasse.Aktiv         = TRUE
               TT_Kasse.ButikkNr      = ipButikkNr
               TT_Kasse.KasseNr       = INT(ENTRY(ii,cKasseNr))
               TT_Kasse.Navn          = "Kasse " + STRING(TT_Kasse.KasseNr) + STRING(ipButikkNr)
               TT_Kasse.eljournal[2]  = IF TT_Kasse.ModellNr = 10 THEN STRING(ipButikkNr) ELSE TT_Kasse.eljournal[2]
               ENTRY(1,TT_Kasse.eljournalid,";") = STRING(ipButikkNr)
               ENTRY(2,TT_Kasse.eljournalid,";") = STRING(TT_Kasse.KasseNr).
    END.
    FOR EACH TT_Kasse:
        CREATE bKasse.
        BUFFER-COPY TT_Kasse TO bKasse NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            DELETE bKasse.
        ELSE 
            RELEASE bKasse.
        DELETE TT_Kasse.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

