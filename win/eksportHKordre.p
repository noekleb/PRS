&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_ELogg NO-UNDO LIKE ELogg.



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

DEFINE OUTPUT PARAMETER ocRetur     AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  iAntEksport AS INTEGER    NO-UNDO.

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
   Temp-Tables and Buffers:
      TABLE: TT_ELogg T "?" NO-UNDO SkoTex ELogg
   END-TABLES.
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

RUN KopierElogg.
/* För att undvika krasch av pågående orderbekräftelse */
/* PAUSE 30 NO-MESSAGE. */

RUN Eksportera.

RUN SlettELogg. /* */

ocRetur = "OK," + String(iAntEksport).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Eksportera) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Eksportera Procedure 
PROCEDURE Eksportera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cKatalog AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cTekst   AS CHARACTER NO-UNDO.
  
    ASSIGN
    cKatalog = "C:\home\lindbak\sendes".
    
    /* Eksport katalog til kasse. */
    {syspara.i 1 1 51 cTekst}
    IF cTekst <> '' THEN 
      cKatalog = RIGHT-TRIM(cTekst,'\').
  
    DEFINE VARIABLE cDatoTime AS CHARACTER  NO-UNDO.
    cDatoTime = "_" + SUBSTR(STRING(YEAR(TODAY)),3) + STRING(MONTH(TODAY)) + STRING(DAY(TODAY)) + STRING(TIME).
    ELOGG:
    FOR EACH TT_ELogg:
        FIND ordre WHERE ordre.ordrenr = INT(TT_Elogg.Verdier) NO-LOCK NO-ERROR.
        IF AVAIL ordre THEN DO:
            IF ordre.EDato = TODAY AND TIME - ordre.etid < 120 THEN
                NEXT ELOGG.
            IF CAN-FIND(FIRST Besthode WHERE BestHode.Ordrenr = Ordre.OrdreNr) THEN DO:
                FIND Butiker WHERE Butiker.Butik = Ordre.CL NO-LOCK NO-ERROR.
                IF AVAIL Butiker AND Butiker.harButikksystem = TRUE THEN DO:
                    RUN HK_Ordre_to_Butikk.p (INT(TT_Elogg.Verdier),cKatalog,"ORDHK" + TRIM(TT_ELogg.Verdier) + cDatoTime).
                    IF RETURN-VALUE = "OK" THEN DO:
                        ASSIGN TT_ELogg.EndringsType = 2
                               iAntEksport = iAntEksport + 1.
                    END.
                END.
                ELSE
                    ASSIGN TT_ELogg.EndringsType = 2. /* fel butikkoppling -- kanske borde loggas */
            END.
            ELSE
                ASSIGN TT_ELogg.EndringsType = 2. /* inge bestilling på ordren -- kanske borde loggas */
        END.
        ELSE
            ASSIGN TT_ELogg.EndringsType = 2. /* elogg där inte ordren finns  */
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KopierElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierElogg Procedure 
PROCEDURE KopierElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bElogg FOR Elogg.

    KOPIER:
    FOR EACH ELogg WHERE ELogg.TabellNavn = "ORDHK" AND
                         ELogg.EksterntSystem = "POS" NO-LOCK:

        FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL bElogg THEN DO:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            ASSIGN TT_ELogg.EndringsType = 1.
/*             bELogg.EndringsType = 2. */
        END.
        IF AVAILABLE TT_Elogg THEN
            RELEASE TT_ELogg.
    END. /* KOPIER */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettELogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettELogg Procedure 
PROCEDURE SlettELogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_Elogg WHERE TT_ELogg.EndringsType   = 2:
        FIND ELogg WHERE ELogg.TabellNavn     = TT_ELogg.TabellNavn AND
                         ELogg.EksterntSystem = TT_ELogg.EksterntSystem AND
                         ELogg.Verdier        = TT_ELogg.Verdier
/*                       AND ELogg.EndringsType   = TT_ELogg.EndringsType  */
                      EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL ELogg THEN DO:
            DELETE ELogg.
            DELETE TT_ELogg.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

