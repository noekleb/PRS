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
DEFINE INPUT  PARAMETER cQuery   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER gcTabell AS CHAR      NO-UNDO.

DEF VAR hBuffer AS HANDLE NO-UNDO.
DEF VAR hQuery  AS HANDLE NO-UNDO.

DEF VAR hTTArt  AS HANDLE NO-UNDO.
DEFINE VARIABLE hSourceproc AS HANDLE      NO-UNDO.
DEFINE VARIABLE cEloggtyp AS CHARACTER   NO-UNDO.
{tmp2artbasdef.i}

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
hSourceproc = SOURCE-PROCEDURE.
IF CAN-DO(hSourceproc:INTERNAL-ENTRIES,"getEloggtyp") THEN
    cEloggtyp = DYNAMIC-FUNCTION('getEloggtyp' IN hSourceproc) .
IF cEloggtyp = "" THEN
    RUN ArtbasToELogg.
ELSE IF cEloggtyp = "WEBBUT" THEN
    RUN ArtinfoToWeb.
ELSE IF cEloggtyp = "WEBBUTARTINFO" THEN
    RUN ArtinfoToWeb.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ArtbasToElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArtbasToElogg Procedure 
PROCEDURE ArtbasToElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hBufferField AS HANDLE     NO-UNDO.
  DEFINE VARIABLE dArtikkelNr  AS DECIMAL  NO-UNDO.
  DEFINE VARIABLE dModellFarge AS DECIMAL    NO-UNDO.
  
  CREATE QUERY  hQuery.
  CREATE BUFFER hBuffer FOR TABLE gcTabell.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE(cQuery).
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:

      hBufferField = hBuffer:BUFFER-FIELD("ArtikkelNr").
      dArtikkelNr  = hBufferField:BUFFER-VALUE().
      hBufferField = hBuffer:BUFFER-FIELD("ModellFarge").
      dModellFarge = hBufferField:BUFFER-VALUE().

      RUN CreateELogg (dArtikkelNr,dModellFarge).
      hQuery:GET-NEXT().
  END.
  hQuery:QUERY-CLOSE().
  hBuffer:BUFFER-RELEASE().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ArtinfoToWeb) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArtinfoToWeb Procedure 
PROCEDURE ArtinfoToWeb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hBufferField AS HANDLE     NO-UNDO.
  DEFINE VARIABLE dArtikkelNr  AS DECIMAL  NO-UNDO.
  DEFINE VARIABLE dModellFarge AS DECIMAL    NO-UNDO.
  CREATE QUERY  hQuery.
  CREATE BUFFER hBuffer FOR TABLE gcTabell.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE(cQuery).
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:

      hBufferField = hBuffer:BUFFER-FIELD("ArtikkelNr").
      dArtikkelNr  = hBufferField:BUFFER-VALUE().
      FIND artbas WHERE artbas.artikkelnr = dArtikkelnr NO-LOCK NO-ERROR.
      IF AVAIL artbas AND ArtBas.WebButikkArtikkel THEN DO:
          IF NOT CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "ArtBas"        AND
                                      ELogg.EksterntSystem = cEloggtyp AND
                                      ELogg.Verdier        = STRING(ArtBas.ArtikkelNr)) THEN DO:
              CREATE ELogg.
              ASSIGN ELogg.TabellNavn     = "ArtBas"
                     ELogg.EksterntSystem = cEloggtyp   
                     ELogg.Verdier        = STRING(ArtBas.ArtikkelNr)
                     ELogg.EndringsType = 1 
                     ELogg.Behandlet    = FALSE NO-ERROR.
              IF ERROR-STATUS:ERROR THEN
                  DELETE Elogg.
              ELSE
                  RELEASE ELogg.
          END.
      END.
      IF cEloggtyp = "WEBBUT" THEN DO:
          IF NOT CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "Lager"  AND
                                      ELogg.EksterntSystem = cEloggtyp AND
                                      ELogg.Verdier        = STRING(ArtBas.ArtikkelNr)) THEN DO:
              CREATE ELogg.
              ASSIGN ELogg.TabellNavn     = "Lager"
                     ELogg.EksterntSystem = cEloggtyp   
                     ELogg.Verdier        = STRING(ArtBas.ArtikkelNr)
                     ELogg.EndringsType = 1 
                     ELogg.Behandlet    = FALSE NO-ERROR.
              IF ERROR-STATUS:ERROR THEN
                  DELETE Elogg.
              ELSE
                  RELEASE ELogg.
          END.
      END.
      hQuery:GET-NEXT().
  END.
  hQuery:QUERY-CLOSE().
  hBuffer:BUFFER-RELEASE().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CreateELogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateELogg Procedure 
PROCEDURE CreateELogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER dArtikkelNr  AS DECIMAL    NO-UNDO.
    DEFINE INPUT  PARAMETER dModellFarge AS DECIMAL    NO-UNDO.
    DEFINE        VARIABLE  cArtikkelNr  AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE iCount        AS INTEGER    NO-UNDO.

    DEFINE BUFFER bArtbas FOR ArtBas.
    IF dModellFarge > 0 THEN DO:
        FOR EACH bArtBas WHERE bArtBas.ModellFarge = dModellFarge AND 
                               bArtBas.Aktivert    = TRUE AND 
                               bArtBas.IKasse      = TRUE NO-LOCK:
            ASSIGN cArtikkelNr = cArtikkelNr + (IF cArtikkelNr <> "" THEN "," ELSE "") + STRING(bArtBas.ArtikkelNr).
        END.
    END.
    ELSE DO:
        FIND bArtBas WHERE bArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
        IF AVAIL bArtBas AND bArtBas.Aktivert = TRUE AND bArtBas.IKasse = TRUE THEN
            ASSIGN cArtikkelNr = STRING(dArtikkelNr).
    END.
    DO iCount = 1 TO NUM-ENTRIES(cArtikkelNr):
        DO TRANSACTION:
            FIND ArtBas EXCLUSIVE-LOCK WHERE
                ArtBas.ArtikkelNr = DEC(ENTRY(iCount,cArtikkelNr)) NO-ERROR.
            IF NOT AVAILABLE ArtBas THEN NEXT.
            ELSE DO: 
                ArtBas.BongTekst = ' ' + ArtBas.BongTekst.
                IF AVAILABLE ArtBas THEN RELEASE ArtBas.
                FIND ArtBas EXCLUSIVE-LOCK WHERE
                    ArtBas.ArtikkelNr = DEC(ENTRY(iCount,cArtikkelNr)) NO-ERROR.
                ArtBas.BongTekst = TRIM(ArtBAs.BongTekst).
                IF AVAILABLE ArtBas THEN RELEASE ArtBas.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

