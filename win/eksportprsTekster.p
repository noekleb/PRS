&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
/* Connected Databases 
          data             PROGRESS
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
DEFINE INPUT  PARAMETER cFtpButiker   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cExportFil    AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cTeksterFiler AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAnttekster   AS INTEGER    NO-UNDO.
DEFINE VARIABLE  iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER INIT "c:\home\lindbak\kasse" NO-UNDO.
DEFINE VARIABLE cTekst          AS CHARACTER                              NO-UNDO.

DEFINE TEMP-TABLE TT_Tekster
    FIELD teksttype AS INTEGER FORMAT ">>>9"     /* I (4)      teksttype */
    FIELD tekstnr    AS INTEGER FORMAT ">>9"     /* I (4)      Tekstnr */
    FIELD transtype  AS INTEGER FORMAT "9"     /* I (4)        Tekstnr */
    FIELD tekst      AS CHARACTER FORMAT "x(15)"  /* C (15)    Tekst */
    FIELD aksjon     AS INTEGER FORMAT "9"        /* I (1)     Posttype, 1=ny/endring, 9=sletting)                     */
    INDEX Aksjon aksjon DESCENDING teksttype ASCENDING
    .

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
      TABLE: TT_ELogg T "?" NO-UNDO data ELogg
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
/* eksport katalog til kasse. */
{syspara.i 1 1 56 cTekst}
IF cTekst <> '' THEN 
  cEksportKatalog = RIGHT-TRIM(cTekst,'\') + '\'.
ASSIGN
  cExportFil = cEksportKatalog + cExportFil
  . 

RUN KopierElogg.
RUN FixTeksterEndringer.
/* Här skall vi loopa runt alla kassor mm */
IF cFtpButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
      RUN ExportTekster IN THIS-PROCEDURE (INT(ENTRY(iCount,cFtpButiker)),ENTRY(iCount,cFtpButiker)). /* parameter = den loopade butiken */
  END.
END.
                                       /* + eventuellt filnamn */
RUN SlettTT_ELoggTekster. /* */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExportTekster) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportTekster Procedure 
PROCEDURE ExportTekster :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iButik     LIKE Butiker.Butik NO-UNDO.
    DEFINE INPUT  PARAMETER cFilSuffix AS CHARACTER  NO-UNDO.
    
    DEFINE VARIABLE         iCount     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cString            AS CHARACTER  NO-UNDO.
    
    DEFINE VARIABLE  cNumericFormat AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cDateFormat    AS CHARACTER  NO-UNDO.
    
    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "EUROPEAN"
           SESSION:DATE-FORMAT    = "dmy".    
    
    ASSIGN iAntTekster = 0. /* tyvärr eftersom jag gör detta flera gånger */
    OUTPUT TO VALUE(cExportFil + cFilsuffix) APPEND.
    FOR EACH TT_Tekster:
         cString = "TEKSTER;1;" + 
                   STRING(TT_Tekster.aksjon) + ";" +
                   STRING(TT_Tekster.teksttype) + ";" + 
                   STRING(TT_Tekster.tekstnr) + ";" + 
                   STRING(TT_Tekster.transtype) + ";" + 
                   REPLACE(REPLACE(TT_Tekster.tekst,";",""),'"'," ")
                   NO-ERROR.
               .
        PUT UNFORMATTED cString SKIP.
        ASSIGN iAntTekster = iAntTekster + 1.
    END.
    OUTPUT CLOSE.
    
    FILE-INFO:FILE-NAME = cExportFil + cFilsuffix.
    IF FILE-INFO:FILE-SIZE = 0 THEN
        OS-DELETE VALUE(cExportFil + cFilsuffix).
    ELSE IF NOT CAN-DO(cTeksterFiler,cExportFil + cFilsuffix) THEN
        ASSIGN cTeksterFiler = cTeksterFiler + (IF cTeksterFiler = "" THEN "" ELSE ",") + cExportFil + cFilsuffix.
        
    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixFeilKodeEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixFeilKodeEndringer Procedure 
PROCEDURE FixFeilKodeEndringer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH TT_ELogg WHERE 
              TT_ELogg.TabellNavn     = "FeilKode" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 3:
          CREATE TT_Tekster.
          ASSIGN TT_Tekster.teksttype = 1000
                 TT_Tekster.tekstnr   = INT(TT_ELogg.Verdier)
                 TT_Tekster.aksjon    = 9.
  END.
  IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = "FeilKode" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 1  AND
              TT_Elogg.Verdier       = "ALLE") THEN DO:
      FOR EACH FeilKode NO-LOCK:
          CREATE TT_Tekster.
          ASSIGN TT_Tekster.teksttype = 1000
                 TT_Tekster.tekstnr   = FeilKode.FeilKode
                 TT_Tekster.transtype = IF Feilkode.TransKode = 0 THEN 3 ELSE 4
                 TT_Tekster.tekst     = SUBSTR(FeilKode.Beskrivelse,1,30)
                 TT_Tekster.aksjon    = 1.
      END.
  END.
  ELSE DO:
      FOR EACH TT_ELogg WHERE 
                  TT_ELogg.TabellNavn     = "FeilKode" AND
                  TT_ELogg.EksterntSystem = "POS"  AND
                  TT_ELogg.EndringsType   = 1:
          FIND FeilKode WHERE FeilKode.FeilKode = INT(TT_ELogg.Verdier) NO-LOCK NO-ERROR.
          IF AVAIL FeilKode THEN DO:
              CREATE TT_Tekster.
              ASSIGN TT_Tekster.teksttype = 1000
                     TT_Tekster.tekstnr   = FeilKode.FeilKode
                     TT_Tekster.transtype = IF Feilkode.TransKode = 0 THEN 3 ELSE 4
                     TT_Tekster.tekst     = SUBSTR(FeilKode.Beskrivelse,1,30)
                     TT_Tekster.aksjon    = 1.
          END.
      END.
  END.
  RELEASE TT_Tekster.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixGaveKTypeEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixGaveKTypeEndringer Procedure 
PROCEDURE FixGaveKTypeEndringer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH TT_ELogg WHERE 
              TT_ELogg.TabellNavn     = "GaveKType" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 3:
          CREATE TT_Tekster.
          ASSIGN TT_Tekster.teksttype = 13
                 TT_Tekster.tekstnr   = INT(TT_ELogg.Verdier)
                 TT_Tekster.aksjon    = 9.
  END.
  IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = "GaveKType" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 1  AND
              TT_Elogg.Verdier       = "ALLE") THEN DO:
      FOR EACH GaveKType NO-LOCK:
          CREATE TT_Tekster.
          ASSIGN TT_Tekster.teksttype = 13
                 TT_Tekster.tekstnr   = GaveKType.IdentType
                 TT_Tekster.transtype = 0
                 TT_Tekster.tekst     = SUBSTR(GaveKType.GKTBeskrivelse,1,30)
                 TT_Tekster.aksjon    = 1.
      END.
  END.
  ELSE DO:
      FOR EACH TT_ELogg WHERE 
                  TT_ELogg.TabellNavn     = "GaveKType" AND
                  TT_ELogg.EksterntSystem = "POS"  AND
                  TT_ELogg.EndringsType   = 1:
          FIND GaveKType WHERE GaveKType.IdentType = INT(TT_ELogg.Verdier) NO-LOCK NO-ERROR.
          IF AVAIL GaveKType THEN DO:
              CREATE TT_Tekster.
              ASSIGN TT_Tekster.teksttype = 13
                     TT_Tekster.tekstnr   = GaveKType.IdentType
                     TT_Tekster.transtype = 0
                     TT_Tekster.tekst     = SUBSTR(GaveKType.GKTBeskrivelse,1,30)
                     TT_Tekster.aksjon    = 1.
          END.
      END.
  END.
  RELEASE TT_Tekster.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixInnBetTypeEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixInnBetTypeEndringer Procedure 
PROCEDURE FixInnBetTypeEndringer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH TT_ELogg WHERE 
              TT_ELogg.TabellNavn     = "InnBetType" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 3:
          CREATE TT_Tekster.
          ASSIGN TT_Tekster.teksttype = 1004
                 TT_Tekster.tekstnr   = INT(TT_ELogg.Verdier)
                 TT_Tekster.aksjon    = 9.
  END.
  IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = "InnBetType" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 1  AND
              TT_Elogg.Verdier       = "ALLE") THEN DO:
      FOR EACH InnBetType NO-LOCK:
          CREATE TT_Tekster.
          ASSIGN TT_Tekster.teksttype = 1004
                 TT_Tekster.tekstnr   = InnBetType.InnBetTId
                 TT_Tekster.transtype = 0
                 TT_Tekster.tekst     = SUBSTR(InnBetType.InnBBeskrivelse,1,30)
                 TT_Tekster.aksjon    = 1.
      END.
  END.
  ELSE DO:
      FOR EACH TT_ELogg WHERE 
                  TT_ELogg.TabellNavn     = "InnBetType" AND
                  TT_ELogg.EksterntSystem = "POS"  AND
                  TT_ELogg.EndringsType   = 1:
          FIND InnBetType WHERE InnBetType.InnBetTId = INT(TT_ELogg.Verdier) NO-LOCK NO-ERROR.
          IF AVAIL InnBetType THEN DO:
              CREATE TT_Tekster.
              ASSIGN TT_Tekster.teksttype = 1004
                     TT_Tekster.tekstnr   = InnBetType.InnBetTId
                     TT_Tekster.transtype = 0
                     TT_Tekster.tekst     = SUBSTR(InnBetType.InnBBeskrivelse,1,30)
                     TT_Tekster.aksjon    = 1.
          END.
      END.
  END.
  RELEASE TT_Tekster.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixKravKodeEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixKravKodeEndringer Procedure 
PROCEDURE FixKravKodeEndringer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH TT_ELogg WHERE 
              TT_ELogg.TabellNavn     = "KravKode" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 3:
          CREATE TT_Tekster.
          ASSIGN TT_Tekster.teksttype = 1001
                 TT_Tekster.tekstnr   = INT(TT_ELogg.Verdier)
                 TT_Tekster.aksjon    = 9.
  END.
  IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = "KravKode" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 1  AND
              TT_Elogg.Verdier       = "ALLE") THEN DO:
      FOR EACH KravKode NO-LOCK:
          CREATE TT_Tekster.
          ASSIGN TT_Tekster.teksttype = 1001
                 TT_Tekster.tekstnr   = KravKode.KravKode
                 TT_Tekster.transtype = 4
                 TT_Tekster.tekst     = SUBSTR(KravKode.Beskrivelse,1,30)
                 TT_Tekster.aksjon    = 1.
      END.
  END.
  ELSE DO:
      FOR EACH TT_ELogg WHERE 
                  TT_ELogg.TabellNavn     = "KravKode" AND
                  TT_ELogg.EksterntSystem = "POS"  AND
                  TT_ELogg.EndringsType   = 1:
          FIND KravKode WHERE KravKode.KravKode = INT(TT_ELogg.Verdier) NO-LOCK NO-ERROR.
          IF AVAIL KravKode THEN DO:
              CREATE TT_Tekster.
              ASSIGN TT_Tekster.teksttype = 1001
                     TT_Tekster.tekstnr   = KravKode.KravKode
                     TT_Tekster.transtype = 4
                     TT_Tekster.tekst     = SUBSTR(KravKode.Beskrivelse,1,30)
                     TT_Tekster.aksjon    = 1.
          END.
      END.
  END.
  RELEASE TT_Tekster.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixTeksterEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixTeksterEndringer Procedure 
PROCEDURE FixTeksterEndringer PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN FixFeilKodeEndringer.
  RUN FixKravKodeEndringer.
  RUN FixGaveKTypeEndringer.
  RUN FixUtbetTypeEndringer.
  RUN FixInnBetTypeEndringer.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixUtbetTypeEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixUtbetTypeEndringer Procedure 
PROCEDURE FixUtbetTypeEndringer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH TT_ELogg WHERE 
              TT_ELogg.TabellNavn     = "UtBetType" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 3:
          CREATE TT_Tekster.
          ASSIGN TT_Tekster.teksttype = 1005
                 TT_Tekster.tekstnr   = INT(TT_ELogg.Verdier)
                 TT_Tekster.aksjon    = 9.
  END.
  IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = "UtBetType" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 1  AND
              TT_Elogg.Verdier       = "ALLE") THEN DO:
      FOR EACH UtBetType NO-LOCK:
          CREATE TT_Tekster.
          ASSIGN TT_Tekster.teksttype = 1005
                 TT_Tekster.tekstnr   = UtBetType.UtBetTId
                 TT_Tekster.transtype = 0
                 TT_Tekster.tekst     = SUBSTR(UtBetType.UtBBeskrivelse,1,30)
                 TT_Tekster.aksjon    = 1.
      END.
  END.
  ELSE DO:
      FOR EACH TT_ELogg WHERE 
                  TT_ELogg.TabellNavn     = "UtBetType" AND
                  TT_ELogg.EksterntSystem = "POS"  AND
                  TT_ELogg.EndringsType   = 1:
          FIND UtBetType WHERE UtbetType.UtBetTId = INT(TT_ELogg.Verdier) NO-LOCK NO-ERROR.
          IF AVAIL UtBetType THEN DO:
              CREATE TT_Tekster.
              ASSIGN TT_Tekster.teksttype = 1005
                     TT_Tekster.tekstnr   = UtBetType.UtBetTId
                     TT_Tekster.transtype = 0
                     TT_Tekster.tekst     = SUBSTR(UtBetType.UtBBeskrivelse,1,30)
                     TT_Tekster.aksjon    = 1.
          END.
      END.
  END.
  RELEASE TT_Tekster.
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
    IF CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "FeilKode" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") OR
        CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "KravKode" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") OR
        CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "GaveKType" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") OR
        CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "UtbetType" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") OR 
        CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "InnBetType" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") THEN DO:

        FIND ELogg WHERE ELogg.TabellNavn     = "FeilKode" AND
                         ELogg.EksterntSystem = "POS"    AND
                         ELogg.Verdier        = "KLARGJOR" NO-LOCK NO-ERROR.
        IF AVAIL ELogg THEN DO:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            ASSIGN TT_ELogg.Verdier = "ALLE".
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
        FIND ELogg WHERE ELogg.TabellNavn     = "KravKode" AND
                         ELogg.EksterntSystem = "POS"    AND
                         ELogg.Verdier        = "KLARGJOR" NO-LOCK NO-ERROR.
        IF AVAIL ELogg THEN DO:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            ASSIGN TT_ELogg.Verdier = "ALLE".
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
        FIND ELogg WHERE ELogg.TabellNavn     = "GaveKType" AND
                         ELogg.EksterntSystem = "POS"    AND
                         ELogg.Verdier        = "KLARGJOR" NO-LOCK NO-ERROR.
        IF AVAIL ELogg THEN DO:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            ASSIGN TT_ELogg.Verdier = "ALLE".
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
        FIND ELogg WHERE ELogg.TabellNavn     = "UtbetType" AND
                         ELogg.EksterntSystem = "POS"    AND
                         ELogg.Verdier        = "KLARGJOR" NO-LOCK NO-ERROR.
        IF AVAIL ELogg THEN DO:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            ASSIGN TT_ELogg.Verdier = "ALLE".
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
        FIND ELogg WHERE ELogg.TabellNavn     = "InnBetType" AND
                         ELogg.EksterntSystem = "POS"    AND
                         ELogg.Verdier        = "KLARGJOR" NO-LOCK NO-ERROR.
        IF AVAIL ELogg THEN DO:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            ASSIGN TT_ELogg.Verdier = "ALLE".
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
    END.
    ELSE DO:
        FOR EACH ELogg WHERE ELogg.TabellNavn = "FeilKode" AND
                             ELogg.EksterntSystem = "POS" NO-LOCK:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
        FOR EACH ELogg WHERE ELogg.TabellNavn = "KravKode" AND
                             ELogg.EksterntSystem = "POS" NO-LOCK:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
        FOR EACH ELogg WHERE ELogg.TabellNavn = "GaveKType" AND
                             ELogg.EksterntSystem = "POS" NO-LOCK:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
        FOR EACH ELogg WHERE ELogg.TabellNavn = "UtbetType" AND
                             ELogg.EksterntSystem = "POS" NO-LOCK:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
        FOR EACH ELogg WHERE ELogg.TabellNavn = "InnBetType" AND
                             ELogg.EksterntSystem = "POS" NO-LOCK:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettTT_ELoggTekster) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTT_ELoggTekster Procedure 
PROCEDURE SlettTT_ELoggTekster :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = "FeilKode" AND
                       TT_ELogg.EksterntSystem = "POS".
        DELETE TT_ELogg.
    END.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = "KravKode" AND
                       TT_ELogg.EksterntSystem = "POS".
        DELETE TT_ELogg.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

