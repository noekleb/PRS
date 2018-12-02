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
DEFINE INPUT  PARAMETER cLanButiker AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cFtpButiker AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cStrTypeFiler AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntStrType    AS INTEGER    NO-UNDO.
DEFINE VARIABLE  iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE cExportFil      AS CHARACTER INIT "strtype."                NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER INIT "c:\home\lindbak\kasse" NO-UNDO.
DEFINE VARIABLE cTekst          AS CHARACTER                              NO-UNDO.

DEFINE TEMP-TABLE TT_StrType NO-UNDO
    /* 1   butnr       integer     3 siffer     Butikknr */
    FIELD strtypenr  AS INTEGER   FORMAT ">>>>>9"  /* I (3)  Størrelsestype */
    FIELD storrnr    AS INTEGER   FORMAT ">>>9"  /* I (3)  Størrelsesnr   */
    FIELD aksjon     AS INTEGER   FORMAT "9"    /* I (1)  Akjsonskode, 1=ny/endring, 9=sletting) */
    INDEX Aksjon aksjon DESCENDING strtypenr ASCENDING storrnr ASCENDING
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
RUN FixStrTypeEndringer.
/* Här skall vi loopa runt alla kassor mm */
IF cLanButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cLanButiker):
      RUN ExportStrType IN THIS-PROCEDURE (INT(ENTRY(iCount,cLanButiker)),"txt"). /* parameter = den loopade butiken */
  END.
END.
IF cFtpButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
      RUN ExportStrType IN THIS-PROCEDURE (INT(ENTRY(iCount,cFtpButiker)),ENTRY(iCount,cFtpButiker)). /* parameter = den loopade butiken */
  END.
END.
                                       /* + eventuellt filnamn */
RUN SlettTT_ELoggStrType. /* */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExportStrType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportStrType Procedure 
PROCEDURE ExportStrType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iButik     LIKE Butiker.Butik NO-UNDO.
    DEFINE INPUT  PARAMETER cFilSuffix AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE         iCount     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE  cNumericFormat    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cDateFormat       AS CHARACTER  NO-UNDO.
    
    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "American"
           SESSION:DATE-FORMAT    = "dmy".
    ASSIGN iAntStrType = 0. /* tyvärr eftersom jag gör detta flera gånger */
    OUTPUT TO VALUE(cExportFil + cFilsuffix) APPEND CONVERT TARGET "IBM850".
    FOR EACH TT_StrType:
        EXPORT iButik
               TT_StrType.strtypenr   
               TT_StrType.storrnr
               TT_StrType.aksjon    
               .
        ASSIGN iAntStrType = iAntStrType + 1.
    END.
    OUTPUT CLOSE.
    
    FILE-INFO:FILE-NAME = cExportFil + cFilsuffix.
    IF FILE-INFO:FILE-SIZE = 0 THEN
        OS-DELETE VALUE(cExportFil + cFilsuffix).
    ELSE IF NOT CAN-DO(cStrTypeFiler,cExportFil + cFilsuffix) THEN
        ASSIGN cStrTypeFiler = cStrTypeFiler + (IF cStrTypeFiler = "" THEN "" ELSE ",") + cExportFil + cFilsuffix.
    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixStrTypeEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixStrTypeEndringer Procedure 
PROCEDURE FixStrTypeEndringer PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH TT_ELogg WHERE 
              TT_ELogg.TabellNavn     = "StrType" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 3:
      FIND StrKonv WHERE StrKonv.Storl = ENTRY(2,TT_ELogg.Verdier) NO-LOCK NO-ERROR.
      IF AVAIL StrKonv THEN DO:
      CREATE TT_StrType.
          ASSIGN TT_StrType.strtypenr = INT(ENTRY(1,TT_ELogg.Verdier))
                 TT_StrType.storrnr   = StrKonv.StrKode
                 TT_StrType.aksjon    = 9.
      END.
  END.
  IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = "StrType" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 1  AND
              TT_Elogg.Verdier        = "ALLE") THEN DO:
      FOR EACH StrType NO-LOCK:
          FOR EACH StrTStr OF StrType:
              FIND StrKonv WHERE StrKonv.Storl = StrTStr.SoStorl NO-LOCK NO-ERROR.
              IF AVAIL StrKonv THEN DO:
                  CREATE TT_StrType.
                  ASSIGN TT_StrType.strtypenr = StrTStr.StrTypeID
                         TT_StrType.storrnr   = StrKonv.StrKode
                         TT_StrType.aksjon    = 1.
              END.
          END.
      END.
  END.
  ELSE DO:
      FOR EACH TT_ELogg WHERE 
                  TT_ELogg.TabellNavn     = "StrType" AND
                  TT_ELogg.EksterntSystem = "POS"  AND
                  TT_ELogg.EndringsType   = 1 AND
                  TT_ELogg.Verdier        <> "ALLE":
      FIND StrTStr WHERE StrTStr.StrTypeID = INT(ENTRY(1,TT_ELogg.Verdier)) AND
                                    StrTStr.SoStorl   = ENTRY(2,TT_ELogg.Verdier) NO-LOCK NO-ERROR.
      IF AVAIL StrTStr THEN DO:
          FIND StrKonv WHERE StrKonv.Storl = StrTStr.SoStorl NO-LOCK NO-ERROR.
          IF AVAIL StrKonv THEN DO:
              CREATE TT_StrType.
              ASSIGN TT_StrType.strtypenr = StrTStr.StrTypeID
                     TT_StrType.storrnr   = StrKonv.StrKode
                     TT_StrType.aksjon    = 1.
          END.
      END.
      END.
  END.
  RELEASE TT_StrType.
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
    IF CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "StrType" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") THEN DO:

        FIND ELogg WHERE ELogg.TabellNavn     = "StrType" AND
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
        FOR EACH ELogg WHERE ELogg.TabellNavn = "StrType" AND
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

&IF DEFINED(EXCLUDE-SlettTT_ELoggStrType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTT_ELoggStrType Procedure 
PROCEDURE SlettTT_ELoggStrType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = "StrType" AND
                       TT_ELogg.EksterntSystem = "POS".
        DELETE TT_ELogg.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

