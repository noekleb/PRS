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
DEFINE OUTPUT PARAMETER cKravKodeFiler AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntKravKode    AS INTEGER    NO-UNDO.
DEFINE VARIABLE  iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE cExportFil      AS CHARACTER INIT "tiltak."                NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER INIT "c:\home\lindbak\kasse" NO-UNDO.
DEFINE VARIABLE cTekst          AS CHARACTER                              NO-UNDO.


DEFINE TEMP-TABLE TT_KravKode
    FIELD kravnr    AS INTEGER FORMAT ">>>9"     /* I (4)      Kravnr */
    FIELD kravtekst AS CHARACTER FORMAT "x(15)"  /* C (15)     Kravtekst */
    FIELD aksjon     AS INTEGER FORMAT "9"        /* I (1)      Posttype, 1=ny/endring, 9=sletting)                     */
    INDEX Aksjon aksjon DESCENDING kravnr ASCENDING
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
RUN FixKravKodeEndringer.
/* Här skall vi loopa runt alla kassor mm */
IF cLanButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cLanButiker):
      RUN ExportKravKode IN THIS-PROCEDURE (INT(ENTRY(iCount,cLanButiker)),"txt"). /* parameter = den loopade butiken */
  END.
END.
IF cFtpButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
      RUN ExportKravKode IN THIS-PROCEDURE (INT(ENTRY(iCount,cFtpButiker)),ENTRY(iCount,cFtpButiker)). /* parameter = den loopade butiken */
  END.
END.
                                       /* + eventuellt filnamn */
RUN SlettTT_ELoggKravKode. /* */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExportKravKode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportKravKode Procedure 
PROCEDURE ExportKravKode :
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
    ASSIGN iAntKravKode = 0. /* tyvärr eftersom jag gör detta flera gånger */
    OUTPUT TO VALUE(cExportFil + cFilsuffix) APPEND CONVERT TARGET "IBM850".
    FOR EACH TT_KravKode:
        EXPORT iButik
               TT_KravKode.kravnr   
               TT_KravKode.kravtekst
               TT_KravKode.aksjon    
               .
        ASSIGN iAntKravKode = iAntKravKode + 1.
    END.
    OUTPUT CLOSE.
    FILE-INFO:FILE-NAME = cExportFil + cFilsuffix.
    IF FILE-INFO:FILE-SIZE = 0 THEN
        OS-DELETE VALUE(cExportFil + cFilsuffix).
    ELSE IF NOT CAN-DO(cKravKodeFiler,cExportFil + cFilsuffix) THEN
        ASSIGN cKravKodeFiler = cKravKodeFiler + (IF cKravKodeFiler = "" THEN "" ELSE ",") + cExportFil + cFilsuffix.
    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixKravKodeEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixKravKodeEndringer Procedure 
PROCEDURE FixKravKodeEndringer PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH TT_ELogg WHERE 
              TT_ELogg.TabellNavn     = "KravKode" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 3:
          CREATE TT_KravKode.
          ASSIGN TT_KravKode.kravnr = INT(TT_ELogg.Verdier)
                 TT_KravKode.aksjon  = 9.
  END.
  IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = "KravKode" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 1  AND
              TT_Elogg.Verdier       = "ALLE") THEN DO:
      FOR EACH KravKode NO-LOCK:
          CREATE TT_KravKode.
          ASSIGN TT_KravKode.kravnr    = KravKode.KravKode
                 TT_KravKode.kravtekst = SUBSTR(KravKode.Beskrivelse,1,15)
                 TT_KravKode.aksjon  = 1.
      END.
  END.
  ELSE DO:
      FOR EACH TT_ELogg WHERE 
                  TT_ELogg.TabellNavn     = "KravKode" AND
                  TT_ELogg.EksterntSystem = "POS"  AND
                  TT_ELogg.EndringsType   = 1:
          FIND KravKode WHERE KravKode.KravKode = INT(TT_ELogg.Verdier) NO-LOCK NO-ERROR.
          IF AVAIL KravKode THEN DO:
              CREATE TT_KravKode.
              ASSIGN TT_KravKode.kravnr = KravKode.KravKode
                     TT_KravKode.kravtekst = SUBSTR(KravKode.Beskrivelse,1,15)
                     TT_KravKode.aksjon  = 1.
          END.
      END.
  END.
  RELEASE TT_KravKode.
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
    FOR EACH ELogg WHERE ELogg.TabellNavn = "KravKode" AND
                         ELogg.EksterntSystem = "POS" NO-LOCK:
        BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
        FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL bElogg THEN
            DELETE bELogg.
        IF AVAILABLE TT_Elogg THEN
            RELEASE TT_ELogg.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettTT_ELoggKravKode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTT_ELoggKravKode Procedure 
PROCEDURE SlettTT_ELoggKravKode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = "KravKode" AND
                       TT_ELogg.EksterntSystem = "POS".
        DELETE TT_ELogg.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

