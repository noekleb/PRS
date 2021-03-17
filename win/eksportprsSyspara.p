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

DEFINE INPUT  PARAMETER cFtpButiker AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cExportFil  AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cSysparaFiler AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntSyspara    AS INTEGER    NO-UNDO.
DEFINE VARIABLE  iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER INIT "c:\home\lindbak\kasse" NO-UNDO.
DEFINE VARIABLE cTekst          AS CHARACTER                              NO-UNDO.


DEFINE TEMP-TABLE TT_Syspara NO-UNDO
    FIELD iDx         AS INTEGER
    FIELD syshid      AS INTEGER
    FIELD syshidbeskr AS CHARACTER
    FIELD sysgr       AS INTEGER
    FIELD sysgrbeskr  AS CHARACTER
    FIELD paranr      AS INTEGER
    FIELD paranrbeskr AS CHARACTER
    FIELD parameter1  AS CHAR
    FIELD parameter2  AS CHAR
    FIELD aksjon      AS INTEGER
    INDEX Aksjon aksjon DESCENDING iDx ASCENDING
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
         HEIGHT             = 14.91
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
RUN FixSysparaEndringer.
IF cFtpButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
      RUN ExportSyspara IN THIS-PROCEDURE (INT(ENTRY(iCount,cFtpButiker)),ENTRY(iCount,cFtpButiker)). /* parameter = den loopade butiken */
  END.
END.
                                       /* + eventuellt filnamn */
RUN SlettTT_ELoggSyspara. /* */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExportSyspara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportSyspara Procedure 
PROCEDURE ExportSyspara :
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
    ASSIGN iAntSyspara = 0. /* tyvärr eftersom jag gör detta flera gånger */
    OUTPUT TO VALUE(cExportFil + cFilsuffix) APPEND.
    FOR EACH TT_Syspara WHERE TT_Syspara.Aksjon = 1:
        PUT UNFORMATTED "SYSHODE"   ";" TT_Syspara.syshid ";" TT_Syspara.syshidbeskr SKIP
                        "SYSGRUPPE" ";" TT_Syspara.syshid ";" TT_Syspara.sysgr ";" TT_Syspara.sysgrbeskr SKIP
                        "SYSPARA"   ";" TT_Syspara.syshid ";" TT_Syspara.sysgr ";" TT_Syspara.paranr ";" TT_Syspara.paranrbeskr ";"
                                        TT_Syspara.parameter1 ";" TT_Syspara.parameter2 SKIP.
        ASSIGN iAntSyspara = iAntSyspara + 1.
    END.
    OUTPUT CLOSE.
    FILE-INFO:FILE-NAME = cExportFil + cFilsuffix.
    IF FILE-INFO:FILE-SIZE = 0 THEN
        OS-DELETE VALUE(cExportFil + cFilsuffix).
    ELSE IF NOT CAN-DO(cSysparaFiler,cExportFil + cFilsuffix) THEN
        ASSIGN cSysparaFiler = cSysparaFiler + (IF cSysparaFiler = "" THEN "" ELSE ",") + cExportFil + cFilsuffix.
    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixSysparaEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixSysparaEndringer Procedure 
PROCEDURE FixSysparaEndringer PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iIDx AS INTEGER     NO-UNDO.
/* finns inga 3:or */
  FOR EACH TT_ELogg WHERE 
              TT_ELogg.TabellNavn     = "Syspara" AND
              TT_ELogg.EksterntSystem = "POS"  AND
              TT_ELogg.EndringsType   = 1:
      FIND Syspara WHERE SysPara.SysHId = INT(ENTRY(1,TT_ELogg.Verdier)) AND
                         SysPara.SysGr  = INT(ENTRY(2,TT_ELogg.Verdier)) AND
                         SysPara.ParaNr = INT(ENTRY(3,TT_ELogg.Verdier)) NO-LOCK NO-ERROR.
      IF AVAIL Syspara THEN DO:
          FIND sysgruppe OF syspara NO-LOCK NO-ERROR.
          IF AVAIL sysgruppe THEN DO:
              FIND syshode OF sysgruppe NO-LOCK NO-ERROR.
              IF AVAIL syshode THEN DO:
                  iIdx = iIdx + 1.
                  CREATE TT_Syspara.
                  ASSIGN TT_Syspara.iDx         = iiDx
                         TT_Syspara.syshid      = syshode.syshid
                         TT_Syspara.syshidbeskr = syshode.beskrivelse
                         TT_Syspara.sysgr       = sysgruppe.sysgr
                         TT_Syspara.sysgrbeskr  = sysgruppe.beskrivelse
                         TT_Syspara.paranr      = syspara.paranr
                         TT_Syspara.paranrbeskr = syspara.beskrivelse
                         TT_Syspara.parameter1  = Syspara.parameter1
                         TT_Syspara.parameter2  = Syspara.parameter2
                         TT_Syspara.aksjon  = 1.
              END.
          END.
      END.
  END.
  RELEASE TT_Syspara.
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
        /* finns inga KLARGJOR */
/*     IF CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "Syspara" AND                       */
/*                             ELogg.EksterntSystem = "POS"    AND                        */
/*                             ELogg.Verdier        = "KLARGJOR") THEN DO:                */
/*                                                                                        */
/*         FIND ELogg WHERE ELogg.TabellNavn     = "Syspara" AND                          */
/*                          ELogg.EksterntSystem = "POS"    AND                           */
/*                          ELogg.Verdier        = "KLARGJOR" NO-LOCK NO-ERROR.           */
/*         IF AVAIL ELogg THEN DO:                                                        */
/*             BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.                                    */
/*             ASSIGN TT_ELogg.Verdier = "ALLE".                                          */
/*             FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR. */
/*             IF AVAIL bElogg THEN                                                       */
/*                 DELETE bELogg.                                                         */
/*             IF AVAILABLE TT_Elogg THEN                                                 */
/*                 RELEASE TT_ELogg.                                                      */
/*         END.                                                                           */
/*     END.                                                                               */
/*     ELSE DO: */
    
    FOR EACH ELogg WHERE ELogg.TabellNavn = "Syspara" AND
                         ELogg.EksterntSystem = "POS" NO-LOCK:
        BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
        FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL bElogg THEN DO:
            DELETE bELogg.
/*             MESSAGE "DELETED"                      */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */
        END.
        IF AVAILABLE TT_Elogg THEN
            RELEASE TT_ELogg.
    END.
/*     END. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettTT_ELoggSyspara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTT_ELoggSyspara Procedure 
PROCEDURE SlettTT_ELoggSyspara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = "Syspara" AND
                       TT_ELogg.EksterntSystem = "POS".
        DELETE TT_ELogg.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

