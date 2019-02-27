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
DEFINE INPUT  PARAMETER cPRSFtpButiker AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cHgrFiler   AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntVarGr   AS INTEGER    NO-UNDO.
DEFINE VARIABLE  iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE cExportFil      AS CHARACTER INIT "hgr."                NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER INIT "c:\home\lindbak\kasse" NO-UNDO.
DEFINE VARIABLE cTekst          AS CHARACTER                              NO-UNDO.

DEFINE TEMP-TABLE TT_Hgr
    FIELD butnr    AS INTEGER FORMAT ">>9"              /* I (3)      Butikknr                                                                                 */
    FIELD hgr      AS INTEGER FORMAT ">>>9"             /* I (4)      Varegruppenr */
    FIELD hgrtekst AS CHARACTER FORMAT "x(30)"          /* C (30)     Varegruppetekst */
    FIELD bonus    AS LOGICAL                           /* L (yes/no) Ikke i bruk (ikke salg direkte på varegr.tast) */
    FIELD mva_like AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Ikke i bruk (ikke salg direkte på varegr.tast) */
    FIELD kunrab1  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for kunder i kundegr 1 */
    FIELD kunrab2  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for kunder i kundegr 2 */
    FIELD kunrab3  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for kunder i kundegr 3 */
    FIELD kunrab4  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for kunder i kundegr 4 */
    FIELD kunrab5  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for kunder i kundegr 5 */
    FIELD kunrab6  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for kunder i kundegr 6 */
    FIELD kunrab7  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for kunder i kundegr 7 */
    FIELD kunrab8  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for andre butikker (intern salg) */
    FIELD kunrab9  AS DECIMAL DECIMALS 2 FORMAT ">9.99" /* De (2,2)   Automatisk %-rabatt for ansatte (personalrabatt) */
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

/* Vi exporterar direkt från ELogg till fil */
/* Ingen extra behandling behöver göras */
RUN KopierElogg.
/* Kanske vi skall hämta iformation om kassor och filer först. */

/* Här skall vi loopa runt alla kassor mm */
IF cLanButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cLanButiker):
      RUN ExportVarGr IN THIS-PROCEDURE (INT(ENTRY(iCount,cLanButiker)),"txt"). /* parameter = den loopade butiken */
  END.
END.
IF cFtpButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
      RUN ExportVarGr IN THIS-PROCEDURE (INT(ENTRY(iCount,cFtpButiker)),ENTRY(iCount,cFtpButiker)). /* parameter = den loopade butiken */
  END.
END.
                                       /* + eventuellt filnamn */
RUN SlettTT_ELoggVarGr. /* */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExportVarGr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportVarGr Procedure 
PROCEDURE ExportVarGr :
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
    ASSIGN iAntVarGr = 0. /* tyvärr eftersom jag gör detta flera gånger */
    OUTPUT TO VALUE(cExportFil + cFilsuffix) APPEND CONVERT TARGET "IBM850".
/* InfoPos kasse vill inte ha några slettet */
    IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = "VarGr" AND
                                 TT_ELogg.EksterntSystem = "POS"   AND
                                 TT_ELogg.EndringsType   = 1       AND
                                 TT_Elogg.Verdier = "ALLE") THEN DO:
        CREATE TT_Hgr.
        ASSIGN 
            TT_Hgr.butnr   = iButik
            .
        FOR EACH VarGr NO-LOCK:
            ASSIGN
                TT_Hgr.hgr      = VarGr.Vg
                TT_Hgr.hgrtekst = VarGr.VgBeskr
                iAntVarGr       = iAntVarGr + 1
                TT_Hgr.KunRab1 = 0
                TT_Hgr.KunRab2 = 0
                TT_Hgr.KunRab3 = 0
                TT_Hgr.KunRab4 = 0
                TT_Hgr.KunRab5 = 0
                TT_Hgr.KunRab6 = 0
                TT_Hgr.KunRab7 = 0
                TT_Hgr.KunRab8 = 0
                TT_Hgr.KunRab9 = 0
                .
/*                    TT_Hgr.bonus    = */
/*                    TT_Hgr.mva_like = */
            FOR EACH VgKundeGrpRabatt OF VarGr NO-LOCK:
                CASE VgKundeGrpRabatt.GruppeId:
                    WHEN 1 THEN TT_Hgr.KunRab1 = VgKundeGrpRabatt.Rabatt%.
                    WHEN 2 THEN TT_Hgr.KunRab2 = VgKundeGrpRabatt.Rabatt%.
                    WHEN 3 THEN TT_Hgr.KunRab3 = VgKundeGrpRabatt.Rabatt%.
                    WHEN 4 THEN TT_Hgr.KunRab4 = VgKundeGrpRabatt.Rabatt%.
                    WHEN 5 THEN TT_Hgr.KunRab5 = VgKundeGrpRabatt.Rabatt%.
                    WHEN 6 THEN TT_Hgr.KunRab6 = VgKundeGrpRabatt.Rabatt%.
                    WHEN 7 THEN TT_Hgr.KunRab7 = VgKundeGrpRabatt.Rabatt%.
                    WHEN 8 THEN TT_Hgr.KunRab8 = VgKundeGrpRabatt.Rabatt%.
                    WHEN 9 THEN TT_Hgr.KunRab9 = VgKundeGrpRabatt.Rabatt%.
                END CASE.
            END.
            EXPORT TT_Hgr.
        END.
    END.
    ELSE DO:
        CREATE TT_Hgr.
        ASSIGN 
            TT_Hgr.butnr   = iButik
            .
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = "VarGr" AND
                           TT_ELogg.EksterntSystem = "POS"   AND
                           TT_ELogg.EndringsType   = 1 BY TT_ELogg.Verdier:
            FIND VarGr WHERE VarGr.Vg = INT(TT_ELogg.Verdier) NO-LOCK NO-ERROR.
            IF AVAIL VarGr THEN DO:
                ASSIGN
                    TT_Hgr.hgr      = VarGr.Vg
                    TT_Hgr.hgrtekst = VarGr.VgBeskr
                    iAntVarGr       = iAntVarGr + 1
                    TT_Hgr.KunRab1 = 0
                    TT_Hgr.KunRab2 = 0
                    TT_Hgr.KunRab3 = 0
                    TT_Hgr.KunRab4 = 0
                    TT_Hgr.KunRab5 = 0
                    TT_Hgr.KunRab6 = 0
                    TT_Hgr.KunRab7 = 0
                    TT_Hgr.KunRab8 = 0
                    TT_Hgr.KunRab9 = 0
                    .
/*                        TT_Hgr.bonus    = */
/*                        TT_Hgr.mva_like = */
                /* Legger ut rabattene */
                FOR EACH VgKundeGrpRabatt OF VarGr NO-LOCK:
                    CASE VgKundeGrpRabatt.GruppeId:
                        WHEN 1 THEN TT_Hgr.KunRab1 = VgKundeGrpRabatt.Rabatt%.
                        WHEN 2 THEN TT_Hgr.KunRab2 = VgKundeGrpRabatt.Rabatt%.
                        WHEN 3 THEN TT_Hgr.KunRab3 = VgKundeGrpRabatt.Rabatt%.
                        WHEN 4 THEN TT_Hgr.KunRab4 = VgKundeGrpRabatt.Rabatt%.
                        WHEN 5 THEN TT_Hgr.KunRab5 = VgKundeGrpRabatt.Rabatt%.
                        WHEN 6 THEN TT_Hgr.KunRab6 = VgKundeGrpRabatt.Rabatt%.
                        WHEN 7 THEN TT_Hgr.KunRab7 = VgKundeGrpRabatt.Rabatt%.
                        WHEN 8 THEN TT_Hgr.KunRab8 = VgKundeGrpRabatt.Rabatt%.
                        WHEN 9 THEN TT_Hgr.KunRab9 = VgKundeGrpRabatt.Rabatt%.
                    END CASE.
                END.
                EXPORT TT_Hgr.
            END.
        END.
    END.
    OUTPUT CLOSE.
    FILE-INFO:FILE-NAME = cExportFil + cFilsuffix.
    IF FILE-INFO:FILE-SIZE = 0 THEN
        OS-DELETE VALUE(cExportFil + cFilsuffix).
    ELSE IF NOT CAN-DO(cHgrFiler,cExportFil + cFilsuffix) THEN
        ASSIGN cHgrFiler = cHgrFiler + (IF cHgrFiler = "" THEN "" ELSE ",") + cExportFil + cFilsuffix.
    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixVarGrEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixVarGrEndringer Procedure 
PROCEDURE FixVarGrEndringer PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* InfoPos kasse tar bara emot nya */
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
    IF CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "VarGr" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") THEN DO:

        FIND ELogg WHERE ELogg.TabellNavn     = "VarGr" AND
                         ELogg.EksterntSystem = "POS"    AND
                         ELogg.Verdier        = "KLARGJOR" NO-LOCK NO-ERROR.
        IF AVAIL ELogg THEN DO:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            ASSIGN TT_ELogg.Verdier = "ALLE".
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg AND cPRSFtpButiker = '' THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
    END.
    ELSE DO:
        FOR EACH ELogg WHERE ELogg.TabellNavn = "VarGr" AND
                             ELogg.EksterntSystem = "POS" NO-LOCK:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg AND cPRSFtpButiker = '' THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettTT_ELoggVarGr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTT_ELoggVarGr Procedure 
PROCEDURE SlettTT_ELoggVarGr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = "VarGr" AND
                       TT_ELogg.EksterntSystem = "POS".
        DELETE TT_ELogg.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

