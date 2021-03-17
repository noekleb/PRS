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
DEFINE OUTPUT PARAMETER cVarGrFiler AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntVarGr   AS INTEGER    NO-UNDO.
DEFINE VARIABLE  iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER INIT "c:\home\lindbak\kasse" NO-UNDO.
DEFINE VARIABLE cTekst          AS CHARACTER                              NO-UNDO.

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
    
    DEFINE VARIABLE iCount  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cString AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE  cNumericFormat AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cDateFormat    AS CHARACTER  NO-UNDO.
    
    ASSIGN iAntVarGr = 0. /* tyvärr eftersom jag gör detta flera gånger */
 
    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "EUROPEAN"
           SESSION:DATE-FORMAT    = "dmy".
 
 
    OUTPUT TO VALUE(cExportFil + cFilsuffix) APPEND.
/* InfoPos kasse vill inte ha några slettet */
    IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = "VarGr" AND
                                 TT_ELogg.EksterntSystem = "POS"   AND
                                 TT_ELogg.EndringsType   = 1       AND
                                 TT_Elogg.Verdier = "ALLE") THEN DO:
        FOR EACH VarGr NO-LOCK:
         cString = "VARGR;" + 
                   '1' + ";" +
                   STRING(VarGr.Vg) + ";" + 
                   REPLACE(REPLACE(VarGr.vgbeskr,";",""),'"'," ") + ";" + 
                   STRING(VarGr.Momskod) + ";" + 
                   STRING(Vargr.kost_proc)  + ";;;;;;;;;"
                   NO-ERROR.
            
            FOR EACH VgKundeGrpRabatt OF VarGr NO-LOCK:
                CASE VgKundeGrpRabatt.GruppeId:
                    WHEN 1 THEN ENTRY(7,cString,';')  = STRING(VgKundeGrpRabatt.Rabatt%).
                    WHEN 2 THEN ENTRY(8,cString,';')  = STRING(VgKundeGrpRabatt.Rabatt%).
                    WHEN 3 THEN ENTRY(9,cString,';')  = STRING(VgKundeGrpRabatt.Rabatt%).
                    WHEN 4 THEN ENTRY(10,cString,';')  = STRING(VgKundeGrpRabatt.Rabatt%).
                    WHEN 5 THEN ENTRY(11,cString,';') = STRING(VgKundeGrpRabatt.Rabatt%).
                    WHEN 6 THEN ENTRY(12,cString,';') = STRING(VgKundeGrpRabatt.Rabatt%).
                    WHEN 7 THEN ENTRY(13,cString,';') = STRING(VgKundeGrpRabatt.Rabatt%).
                    WHEN 8 THEN ENTRY(14,cString,';') = STRING(VgKundeGrpRabatt.Rabatt%).
                    WHEN 9 THEN ENTRY(15,cString,';') = STRING(VgKundeGrpRabatt.Rabatt%).
                END CASE.
            END.
            PUT UNFORMATTED cString SKIP.
        END.
    END.
    ELSE DO:
        /* Sletteposter */
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = "VarGr" AND
                           TT_ELogg.EksterntSystem = "POS"   AND
                           TT_ELogg.EndringsType   = 3 BY TT_ELogg.Verdier:            
            DO:
                cString = "VARGR;" + 
                   STRING(TT_ELogg.EndringsType) + ";" +
                   TT_ELogg.Verdier + ";;;;;;;;;;;;"
                   NO-ERROR.
                PUT UNFORMATTED cString SKIP.
            END.
        END.
        /* Nye/endrede */
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = "VarGr" AND
                           TT_ELogg.EksterntSystem = "POS"   AND
                           TT_ELogg.EndringsType   = 1 BY TT_ELogg.Verdier:
            FIND VarGr WHERE VarGr.Vg = INT(TT_ELogg.Verdier) NO-LOCK NO-ERROR.
            
            IF AVAIL VarGr THEN DO:
                cString = "VARGR;" + 
                   STRING(TT_ELogg.EndringsType) + ";" +
                   STRING(VarGr.Vg) + ";" + 
                   REPLACE(REPLACE(VarGr.vgbeskr,";",""),'"'," ") + ";" + 
                   STRING(VarGr.Momskod) + ";" + 
                   STRING(Vargr.kost_proc)  + ";;;;;;;;;"
                   NO-ERROR.
                
                /* Legger ut rabattene */
                FOR EACH VgKundeGrpRabatt OF VarGr NO-LOCK:
                    CASE VgKundeGrpRabatt.GruppeId:
                        WHEN 1 THEN ENTRY(7,cString,';')  = STRING(VgKundeGrpRabatt.Rabatt%).
                        WHEN 2 THEN ENTRY(8,cString,';')  = STRING(VgKundeGrpRabatt.Rabatt%).
                        WHEN 3 THEN ENTRY(9,cString,';')  = STRING(VgKundeGrpRabatt.Rabatt%).
                        WHEN 4 THEN ENTRY(10,cString,';')  = STRING(VgKundeGrpRabatt.Rabatt%).
                        WHEN 5 THEN ENTRY(11,cString,';') = STRING(VgKundeGrpRabatt.Rabatt%).
                        WHEN 6 THEN ENTRY(12,cString,';') = STRING(VgKundeGrpRabatt.Rabatt%).
                        WHEN 7 THEN ENTRY(13,cString,';') = STRING(VgKundeGrpRabatt.Rabatt%).
                        WHEN 8 THEN ENTRY(14,cString,';') = STRING(VgKundeGrpRabatt.Rabatt%).
                        WHEN 9 THEN ENTRY(15,cString,';') = STRING(VgKundeGrpRabatt.Rabatt%).
                    END CASE.
                END.
                PUT UNFORMATTED cString SKIP.
            END.
        END.
    END.
    OUTPUT CLOSE.
 
    FILE-INFO:FILE-NAME = cExportFil + cFilsuffix.
    IF FILE-INFO:FILE-SIZE = 0 THEN
        OS-DELETE VALUE(cExportFil + cFilsuffix).
    ELSE IF NOT CAN-DO(cVarGrFiler,cExportFil + cFilsuffix) THEN
        ASSIGN cVarGrFiler = cVarGrFiler + (IF cVarGrFiler = "" THEN "" ELSE ",") + cExportFil + cFilsuffix.

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
            IF AVAIL bElogg THEN
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

