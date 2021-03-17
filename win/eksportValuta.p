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
    Notes       : Det är KasValuta som vi exporterar
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER cLanButiker AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cFtpButiker AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cPRSFtpButiker AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cValutaFiler AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntValuta   AS INTEGER    NO-UNDO.

DEFINE VARIABLE  iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE cExportFil      AS CHARACTER INIT "valuta."                NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER INIT "c:\home\lindbak\kasse" NO-UNDO.
DEFINE VARIABLE cTekst          AS CHARACTER                              NO-UNDO.
DEFINE VARIABLE iCl             AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE TT_Valuta
    FIELD butnr       AS INTEGER FORMAT ">>9"                 /* I (3)          Butikknr                         */
    FIELD valnr       AS INTEGER FORMAT ">9"                  /* I (2)          Valutanr                         */
    FIELD navn        AS CHARACTER FORMAT "x(30)"             /* C (30)         Full valutatekst                 */
    FIELD forkortelse AS CHARACTER FORMAT "x(3)"              /* C (3)          Valutaforkortelse                */
    FIELD indeks      AS INTEGER FORMAT ">>9"                 /* I (3)          Antall enheter av valutaen       */
    FIELD mottak      AS DECIMAL DECIMALS 3 FORMAT ">>>9.999" /* De (4,3)       Valutakurs ved mottak av valuta  */
    FIELD retur       AS DECIMAL DECIMALS 3 FORMAT ">>>9.999" /* De (4,3)       Valutakurs ved retur av valuta   */
    .

DEFINE BUFFER clButiker FOR Butiker. 

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

{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCL NO-ERROR.
IF NOT AVAILABLE clButiker THEN 
  RETURN.

/* Vi exporterar direkt från ELogg till fil */
/* Ingen extra behandling behöver göras */
RUN KopierElogg.
/* Kanske vi skall hämta iformation om kassor och filer först. */

/* Här skall vi loopa runt alla kassor mm */
IF cLanButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cLanButiker):
      RUN ExportValuta IN THIS-PROCEDURE (INT(ENTRY(iCount,cLanButiker)),"txt"). /* parameter = den loopade butiken */
  END.
END.
IF cFtpButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
      RUN ExportValuta IN THIS-PROCEDURE (INT(ENTRY(iCount,cFtpButiker)),ENTRY(iCount,cFtpButiker)). /* parameter = den loopade butiken */
  END.
END.
RUN SlettTT_ELoggValuta. /* */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExportValuta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportValuta Procedure 
PROCEDURE ExportValuta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iButik     LIKE Butiker.Butik NO-UNDO.
    DEFINE INPUT  PARAMETER cFilSuffix AS CHARACTER  NO-UNDO.
    
    DEFINE VARIABLE  cNumericFormat AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cDateFormat    AS CHARACTER  NO-UNDO.
    
    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "American"
           SESSION:DATE-FORMAT    = "dmy".
           
    FIND Butiker NO-LOCK WHERE
      Butiker.Butik = iButik NO-ERROR.       
           
    /* InfoPos kasse vill inte ha några slettet alltid alla valutor */
    IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = "KasValuta" AND
                                     TT_ELogg.EksterntSystem = "POS") THEN 
    DO:
        OUTPUT TO VALUE(cExportFil + cFilsuffix) APPEND CONVERT TARGET "IBM850".
        CREATE TT_Valuta.
        ASSIGN TT_Valuta.butnr = iButik
               iAntValuta      = 0. /* tyvärr, vi kanske gör detta flera gånger */
        
        IF CAN-FIND(FIRST KasValuta WHERE 
                    KasValuta.ProfilNr = Butiker.ProfilNr) THEN 
        EKSPORT_PROFIL:
        FOR EACH KasValuta  NO-LOCK WHERE 
            KasValuta.ValNr > 0 AND
            KasValuta.ProfilNr = Butiker.ProfilNr:

            ASSIGN TT_Valuta.valnr          = KasValuta.ValNr
                   TT_Valuta.navn           = KasValuta.ValNavn     
                   TT_Valuta.forkortelse    = KasValuta.ValKod
                   TT_Valuta.indeks         = KasValuta.indeks
                   TT_Valuta.mottak         = IF KasValuta.ValAktiv THEN KasValuta.KasseValKurs ELSE 0
                   TT_Valuta.retur          = KasValuta.retur
                   iAntValuta               = iAntValuta + 1.
            EXPORT TT_Valuta.
        END. /* EKSPORT_PROFIL */
        
        ELSE 
        EKSPORT_CL:
        FOR EACH KasValuta  NO-LOCK WHERE 
            KasValuta.ValNr > 0 AND
            KasValuta.ProfilNr = clButiker.ProfilNr:

            ASSIGN TT_Valuta.valnr          = KasValuta.ValNr
                   TT_Valuta.navn           = KasValuta.ValNavn     
                   TT_Valuta.forkortelse    = KasValuta.ValKod
                   TT_Valuta.indeks         = KasValuta.indeks
                   TT_Valuta.mottak         = IF KasValuta.ValAktiv THEN KasValuta.KasseValKurs ELSE 0
                   TT_Valuta.retur          = KasValuta.retur
                   iAntValuta               = iAntValuta + 1.
            EXPORT TT_Valuta.
        END. /* EKSPORT_CL */
        
        OUTPUT CLOSE.
    END.
    
    FILE-INFO:FILE-NAME = cExportFil + cFilsuffix.
    IF FILE-INFO:FILE-SIZE = 0 THEN
        OS-DELETE VALUE(cExportFil + cFilsuffix).
    ELSE IF NOT CAN-DO(cValutaFiler,cExportFil + cFilsuffix) THEN
        ASSIGN cValutaFiler = cValutaFiler + (IF cValutaFiler = "" THEN "" ELSE ",") + cExportFil + cFilsuffix.
    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixValutaEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixValutaEndringer Procedure 
PROCEDURE FixValutaEndringer PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* InfoPos kasse tar bara emot alla */
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
    IF CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "KasValuta" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") THEN DO:

        FIND ELogg WHERE ELogg.TabellNavn     = "KasValuta" AND
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
        FOR EACH ELogg WHERE ELogg.TabellNavn = "KasValuta" AND
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

&IF DEFINED(EXCLUDE-SlettTT_ELoggValuta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTT_ELoggValuta Procedure 
PROCEDURE SlettTT_ELoggValuta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = "KasValuta" AND
                       TT_ELogg.EksterntSystem = "POS".
        DELETE TT_ELogg.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

