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
DEFINE INPUT  PARAMETER cLanButiker     AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cFtpButiker     AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cExportDir      AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cProdusentFiler AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntProdusent   AS INTEGER    NO-UNDO.
DEFINE VARIABLE  iCount AS INTEGER    NO-UNDO.
/* DEFINE VARIABLE  cExportDir     AS CHARACTER INIT "c:\home\lindbak\sendes\" NO-UNDO. */
DEFINE VARIABLE cTabellNavn AS CHARACTER INIT "Produsent" NO-UNDO.

DEFINE TEMP-TABLE TT_RigalProdusent
    FIELD cKode     AS CHARACTER   /* Kode      "LEV" Leverandør */
    FIELD iNummer   AS INTEGER     /* Produsentnr       I(8) */
    FIELD cNavn     AS CHARACTER   /* C(30)*/
    FIELD cAdresse1 AS CHARACTER   /* Adresse   C(30) */
    FIELD cAdresse2 AS CHARACTER   /* Adresse   C(30) */
    FIELD iPostnr   AS INTEGER
    FIELD cPostAdr  AS CHARACTER
    FIELD cKontakt  AS CHARACTER
    FIELD cTelefon1 AS CHARACTER
    FIELD cTelefon2 AS CHARACTER
    FIELD cTelefax  AS CHARACTER
    FIELD cModemNr  AS CHARACTER
    FIELD dEANLokasjon AS DECIMAL
    FIELD cEmail       AS CHARACTER
    FIELD cFlag        AS CHARACTER /* Flag     Funksjonskode   C(1)    "N" = Nyopplegg"E" = Endring"S" = Sletting */
    INDEX iNummer iNummer ASCENDING
    .


/*
11.1.1  Kode    "LEV""PRD"              LeverandørProdusent
11.1.2  Nummer          
11.1.3  Navn    Leverandørnavn  C(30)   
11.1.4  Adresse1        Adresse C(30)   
11.1.5  Adresse2        Utfyllende adresse      C(30)   
11.1.6  Poststedsnummer Postnr  I       
11.1.7  Poststedsnavn                   
11.1.8  Kontakt_person  Navn på kontaktperson   C(30)   
11.1.9  Telefonnr1      Leverandørs telefonnr   C(30)   
11.1.10 Telefonnr2                      
11.1.11 Telefaxnr       Leverandørs faxnr       C(30)   
11.1.12 Modemnr                 
11.1.13 EANlokasjonsnr          I(13)   
11.1.14 Emailadresse            C(30)   
11.1.15 Flag    Funksjonskode   C(1)    "N" = Nyopplegg"E" = Endring"S" = Sletting
*/

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
         HEIGHT             = 6.76
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
RUN KopierElogg.
RUN FixProdusentEndringer.
/* Här skall vi loopa runt alla kassor mm */
IF cLanButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cLanButiker):
      RUN ExportProdusent IN THIS-PROCEDURE (INT(ENTRY(iCount,cLanButiker))). /* parameter = den loopade butiken */
  END.
END.
                                       /* + eventuellt filnamn */
RUN SlettTT_ELoggProdusent. /* */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExportProdusent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportProdusent Procedure 
PROCEDURE ExportProdusent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iButik     LIKE Butiker.Butik NO-UNDO.
    DEFINE VARIABLE         iCount     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE  cNumericFormat    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cDateFormat       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cFilNavn          AS CHARACTER  NO-UNDO.
    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "American"
           SESSION:DATE-FORMAT    = "dmy"
          cFilNavn = cExportDir + "L01" + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + STRING(iButik / 1000,"9.999").
           .
    ASSIGN iAntProdusent = 0. /* tyvärr eftersom jag gör detta flera gånger */
    IF SEARCH(cFilNavn) = ? THEN DO:
        OUTPUT TO VALUE(cFilNavn) CONVERT TARGET "IBM850".
        PUT UNFORMATTED "RIGAL02,8.0,......." SKIP.
    END.
    ELSE
        OUTPUT TO VALUE(cFilNavn) APPEND CONVERT TARGET "IBM850".
    FOR EACH TT_RigalProdusent WHERE 
        TT_RigalProdusent.iNummer > 0:
        EXPORT DELIMITER ","
               TT_RigalProdusent.
        ASSIGN iAntProdusent = iAntProdusent + 1.
    END.
    OUTPUT CLOSE.
    FILE-INFO:FILE-NAME = cFilNavn.
    IF FILE-INFO:FILE-SIZE = 0 THEN
        OS-DELETE VALUE(cFilNavn).
    ELSE IF NOT CAN-DO(cProdusentFiler,cFilNavn) THEN
        ASSIGN cProdusentFiler = cProdusentFiler + (IF cProdusentFiler = "" THEN "" ELSE ",") + cFilNavn.
    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixProdusentEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixProdusentEndringer Procedure 
PROCEDURE FixProdusentEndringer PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iNrKoll AS INTEGER    NO-UNDO.
  FOR EACH TT_ELogg WHERE 
              TT_ELogg.TabellNavn     = cTabellNavn AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 3:
          CREATE TT_RigalProdusent.
          ASSIGN TT_RigalProdusent.cKode = "PRD"
                 TT_RigalProdusent.iNummer = INT(TT_ELogg.Verdier)
                 TT_RigalProdusent.cFlag  = "S".
  END.
  IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 1  AND
              TT_Elogg.Verdier       = "ALLE") THEN DO:
      FOR EACH Produsent NO-LOCK:
          ASSIGN iNrKoll = INT(REPLACE(Produsent.PostNr," ","")) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN
              ASSIGN iNrKoll = 0.
          FIND Post WHERE Post.Postnr = Produsent.PostNr NO-LOCK NO-ERROR.
          CREATE TT_RigalProdusent.
          ASSIGN TT_RigalProdusent.cKode     = "PRD"
                 TT_RigalProdusent.iNummer   = Produsent.ProdNr
                 TT_RigalProdusent.cNavn     = Produsent.Beskrivelse
                 TT_RigalProdusent.cAdresse1 = Produsent.Adresse1
                 TT_RigalProdusent.cAdresse2 = Produsent.Adresse2
                 TT_RigalProdusent.iPostnr   = iNrKoll
                 TT_RigalProdusent.cPostAdr  = IF AVAIL Post THEN Post.Beskrivelse ELSE ""
                 TT_RigalProdusent.cKontakt  = TRIM(Produsent.Kontakt)
                 TT_RigalProdusent.cTelefon1 = Produsent.Telefon
                 TT_RigalProdusent.cTelefon2 = ""
                 TT_RigalProdusent.cTelefax  = ""
                 TT_RigalProdusent.cModemNr  = ""
                 TT_RigalProdusent.cEmail    = ""
                 TT_RigalProdusent.cFlag  = "E".
      END.
  END.
  ELSE DO:
      FOR EACH TT_ELogg WHERE 
                  TT_ELogg.TabellNavn     = cTabellNavn AND
                  TT_ELogg.EksterntSystem = "POS"  AND
                  TT_ELogg.EndringsType   = 1:
          FIND Produsent WHERE Produsent.ProdNr = INT(TT_ELogg.Verdier) NO-LOCK NO-ERROR.
          IF AVAIL Produsent THEN DO:
              ASSIGN iNrKoll = INT(REPLACE(Produsent.PostNr," ","")) NO-ERROR.
              IF ERROR-STATUS:ERROR THEN
                  ASSIGN iNrKoll = 0.
              FIND Post WHERE Post.Postnr = Produsent.PostNr NO-LOCK NO-ERROR.
              CREATE TT_RigalProdusent.
              CREATE TT_RigalProdusent.
              ASSIGN TT_RigalProdusent.cKode     = "PRD"
                     TT_RigalProdusent.iNummer   = Produsent.ProdNr
                     TT_RigalProdusent.cNavn     = Produsent.Beskrivelse
                     TT_RigalProdusent.cAdresse1 = Produsent.Adresse1
                     TT_RigalProdusent.cAdresse2 = Produsent.Adresse2
                     TT_RigalProdusent.iPostnr   = iNrKoll
                     TT_RigalProdusent.cPostAdr  = IF AVAIL Post THEN Post.Beskrivelse ELSE ""
                     TT_RigalProdusent.cKontakt  = TRIM(Produsent.Kontakt)
                     TT_RigalProdusent.cTelefon1 = Produsent.Telefon
                     TT_RigalProdusent.cTelefon2 = ""
                     TT_RigalProdusent.cTelefax  = ""
                     TT_RigalProdusent.cModemNr  = ""
                     TT_RigalProdusent.cEmail    = ""
                     TT_RigalProdusent.cFlag  = "E".
          END.
      END.
  END.
  RELEASE TT_RigalProdusent.
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
    FOR EACH ELogg WHERE ELogg.TabellNavn = cTabellNavn AND
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

&IF DEFINED(EXCLUDE-SlettTT_ELoggProdusent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTT_ELoggProdusent Procedure 
PROCEDURE SlettTT_ELoggProdusent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = cTabellNavn AND
                       TT_ELogg.EksterntSystem = "POS".
        DELETE TT_ELogg.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

