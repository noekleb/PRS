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
DEFINE INPUT  PARAMETER cLanButiker   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cFtpButiker   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cPRSFtpButiker   AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cButikerFiler AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntbutiker   AS INTEGER    NO-UNDO.
DEFINE VARIABLE  iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE cExportFil     AS CHARACTER INIT "butikk." NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER INIT "c:\home\lindbak\kasse" NO-UNDO.
DEFINE VARIABLE cTekst          AS CHARACTER                              NO-UNDO.

DEFINE TEMP-TABLE TT_Butikk
    FIELD wbutnr    AS INTEGER   FORMAT ">>9"     /* I (4)      wbutnr */
    FIELD aksjon    AS INTEGER   FORMAT "9"     /* I (4)      Tekstnr */
    FIELD navn      AS CHARACTER FORMAT "x(30)"     /* I (4)        Tekstnr */
    FIELD adresse   AS CHARACTER FORMAT "x(30)"  /* C (15)    Tekst */
    FIELD postnr    AS INTEGER   FORMAT "99999"        /* I (1)     Posttype, 1=ny/endring, 9=sletting)                     */
    FIELD poststed  AS CHARACTER FORMAT "x(30)"  /* C (15)    Tekst */
    FIELD kontakt   AS CHARACTER FORMAT "x(30)"  /* C (15)    Tekst */
    FIELD telefon   AS CHARACTER FORMAT "x(15)"  /* C (15)    Tekst */
    FIELD butlager  AS LOGICAL   
   INDEX Aksjon aksjon DESCENDING wbutnr ASCENDING
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
RUN FixAlleButEndringer.
/* Här skall vi loopa runt alla kassor mm */
IF cLanButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cLanButiker):
      RUN ExportButiker IN THIS-PROCEDURE (INT(ENTRY(iCount,cLanButiker)),"txt"). /* parameter = den loopade butiken */
  END.
END.
IF cFtpButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
      RUN ExportButiker IN THIS-PROCEDURE (INT(ENTRY(iCount,cFtpButiker)),ENTRY(iCount,cFtpButiker)). /* parameter = den loopade butiken */
  END.
END.
                                       /* + eventuellt filnamn */
RUN SlettTT_ELoggButiker. /* */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExportButiker) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportButiker Procedure 
PROCEDURE ExportButiker :
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
           
    ASSIGN iAntButiker = 0. /* tyvärr eftersom jag gör detta flera gånger */
    
    OUTPUT TO VALUE(cExportFil + cFilsuffix) APPEND CONVERT TARGET "IBM850".
    
    FOR EACH TT_Butikk:
        EXPORT iButik
               TT_Butikk.wbutnr  
               TT_Butikk.aksjon   
               TT_Butikk.navn    
               TT_Butikk.adresse     
               TT_Butikk.postnr  
               TT_Butikk.poststed
               TT_Butikk.kontakt 
               TT_Butikk.telefon 
               TT_Butikk.butlager.
        ASSIGN iAntButiker = iAntButiker + 1.
    END.
    
    OUTPUT CLOSE.
    
    FILE-INFO:FILE-NAME = cExportFil + cFilsuffix.
    IF FILE-INFO:FILE-SIZE = 0 THEN
        OS-DELETE VALUE(cExportFil + cFilsuffix).
    ELSE IF NOT CAN-DO(cButikerFiler,cExportFil + cFilsuffix) THEN
        ASSIGN cButikerFiler = cButikerFiler + (IF cButikerFiler = "" THEN "" ELSE ",") + cExportFil + cFilsuffix.
    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixAlleButEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixAlleButEndringer Procedure 
PROCEDURE FixAlleButEndringer PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN FixButikerEndringer.
  RUN FixKjedensButikkerEndringer.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixButikerEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixButikerEndringer Procedure 
PROCEDURE FixButikerEndringer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iTest AS INTEGER    NO-UNDO.
  FOR EACH TT_ELogg WHERE 
              TT_ELogg.TabellNavn     = "Butiker" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 3:
          CREATE TT_Butikk.
          ASSIGN TT_Butikk.wbutnr = INT(TT_ELogg.Verdier)
                 TT_Butikk.aksjon = 2.
  END.
  IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = "Butiker" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 1  AND
              TT_Elogg.Verdier       = "ALLE") THEN DO:
      FOR EACH Butiker NO-LOCK:
          FIND Post WHERE Post.Postnr = Butiker.Buponr NO-LOCK NO-ERROR.
          ASSIGN iTest = INT(Butiker.BuPonr) NO-ERROR.
          IF ERROR-STATUS:ERROR OR iTest > 99999 THEN
              ASSIGN iTest = 0.
          CREATE TT_Butikk.
          ASSIGN TT_Butikk.wbutnr   = Butiker.Butik
                 TT_Butikk.aksjon   = 1
                 TT_Butikk.navn     = SUBSTR(Butiker.ButNamn,1,30)
                 TT_Butikk.adresse  = SUBSTR(Butiker.BuAdr,1,30)
                 TT_Butikk.postnr   = iTest
                 TT_Butikk.poststed = IF AVAIL Post THEN SUBSTR(Post.Beskrivelse,1,30) ELSE ""
                 TT_Butikk.kontakt  = SUBSTR(Butiker.BuKon,1,30)
                 TT_Butikk.telefon  = SUBSTR(Butiker.BuTel,1,15)
                 TT_Butikk.butlager = Butiker.NedlagtDato = ?.
      END.
  END.
  ELSE DO:
      FOR EACH TT_ELogg WHERE 
                  TT_ELogg.TabellNavn     = "Butiker" AND
                  TT_ELogg.EksterntSystem = "POS"  AND
                  TT_ELogg.EndringsType   = 1:
          FIND Butiker WHERE Butiker.butik = INT(TT_ELogg.Verdier) NO-LOCK NO-ERROR.
          IF AVAIL Butiker THEN DO:
              FIND Post WHERE Post.Postnr = Butiker.Buponr NO-LOCK NO-ERROR.
              ASSIGN iTest = INT(Butiker.BuPonr) NO-ERROR.
              IF ERROR-STATUS:ERROR OR iTest > 99999 THEN
                  ASSIGN iTest = 0.
              CREATE TT_Butikk.
              ASSIGN TT_Butikk.wbutnr   = Butiker.Butik
                     TT_Butikk.aksjon   = 1
                     TT_Butikk.navn     = SUBSTR(Butiker.ButNamn,1,30)
                     TT_Butikk.adresse  = SUBSTR(Butiker.BuAdr,1,30)
                     TT_Butikk.postnr   = iTest
                     TT_Butikk.poststed = IF AVAIL Post THEN SUBSTR(Post.Beskrivelse,1,30) ELSE ""
                     TT_Butikk.kontakt  = SUBSTR(Butiker.BuKon,1,30)
                     TT_Butikk.telefon  = SUBSTR(Butiker.BuTel,1,15)
                     TT_Butikk.butlager = TRUE.
          END.
      END.
  END.
  RELEASE TT_Butikk.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixKjedensButikkerEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixKjedensButikkerEndringer Procedure 
PROCEDURE FixKjedensButikkerEndringer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iTest AS INTEGER    NO-UNDO.
  FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = "Kjedensbutikker" AND
                          TT_ELogg.EksterntSystem = "POS"     AND
                          TT_ELogg.EndringsType   = 3         AND
                          NUM-ENTRIES(TT_Elogg.Verdier) = 4   AND
                          INT(ENTRY(4,TT_ELogg.Verdier)) < 1000:
      CREATE TT_Butikk.
      ASSIGN TT_Butikk.wbutnr = INT(ENTRY(4,TT_ELogg.Verdier))
             TT_Butikk.aksjon = 2.
  END.
  IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = "Kjedensbutikker" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 1  AND
              TT_Elogg.Verdier       = "ALLE") THEN DO:
      FOR EACH Kjedensbutikker WHERE Kjedensbutikker.ButikkNr < 1000 AND
                 NOT CAN-FIND(Butiker WHERE Butiker.Butik = Kjedensbutikker.ButikkNr) NO-LOCK:
          FIND Post WHERE Post.Postnr = Kjedensbutikker.PostNr NO-LOCK NO-ERROR.
          ASSIGN iTest = INT(Kjedensbutikker.PostNr) NO-ERROR.
          IF ERROR-STATUS:ERROR OR iTest > 99999 THEN
              ASSIGN iTest = 0.
          CREATE TT_Butikk.
          ASSIGN TT_Butikk.wbutnr   = Kjedensbutikker.ButikkNr
                 TT_Butikk.aksjon   = 1               
                 TT_Butikk.navn     = SUBSTR(KjedensButikker.ButikkNavn,1,30)
                 TT_Butikk.adresse  = SUBSTR(KjedensButikker.Adresse1,1,30)
                 TT_Butikk.postnr   = iTest
                 TT_Butikk.poststed = IF AVAIL Post THEN SUBSTR(Post.Beskrivelse,1,30) ELSE ""
                 TT_Butikk.kontakt  = SUBSTR(KjedensButikker.Kontaktperson,1,30)
                 TT_Butikk.telefon  = SUBSTR(KjedensButikker.Telefon,1,15)
                 TT_Butikk.butlager = FALSE.
      END.
  END.
  ELSE DO:
      FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = "Kjedensbutikker" AND TT_ELogg.EksterntSystem = "POS" AND
                              TT_ELogg.EndringsType   = 1 AND NUM-ENTRIES(TT_Elogg.Verdier) = 4 AND
                              INT(ENTRY(4,TT_ELogg.Verdier)) < 1000 AND
                              NOT CAN-FIND(Butiker WHERE Butiker.Butik = Kjedensbutikker.ButikkNr):
          FIND KjedensButikker WHERE KjedensButikker.ButikkNr = INT(ENTRY(4,TT_ELogg.Verdier)) NO-LOCK NO-ERROR.
          IF AVAIL KjedensButikker THEN DO:
              CREATE TT_Butikk.
              ASSIGN TT_Butikk.wbutnr   = Kjedensbutikker.ButikkNr
                     TT_Butikk.aksjon   = 1               
                     TT_Butikk.navn     = SUBSTR(KjedensButikker.ButikkNavn,1,30)
                     TT_Butikk.adresse  = SUBSTR(KjedensButikker.Adresse1,1,30)
                     TT_Butikk.postnr   = iTest
                     TT_Butikk.poststed = IF AVAIL Post THEN SUBSTR(Post.Beskrivelse,1,30) ELSE ""
                     TT_Butikk.kontakt  = SUBSTR(KjedensButikker.Kontaktperson,1,30)
                     TT_Butikk.telefon  = SUBSTR(KjedensButikker.Telefon,1,15)
                     TT_Butikk.butlager = FALSE.
          END.
      END.
  END.
  RELEASE TT_Butikk.

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
    IF CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "Butiker" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") OR
        CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "KjedensButikker" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") THEN 
    DO:

        FIND ELogg WHERE ELogg.TabellNavn     = "Butiker" AND
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
        FIND ELogg WHERE ELogg.TabellNavn     = "KjedensButikker" AND
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
        FOR EACH ELogg WHERE ELogg.TabellNavn = "Butiker" AND
                             ELogg.EksterntSystem = "POS" NO-LOCK:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg AND cPRSFtpButiker = '' THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
        FOR EACH ELogg WHERE ELogg.TabellNavn = "KjedensButikker" AND
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

&IF DEFINED(EXCLUDE-SlettTT_ELoggButiker) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTT_ELoggButiker Procedure 
PROCEDURE SlettTT_ELoggButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = "Butiker" AND
                       TT_ELogg.EksterntSystem = "POS".
        DELETE TT_ELogg.
    END.
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = "KjedensButikker" AND
                       TT_ELogg.EksterntSystem = "POS".
        DELETE TT_ELogg.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

