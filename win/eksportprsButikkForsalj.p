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
DEFINE INPUT  PARAMETER cFtpButiker    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cExportFil     AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cKassererFiler AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntKasserere  AS INTEGER    NO-UNDO.
DEFINE VARIABLE  iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER INIT "c:\home\lindbak\kasse" NO-UNDO.
DEFINE VARIABLE cTekst          AS CHARACTER                              NO-UNDO.
DEFINE VARIABLE iKasseEksportFormat AS INTEGER NO-UNDO.

DEFINE BUFFER    bTT_Elogg FOR TT_Elogg.

DEFINE TEMP-TABLE TT_ButForsTilEksport
    FIELD Butik  LIKE Butiker.Butik
    FIELD ForsNr LIKE Forsalj.ForsNr
    FIELD Slettes AS LOGICAL
    INDEX BSF IS PRIMARY Butik Slettes ForsNr.

DEFINE TEMP-TABLE TT_Kasserer
    FIELD butnr        AS INTEGER FORMAT ">>9"     /* I (3)             Butikknr                                                */
    FIELD kassnr       AS INTEGER FORMAT ">>9"     /* I (3)             Kasserernr                                              */
    FIELD passord      AS INTEGER FORMAT ">>9"     /* I (3)             Passord                                                 */
    FIELD fornavn      AS CHARACTER FORMAT "x(15)" /* C (15)    Fornavn                                                 */
    FIELD aksjon       AS INTEGER FORMAT "9"       /* I (1)             Posttype, 1=ny/endring, 2=sletting)                     */
    FIELD girabatt     AS INTEGER FORMAT "9"       /* I (1)             Lov å gi manuell rabatt (0=Nei, 1=passord/kort; 2=Ja)   */
    FIELD endrepris    AS INTEGER FORMAT "9"       /* I (1)             Lov å endre prisen (0=Nei, 1=passord/kort; 2=Ja)        */
    FIELD dato         AS DATE                     /* Da            Fødelsedato (format "dd-mm-yyyy"                        */
    FIELD taretur      AS INTEGER FORMAT "9"       /* I (1)             Lov å ta imot returer (0=Nei, 1=passord/kort; 2=Ja)     */
    FIELD sletttidl    AS INTEGER FORMAT "9"       /* I (1)             Lov å slette varelinjer (0=Nei, 1=passord/kort; 2=Ja)   */
    FIELD slettbong    AS INTEGER FORMAT "9"       /* I (1)             Lov å makulere bong (0=Nei, 1=passord/kort; 2=Ja)       */
    FIELD slettforst   AS INTEGER FORMAT "9"       /* I (1)             Lov å slette 1. varelinje (0=Nei, 1=passord/kort; 2=Ja) */
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

/* Setter datoformat for eksport til kassen. */
{syspara.i 1 1 55 iKasseEksportFormat INT}

/* Vi exporterar direkt från ELogg till fil */
/* Ingen extra behandling behöver göras */
RUN KopierElogg.
/* Kanske vi skall hämta iformation om kassor och filer först. */
RUN FixButikkForsaljEndringer.
/* Här skall vi loopa runt alla kassor mm */
IF cFtpButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
      RUN ExportButikkForsalj IN THIS-PROCEDURE (INT(ENTRY(iCount,cFtpButiker)),ENTRY(iCount,cFtpButiker)). /* parameter = den loopade butiken */
  END.
END.
                                       /* + eventuellt filnamn */
RUN SlettTT_ELoggButikkForsalj. /* */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExportButikkForsalj) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportButikkForsalj Procedure 
PROCEDURE ExportButikkForsalj :
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
    DEFINE VARIABLE cString            AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDato              AS CHARACTER  NO-UNDO.
    
    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "EUROPEAN"
           SESSION:DATE-FORMAT    = "dmy".   
    
    OUTPUT TO VALUE(cExportFil + cFilsuffix) APPEND.
    CREATE TT_Kasserer.
    ASSIGN TT_Kasserer.butnr = iButik.

    FOR EACH TT_ButForsTilEksport WHERE TT_ButForsTilEksport.Butik = iButik:
       IF TT_ButForsTilEksport.Slettes = TRUE THEN 
       DO:
         cString = "FORSALJ;3;" + 
                   STRING(TT_ButForsTilEksport.Forsnr)
                   NO-ERROR.
         PUT UNFORMATTED cString SKIP.
       END.

       ELSE DO:
           FIND butikkforsalj WHERE butikkforsalj.Butik = iButik AND 
               butikkforsalj.KassererId = TT_ButForsTilEksport.Forsnr NO-LOCK NO-ERROR.
           FIND Forsalj WHERE Forsalj.Forsnr = butikkforsalj.Forsnr NO-LOCK NO-ERROR.
           IF AVAIL Forsalj THEN 
           DO:
               cDato   = STRING(YEAR(Forsalj.FodtDato),"9999") +
                         STRING(MONTH(Forsalj.FodtDato),"99")  +
                         STRING(DAY(Forsalj.FodtDato),"99").
               IF cDato = ? THEN 
                 cDato = '00000000'.
                   
               cString = "FORSALJ" + ";" +
                         "1" + ";" +
                         STRING(TT_ButForsTilEksport.Forsnr) + ";" + 
                         STRING(Forsalj.passord) + ";" + 
                         Forsalj.navnikasse
/*                          STRING(Forsalj.Rabatt) + ";" +                                          */
/*                          STRING(Forsalj.Prisendring) + ";" +                                     */
/* /*                          cDato  + ";" + */                                                    */
/*                          STRING(Forsalj.Retur)  + ";" +                                          */
/*                          STRING(Forsalj.slettTidligere)  + ";" +                                 */
/*                          STRING(Forsalj.SlettBong)  + ";" +                                      */
/*                          STRING(Forsalj.SletteForste)  + ";" +                                   */
/*                          STRING(Forsalj.FoAnstNr)  + ";" +                                       */
/*                          STRING(Forsalj.FoNamn)  + ";" +                                         */
/*                          STRING(Forsalj.FoAdr)  + ";" +                                          */
/*                          STRING(Forsalj.FoPoNr)  + ";" +                                         */
/*                          STRING(Forsalj.FoPadr)  + ";" +                                         */
/*                          STRING(Forsalj.FoTel)  + ";" +                                          */
/*                          STRING(Forsalj.FoPersNr)  + ";" +                                       */
/*                          STRING(Forsalj.AnsattNr)  + ";" +                                       */
/*                          STRING(Forsalj.Retur)  + ";" +                                          */
/*                          (IF Forsalj.FodtDato = ? THEN '' ELSE STRING(Forsalj.FodtDato)) + ";" + */
/*                          STRING(Forsalj.ForsaljAktiv)  + ";" +                                   */
/*                          STRING(Forsalj.BrukerId2)  + ";" +                                      */
/*                          STRING(Forsalj.ButikkNr)  + ";" +                                       */
/*                          STRING(Forsalj.BrukeridPRS)  + ";" +                                    */
/*                          STRING(Forsalj.FoForNavn)  + ";" +                                      */
/*                          STRING(Forsalj.Jobbtittel)  + ";" +                                     */
/*                          STRING(Forsalj.FoAdr2)                                                  */
                         NO-ERROR.
               IF ERROR-STATUS:ERROR THEN
                   NEXT.
               ELSE 
                   PUT UNFORMATTED cString SKIP.
           END.
       END.
    END.
    OUTPUT CLOSE.
    
    FILE-INFO:FILE-NAME = cExportFil + cFilsuffix.
    IF FILE-INFO:FILE-SIZE = 0 THEN
        OS-DELETE VALUE(cExportFil + cFilsuffix).
    ELSE IF NOT CAN-DO(cKassererFiler,cExportFil + cFilsuffix) THEN
        ASSIGN cKassererFiler = cKassererFiler + (IF cKassererFiler = "" THEN "" ELSE ",") + cExportFil + cFilsuffix.

    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixButikkForsaljEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixButikkForsaljEndringer Procedure 
PROCEDURE FixButikkForsaljEndringer PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* InfoPos kasse tar bara emot nya */
    DEFINE VARIABLE cButiker AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCount   AS INTEGER    NO-UNDO.
    ASSIGN cButiker = cFtpButiker.
    
    FOR EACH TT_Elogg WHERE
          TT_ELogg.TabellNavn     = "Forsalj" AND
          TT_ELogg.EksterntSystem = "POS"     AND
          TT_ELogg.EndringsType   = 1:
        FOR EACH ButikkForsalj WHERE ButikkForsalj.ForsNr = INT(TT_Elogg.Verdier) NO-LOCK.
        IF NOT CAN-FIND(FIRST bTT_Elogg WHERE bTT_ELogg.TabellNavn     = "ButikkForsalj" AND
                                                  bTT_ELogg.EksterntSystem = "POS"           AND
                                                  bTT_ELogg.EndringsType = 1                 AND
                                                  bTT_Elogg.Verdier = STRING(ButikkForsalj.Butik) + CHR(1) +
                                                  STRING(ButikkForsalj.KassererId)) THEN DO:
                CREATE bTT_Elogg.
                ASSIGN bTT_ELogg.TabellNavn     = "ButikkForsalj"           
                       bTT_ELogg.EksterntSystem = "POS"
                       bTT_ELogg.EndringsType = 1
                       bTT_Elogg.Verdier = STRING(ButikkForsalj.Butik) + CHR(1) + STRING(ButikkForsalj.KassererId).
                RELEASE bTT_Elogg.
            END.
                
        END.
        DELETE TT_ELogg.
    END.
    
    FOR EACH TT_Elogg WHERE
        TT_ELogg.TabellNavn     = "ButikkForsalj" AND
        TT_ELogg.EksterntSystem = "POS"           AND
        TT_ELogg.EndringsType = 3:

        CREATE TT_ButForsTilEksport.
        ASSIGN TT_ButForsTilEksport.Butik   = INT(ENTRY(1,TT_ELogg.Verdier,CHR(1)))
               TT_ButForsTilEksport.ForsNr  = INT(ENTRY(2,TT_ELogg.Verdier,CHR(1)))
               TT_ButForsTilEksport.Slettes = TRUE
               iAntKasserere                = iAntKasserere + 1.
        RELEASE TT_ButForsTilEksport.
    END.
    
    IF CAN-FIND(FIRST TT_Elogg WHERE TT_ELogg.TabellNavn = "ButikkForsalj" AND
                                     TT_ELogg.EksterntSystem = "POS" AND
                                     TT_ELogg.Verdier = "ALLE") THEN DO:
        DO iCount = 1 TO NUM-ENTRIES(cButiker):
            FOR EACH ButikkForsalj WHERE ButikkForsalj.Butik = INT(ENTRY(iCount,cButiker)):
                CREATE TT_ButForsTilEksport.
                ASSIGN TT_ButForsTilEksport.Butik   = ButikkForsalj.Butik
                       TT_ButForsTilEksport.ForsNr  = ButikkForsalj.KassererId
                       TT_ButForsTilEksport.Slettes = FALSE
                       iAntKasserere                = iAntKasserere + 1.
                RELEASE TT_ButForsTilEksport.
            END.
        END.
    END.
    ELSE FOR EACH TT_Elogg WHERE
        TT_ELogg.TabellNavn     = "ButikkForsalj" AND
        TT_ELogg.EksterntSystem = "POS"           AND
        TT_ELogg.EndringsType = 1:
        CREATE TT_ButForsTilEksport.
        ASSIGN TT_ButForsTilEksport.Butik   = INT(ENTRY(1,TT_ELogg.Verdier,CHR(1)))
               TT_ButForsTilEksport.ForsNr  = INT(ENTRY(2,TT_ELogg.Verdier,CHR(1)))
               TT_ButForsTilEksport.Slettes = FALSE
               iAntKasserere                = iAntKasserere + 1.
        RELEASE TT_ButForsTilEksport.
    END.
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
    IF CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "Forsalj" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") OR
        CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "ButikkForsalj" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") THEN DO:

        FIND ELogg WHERE ELogg.TabellNavn     = "Forsalj" AND
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
        FIND ELogg WHERE ELogg.TabellNavn     = "ButikkForsalj" AND
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
        FOR EACH ELogg WHERE ELogg.TabellNavn = "Forsalj" AND
                             ELogg.EksterntSystem = "POS" NO-LOCK:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
        FOR EACH ELogg WHERE ELogg.TabellNavn = "ButikkForsalj" AND
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

&IF DEFINED(EXCLUDE-SlettTT_ELoggButikkForsalj) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTT_ELoggButikkForsalj Procedure 
PROCEDURE SlettTT_ELoggButikkForsalj :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = "ButikkForsalj" AND
                       TT_ELogg.EksterntSystem = "POS".
        DELETE TT_ELogg.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

