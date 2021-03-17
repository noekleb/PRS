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
DEFINE OUTPUT PARAMETER cProdFamFiler AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntKunder  AS INTEGER    NO-UNDO.

DEFINE VARIABLE  iCount         AS INTEGER                                NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER INIT "c:\home\lindbak\kasse" NO-UNDO.
DEFINE VARIABLE cTekst          AS CHARACTER                              NO-UNDO.
DEFINE VARIABLE iKasseEksportFormat AS INTEGER NO-UNDO.
DEFINE VARIABLE cDatoTekst      AS CHARACTER EXTENT 10 NO-UNDO.

DEFINE TEMP-TABLE TT_ProduktFamilie
/*  1 */ FIELD ProdFamId AS DECIMAL FORMAT ">>>>>>>>9"
/*  2 */ FIELD ProdFamNavn AS CHARACTER FORMAT "X(30)"
/*  3 */ FIELD ProdFamPrisLinje AS DECIMAL FORMAT ">>>,>>>,>9.99"
/*  4 */ FIELD ProdFamAutoReg AS LOG FORMAT "yes/no"
/*  5 */ FIELD ProdFamaktiv AS LOG FORMAT "yes/no"
/*  6 */ FIELD Aksjon AS INTEGER  
         .
DEFINE TEMP-TABLE TT_ProduktFamMedlem
/*  1 */ FIELD ProdFamId AS DECIMAL FORMAT ">>>>>>>>9"
/*  2 */ FIELD ProdFamArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>9"
/*  3 */ FIELD ProdFamStrKode AS INTEGER FORMAT ">>>>9"
         .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */
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
         WIDTH              = 60.4.
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
RUN FixProduktFamilieEndringer.
/* Här skall vi loopa runt alla kassor mm */
IF cFtpButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
      RUN ExportProduktFamilie IN THIS-PROCEDURE (INT(ENTRY(iCount,cFtpButiker)),ENTRY(iCount,cFtpButiker)). /* parameter = den loopade butiken */
  END.
END.
                                       /* + eventuellt filnamn */
RUN SlettTT_ELoggProduktFamilie. /* */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExportProduktFamilie) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportProduktFamilie Procedure 
PROCEDURE ExportProduktFamilie PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  TN   20/08-02 Kun kunder med betalingstype 2 legges ut. Andre kunder sendes
                det sletteposter (action 2) på.
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iButik     LIKE Butiker.Butik NO-UNDO.
    DEFINE INPUT  PARAMETER cFilSuffix AS CHARACTER  NO-UNDO.
    
    DEFINE VARIABLE iCount     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iNumeric   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE dDeciTst   AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE cString    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDato      AS CHARACTER  NO-UNDO.

    DEFINE VARIABLE  cNumericFormat AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cDateFormat    AS CHARACTER  NO-UNDO.
    
    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "EUROPEAN"
           SESSION:DATE-FORMAT    = "dmy".

    OUTPUT TO VALUE(cExportFil + cFilsuffix) APPEND.
    
    CREATE TT_ProduktFamilie.
    ASSIGN iAntKunder = 0. /* tyvärr, vi loopar runt butiker */

    /* Legger ut sletteposter for kunde.          */
    /* Dette lar vi gå til alle butikker uansett. */
    FOR EACH TT_Elogg WHERE
             TT_ELogg.TabellNavn     = "ProduktFamilie" AND
             TT_ELogg.EksterntSystem = "POS"   AND
             TT_ELogg.EndringsType   = 3:

        ASSIGN TT_ProduktFamilie.ProdFamId = DEC(ENTRY(1,TT_ELogg.Verdier,";")) 
               TT_ProduktFamilie.aksjon    = 3
               iAntKunder                  = iAntKunder + 1.

        ASSIGN cString = 'ProduktFamilie;3'  + ";" +
                         ENTRY(1,TT_ELogg.Verdier,";")  + ";;;;"
               iAntKunder      = iAntKunder + 1.
        PUT UNFORMATTED CString SKIP.
    END.
    
    /* Legger ut nye og endringer */
    FOR EACH TT_Elogg WHERE
             TT_Elogg.TabellNavn     = "ProduktFamilie" AND
             TT_ELogg.EksterntSystem = "POS"   AND
             TT_ELogg.EndringsType = 1:

        FIND ProduktFamilie NO-LOCK WHERE
          ProduktFamilie.ProdFamId = DEC(ENTRY(1,TT_ELogg.Verdier,";")) NO-ERROR.
        IF NOT AVAIL ProduktFamilie THEN 
            NEXT.

        ASSIGN
/*  1 og 2 */ cString = 'ProduktFamilie' + ';1;' +
/*  3 */               STRING(ProduktFamilie.ProdFamId) + ';' +             
/*  4 */               REPLACE(REPLACE(ProduktFamilie.ProdFamNavn,";",""),'"'," ") + ";" +
/*  5 */               STRING(ProduktFamilie.ProdFamPrisLinje) + ';' +                       
/*  6 */               STRING(ProduktFamilie.ProdFamAutoReg) + ';' +            
/*  7 */               STRING(ProduktFamilie.ProdFamaktiv)
                       NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            NEXT.
        PUT UNFORMATTED cString SKIP.

        FOR EACH ProduktFamMedlem OF ProduktFamilie WHERE 
            ProduktFamilie.ProdFamaktiv = TRUE 
            NO-LOCK:
              
            ASSIGN
/*  1 og 2 */ cString = 'ProduktFamMedlem' + ';1;' +
/*  2 */                STRING(ProduktFamMedlem.ProdFamId) + ";" +         
/*  3 */                STRING(ProduktFamMedlem.ProdFamArtikkelNr) + ";" +
/*  4 */                STRING(ProduktFamMedlem.ProdFamStrKode)   
              NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                NEXT.
            PUT UNFORMATTED cString SKIP.
        END.
    END.

    OUTPUT CLOSE.
    FILE-INFO:FILE-NAME = cExportFil + cFilsuffix.
    IF FILE-INFO:FILE-SIZE = 0 THEN
        OS-DELETE VALUE(cExportFil + cFilsuffix).
    ELSE IF NOT CAN-DO(cProdFamFiler,cExportFil + cFilsuffix) THEN
        ASSIGN cProdFamFiler = cProdFamFiler + (IF cProdFamFiler = "" THEN "" ELSE ",") + cExportFil + cFilsuffix.

    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixProduktFamilieEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixProduktFamilieEndringer Procedure 
PROCEDURE FixProduktFamilieEndringer PRIVATE :
/* /*------------------------------------------------------------------------------   */
/*   Purpose:                                                                         */
/*   Parameters:  <none>                                                              */
/*   Notes:                                                                           */
/* ------------------------------------------------------------------------------*/   */
     DEFINE VARIABLE iNr AS INTEGER    NO-UNDO.
     DEFINE BUFFER bTT_Elogg FOR TT_Elogg.

     /* Hvis ALLE kunder skal legges ut. */
     IF CAN-FIND(TT_ELogg WHERE TT_ELogg.TabellNavn     = "ProduktFamilie" AND
                                TT_ELogg.EksterntSystem = "POS"   AND
                                TT_ELogg.EndringsType = 1         AND
                                TT_ELogg.Verdier = "ALLE") THEN DO:
         /* Tar bort de andre loggpostene da disse ikke behøves. */
         FOR EACH bTT_ELogg WHERE bTT_ELogg.TabellNavn     = "ProduktFamilie" AND
                                  bTT_ELogg.EksterntSystem = "POS"   AND
                                  bTT_ELogg.EndringsType = 1:
             DELETE bTT_ELogg.
         END.
         FOR EACH ProduktFamilie WHERE ProduktFamilie.ProdFamAktiv = TRUE NO-LOCK:
                 IF CAN-FIND(bTT_ELogg WHERE bTT_ELogg.TabellNavn = "ProduktFamilie" AND
                       bTT_ELogg.EksterntSystem = "POS"  AND
                       bTT_ELogg.Verdier = STRING(ProduktFamilie.ProdFamId)) THEN
                     NEXT.
                 CREATE bTT_ELogg.
                 ASSIGN bTT_ELogg.TabellNavn     = "ProduktFamilie"
                        bTT_ELogg.EksterntSystem = "POS"  
                        bTT_ELogg.EndringsType   = 1
                        bTT_ELogg.Verdier        = STRING(ProduktFamilie.ProdFamId).
                 RELEASE bTT_ELogg.
         END.
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
    IF CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "ProduktFamilie" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") THEN DO:

        FIND ELogg WHERE ELogg.TabellNavn     = "ProduktFamilie" AND
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
        FOR EACH ELogg WHERE ELogg.TabellNavn = "ProduktFamilie" AND
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

&IF DEFINED(EXCLUDE-SlettTT_ELoggProduktFamilie) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTT_ELoggProduktFamilie Procedure 
PROCEDURE SlettTT_ELoggProduktFamilie :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = "ProduktFamilie" AND
                       TT_ELogg.EksterntSystem = "POS".
        DELETE TT_ELogg.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */
