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
DEFINE INPUT  PARAMETER cFtpButiker  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cExportFil   AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cSelgerFiler AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntSelgere  AS INTEGER    NO-UNDO.
DEFINE VARIABLE  iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER INIT "c:\home\lindbak\kasse" NO-UNDO.
DEFINE VARIABLE cTekst          AS CHARACTER                              NO-UNDO.
DEFINE VARIABLE cDatoTekst        AS CHARACTER EXTENT 10 NO-UNDO.

DEFINE BUFFER    bTT_Elogg FOR TT_Elogg.

DEFINE TEMP-TABLE TT_ButSelgTilEksport
    FIELD Butik  LIKE Butiker.Butik
    FIELD SelgerId LIKE ButikkSelger.SelgerId
    FIELD Slettes AS LOGICAL
    INDEX BSF IS PRIMARY Butik Slettes SelgerId.

DEFINE TEMP-TABLE TT_Selger
    FIELD butnr        AS INTEGER FORMAT ">>>>>9"     /* I (3)             Butikknr                                                */
    FIELD selgernr       AS INTEGER FORMAT ">>9"     /* I (3)             Selgernr                                              */
    FIELD fornavn      AS CHARACTER FORMAT "x(15)" /* C (15)    Fornavn                                                 */
    FIELD aksjon       AS INTEGER FORMAT "9"       /* I (1)             Posttype, 1=ny/endring, 9=sletting)                     */
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

&IF DEFINED(EXCLUDE-DatoChar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DatoChar Procedure 
FUNCTION DatoChar RETURNS CHARACTER
    ( INPUT dDato AS DATE ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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
RUN FixButikkSelgerEndringer.

IF cFtpButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
      RUN ExportButikkSelger IN THIS-PROCEDURE (INT(ENTRY(iCount,cFtpButiker)),ENTRY(iCount,cFtpButiker)). /* parameter = den loopade butiken */
  END.
END.
                                       /* + eventuellt filnamn */
RUN SlettTT_ELoggButikkSelger. /* */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExportButikkSelger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportButikkSelger Procedure 
PROCEDURE ExportButikkSelger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iButik     LIKE Butiker.Butik NO-UNDO.
    DEFINE INPUT  PARAMETER cFilSuffix AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE         iCount     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cString            AS CHARACTER  NO-UNDO.
    
    DEFINE VARIABLE  cNumericFormat AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cDateFormat    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cPwd AS CHARACTER   NO-UNDO.
    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "EUROPEAN"
           SESSION:DATE-FORMAT    = "dmy".
        
    OUTPUT TO VALUE(cExportFil + cFilsuffix) APPEND.
    
    CREATE TT_Selger.
    ASSIGN TT_Selger.butnr = iButik.

    FOR EACH TT_ButSelgTilEksport WHERE TT_ButSelgTilEksport.Butik = iButik:
       IF TT_ButSelgTilEksport.Slettes = TRUE THEN DO:
           cString = "SELGER;" + 
              STRING(IF TT_ButSelgTilEksport.Slettes = FALSE THEN 1 ELSE 3) + ";" +
              STRING(TT_ButSelgTilEksport.SelgerId) + ";"
              NO-ERROR.
           PUT UNFORMATTED cString SKIP.
       END.
       ELSE DO:
       
           FIND ButikkSelger WHERE ButikkSelger.Butikk = iButik AND 
                                   ButikkSelger.SelgerId = TT_ButSelgTilEksport.SelgerId NO-LOCK NO-ERROR.
           IF AVAIL ButikkSelger THEN DO:
               FIND Selger WHERE Selger.SelgerNr = ButikkSelger.SelgerNr NO-LOCK NO-ERROR.
               IF AVAIL Selger THEN DO:
                   IF Selger.deciPWD > 0 THEN DO:
                       cPWD = STRING(Selger.deciPWD).
                       IF INT(SUBSTR(cPWD,1,1)) < 5 THEN
                           cPWD = "A" + STRING(3 * Selger.deciPWD).
                       ELSE
                           cPWD = "B" + STRING(5 * Selger.deciPWD).
                   END.
                   ELSE
                       cPWD = "R". /* reset */
                   cDatoTekst[1] = DatoChar(Selger.FodtDato). 
       
                   cString = "SELGER;" + 
                      STRING(IF TT_ButSelgTilEksport.Slettes = FALSE THEN 1 ELSE 3) + ";" +
                      STRING(ButikkSelger.SelgerId) + ";" + 
                      REPLACE(REPLACE(Selger.NavnIKasse,";",""),'"'," ") + ";" +
                      STRING(Selger.SelgerNr) + ";" +       
                      STRING(Selger.Navn) + ";" +          
                      STRING(Selger.AnsattNr) + ";" +      
                      STRING(Selger.Adresse1) + ";" +      
                      STRING(Selger.Telefon) + ";" +       
                      STRING(Selger.PersonNr) + ";" +      
                      STRING(Selger.Mobiltelefon) + ";" +  
                      STRING(Selger.PostNr) + ";" +        
                      STRING(Selger.Adresse2) + ";" +      
                      STRING(Selger.ButikkNr) + ";" +      
                      STRING(Selger.BrukeridPRS) + ";" +   
                      STRING(Selger.ForNavn) + ";" +       
                      STRING(Selger.JobTittel) + ";" +     
                      cDatoTekst[1] + ";" +
                      cPWD
                      NO-ERROR.
                   IF ERROR-STATUS:ERROR THEN
                       NEXT.
                   ELSE
                       PUT UNFORMATTED cString SKIP.
               END.
           END.
       END.
    END.
    OUTPUT CLOSE.
    
    FILE-INFO:FILE-NAME = cExportFil + cFilsuffix.
    IF FILE-INFO:FILE-SIZE = 0 THEN
        OS-DELETE VALUE(cExportFil + cFilsuffix).
    ELSE IF NOT CAN-DO(cSelgerFiler,cExportFil + cFilsuffix) THEN
        ASSIGN cSelgerFiler = cSelgerFiler + (IF cSelgerFiler = "" THEN "" ELSE ",") + cExportFil + cFilsuffix.

    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixButikkSelgerEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixButikkSelgerEndringer Procedure 
PROCEDURE FixButikkSelgerEndringer :
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
          TT_ELogg.TabellNavn     = "Selger" AND
          TT_ELogg.EksterntSystem = "POS"     AND
          TT_ELogg.EndringsType   = 1:
        FOR EACH ButikkSelger WHERE ButikkSelger.SelgerNr = INT(TT_Elogg.Verdier) NO-LOCK.
            FIND FIRST bTT_Elogg WHERE bTT_ELogg.TabellNavn     = "butikkSelger" AND
                                       bTT_ELogg.EksterntSystem = "POS"           AND
                                       bTT_Elogg.Verdier        = STRING(ButikkSelger.Butik) + CHR(1) + STRING(ButikkSelger.SelgerId) NO-ERROR.
            IF NOT AVAIL bTT_Elogg THEN DO:
                CREATE bTT_Elogg.
                ASSIGN bTT_ELogg.TabellNavn     = "butikkSelger"
                       bTT_ELogg.EksterntSystem = "POS"
                       bTT_ELogg.EndringsType   = 1
                       bTT_Elogg.Verdier        = STRING(ButikkSelger.Butik) + CHR(1) + string(ButikkSelger.SelgerId).
/*                        bTT_Elogg.Verdier = STRING(ButikkSelger.Butik) + CHR(1) + TT_Elogg.Verdier. */
            END.
            ELSE
                bTT_ELogg.EndringsType = 1.
            RELEASE bTT_Elogg.
        END.
        DELETE TT_ELogg.
    END.
    FOR EACH TT_Elogg WHERE
        TT_ELogg.TabellNavn     = "butikkSelger" AND
        TT_ELogg.EksterntSystem = "POS"           AND
        TT_ELogg.EndringsType = 3:

        CREATE TT_ButSelgTilEksport.
        ASSIGN TT_ButSelgTilEksport.Butik   = INT(ENTRY(1,TT_ELogg.Verdier,CHR(1)))
               TT_ButSelgTilEksport.SelgerId  = INT(ENTRY(2,TT_ELogg.Verdier,CHR(1)))
               TT_ButSelgTilEksport.Slettes = TRUE
               iAntSelgere                  = iAntSelgere + 1.
        RELEASE TT_ButSelgTilEksport.
    END.
    IF CAN-FIND(FIRST TT_Elogg WHERE TT_ELogg.TabellNavn = "butikkSelger" AND
                                     TT_ELogg.EksterntSystem = "POS" AND
                                     TT_ELogg.Verdier = "ALLE") THEN DO:
        DO iCount = 1 TO NUM-ENTRIES(cButiker):
            FOR EACH ButikkSelger WHERE ButikkSelger.Butik = INT(ENTRY(iCount,cButiker)):
                CREATE TT_ButSelgTilEksport.
                ASSIGN TT_ButSelgTilEksport.Butik   = ButikkSelger.Butik
                       TT_ButSelgTilEksport.SelgerId  = ButikkSelger.SelgerId
                       TT_ButSelgTilEksport.Slettes = FALSE
                       iAntSelgere                = iAntSelgere + 1.
                RELEASE TT_ButSelgTilEksport.
            END.
        END.
    END.
    ELSE FOR EACH TT_Elogg WHERE
        TT_ELogg.TabellNavn     = "butikkSelger" AND
        TT_ELogg.EksterntSystem = "POS"           AND
        TT_ELogg.EndringsType = 1:
        CREATE TT_ButSelgTilEksport.
        ASSIGN TT_ButSelgTilEksport.Butik   = INT(ENTRY(1,TT_ELogg.Verdier,CHR(1)))
               TT_ButSelgTilEksport.SelgerId  = INT(ENTRY(2,TT_ELogg.Verdier,CHR(1)))
               TT_ButSelgTilEksport.Slettes = FALSE
               iAntSelgere                = iAntSelgere + 1.
        RELEASE TT_ButSelgTilEksport.
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
    IF CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "Selger" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") OR 
        CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "butikkSelger" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") THEN DO:

        FIND ELogg WHERE ELogg.TabellNavn     = "Selger" AND
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
        FIND ELogg WHERE ELogg.TabellNavn     = "butikkSelger" AND
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
        FOR EACH ELogg WHERE ELogg.TabellNavn = "Selger" AND
                             ELogg.EksterntSystem = "POS" NO-LOCK:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
        FOR EACH ELogg WHERE ELogg.TabellNavn = "butikkSelger" AND
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

&IF DEFINED(EXCLUDE-SlettTT_ELoggButikkSelger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTT_ELoggButikkSelger Procedure 
PROCEDURE SlettTT_ELoggButikkSelger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = "butikkSelger" AND
                       TT_ELogg.EksterntSystem = "POS".
        DELETE TT_ELogg.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-DatoChar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DatoChar Procedure 
FUNCTION DatoChar RETURNS CHARACTER
    ( INPUT dDato AS DATE ):
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
        DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
        
        cTekst = IF dDato = ? 
                   THEN '00000000' 
                   ELSE (
                         STRING(YEAR(dDato),'9999') + 
                         STRING(MONTH(dDato),'99') + 
                         STRING(DAY(dDato),'99')
                        ).
        RETURN cTekst.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

