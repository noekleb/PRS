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
DEFINE INPUT  PARAMETER cFtpButiker   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cExportFil    AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cButikerFiler AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntbutiker   AS INTEGER    NO-UNDO.
DEFINE VARIABLE  iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER INIT "c:\home\lindbak\kasse" NO-UNDO.
DEFINE VARIABLE cTekst          AS CHARACTER                              NO-UNDO.
DEFINE VARIABLE cDatoTekst        AS CHARACTER EXTENT 10 NO-UNDO.

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

RUN KopierElogg.
/* Här skall vi loopa runt alla kassor mm */
IF cFtpButiker <> "" THEN DO:
  IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = "Butiker" AND TT_ELogg.EksterntSystem = "POS") THEN
      DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
          RUN ExportButiker IN THIS-PROCEDURE (INT(ENTRY(iCount,cFtpButiker)),ENTRY(iCount,cFtpButiker)). /* parameter = den loopade butiken */
      END.
  IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = "ekstbutiker" AND TT_ELogg.EksterntSystem = "POS") THEN
      DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
          RUN ExportExtButiker IN THIS-PROCEDURE (INT(ENTRY(iCount,cFtpButiker)),ENTRY(iCount,cFtpButiker)). /* parameter = den loopade butiken */
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
    DEFINE VARIABLE  cString           AS CHARACTER  NO-UNDO.
    
    ASSIGN iAntButiker = 0. /* tyvärr eftersom jag gör detta flera gånger */
    DEFINE VARIABLE  cNumericFormat AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cDateFormat    AS CHARACTER  NO-UNDO.
    
    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "EUROPEAN"
           SESSION:DATE-FORMAT    = "dmy".
    
    OUTPUT TO VALUE(cExportFil + cFilsuffix) APPEND.

    IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = "Butiker" AND
                                 TT_ELogg.EksterntSystem = "POS"   AND
                                 TT_ELogg.EndringsType   = 1       AND
                                 TT_Elogg.Verdier = "ALLE") THEN 
    DO:
        FOR EACH Butiker NO-LOCK:
            FIND Post NO-LOCK WHERE
              Post.PostNr = Butiker.BuPoNr NO-ERROR.
            
            cDatoTekst[1] = DatoChar(Butiker.NedlagtDato). 
            
            cString = "BUTIKER;" + (IF Butiker.NedlagtDato <> ? THEN "3;" ELSE "1;") +
/*             "1;" + /* Alltid endringstype = 1 */ */
            STRING(Butiker.Butik) + ";" + 
            REPLACE(REPLACE(Butiker.ButNamn,";",""),'"'," ") + ";" + 
            REPLACE(REPLACE(Butiker.BuAdr,";",""),'"'," ")  + ";" +
            STRING(Butiker.Buponr)  + ";" +
            (IF AVAIL Post THEN Post.Beskrivelse ELSE "")  + ";" + 
            Butiker.BuKon + ";" +
            Butiker.BuTel + ";" +
            cDatoTekst[1]
            NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                NEXT.
            ELSE 
                PUT UNFORMATTED cString SKIP.
            ASSIGN iAntButiker = iAntButiker + 1.
        END.
    END.
    
    ELSE DO:
        /* SLetteposter */
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = "Butiker" AND
                                TT_ELogg.EksterntSystem = "POS" AND
                                TT_ELogg.EndringsType   = 3 BY TT_ELogg.Verdier:
            cString = "BUTIKER;" + 
                  STRING(TT_ELogg.EndringsType) + ";" + /* Alltid endringstype = 1 */
                   TT_ELogg.Verdier + ";;;;;;;"
                   NO-ERROR.
            PUT UNFORMATTED cString SKIP.       
            ASSIGN iAntButiker = iAntButiker + 1.
        END.
        
        /* Nye/endrede */
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = "Butiker" AND
                                TT_ELogg.EksterntSystem = "POS" AND
                                TT_ELogg.EndringsType   = 1 BY TT_ELogg.Verdier:
          FIND Butiker NO-LOCK WHERE
            Butiker.Butik = INT(TT_ELogg.Verdier) NO-ERROR.
          IF AVAILABLE Butiker THEN 
          DO:     
            FIND Post NO-LOCK WHERE
              Post.PostNr = Butiker.BuPoNr NO-ERROR.
            cString = "BUTIKER;" + (IF Butiker.NedlagtDato <> ? THEN "3;" ELSE "1;") +
/*                   STRING(TT_ELogg.EndringsType) + ";" + /* Alltid endringstype = 1 */ */
                  STRING(Butiker.Butik) + ";" + 
                  REPLACE(REPLACE(Butiker.ButNamn,";",""),'"'," ") + ";" + 
                  REPLACE(REPLACE(Butiker.BuAdr,";",""),'"'," ")  + ";" +
                  STRING(Butiker.Buponr)  + ";" +
                  (IF AVAIL Post THEN Post.Beskrivelse ELSE "")  + ";" + 
                  Butiker.BuKon + ";" +
                  Butiker.BuTel + ";" +
                  STRING(Butiker.NedlagtDato = ?)
                  NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                NEXT.
            ELSE 
                PUT UNFORMATTED cString SKIP.       
            ASSIGN iAntButiker = iAntButiker + 1.
          END.
        END.
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

&IF DEFINED(EXCLUDE-ExportExtButiker) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportExtButiker Procedure 
PROCEDURE ExportExtButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iextButik     LIKE ekstbutiker.Butik NO-UNDO.
    DEFINE INPUT  PARAMETER cFilSuffix AS CHARACTER  NO-UNDO.
    
    DEFINE VARIABLE         iCount     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE  cString           AS CHARACTER  NO-UNDO.
    
    ASSIGN iAntButiker = 0. /* tyvärr eftersom jag gör detta flera gånger */
    DEFINE VARIABLE  cNumericFormat AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE  cDateFormat    AS CHARACTER  NO-UNDO.
    
    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "EUROPEAN"
           SESSION:DATE-FORMAT    = "dmy".
    
    OUTPUT TO VALUE(cExportFil + cFilsuffix) APPEND.

    IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = "ekstbutiker" AND
                                 TT_ELogg.EksterntSystem = "POS"   AND
                                 TT_ELogg.EndringsType   = 1       AND
                                 TT_Elogg.Verdier = "ALLE") THEN 
    DO:
        FOR EACH ekstbutiker NO-LOCK:
            cString = "EKSTBUTIKER;" + 
            "1;" + /* Alltid endringstype = 1 */
            STRING(ekstbutiker.Butik) + ";" + 
            REPLACE(REPLACE(ekstbutiker.ButNamn,";",""),'"'," ") + ";" + 
            STRING(ekstbutiker.Buponr)  + ";" +
            ekstbutiker.BuKon + ";" +
            ekstbutiker.BuTel + ";" +
            cDatoTekst[1]
            NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                NEXT.
            ELSE 
                PUT UNFORMATTED cString SKIP.
            ASSIGN iAntButiker = iAntButiker + 1.
        END.
    END.
    
    ELSE DO:
        /* SLetteposter */
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = "ekstbutiker" AND
                                TT_ELogg.EksterntSystem = "POS" AND
                                TT_ELogg.EndringsType   = 3 BY TT_ELogg.Verdier:
            cString = "EKSTBUTIKER;" + 
                  STRING(TT_ELogg.EndringsType) + ";" + /* Alltid endringstype = 1 */
                   TT_ELogg.Verdier + ";;"
                   NO-ERROR.
            PUT UNFORMATTED cString SKIP.       
            ASSIGN iAntButiker = iAntButiker + 1.
        END.
        
        /* Nye/endrede */
        FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = "ekstbutiker" AND
                                TT_ELogg.EksterntSystem = "POS" AND
                                TT_ELogg.EndringsType   = 1 BY TT_ELogg.Verdier:
          FIND ekstbutiker NO-LOCK WHERE
            ekstbutiker.Butik = INT(TT_ELogg.Verdier) NO-ERROR.
          IF AVAILABLE ekstbutiker THEN 
          DO:     
            cString = "EKSTBUTIKER;" + 
                  STRING(TT_ELogg.EndringsType) + ";" + /* Alltid endringstype = 1 */
                  STRING(ekstbutiker.Butik) + ";" + 
                  REPLACE(REPLACE(ekstbutiker.ButNamn,";",""),'"'," ") + ";" + 
                  ekstbutiker.kortnavn
                  NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                NEXT.
            ELSE 
                PUT UNFORMATTED cString SKIP.       
            ASSIGN iAntButiker = iAntButiker + 1.
          END.
        END.
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
        CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "ekstbutiker" AND
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
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
        FIND ELogg WHERE ELogg.TabellNavn     = "ekstbutiker" AND
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
        FOR EACH ELogg WHERE ELogg.TabellNavn = "Butiker" AND
                             ELogg.EksterntSystem = "POS" NO-LOCK:
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF AVAIL bElogg THEN
                DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
        FOR EACH ELogg WHERE ELogg.TabellNavn = "ekstbutiker" AND
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
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = "ekstbutiker" AND
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

