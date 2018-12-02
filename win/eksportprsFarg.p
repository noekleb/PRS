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
DEFINE OUTPUT PARAMETER cFargeFiler AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntFarg    AS INTEGER    NO-UNDO.
DEFINE VARIABLE  iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER INIT "c:\home\lindbak\kasse" NO-UNDO.
DEFINE VARIABLE cTekst          AS CHARACTER                              NO-UNDO.


/* TN 7/10-08 Oppdatert format. */
DEFINE TEMP-TABLE TT_Farge NO-UNDO
    FIELD fargenr    AS INTEGER FORMAT ">>>>9"    /* I (5 Øket fra 4 til 5) Fargeenr */
    FIELD fargetekst AS CHARACTER FORMAT "x(30)"  /* C (30 Øket fra 10 til 30)     Fargheetekst */
    FIELD aksjon     AS INTEGER FORMAT "9"        /* I (1)      Posttype, 1=ny/endring, 9=sletting)                     */
    INDEX Aksjon aksjon DESCENDING fargenr ASCENDING
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
RUN FixFargEndringer.
/* Här skall vi loopa runt alla kassor mm */
IF cFtpButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
      RUN ExportFarg IN THIS-PROCEDURE (INT(ENTRY(iCount,cFtpButiker)),ENTRY(iCount,cFtpButiker)). /* parameter = den loopade butiken */
  END.
END.
                                       /* + eventuellt filnamn */
RUN SlettTT_ELoggFarg. /* */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExportFarg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportFarg Procedure 
PROCEDURE ExportFarg :
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
    
    ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
           cDateFormat            = SESSION:DATE-FORMAT
           SESSION:NUMERIC-FORMAT = "EUROPEAN"
           SESSION:DATE-FORMAT    = "dmy".

    ASSIGN iAntFarg = 0. /* tyvärr eftersom jag gör detta flera gånger */
    OUTPUT TO VALUE(cExportFil + cFilsuffix) APPEND.

    FOR EACH TT_Farge WHERE 
        TT_Farge.FargeNr > 0:
        cString = "FARG" + ";" +
                   STRING(TT_Farge.aksjon) + ";" +
                   STRING(TT_Farge.fargenr) + ";" + 
                   TRIM(REPLACE(REPLACE(TT_Farge.fargetekst,";",""),'"'," "))
                   NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            NEXT.
        ELSE 
            PUT UNFORMATTED cString SKIP.
        ASSIGN iAntFarg = iAntFarg + 1.
    END.
    
    OUTPUT CLOSE.
    FILE-INFO:FILE-NAME = cExportFil + cFilsuffix.
    IF FILE-INFO:FILE-SIZE = 0 THEN
        OS-DELETE VALUE(cExportFil + cFilsuffix).
    ELSE IF NOT CAN-DO(cFargeFiler,cExportFil + cFilsuffix) THEN
        ASSIGN cFargeFiler = cFargeFiler + (IF cFargeFiler = "" THEN "" ELSE ",") + cExportFil + cFilsuffix.
        
    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixFargEndringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixFargEndringer Procedure 
PROCEDURE FixFargEndringer PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH TT_ELogg WHERE 
              TT_ELogg.TabellNavn     = "Farg" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 3:
          CREATE TT_Farge.
          ASSIGN TT_Farge.fargenr = INT(TT_ELogg.Verdier)
                 TT_Farge.aksjon  = TT_ELogg.EndringsType.
  END.
  IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = "Farg" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 1  AND
              TT_Elogg.Verdier       = "ALLE") THEN DO:
      FOR EACH Farg NO-LOCK:
          CREATE TT_Farge.
          ASSIGN TT_Farge.fargenr    = Farg.Farg
                 TT_Farge.fargetekst = Farg.FarBeskr /* Farg.KFarge  */
                 TT_Farge.aksjon  = 1.
      END.
  END.
  ELSE DO:
      FOR EACH TT_ELogg WHERE 
                  TT_ELogg.TabellNavn     = "Farg" AND
                  TT_ELogg.EksterntSystem = "POS"  AND
                  TT_ELogg.EndringsType   = 1:
          FIND Farg WHERE Farg.Farg = INT(TT_ELogg.Verdier) NO-LOCK NO-ERROR.
          IF AVAIL Farg THEN DO:
              CREATE TT_Farge.
              ASSIGN TT_Farge.fargenr = Farg.Farg
                     TT_Farge.fargetekst = Farg.FarBeskr
                     TT_Farge.aksjon  = 1.
          END.
      END.
  END.
  RELEASE TT_Farge.
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
    IF CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "Farg" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") THEN DO:

        FIND ELogg WHERE ELogg.TabellNavn     = "Farg" AND
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
        FOR EACH ELogg WHERE ELogg.TabellNavn = "Farg" AND
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

&IF DEFINED(EXCLUDE-SlettTT_ELoggFarg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTT_ELoggFarg Procedure 
PROCEDURE SlettTT_ELoggFarg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = "Farg" AND
                       TT_ELogg.EksterntSystem = "POS".
        DELETE TT_ELogg.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

