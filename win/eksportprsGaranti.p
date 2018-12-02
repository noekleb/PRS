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
DEFINE OUTPUT PARAMETER cGarantiFiler AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER iAntGaranti   AS INTEGER    NO-UNDO.

DEFINE VARIABLE  iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER INIT "c:\home\lindbak\kasse" NO-UNDO.
DEFINE VARIABLE cTekst          AS CHARACTER                              NO-UNDO.

DEFINE TEMP-TABLE TT_Garanti
    FIELD garantikl  LIKE Garanti.garantikl    
    FIELD aksjon     AS INTEGER FORMAT "9"     /* I (1)      Posttype, 1=ny/endring, 9=sletting)                     */
    FIELD mndant LIKE Garanti.mndant
    FIELD bonga5 LIKE Garanti.bonga5
    FIELD radnr  AS INTEGER
    FIELD garantitekst AS CHARACTER  
    FIELD fritekst AS CHAR EXTENT 5 FORMAT "x(70)"
    INDEX Aksjon aksjon DESCENDING Garantikl ASCENDING
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
RUN FixGarantiendringer.
/* Här skall vi loopa runt alla kassor mm */
IF cFtpButiker <> "" THEN DO:
  DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
      RUN ExportGaranti IN THIS-PROCEDURE (INT(ENTRY(iCount,cFtpButiker)),ENTRY(iCount,cFtpButiker)). /* parameter = den loopade butiken */
  END.
END.
                                       /* + eventuellt filnamn */
RUN SlettTT_ELoggGaranti. /* */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExportGaranti) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportGaranti Procedure 
PROCEDURE ExportGaranti :
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

    ASSIGN iAntGaranti = 0. /* tyvärr eftersom jag gör detta flera gånger */
    OUTPUT TO VALUE(cExportFil + cFilsuffix) APPEND.
    FOR EACH TT_Garanti WHERE 
        TT_Garanti.Garantikl > 0:
        
        cString = "GARANTI;1;" + 
                   STRING(TT_Garanti.aksjon) + ";" +
                   STRING(TT_Garanti.Garantikl) + ";" + 
                   STRING(TT_Garanti.mndant) + ";" + 
                   STRING(TT_Garanti.bonga5) + ";" + 
                   STRING(TT_Garanti.radnr) + ";" + 
                   STRING(TT_Garanti.garantitekst) + ";" + 
                   REPLACE(REPLACE(TT_Garanti.fritekst[1],";",""),'"'," ")  + ";" +  
                   REPLACE(REPLACE(TT_Garanti.fritekst[2],";",""),'"'," ")  + ";" +  
                   REPLACE(REPLACE(TT_Garanti.fritekst[3],";",""),'"'," ")  + ";" +  
                   REPLACE(REPLACE(TT_Garanti.fritekst[4],";",""),'"'," ")  + ";" +  
                   REPLACE(REPLACE(TT_Garanti.fritekst[5],";",""),'"'," ")  
                   NO-ERROR.
            
        PUT UNFORMATTED cString SKIP.
        ASSIGN iAntGaranti = iAntGaranti + 1.
    END.
    OUTPUT CLOSE.

    FILE-INFO:FILE-NAME = cExportFil + cFilsuffix.
    IF FILE-INFO:FILE-SIZE = 0 THEN
        OS-DELETE VALUE(cExportFil + cFilsuffix).
    ELSE IF NOT CAN-DO(cGarantiFiler,cExportFil + cFilsuffix) THEN
        ASSIGN cGarantiFiler = cGarantiFiler + (IF cGarantiFiler = "" THEN "" ELSE ",") + cExportFil + cFilsuffix.
        
    ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
           SESSION:DATE-FORMAT    = cDateFormat.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixGarantiendringer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixGarantiendringer Procedure 
PROCEDURE FixGarantiendringer PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cFritekstTMP      AS CHARACTER NO-UNDO.
  FOR EACH TT_ELogg WHERE 
              TT_ELogg.TabellNavn     = "Garanti" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 3:
          CREATE TT_Garanti.
          ASSIGN TT_Garanti.garantikl = INT(TT_ELogg.Verdier)
                 TT_Garanti.aksjon  = 9.
  END.
  IF CAN-FIND(FIRST TT_ELogg WHERE TT_ELogg.TabellNavn = "Garanti" AND
              TT_ELogg.EksterntSystem = "POS"    AND
              TT_ELogg.EndringsType   = 1  AND
              TT_Elogg.Verdier       = "ALLE") THEN DO:
      FOR EACH Garanti NO-LOCK:
          RUN FritekstTMP(OUTPUT cFritekstTMP).
          CREATE TT_Garanti.
          ASSIGN TT_Garanti.Garantikl    = Garanti.Garantikl
                 TT_Garanti.aksjon  = 1
                 TT_Garanti.mndant          = Garanti.mndant
                 TT_Garanti.bonga5          = Garanti.bonga5
                 TT_Garanti.radnr           = 1
                 TT_Garanti.garantitekst    = Garanti.garantitekst.
          DO iCount = 1 TO 5:
              ASSIGN TT_Garanti.fritekst[iCount] = ENTRY(iCount,cFritekstTMP,CHR(1)).
          END.
          CREATE TT_Garanti.
          ASSIGN TT_Garanti.Garantikl    = Garanti.Garantikl
                 TT_Garanti.aksjon  = 1
                 TT_Garanti.mndant          = Garanti.mndant
                 TT_Garanti.bonga5          = Garanti.bonga5
                 TT_Garanti.radnr           = 2
                 TT_Garanti.garantitekst    = Garanti.garantitekst.
          DO iCount = 6 TO 10:
              ASSIGN TT_Garanti.fritekst[iCount - 5] = ENTRY(iCount,cFritekstTMP,CHR(1)).
          END.
          CREATE TT_Garanti.
          ASSIGN TT_Garanti.Garantikl    = Garanti.Garantikl
                 TT_Garanti.aksjon  = 1
                 TT_Garanti.mndant          = Garanti.mndant
                 TT_Garanti.bonga5          = Garanti.bonga5
                 TT_Garanti.radnr           = 3
                 TT_Garanti.garantitekst    = Garanti.garantitekst.
          DO iCount = 11 TO 15:
              ASSIGN TT_Garanti.fritekst[iCount - 10] = ENTRY(iCount,cFritekstTMP,CHR(1)).
          END.
      END.
  END.
  ELSE DO:
      FOR EACH TT_ELogg WHERE 
                  TT_ELogg.TabellNavn     = "Garanti" AND
                  TT_ELogg.EksterntSystem = "POS"  AND
                  TT_ELogg.EndringsType   = 1:
          FIND Garanti WHERE Garanti.Garantikl = INT(TT_ELogg.Verdier) NO-LOCK NO-ERROR.
          IF AVAIL Garanti THEN DO:
              RUN FritekstTMP(OUTPUT cFritekstTMP).
              CREATE TT_Garanti.
              ASSIGN TT_Garanti.Garantikl    = Garanti.Garantikl
                     TT_Garanti.aksjon  = 1
                     TT_Garanti.mndant          = Garanti.mndant
                     TT_Garanti.bonga5          = Garanti.bonga5
                     TT_Garanti.radnr           = 1
                     TT_Garanti.garantitekst    = Garanti.garantitekst.
              DO iCount = 1 TO 5:
                  ASSIGN TT_Garanti.fritekst[iCount] = ENTRY(iCount,cFritekstTMP,CHR(1)).
              END.
              CREATE TT_Garanti.
              ASSIGN TT_Garanti.Garantikl    = Garanti.Garantikl
                     TT_Garanti.aksjon  = 1
                     TT_Garanti.mndant          = Garanti.mndant
                     TT_Garanti.bonga5          = Garanti.bonga5
                     TT_Garanti.radnr           = 2
                     TT_Garanti.garantitekst    = Garanti.garantitekst.
              DO iCount = 6 TO 10:
                  ASSIGN TT_Garanti.fritekst[iCount - 5] = ENTRY(iCount,cFritekstTMP,CHR(1)).
              END.
              CREATE TT_Garanti.
              ASSIGN TT_Garanti.Garantikl    = Garanti.Garantikl
                     TT_Garanti.aksjon  = 1
                     TT_Garanti.mndant          = Garanti.mndant
                     TT_Garanti.bonga5          = Garanti.bonga5
                     TT_Garanti.radnr           = 3
                     TT_Garanti.garantitekst    = Garanti.garantitekst.
              DO iCount = 11 TO 15:
                  ASSIGN TT_Garanti.fritekst[iCount - 10] = ENTRY(iCount,cFritekstTMP,CHR(1)).
              END.
          END.
      END.
  END.
  RELEASE TT_Garanti.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FritekstTMP) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FritekstTMP Procedure 
PROCEDURE FritekstTMP :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER cFritekstTMP      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iEntry AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cString        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCount         AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cFritekst      AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cTxt15         AS CHARACTER EXTENT 15 NO-UNDO.
    DEFINE VARIABLE iIndex         AS INTEGER    NO-UNDO.
    DO iCount = 1 TO NUM-ENTRIES(TRIM(garanti.fritekst),CHR(10)):
        ASSIGN cFritekst = TRIM(ENTRY(iCount,TRIM(garanti.fritekst),CHR(10))).
        IF LENGTH(cFritekst) = 0 AND iIndex = 0 THEN
            NEXT.
        IF LENGTH(cFritekst) < 71 THEN DO:
            ASSIGN iIndex = iIndex + 1
                   cTxt15[iIndex] = cFritekst.
        END.
        ELSE DO WHILE TRIM(cFritekst) <> "":
            ASSIGN cString = SUBSTR(cFriTekst,1,40).
            IF LENGTH(TRIM(cString)) < 71 THEN DO:
                ASSIGN iIndex = iIndex + 1
                       cTxt15[iIndex] = cString
                       cFriTekst = IF cString = "" THEN "" ELSE TRIM(REPLACE(cFritekst,cString,"")).
            END.
            ELSE DO iCount = 70 TO 1 BY -1:
                IF SUBSTR(cString,iCount,1) = " " THEN DO:
                    ASSIGN cString = SUBSTR(cString,1,iCount - 1)
                           iIndex = iIndex + 1
                           cTxt15[iIndex] = cString
                           cFriTekst = IF cString = "" THEN "" ELSE TRIM(REPLACE(cFritekst,cString,"")).
                    LEAVE.
                END.
            END.
            IF iIndex = 15 THEN
                LEAVE.
        END.
        IF iIndex = 15 THEN
            LEAVE.
    END.
    ASSIGN cFritekstTMP = FILL(CHR(1),14).
    DO iCount = 1 TO 15.
        ASSIGN ENTRY(iCount,cFritekstTMP,CHR(1)) = cTxt15[iCount].
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FritekstTMPOld) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FritekstTMPOld Procedure 
PROCEDURE FritekstTMPOld :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER cFritekstTMP      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER iAntEksportPoster AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cFritekstTMP15 AS CHARACTER EXTENT 15 NO-UNDO.
    DEFINE VARIABLE cString        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cStringTMP     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCount         AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cFritekst      AS CHARACTER  NO-UNDO.
    DO iCount = 1 TO NUM-ENTRIES(garanti.fritekst,CHR(10)):
        IF TRIM(ENTRY(iCount,garanti.fritekst,CHR(10))) = "" THEN
            NEXT.
        ASSIGN cFritekst = cFritekst + (IF cFritekst <> "" THEN " " ELSE "") + TRIM(ENTRY(iCount,garanti.fritekst,CHR(10))).
    END.
    REPEAT:
        ASSIGN cString = SUBSTR(cFritekst,1,71).
        IF LENGTH(TRIM(cString)) = 0 THEN
            LEAVE.
        ASSIGN iAntEksportPoster = iAntEksportPoster + 1.
        IF LENGTH(TRIM(cString)) < 71 THEN
            ASSIGN cString = TRIM(cString).
        ELSE DO: 
            ASSIGN cStringTMP = "".
            DO iCount = 1 TO NUM-ENTRIES(cString," ") - 1:
                ASSIGN cStringTMP = cStringTMP + (IF cStringTMP <> "" THEN " " ELSE "") +
                                                    ENTRY(iCount,cString," ").
            END.
            ASSIGN cString = cStringTMP.
        END.
        ASSIGN cFritekst = TRIM(REPLACE(cFriTekst,cString,""))
               cFritekstTMP15[iAntEksportPoster] = cString.
    END.
    DO iCount = 1 TO iAntEksportPoster.
        ASSIGN cFritekstTMP = cFritekstTMP + (IF cFritekstTMP <> "" THEN "," ELSE "") + cFritekstTMP15[iCount].
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
    IF CAN-FIND(ELogg WHERE ELogg.TabellNavn     = "Garanti" AND
                            ELogg.EksterntSystem = "POS"    AND
                            ELogg.Verdier        = "KLARGJOR") THEN DO:

        FIND ELogg WHERE ELogg.TabellNavn     = "Garanti" AND
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
        FOR EACH ELogg WHERE ELogg.TabellNavn = "Garanti" AND
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

&IF DEFINED(EXCLUDE-SlettTT_ELoggGaranti) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettTT_ELoggGaranti Procedure 
PROCEDURE SlettTT_ELoggGaranti :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn     = "Garanti" AND
                       TT_ELogg.EksterntSystem = "POS".
        DELETE TT_ELogg.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

