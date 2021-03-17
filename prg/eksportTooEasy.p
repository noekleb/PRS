&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
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

DEFINE VARIABLE dStartdatum AS DATE        NO-UNDO.
DEFINE VARIABLE cFullFilnamn AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cDateFormat  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNumFormat AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cFilkatalog  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cKundNr      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFilnamn     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iAntdagar    AS INTEGER     NO-UNDO.
DEFINE VARIABLE cButikslistaPRS AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cButikslistaTooEasy AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFTPserver AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFTPpwd       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFTPuser      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFTPkatalog   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFTPrunScript AS CHARACTER   NO-UNDO.






DEFINE TEMP-TABLE rapport NO-UNDO
    FIELD butik AS INTE
    FIELD datum AS DATE
    FIELD timme AS INTE
    FIELD antal AS INTE
    FIELD summa AS DECI
    FIELD dummy AS INTE
    FIELD butikTooEasy AS CHAR
    INDEX bdt IS PRIMARY UNIQUE butik datum timme.

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
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 13.95
         WIDTH              = 42.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */


{syspara.i 210 203 1 cFilkatalog} 
{syspara.i 210 203 2 cKundNr} 
{syspara.i 210 203 3 iAntdagar INT} 
{syspara.i 210 203 4 cButikslistaPRS} 
{syspar2.i 210 203 4 cButikslistaTooEasy} 
{syspara.i 210 203 5 cFTPserver}
{syspara.i 210 203 6 cFTPuser}
{syspara.i 210 203 7 cFTPpwd}
{syspara.i 210 203 8 cFTPkatalog}

cFilkatalog = RIGHT-TRIM(cFilkatalog,"\").
RUN ByggData.
IF CAN-FIND(FIRST rapport) THEN DO:
    RUN Exportera.
/*     IF SEARCH("cmd\tooeasyftp.cmd") <> ? AND SEARCH(cFullFilnamn) <> ? THEN  */
/*         OS-COMMAND VALUE("cmd\brunngardftp.cmd" + " " + cFilNavn).           */
END.

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggData Procedure 
PROCEDURE ByggData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE daLoopDatum AS DATE        NO-UNDO.
    DEFINE VARIABLE iTimme      AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cTid        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iButik      AS INTEGER     NO-UNDO.
    DEFINE VARIABLE ii          AS INTEGER     NO-UNDO.
    DO ii = 1 TO NUM-ENTRIES(cButikslistaPRS):
        iButik = INT(ENTRY(ii,cButikslistaPRS)).
        DO daLoopDatum = TODAY - iAntdagar TO TODAY - 1:
            FOR EACH akt_rapp WHERE akt_rapp.dato  = daLoopDatum AND
                                    akt_rapp.butik = iButik NO-LOCK.
                /* vi har ev halvtimme. görs om till hel */
                IF akt_rapp.tid < 10000 THEN
                    iTimme = 1.
                ELSE DO:
                    cTid = STRING(akt_rapp.tid).
                    cTid = SUBSTR(cTid,1,LENGTH(cTid) - 4).
                    iTimme = INT(cTid).
                END.
                FIND rapport WHERE rapport.butik = akt_rapp.butik AND
                                   rapport.datum = akt_rapp.dato  AND
                                   rapport.timme = iTimme NO-ERROR.
                IF NOT AVAIL rapport THEN DO:
                    CREATE rapport.
                    ASSIGN rapport.butik = akt_rapp.butik
                           rapport.datum = akt_rapp.dato 
                           rapport.timme = iTimme
                           rapport.butikTooEasy = ENTRY(ii,cButikslistaTooEasy).
                END.
                ASSIGN rapport.antal = rapport.antal + (akt_rapp.ant_kunder)
                       rapport.summa = rapport.summa + akt_rapp.oms_verd + akt_rapp.mva_kr.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Exportera) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Exportera Procedure 
PROCEDURE Exportera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDT AS CHARACTER   NO-UNDO.
    
    cDateFormat  = SESSION:DATE-FORMAT.
    cNumFormat   = SESSION:NUMERIC-FORMAT.
    
    SESSION:DATE-FORMAT    = "ymd".
    
    cDT = ENTRY(1,STRING(NOW)).
    cDT = REPLACE(cDT,":","").
    cDT = REPLACE(cDT,"/","").
    cDT = REPLACE(cDT," ","").
    
    cFilnamn = cKundNr + "FSG" + cDT + ".csv".
    cFullFilnamn = cFilkatalog + "\" + cFilnamn.
    /* FILNAMNET måste skapas innan "AMERICAN" */
    SESSION:NUMERIC-FORMAT = "AMERICAN".
    
    OUTPUT TO VALUE(cFullFilnamn).
    FOR EACH rapport:
        PUT UNFORMATTED rapport.butikTooEasy ";"
                        rapport.datum FORMAT "9999-99-99" ";"
                        rapport.timme ";"
                        rapport.antal ";"
            TRIM(STRING(rapport.summa,"->>>>>>9.99")) SKIP.
    END.
    OUTPUT CLOSE.
    SESSION:DATE-FORMAT    = cDateFormat.
    SESSION:NUMERIC-FORMAT = cNumFormat.

    RUN sendFilerFTP.
/*                                                                            */
/*     IF SEARCH(cFTPserver) <> ? AND SEARCH(cFullFilnamn) <> ? THEN          */
/*         OS-COMMAND VALUE(cFTPserver + " " + cFTPpwd + " " + cFullFilnamn). */
/*                                                                            */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sendFilerFTP) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendFilerFTP Procedure 
PROCEDURE sendFilerFTP :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE cScript AS CHARACTER   NO-UNDO.

{syspara.i 210 203 5 cFTPserver}
{syspara.i 210 203 6 cFTPuser}
{syspara.i 210 203 7 cFTPpwd}
{syspara.i 210 203 8 cFTPkatalog}
{syspara.i 210 203 9 cFTPrunScript}


{syspara.i 210 203 1 cFilkatalog} 
{syspara.i 210 203 2 cKundNr} 
{syspara.i 210 203 3 iAntdagar INT} 
{syspara.i 210 203 4 cButikslistaPRS} 
{syspar2.i 210 203 4 cButikslistaTooEasy} 
{syspara.i 210 203 5 cFTPserver}
{syspara.i 210 203 6 cFTPuser}
{syspara.i 210 203 7 cFTPpwd}
{syspara.i 210 203 8 cFTPkatalog}

DEFINE VARIABLE cScriptFil AS CHARACTER   NO-UNDO.

cScriptFil = "script.ftp".

OUTPUT TO VALUE(cFilkatalog + "\" + cScriptFil).

PUT UNFORMATTED "open " cFTPserver SKIP
                cFTPuser SKIP
                cFTPpwd  SKIP
                "prompt n" SKIP
                "cd " cFTPkatalog SKIP
                "put " cFilnamn SKIP
                "quit" SKIP.

/* :: >>script.ftp ECHO dir                                                */
/* :: >>script.ftp ECHO mget *.*                                           */
/*                                                                         */
/* :: Use the temporary script for unattended FTP                          */
/* :: Note: depending on your OS version you may have to add a '-n' switch */

OUTPUT CLOSE.

OS-COMMAND SILENT VALUE(cFTPrunScript + " " + cFilkatalog + " " + cScriptFil + " " + cFilnamn).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

