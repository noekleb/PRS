&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
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

DEFINE VARIABLE hServer AS HANDLE      NO-UNDO.
DEFINE VARIABLE cConnectionString AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lConnected AS LOGICAL     NO-UNDO.

DEFINE VARIABLE cPF AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMailTo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.

DEFINE VAR cKundnamn  AS CHARACTER   NO-UNDO.
DEFINE VAR dtAskTime  AS DATETIME    NO-UNDO.
DEFINE VAR dtLastTime AS DATETIME    NO-UNDO.
DEFINE VAR deMSgrens  AS DECIMAL     NO-UNDO.
DEFINE VAR lOK        AS LOGICAL     NO-UNDO.
DEFINE VAR cMessage   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBody AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE ttWebKommKontroll NO-UNDO
    FIELD pf        AS CHAR
    FIELD kund      AS CHAR
    FIELD asconnect AS CHAR
    FIELD mailto    AS CHAR.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

TEMP-TABLE ttWebKommKontroll:READ-JSON("file",SESSION:PARAMETER,"empty").

FIND FIRST ttWebKommKontroll NO-ERROR.
IF AVAIL ttWebKommKontroll THEN DO:

    RUN connectdb.p(ttWebKommKontroll.pf) NO-ERROR.
    IF NOT CONNECTED("skotex") THEN DO:
        
        RUN WebKommKontroll_STDsendmail.p(ttWebKommKontroll.mailto,
                                          "WebKommKontroll " + ttWebKommKontroll.kund,
                                          "Ingen kontakt med skotex.",
                                          "").
    END.
    ELSE DO:
    /*     connecta appserver */
        CREATE SERVER hServer. 
        lConnected = hServer:CONNECT(ttWebKommKontroll.asconnect) NO-ERROR.
        IF lConnected THEN DO: 
            RUN asWebKommKontroll.p ON hServer (OUTPUT cKundnamn, OUTPUT dtAskTime, OUTPUT dtLastTime,OUTPUT deMSgrens, OUTPUT lOK, OUTPUT cMessage).
            cBody = cMessage.
            hServer:DISCONNECT().
    
            IF NOT lOK THEN
                RUN WebKommKontroll_STDsendmail.p(ttWebKommKontroll.mailto,
                                              "WebKommKontroll " + cKundnamn,
                                              cBody,
                                              "").
            ELSE DO:
                IF dtAskTime - dtLastTime > deMSgrens THEN DO:
                    cBody = "Senaste kommunikation " + STRING(dtLastTime) + ". Gräns passerad".
                    RUN WebKommKontroll_STDsendmail.p(ttWebKommKontroll.mailto,
                                                  "WebKommKontroll " + cKundnamn,
                                                  cBody,
                                                  "").
                END.
            END.
        END. 
        ELSE DO:
            lOK = FALSE.
            cBody = "".
            RUN WebKommKontroll_STDsendmail.p(ttWebKommKontroll.mailto,
                                              "WebKommKontroll " + ttWebKommKontroll.kund,
                                              "Ingen kontakt med appserver.",
                                              "").
        END.
        DELETE OBJECT hServer.
    
    END.
END.

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


