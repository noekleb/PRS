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
DEF VAR obOk                 AS LOG NO-UNDO.
DEF VAR cConnectStringSkotex AS CHAR NO-UNDO.
DEF VAR cConnectStringData   AS CHAR NO-UNDO.

DEF VAR iAnt        AS INT  NO-UNDO.
DEF VAR i2Ant       AS INT  NO-UNDO.
DEF VAR cLinje      AS CHAR NO-UNDO.
DEF VAR iLinjeNr    AS INT  NO-UNDO.
DEF VAR cIpLst      AS CHAR NO-UNDO.
DEF VAR cPeriodeLst AS CHAR NO-UNDO.
DEFINE VARIABLE cLoggFil    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cErrLoggFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE cJobNr AS CHARACTER NO-UNDO.

DEF STREAM Inn.
DEF STREAM Ut.

DEFINE TEMP-TABLE ttComTbl NO-UNDO 
    FIELD cButikk       AS CHAR FORMAT "x(20)"
    FIELD cIpAdr        AS CHAR FORMAT "x(10)"
    FIELD cPortNr       AS CHAR FORMAT "x(10)"
    FIELD cDbNavn       AS CHAR FORMAT "x(20)"
    FIELD cLdbNavn      AS CHAR FORMAT "x(20)"
    FIELD cDivParam     AS CHAR FORMAT "x(30)"
    FIELD cDataPortNr   AS CHAR FORMAT "x(10)"
    FIELD cDataDbNavn   AS CHAR FORMAT "x(20)"
    FIELD cDataLdbNavn  AS CHAR FORMAT "x(20)"
    FIELD cDataDivParam AS CHAR FORMAT "x(30)"
    INDEX Butikk IS PRIMARY UNIQUE cButikk.

DEFINE TEMP-TABLE ttPeriode NO-UNDO 
    FIELD cButikk   AS CHAR FORMAT "x(20)"
    FIELD FraDato AS DATE FORMAT "99/99/99"
    FIELD TilDato AS DATE FORMAT "99/99/99"
    FIELD Postert AS LOG 
    INDEX Periode IS PRIMARY UNIQUE cButikk.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-Ping) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Ping Procedure 
FUNCTION Ping RETURNS LOGICAL
  ( INPUT cHost AS CHAR )  FORWARD.

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

DO iLoop = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
    IF ENTRY(1,ENTRY(iLoop,SESSION:PARAMETER),"=") = "JOBNR" THEN
        cJobNr = ENTRY(2,ENTRY(iLoop,SESSION:PARAMETER),"=").
END.

ASSIGN
    cIpLst      = 'PRS_' + cJobNr + '_but_ipListe.csv'
    cPeriodeLst = 'PRS_' + cJobNr + '_but_periode.csv'.
RUN setLoggFilNavn.

RUN bibl_loggDbFri.p (cLoggFil, '').
RUN bibl_loggDbFri.p (cErrLoggFil, '').
RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p: innsamlingsrutine startet.').

/* Leser inn konfigurasjonsfiler. IP liste og Periode liste. */
RUN InnlesKonfig (OUTPUT obOk).
IF obOk = FALSE THEN
  DO:
    RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p: innsamlingsrutine AVBRUTT.').
    RETURN.
  END.

/* Henter data fra butikkene */
BUTIKKLOOP:
FOR EACH ttComTbl:
  RUN setLoggFilNavn.
  RUN dbDisconnect (OUTPUT obOk).

  /* Connect parametre */
  ASSIGN
      cConnectStringSkotex =   ' -db ' + ttComTbl.cDbNavn  
                             + ' -ld ' + ttComTbl.cLDbNavn 
                             + ' -H '  + ttComTbl.cIpAdr   
                             + ' -S '  + ttComTbl.cPortNr  
                             + ' -N tcp '
                             + ttComTbl.cDivParam  

      cConnectStringData   =   ' -db ' + ttComTbl.cDataDbNavn  
                             + ' -ld ' + ttComTbl.cDataLDbNavn 
                             + ' -H '  + ttComTbl.cIpAdr   
                             + ' -S '  + ttComTbl.cDataPortNr  
                             + ' -N tcp '
                             + ttComTbl.cDataDivParam  
      .
  /* Koble på databae */
  obOk = FALSE.
  RUN dbConnect (OUTPUT obOk).
  IF obOk = FALSE THEN
  DO:
      RUN dbDisconnect (OUTPUT obOk).
      NEXT.
  END.

  /* Les inn data */
  obOk = FALSE.
  RUN dbCollect (OUTPUT obOk).

  /* Koble fra database */
  RUN dbDisconnect (OUTPUT obOk).
  
END. /* BUTIKKLOOP */

RUN LagreKonfig (OUTPUT obOk).

RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p: innsamlingsrutine avsluttet.').

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-dbCollect) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dbCollect Procedure 
PROCEDURE dbCollect :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER bOk AS LOG NO-UNDO.
    
    DEF VAR pdDato      AS DATE NO-UNDO.
    DEF VAR ocRetur     AS CHAR NO-UNDO.
    DEF VAR iAntEksport AS INT NO-UNDO.
    DEFINE VARIABLE bLog AS LOG NO-UNDO.


    bOk = FALSE.

    FIND ttPeriode WHERE 
        ttPeriode.cButikk = ttComTbl.cButikk NO-ERROR.
    IF NOT AVAILABLE ttPeriode THEN
    DO:
        CREATE ttPeriode.
        ASSIGN
            ttPeriode.cButikk = ttComTbl.cButikk.
    END.
    ASSIGN 
        ttPeriode.FraDato = TODAY - 10
        ttPeriode.TilDato = TODAY.

    IF NOT AVAILABLE ttPeriode THEN
    DO:
        RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p: dbCollect: Finner ikke perioderecord' 
                          + ' Butikk: ' + ttComTbl.cButikk    
                          ).
        RETURN.
    END.
    IF ttPeriode.TilDato > TODAY THEN
        ttPeriode.TilDato = TODAY.

    DATOLOOP:
    DO pdDato = ttPeriode.FraDato TO ttPeriode.TilDato:
        STOP_BLOKK:
        DO ON STOP UNDO, RETRY:
            IF RETRY THEN
            DO:
                RUN bibl_loggDbFri.p (cErrLoggFil, 'hkDataInnsamling.p: dbCollect: ** Tap av DB oppkobling ved kjøring av eksportsalgHKPRSStg.p (STOP)' 
                              + ' Butikk: ' + ttComTbl.cButikk    
                              ).
                bOk = FALSE.
                UNDO STOP_BLOKK, LEAVE DATOLOOP.
            END.
            DO ON ERROR UNDO, RETRY:
              IF RETRY THEN
              DO:
                  RUN bibl_loggDbFri.p (cErrLoggFil, 'hkDataInnsamling.p: dbCollect: ** Feil ved kjøring av eksportsalgHKPRSStg.p (ERROR)' 
                                + ' Butikk: ' + ttComTbl.cButikk    
                                ).
                  bOk = FALSE.
                  UNDO, LEAVE DATOLOOP.
              END.
              RUN eksportsalgHKPRSStg.p (pdDato, pdDato, FALSE, '', OUTPUT ocRetur, OUTPUT iAntEksport).
              ASSIGN 
                ttPeriode.FraDato = pdDato.
              RUN LagreKonfig(OUTPUT bLog).
            END.
        END. /* STOP_BLOKK */
        ASSIGN
            bOk = TRUE
            .
        RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p: dbCollect: ' 
                          + ' Butikk: ' + ttComTbl.cButikk    
                          + ' Dato: ' + STRING(pdDato)    
                          + ' Antall poster: ' + STRING(iAntEksport)    
                          + ' ocRetur: ' + ocRetur    
                          ).
    END. /* DATOLOOP */
    IF bOk THEN
        ASSIGN
            ttPeriode.Postert = TRUE
            ttPeriode.FraDato = (TODAY + 1) - 5 
            ttPeriode.TilDato = (TODAY + 1) 
            .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dbConnect) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dbConnect Procedure 
PROCEDURE dbConnect :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  
  
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER bOk AS LOG NO-UNDO.

    bOk = TRUE.

    /* Sjekker om butikkens PC er tilgjengelig. */
    IF DYNAMIC-FUNCTION('Ping',ttComTbl.cIpAdr) THEN 
        RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p: dbConnect: Vellykket PING mot' 
                              + ' Butikk: ' + ttComTbl.cButikk    
                              + ' IpAdresse: ' + ttComTbl.cIpAdr    
                             ).
    ELSE  DO:
        RUN bibl_loggDbFri.p (cErrLoggFil, 'hkDataInnsamling.p: dbConnect: ** Feil ved PING mot' 
                              + ' Butikk: ' + ttComTbl.cButikk    
                              + ' IpAdresse: ' + ttComTbl.cIpAdr    
                             ).
        bOk = FALSE.
        RETURN.
    END.
    
    RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p: dbConnect: ' 
                      + ' SkotexParam: ' + cConnectStringSkotex    
                      + ' DataParam: ' + cConnectStringData    
                      ).

    DO ON STOP UNDO, RETRY:
        IF RETRY THEN
        DO:
            RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p: dbConnect: ** Feil ved CONNECT (STOP) av ' 
                              + ' Butikk: ' + ttComTbl.cButikk    
                              + ' DB: ' + ttComTbl.cLdbNavn    
                              + ' Status: ' 
                              + STRING(ERROR-STATUS:ERROR)
                              ).
            UNDO, LEAVE.
        END.
        DO ON ERROR UNDO, RETRY:
            IF RETRY THEN 
            DO:
                RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p: dbConnect: ** Feil ved CONNECT (ERROR) av ' 
                                  + ' Butikk: ' + ttComTbl.cButikk    
                                  + ' DB: ' + ttComTbl.cLdbNavn    
                                  + ' Status: ' 
                                  + STRING(ERROR-STATUS:ERROR)
                                  ).
                UNDO, LEAVE.
            END. 
            CONNECT VALUE(cConnectStringSkotex) NO-ERROR.
        END.
    END.
    IF NOT CONNECTED(ttComTbl.cLdbNavn) THEN
    DO:
        RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p: dbConnect: ** Feil ved oppkobling av ' 
                          + ' Butikk: ' + ttComTbl.cButikk    
                          + ' DB: ' + ttComTbl.cLdbNavn    
                          + ' Status: ' 
                          + STRING(ERROR-STATUS:ERROR)
                          ).
        RUN bibl_loggDbFri.p (cErrLoggFil, 'hkDataInnsamling.p: dbConnect: ** Feil ved oppkobling av ' 
                          + ' Butikk: ' + ttComTbl.cButikk    
                          + ' DB: ' + ttComTbl.cLdbNavn    
                          + ' Status: ' 
                          + STRING(ERROR-STATUS:ERROR)
                          ).
        bOk = FALSE.
    END.
    ELSE DO:
        RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p: dbConnect: oppkoblet ' 
                          + ' Butikk: ' + ttComTbl.cButikk    
                          + ' DB: ' + ttComTbl.cLdbNavn    
                          ).
    END.

    DO ON STOP UNDO, RETRY:
        IF RETRY THEN
        DO:
            RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p: dbConnect: ** Feil ved CONNECT av ' 
                              + ' Butikk: ' + ttComTbl.cButikk    
                              + ' DB: ' + ttComTbl.cDataLdbNavn    
                              + ' Status: ' 
                              + STRING(ERROR-STATUS:ERROR)
                              ).
            UNDO, LEAVE.
        END.
        DO ON ERROR UNDO, RETRY:
            IF RETRY THEN 
            DO:
                RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p: dbConnect: ** Feil ved CONNECT (ERROR) av ' 
                                  + ' Butikk: ' + ttComTbl.cButikk    
                                  + ' DB: ' + ttComTbl.cDataLdbNavn    
                                  + ' Status: ' 
                                  + STRING(ERROR-STATUS:ERROR)
                                  ).
                UNDO, LEAVE.
            END. 
            CONNECT VALUE(cConnectStringData) NO-ERROR.
        END.
    END.
    IF NOT CONNECTED(ttComTbl.cDataLdbNavn) THEN
    DO:
        RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p: dbConnect: ** Feil ved oppkobling av ' 
                          + ' Butikk: ' + ttComTbl.cButikk    
                          + ' DB: ' + ttComTbl.cDataLdbNavn    
                          + ' Status: ' 
                          + STRING(ERROR-STATUS:ERROR)
                          ).
        RUN bibl_loggDbFri.p (cErrLoggFil, 'hkDataInnsamling.p: dbConnect: ** Feil ved oppkobling av ' 
                          + ' Butikk: ' + ttComTbl.cButikk    
                          + ' DB: ' + ttComTbl.cDataLdbNavn    
                          + ' Status: ' 
                          + STRING(ERROR-STATUS:ERROR)
                          ).
        bOk = FALSE.
    END.
    ELSE DO:
        RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p: dbConnect: oppkoblet ' 
                          + ' Butikk: ' + ttComTbl.cButikk    
                          + ' DB: ' + ttComTbl.cDataLdbNavn    
                          ).
    END.
    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-dbDisconnect) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dbDisconnect Procedure 
PROCEDURE dbDisconnect :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER bOk AS LOG NO-UNDO.

    bOk = TRUE.

    IF CONNECTED(ttComTbl.cLDbNavn) OR 
       CONNECTED(ttComTbl.cDataLDbNavn) THEN 
    DO:  
        DISCONNECT VALUE(ttComTbl.cLDbNavn) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO:
            RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p: dbDisconnect: Feil ved frakobling av ' 
                              + ' DB Param ' + ttComTbl.cButikk    
                              + ' DB: ' + ttComTbl.cLdbNavn    
                              + ' Status: ' 
                              + STRING(ERROR-STATUS:ERROR)
                              ).
            bOk = FALSE.
        END.
        
        DISCONNECT VALUE(ttComTbl.cDataLdbNavn) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO:
            RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p: dbDisconnect: Feil ved frakobling av ' 
                              + ' Butikk: ' + ttComTbl.cButikk    
                              + ' DB: ' + ttComTbl.cDataLdbNavn    
                              + ' Status: ' 
                              + STRING(ERROR-STATUS:ERROR)
                              ).
            IF bOk THEN
                bOk = FALSE.
        END.

        RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p: dbDisconnect:' 
                         + ' DB Param ' + ttComTbl.cButikk    
                         + ' frakoblet. ' 
                         ).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-InnlesKonfig) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InnlesKonfig Procedure 
PROCEDURE InnlesKonfig :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* hkDataInnlesKonfig.p */

DEF OUTPUT PARAMETER bOk AS LOG NO-UNDO.

RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p - InnlesKonfig: Innlesning av konfigurasjonsfiler.').
ASSIGN
    bOk = FALSE.

IF SEARCH(cIpLst) = ? THEN
DO:
    RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p - InnlesKonfig: Finner ikke fil med ip adreser (' + cIpLst + ').').
    RETURN.
END.
IF SEARCH(cPeriodeLst) = ? THEN
DO:
    RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p - InnlesKonfig: Finner ikke fil med periodeinformasjon (' + cPeriodeLst + ').').
    RETURN.
END.

/* Leser inn IP liste. */
FOR EACH ttComTbl:
    DELETE ttComTbl.
END.
FOR EACH ttPeriode:
    DELETE ttPeriode.
END.

RUN lesInnButikkListe (OUTPUT bOk).
IF bOk = TRUE THEN
    RUN lesInnPeriodeListe (OUTPUT bOk).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LagreKonfig) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreKonfig Procedure 
PROCEDURE LagreKonfig :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER bOk AS LOG NO-UNDO.

    RUN bibl_loggDbFri.p (cLoggFil, 'SaveKonfig.p: Lagre periodetabell.').

    OUTPUT STREAM Ut TO VALUE(cPeriodeLst) NO-ECHO.
    
    PUT STREAM Ut UNFORMATTED 
        'Navn;FraDato;TilDato'
        SKIP.
    
    FOR EACH ttPeriode 
        BREAK BY ttPeriode.cButikk:
        PUT STREAM Ut UNFORMATTED
            ttPeriode.cButikk ';' 
            ttPeriode.FraDato ';'
            ttPeriode.TilDato 
            SKIP.
    END.

    OUTPUT STREAM Ut CLOSE.

    bOk = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesInnButikkListe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesInnButikkListe Procedure 
PROCEDURE lesInnButikkListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER bOk AS LOG NO-UNDO.

    RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p - lesInnButikkListe: Innlesning av butikkliste og ip adresser.').
    ASSIGN 
      iAnt     = 0
      iLinjeNr = 0.
    INPUT STREAM Inn FROM VALUE(cIpLst) NO-ECHO.
    REPEAT:
        IMPORT STREAM Inn UNFORMATTED cLinje.

        iLinjeNr = iLinjeNr + 1.
        IF TRIM(cLinje) BEGINS '#' OR 
           TRIM(cLinje) = '' OR
           TRIM(cLinje) BEGINS 'Navn' THEN
            NEXT.

        IF NUM-ENTRIES(TRIM(cLinje),';') <> 10 THEN 
          DO:
            RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p - lesInnButikkListe: Feil antall entries på linje ' + STRING(iLinjeNr) + ': ' + cLinje).
            NEXT.
          END.

        IF NOT CAN-FIND(FIRST ttComTbl WHERE 
                        ttComTbl.cButikk = ENTRY(1,cLinje,';')) THEN
        DO:
            CREATE ttComTbl.
            ASSIGN
                iAnt                   = iAnt + 1
                ttComTbl.cButikk       = ENTRY(1,cLinje,';')
                ttComTbl.cIpAdr        = ENTRY(2,cLinje,';')
                ttComTbl.cPortNr       = ENTRY(3,cLinje,';')
                ttComTbl.cDbNavn       = ENTRY(4,cLinje,';')
                ttComTbl.cLDbNavn      = ENTRY(5,cLinje,';')
                ttComTbl.cDivParam     = ENTRY(6,cLinje,';')
                ttComTbl.cDataPortNr   = ENTRY(7,cLinje,';')
                ttComTbl.cDataDbNavn   = ENTRY(8,cLinje,';')
                ttComTbl.cDataLDbNavn  = ENTRY(9,cLinje,';')
                ttComTbl.cDataDivParam = ENTRY(10,cLinje,';')
                NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
            DO:
                RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p - lesInnButikkListe: Feil i ASSIGN på linje ' + STRING(iAnt) + ': ' + cLinje).
                NEXT.
            END.
        END.

        RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p - lesInnButikkListe:' 
                              + ' DBParam: ' + ttComTbl.cButikk    
                              + ' -db ' + ttComTbl.cDbNavn  
                              + ' -ld ' + ttComTbl.cLDbNavn 
                              + ' -H ' + ttComTbl.cIpAdr   
                              + ' -S ' + ttComTbl.cPortNr  
                              + ' ' + ttComTbl.cDivParam  
                              + ' -db ' + ttComTbl.cDataDbNavn  
                              + ' -ld ' + ttComTbl.cDataLDbNavn 
                              + ' -S ' + ttComTbl.cDataPortNr  
                              ).
    END.
    INPUT STREAM Inn CLOSE.

    IF iAnt = 0 THEN
        bOk = FALSE.
    ELSE bOk = TRUE.

    RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p - lesInnButikkListe: Antall innlest butikker ' + STRING(iAnt) + ' (IP adresser). ' + STRING(bOk)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesInnPeriodeListe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesInnPeriodeListe Procedure 
PROCEDURE lesInnPeriodeListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER bOk AS LOG NO-UNDO.

    RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p - lesInnPeriodeListe: Innlesning av butikkliste og periode.').
    ASSIGN 
      i2Ant    = 0
      iLinjeNr = 0.
    INPUT STREAM Inn FROM VALUE(cPeriodeLst) NO-ECHO.
    REPEAT:
        IMPORT STREAM Inn UNFORMATTED cLinje.
        IF TRIM(cLinje) = '' OR 
           TRIM(cLinje) BEGINS '#' OR
           TRIM(cLinje) BEGINS 'Navn' 
          THEN NEXT.

        IF NUM-ENTRIES(TRIM(cLinje),';') <> 3 THEN 
        DO:
            RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p - lesInnPeriodeListe: Feil antall entries på linje ' + STRING(iLinjeNr) + ': ' + cLinje).
            NEXT.
        END.

        IF NOT CAN-FIND(FIRST ttPeriode WHERE 
                              ttPeriode.cButikk = ENTRY(1,cLinje,';')) THEN
        DO:
            CREATE ttPeriode.
            ASSIGN
                i2Ant               = i2Ant + 1
                ttPeriode.cButikk   = ENTRY(1,cLinje,';')
                ttPeriode.FraDato   = DATE(ENTRY(2,cLinje,';'))
                ttPeriode.TilDato   = DATE(ENTRY(3,cLinje,';'))
                NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
            DO:
                RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p - lesInnPeriodeListe: Feil i ASSIGN på linje ' + STRING(i2Ant) + ': ' + cLinje).
                NEXT.
            END.
        END.

        /* Ingen ukjente datoer */
        IF ttPeriode.FraDato = ? THEN ttPeriode.FraDato = TODAY - 5.
        IF ttPeriode.TilDato = ? THEN ttPeriode.TilDato = TODAY.

        RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p - lesInnPeriodeListe:' 
                              + ' Periode ' + ttPeriode.cButikk    
                              + ' ' + STRING(ttPeriode.FraDato)   
                              + ' ' + STRING(ttPeriode.TilDato)  
                              ).
    END.
    INPUT STREAM Inn CLOSE.

    IF iAnt = 0 THEN
      bOk = FALSE.
    ELSE bOk = TRUE.

    RUN bibl_loggDbFri.p (cLoggFil, 'hkDataInnsamling.p - lesInnPeriodeListe: Antall innlest butikker/perioder ' + STRING(iAnt) + '.').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-setLoggFilNavn) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLoggFilNavn Procedure
PROCEDURE setLoggFilNavn:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/

  ASSIGN 
    cLoggFil    = 'PRSStg_' + cJobNr + '_import'    + REPLACE(STRING(TODAY),'/','-')
    cErrLoggFil = 'PRSStg_' + cJobNr + '_importERR' + REPLACE(STRING(TODAY),'/','-').

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-Ping) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Ping Procedure 
FUNCTION Ping RETURNS LOGICAL
  ( INPUT cHost AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VARIABLE strMachineName AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE ipHost         AS System.Net.IPHostEntry NO-UNDO . 
    DEFINE VARIABLE ipAddr         AS System.Net.IPAddress NO-UNDO EXTENT . 
    DEFINE VARIABLE ping           AS System.Net.NetworkInformation.Ping      NO-UNDO. 
    DEFINE VARIABLE pingReply      AS System.Net.NetworkInformation.PingReply NO-UNDO.
    DEFINE VARIABLE pingTimeOut    AS INT INIT 120 NO-UNDO.
    DEFINE VARIABLE lStatus        AS LOGICAL NO-UNDO. 

    IF cHost = "localhost"  THEN
         strMachineName = System.Net.Dns:GetHostName().
    ELSE strMachineName = cHost.
    
    ipHost = System.Net.Dns:GetHostByName(strMachineName) NO-ERROR.
    IF ipHost = ? THEN RETURN FALSE. 
    ipAddr = ipHost:AddressList .
    ping = NEW System.Net.NetworkInformation.Ping().
    
    /* MESSAGE strMachineName SKIP ipHost:ToString() SKIP ipAddr[1]:ToString() VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    pingReply = ping:SEND(ipAddr[1]:ToString(),pingTimeOut) NO-ERROR.
    /* MESSAGE PingReply:STATUS PingReply:RoundTripTime VIEW-AS ALERT-BOX. */ 

    lStatus = STRING(PingReply:STATUS) = "Success". 
    DELETE OBJECT ping NO-ERROR. 
    IF lStatus THEN RETURN TRUE. ELSE RETURN FALSE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

