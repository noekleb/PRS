&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : eksportSBudsjettTimeGrip.p
    Purpose     : Eksport av salgsbudsjett til TimeGrip.

    Syntax      :

    Description : Eksporterer alle aktive salgsbudsjett til TimeGrip.

    Author(s)   : Tom Nøkleby
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    DEFINE VAR ocReturn AS CHARACTER  NO-UNDO.
&ELSE
    DEFINE OUTPUT PARAMETER ocReturn AS CHARACTER  NO-UNDO.
&ENDIF

DEF VAR bStream     AS LOG  NO-UNDO.
DEF VAR lTid        AS INT  NO-UNDO.
DEF VAR cTekst      AS CHAR NO-UNDO.
DEF VAR iAnt        AS INT  NO-UNDO.

/* Filhåndtering */
DEF VAR cFilNavn    AS CHAR NO-UNDO.
DEF VAR ctmpFilNavn AS CHAR NO-UNDO.
DEF VAR cKatalog    AS CHAR NO-UNDO.
DEF VAR obOk        AS CHAR NO-UNDO.
DEF VAR lAutoFormat AS LOG  NO-UNDO.
DEFINE VARIABLE cMndLst     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iMnd        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cMndNavn    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUkeDag     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUkeDagNavn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDagLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iWDay AS INTEGER NO-UNDO.
DEFINE VARIABLE dDato AS DATE    NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE iX AS INTEGER NO-UNDO.
    
DEF STREAM Ut.

DEF TEMP-TABLE tmpSBudDag LIKE SBudDag
    FIELD Aar AS INT
    FIELD Mnd AS INT
    FIELD Dag AS INT 
    FIELD ButikkNr AS INT 
    INDEX Utlegg Aar Mnd Dag ButikkNr.
     
DEF TEMP-TABLE tmpSBudManed LIKE SBudManed
    FIELD Aar AS INT
    FIELD Mnd AS INT
    FIELD ButikkNr AS INT 
    INDEX MndUtlegg Aar Mnd ButikkNr.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */


{syspara.i 1 1 61 cKatalog}
IF cKatalog = '' THEN 
DO:
  {syspara.i 1 1 51 cKatalog}
  IF cKatalog = '' THEN
    cKatalog = 'c:\home\lindbak\sendes'. 
END.

ASSIGN 
    lTid        = TIME
    lAutoFormat = TRUE 
    cLogg       = 'eksportSBudsjettTimeGrip' + REPLACE(STRING(TODAY),'/','')
    .
    
{syspara.i 23 1 1 cMndLst}
IF cMndLst = '' THEN cMndLst = 'JAN,FEB,MAR,APR,MAI,JUN,JUL,AUG,SEP,OKT,NOV,DES'.
{syspara.i 23 1 2 cDagLst}
IF cDagLst = '' THEN cDagLst = 'SØN,MAN,TIR,ONS,TOR,FRE,LØR'.
      
RUN ByggTmpTable. 

/* Legger ut data til fil. */
IF lAutoFormat THEN 
    RUN AutoEksport. /* automatisk format */
ELSE 
    RUN Eksporter. /* Manuelt format */

ocReturn = "OK," + String(iAnt) + cTekst.

lTid = TIME - lTid.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-AutoEksport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AutoEksport Procedure 
PROCEDURE AutoEksport :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF VAR iButikkNr      AS INT       NO-UNDO.
    DEF VAR cFilNavn       AS CHAR      NO-UNDO.
    DEF VAR cEkstent       AS CHAR      NO-UNDO.
    DEF VAR cSistSolgtDato AS CHARACTER NO-UNDO.
    DEF VAR cVVareKost     AS CHARACTER NO-UNDO.
    DEF VAR iantDag        AS INT       NO-UNDO.
    DEF VAR iAar           AS INT       NO-UNDO.
    DEF VAR cDatoStamp     AS CHAR      NO-UNDO. 

    DEFINE BUFFER bufTelleHode FOR TelleHode.

    {syspara.i 5 1 1 iButikkNr INT}

    ASSIGN
        cDatoStamp = REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM"),':','')
        iAar       = YEAR(TODAY) - 1
        cEkstent   = 'csv'
        cKatalog   = RIGHT-TRIM(cKatalog,'\')
        cFilNavn   = cKatalog + '\' + 'SBUD' + STRING(iAar,"9999") + '_' + cDatoStamp + '.'.
    ctmpFilNavn = cKatalog + '\' + 'tmpSBUD' + STRING(iAar,"9999") + '_' + cDatoStamp + '.'.

    OUTPUT STREAM Ut TO VALUE(ctmpFilNavn + cEkstent).

    RUN bibl_logg.p (cLogg, 'eksportSBudsjettTimeGrip.p' + ' Fil: ' + string(cFilNavn) + cEkstent).

    SBUDMANED:
    FOR EACH tmpSbudManed  
        BREAK BY tmpSBudManed.Aar
        BY tmpSBudManed.Mnd
        BY tmpSBudManed.ButikkNr
        :

        FIND Butiker NO-LOCK WHERE 
            Butiker.Butik = tmpSBudManed.ButikkNr NO-ERROR.
        FIND SBudHode NO-LOCK WHERE 
            SBudHode.SBudId = tmpSBudManed.SBudId NO-ERROR.
            
        IF FIRST-OF(tmpSBudManed.Mnd) THEN
        DO:
            /* Teller opp dagene i måneden */
            iAntDag = 0.
            FOR EACH tmpSBudDag WHERE 
                tmpSBudDag.SBudId = tmpSBudManed.SBudId AND
                tmpSBudDag.Aar    = tmpSBudManed.Aar AND
                tmpSBudDag.Mnd    = tmpSBudManed.Mnd NO-LOCK:
                iAntDag = iAntDag + 1.
            END.
            PUT STREAM Ut UNFORMATTED 
                'SbudId;' +
                'Beskrivelse;' + 
                'ButikkNr' +
                'Navn;' +
                'År;' +
                'ÅrMnd;' + 
                'MndNavn;' +
                'Ukedag;' + 
                'UkedagNavn;' +
                'Oms dag;' + 
                'Oms% dag;' + 
                'Db dag;' + 
                'Db% dag;' + 
                'Endret;' + 
                'Kl;' + 
                'Endret av'                
                .
        END.
    
        /* Legger ut budsjettet for måned og butikk */
        FOR EACH tmpSBudDag WHERE 
            tmpSBudDag.SBudId = tmpSBudManed.SBudId AND
            tmpSBudDag.Aar    = tmpSBudManed.Aar AND
            tmpSBudDag.Mnd    = tmpSBudManed.Mnd
            BREAK BY tmpSBudDag.SBudId
            BY tmpSBudDag.Aar
            BY tmpSBudDag.Mnd 
            BY tmpSBudDag.Dag:

            ASSIGN 
                iMnd        = INT(SUBSTRING(STRING(tmpSBudManed.AarMnd,'999999'),5,2))
                cMndNavn    = ' ' + ENTRY (iMnd,cMndLst)
                cUkeDag     = SUBSTRING(STRING(tmpSBudDag.AarMndDag,'99999999'),7,2)      
                dDato       = DATE (INT(SUBSTRING(STRING(tmpSBudDag.AarMndDag,'99999999'),5,2)), 
                                    INT(SUBSTRING(STRING(tmpSBudDag.AarMndDag,'99999999'),7,2)), 
                                    INT(SUBSTRING(STRING(tmpSBudDag.AarMndDag,'99999999'),1,4)))
                iWDay        = WEEKDAY(dDato)
                cUkeDagNavn  = ' ' + ENTRY (iWDay,cDagLst)
                NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
            DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:        
                    RUN bibl_loggDbFri.p (cLogg,
                        '   ** Feil ved utlegg av budsjett ' + 
                             ' SBudId: ' + STRING(tmpSBudManed.SBudId) +  
                             ' Aar: ' + STRING(tmpSBudManed.Aar) +  
                             ' Mnd: ' + STRING(tmpSBudManed.Mnd) + ' Feil: ' + 
                            STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix)  
                        ).
            END.
            ELSE               
                PUT STREAM Ut UNFORMATTED
                    tmpSBudDag.SBudId ';'
                    SBudHode.SBudBeskrivelse ';'
                    tmpSBudDag.ButikkNr ';'
                    Butiker.ButNamn ';'
                    tmpSBudDag.Aar ';'
                    tmpSBudDag.AarMnd  ';'
                    cMndNavn ';'
                    cUkeDag ';'
                    cUkeDagNavn ';'
                    tmpSBudDag.SalgBudsjett ';'
                    tmpSBudDag.SalgProsent ';'
                    tmpSBudDag.DbBudsjett ';'
                    tmpSBudDag.DbProsent ';'
                    tmpSBudDag.EDato ';'
                    STRING(tmpSBudDag.ETid,"HH:MM:SS") ';'
                    tmpSBudDag.BrukerId
                SKIP.
        END.

    END. /* SBUDMANED */
    iAnt = IAnt + 1.

    OUTPUT STREAM Ut CLOSE.

    /* Gir filen dens riktige navn og tar bort den temporære filen. */
    OS-COPY value(ctmpFilNavn + cEkstent) value(cFilNavn + cEkstent).
    IF SEARCH(cFilNavn + cEkstent) <> ? THEN
        OS-DELETE VALUE(ctmpFilNavn + cEkstent).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTable Procedure 
PROCEDURE ByggTmpTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE tmpSBudDag.

FOR EACH SBudHode WHERE 
    SBudHode.Aktiv = TRUE,
    EACH SBudManed OF SBudHode NO-LOCK:
    
    CREATE tmpSBudManed.
    BUFFER-COPY SBudManed TO tmpSBudManed
        ASSIGN
        tmpSBudManed.ButikkNr = SBudHode.ButikkNr
        tmpSBudManed.Aar      = SBudHode.Aar
        tmpSbudManed.Mnd      = INT(SUBSTRING(STRING(SBudManed.AarMnd),5))
        NO-ERROR.

    FOR EACH SBudDag OF SBudManed NO-LOCK:
        CREATE tmpSBudDag.
        BUFFER-COPY SBudDag TO tmpSBudDag
            ASSIGN
            tmpSBudDag.ButikkNr = SBudHode.ButikkNr
            tmpSBudDag.Aar      = SBudHode.Aar
            tmpSBudDag.Mnd      = INT(SUBSTRING(STRING(SBudManed.AarMnd),5))
            tmpSBudDag.Dag      = INT(SUBSTRING(STRING(SBudDag.AarMndDag),7))
            NO-ERROR.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Eksporter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Eksporter Procedure 
PROCEDURE Eksporter :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR iButikkNr       AS INT       NO-UNDO.
DEF VAR cFilNavn        AS CHAR      NO-UNDO.
DEF VAR cEkstent        AS CHAR NO-UNDO.
DEF VAR cSistSolgtDato  AS CHARACTER NO-UNDO.
DEF VAR cVVareKost      AS CHARACTER NO-UNDO.
DEF VAR iantDag         AS INT       NO-UNDO.
DEF VAR iAar            AS INT       NO-UNDO.
DEF VAR cDatoStamp      AS CHAR      NO-UNDO. 

DEFINE BUFFER bufTelleHode FOR TelleHode.

{syspara.i 5 1 1 iButikkNr INT}

ASSIGN
    cDatoStamp  = REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM"),':','')
    iAar        = YEAR(TODAY) - 1
    cEkstent    = 'csv'
    cKatalog    = RIGHT-TRIM(cKatalog,'\')
    cFilNavn    = cKatalog + '\' + 'SBUD' + STRING(iAar,"9999") + '_' + cDatoStamp + '.'.
    ctmpFilNavn = cKatalog + '\' + 'tmpSBUD' + STRING(iAar,"9999") + '_' + cDatoStamp + '.'.

OUTPUT STREAM Ut TO VALUE(ctmpFilNavn + cEkstent).

RUN bibl_logg.p (cLogg, 'eksportSBudsjettTimeGrip.p' + ' Fil: ' + string(cFilNavn) + cEkstent).

SBUDMANED:
FOR EACH tmpSbudManed  
    BREAK BY tmpSBudManed.Aar
          BY tmpSBudManed.Mnd
          BY tmpSBudManed.ButikkNr
          :

    IF FIRST-OF(tmpSBudManed.Mnd) THEN
    DO:
        /* Teller opp dagene i måneden */
        iAntDag = 0.
        FOR EACH tmpSBudDag WHERE 
            tmpSBudDag.SBudId = tmpSBudManed.SBudId AND
            tmpSBudDag.Aar    = tmpSBudManed.Aar AND
            tmpSBudDag.Mnd    = tmpSBudManed.Mnd NO-LOCK:
            iAntDag = iAntDag + 1.
        END.
        PUT STREAM Ut UNFORMATTED 
            'YEAR;' tmpSBudManed.Aar SKIP
            'MONTH;' tmpSBudManed.Mnd SKIP 
            'ART_CODE;_SDY' SKIP
            'DAYS;' STRING(iAntDag) FILL(';',iAntDag) SKIP
            'UNIT;DAYS' SKIP                
        .
    END.
    
    /* Legger ut budsjettet for måned og butikk */
    PUT STREAM Ut UNFORMATTED 
        tmpSBudManed.ButikkNr.
    FOR EACH tmpSBudDag WHERE 
        tmpSBudDag.SBudId = tmpSBudManed.SBudId AND
        tmpSBudDag.Aar    = tmpSBudManed.Aar AND
        tmpSBudDag.Mnd    = tmpSBudManed.Mnd
        BREAK BY tmpSBudDag.SBudId
              BY tmpSBudDag.Aar
              BY tmpSBudDag.Mnd 
              BY tmpSBudDag.Dag:
        PUT STREAM Ut UNFORMATTED 
            ';' tmpSBudDag.SalgBudsjett.
    END.
    /* Linjeskift etter at en måned for en butikk er lagt ut. */
    PUT STREAM Ut UNFORMATTED SKIP.

END. /* SBUDMANED */
iAnt = IAnt + 1.

OUTPUT STREAM Ut CLOSE.

    /* Gir filen dens riktige navn og tar bort den temporære filen. */
    OS-COPY value(ctmpFilNavn + cEkstent) value(cFilNavn + cEkstent).
    IF SEARCH(cFilNavn + cEkstent) <> ? THEN
        OS-DELETE VALUE(ctmpFilNavn + cEkstent).
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

