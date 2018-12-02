&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : eksporterSBud_run.p
    Purpose     : Eksport av salgsbudsjett pr. dag.

    Syntax      : run eksporterSBud_run.p.

    Description : Eksporterer dagbudsjett på alle aktive salgsbudsjetter.

    Author(s)   : Tom Nøkleby
    Created     : 11/11-14
    Notes       : Laget for Gant for eksport til TimeGrip.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF VAR cFilNavn AS CHAR NO-UNDO.
DEF VAR cKatalog AS CHAR NO-UNDO.
DEF VAR cUDag    AS CHAR NO-UNDO.
DEF VAR cMnd     AS CHAR NO-UNDO.
DEFINE VARIABLE bTest AS CHARACTER NO-UNDO.

DEF STREAM Ut.

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

{syspara.i 50 21 3 cKatalog}

ASSIGN
    cFilNavn = 'SBudTG' + REPLACE(STRING(TODAY),'/','') + '-' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.csv'.
    .

RUN eksporterSbud.

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-eksporterSbud) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE eksporterSbud Procedure 
PROCEDURE eksporterSbud :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

OUTPUT STREAM Ut TO VALUE(cKatalog + '\' + cFilNavn) NO-ECHO.

    PUT STREAM Ut UNFORMATTED
        'SBudId;'
        'Beskrivelse;'
        'ButikkNr;'
        'Navn;'
        'Å;'
        'Mnd;'
        ';'
        'Ukedag;'
        ';'
        'Oms dag;'
        'Oms% dag;'
        'Db dag;'
        'Db% dag;'
        'Endret;'
        'Kl;'
        'Endret av'                   
    SKIP.    
HODE:                    
FOR EACH SBudHode NO-LOCK WHERE
    SBudHode.Aktiv = TRUE:
    FIND Butiker NO-LOCK WHERE 
        Butiker.Butik = SBudHode.ButikkNr NO-ERROR.
    LINJE:
    FOR EACH SBudDag OF SBudHode NO-LOCK:
        RUN Mnd.
        RUN UDag.
        PUT STREAM Ut UNFORMATTED
            SBudHode.SBudId ';'
            SBudHode.SBudBeskrivelse  ';'
            SBudHode.ButikkNr  ';'
            Butiker.ButNamn ';'
            SbudHode.Aar  ';'
            SbudDag.AarMnd  ';'
            cMnd  ';'
            SBudDag.AarMndDag  ';'
            cUdag  ';'
            TRIM(STRING(SBudDag.SalgBudsjett,"->>>>>>>>9.99"))  ';'
            TRIM(STRING(SBudDag.SalgProsent,"->>>9.99")) ';'
            TRIM(STRING(SBudDag.DbBudsjett,"->>>>>>>>9.99")) ';'
            TRIM(STRING(SBudDag.DbProsent,"->>>9.99")) ';'
            SBudDag.EDato  ';'
            STRING(SBudDag.ETid,"HH:MM:SS")  ';'
            SBudDag.Brukerid                   
            SKIP.

    END. /* LINJE */

END. /* HODE */
OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Mnd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mnd Procedure 
PROCEDURE Mnd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cMndLst AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iMnd    AS INTEGER   NO-UNDO.
  
{syspara.i 23 1 1 cMndLst}
IF cMndLst = '' THEN cMndLst = 'JAN,FEB,MAR,APR,MAI,JUN,JUL,AUG,SEP,OKT,NOV,DES'.
      
  IF AVAIL SBudDag THEN 
  DO:
    ASSIGN 
      iMnd    = INT(SUBSTRING(STRING(SBudDag.AarMndDag,'99999999'),5,2))
      cMnd = ' ' + ENTRY (iMnd,cMndLst)
      NO-ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UDag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UDag Procedure 
PROCEDURE UDag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cDagLst AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iWDay   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE dDato   AS DATE NO-UNDO.

{syspara.i 23 1 2 cDagLst}  
IF cDagLst = '' THEN 
    cDagLst = 'SØN,MAN,TIR,ONS,TOR,FRE,LØR'.
      
  IF AVAIL SBudDag THEN 
  DO:
    ASSIGN 
      dDato   = DATE (INT(SUBSTRING(STRING(SBudDag.AarMndDag,'99999999'),5,2)), 
                 INT(SUBSTRING(STRING(SBudDag.AarMndDag,'99999999'),7,2)), 
                 INT(SUBSTRING(STRING(SBudDag.AarMndDag,'99999999'),1,4)))
      iWDay   = WEEKDAY(dDato)
      cUDag = ' ' + ENTRY (iWDay,cDagLst)
      NO-ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

