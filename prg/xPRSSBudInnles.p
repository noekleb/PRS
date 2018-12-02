&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xPRSSBudInnles.p
    Purpose     : Innlesning av data til salgsbudsjett registeret

    Syntax      : xPRSSBudInnles.p (lFilId, h_Parent, output iantLinjer).

    Description : Leser inn data i materialkode. Setter opp standardverdier. 

    Author(s)   : Tom Nøkleby
    Created     : 19/10/2015
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.
DEF VAR cMndLst       AS CHAR NO-UNDO.
DEF VAR cLogg         AS CHAR NO-UNDO.
DEF VAR cTekst        AS CHAR NO-UNDO.
DEF VAR iLoop         AS INT  NO-UNDO.

DEF VAR piLinjeNr AS INT  NO-UNDO.
DEF VAR pcLinje   AS CHAR NO-UNDO.
DEF VAR piAntFeil AS INT  NO-UNDO. 

DEFINE VARIABLE lDec  AS DECIMAL NO-UNDO.
DEFINE VARIABLE cStr  AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE bOk   AS LOG NO-UNDO.

DEF STREAM InnFil.

DEFINE BUFFER bSBudDag FOR SBudDag.
DEFINE TEMP-TABLE ttSBudDag LIKE SBudDag
    FIELD ButNr AS INT 
    FIELD Aar   AS INT
    FIELD Maned AS INT
    FIELD Dato  AS DATE
    INDEX ButNr SBudId Aar Maned.

DEFINE TEMP-TABLE ttError
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
    .
{windows.i}
{incl/devmode.i}
{incl/custdevmode.i}
{AssignRutiner.i}

ASSIGN
    cLogg   = 'xPRSSBudInnles'.
{syspara.i 23 1 1 cMndLst}
IF cMndLst = '' THEN cMndLst = 'JAN,FEB,MAR,APR,MAI,JUN,JUL,AUG,SEP,OKT,NOV,DES'.
    

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

RUN bibl_loggDbFri.p (cLogg,
    'Import starter.'
    ).

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RUN bibl_loggDbFri.p (cLogg,
        " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ")."
        ).
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

RUN bibl_loggDbFri.p (cLogg,
    "Import av fil: " + cFilNavn
    ).

RUN LesInnFil.
RUN PosterData.

/* Stempler posten som innlest. */
DO TRANSACTION:
    FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
    ASSIGN
        VPIFilHode.VPIFilStatus = 5
        .
END.
IF AVAILABLE VPIFilHode THEN
    FIND CURRENT VPIFilHode    NO-LOCK.

RUN bibl_logg.p (cLogg, 'Fil:  ' + VPIFilHode.FilNavn + ' Antall linjer: ' + STRING(iTotAntLinjer)+ '.' ).
RUN bibl_loggDbFri.p (cLogg,
    'Import ferdig.'
    ).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE b2Ok AS LOG NO-UNDO.
DEF VAR iMnd AS INT NO-UNDO.

  RUN bibl_loggDbFri.p (cLogg,
    'Subrutine: LesInnFil.'
    ).

  RUN TellOppLinjer.

  ASSIGN
      piLinjeNr  = 0
      cLinje     = ''
      piAntFeil  = 0
      iAntLinjer = 0
      b2Ok       = TRUE 
      .
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED cLinje.

    ASSIGN
      iAntLinjer = iAntLinjer + 1
      .
    /* Skipper tomme linjer */
    IF cLinje = "" THEN
        NEXT LESERLINJER.
        
    /* Skipper overskriftslinje fra mal. */
    IF cLinje BEGINS "1;2;3;4;5" THEN
        NEXT LESERLINJER.
    
    
    /* Skipper overskrift og linjer med feil. */
    ASSIGN lDec = DECIMAL(ENTRY(1,cLinje,';')) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        NEXT LESERLINJER.

    /* Skipper linjer med for få entries. */
    IF NUM-ENTRIES(cLinje,";") < 13 THEN
    DO:
        RUN bibl_loggDbFri.p (cLogg,
            "** Feil på linje (Skal være 13 entries) " + STRING(iAntLinjer) + ": " + cLinje
            ).
        NEXT LESERLINJER.
    END.
    
    ASSIGN iMnd = LOOKUP(TRIM(ENTRY(5,cLinje,';')),cMndLst).
    CREATE ttSBudDag.

    ASSIGN cTekst = REPLACE(ENTRY(8,cLinje,';'),'.','/').

    IF SESSION:DATE-FORMAT = 'dmy' THEN
    DO:  /* DMY */
        ttSBudDag.Dato = DATE(REPLACE(ENTRY(8,cLinje,';'),'.','/')).
    END.
    ELSE IF SESSION:DATE-FORMAT = 'ymd' THEN 
        DO: /* YMD */
            cTekst  = REPLACE(ENTRY(8,cLinje,';'),'.','/').
            ttSBudDag.Dato = DATE(
                                  INT(ENTRY(2,cTekst,'/')),
                                  INT(ENTRY(1,cTekst,'/')),
                                  INT(ENTRY(3,cTekst,'/'))
                                 ).
        END.
    ELSE DO:
        RUN bibl_loggDbFri.p (cLogg,
            '** Ukjent datoformat: ' + STRING(SESSION:DATE-FORMAT) + ' Dato: ' + ENTRY(8,cLinje,';') + ' record: ' + cLinje
            ).
        NEXT LESERLINJER.
    END.


    ASSIGN 
        ttSBudDag.ButNr        = INT(ENTRY(1,cLinje,';'))
        ttSBudDag.SBudId       = INT(ENTRY(3,cLinje,';'))
        ttSBudDag.Aar          = INT(ENTRY(4,cLinje,';'))
        ttSBudDag.Maned        = iMnd
        ttSBudDag.AarMnd       = INT(STRING(ttSBudDag.Aar,"9999") + STRING(ttSBudDag.Maned,"99"))
        ttSBudDag.AarMndDag    = INT(STRING(ttSBudDag.AarMnd,"999999") + STRING(DAY(ttSBudDag.Dato),"99"))
        ttSBudDag.SalgBudsjett = DEC(ENTRY(10,cLinje,';'))
        ttSBudDag.SalgProsent  = DEC(ENTRY(11,cLinje,';'))
        ttSBudDag.DbBudsjett   = DEC(ENTRY(12,cLinje,';'))
        ttSBudDag.DbProsent    = DEC(ENTRY(13,cLinje,';'))
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO iLoop = 1 TO ERROR-STATUS:NUM-MESSAGES:
        ERROR-STATUS:GET-NUMBER(iLoop).
        cTekst = ERROR-STATUS:GET-MESSAGE(iLoop).    
        RUN bibl_loggDbFri.p (cLogg,
            '** Feil: ' + cTekst 
            ).
        RUN bibl_loggDbFri.p (cLogg,
            '       Dato format:' + STRING(SESSION:DATE-FORMAT) + ' Dato: ' + ENTRY(8,cLinje,';') 
            ).
    END.

    STATUS DEFAULT "Lese linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PosterData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PosterData Procedure 
PROCEDURE PosterData :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
------------------------------------------------------------------------------*/
  DEF VAR piSBudId       LIKE SBudHode.SBudId NO-UNDO.
  DEF VAR plSalgBudsjett LIKE SBudHode.Salgbudsjett NO-UNDO.
  DEF VAR plDbBudsjett   LIKE SBudHode.DbBudsjett   NO-UNDO.

  RUN bibl_loggDbFri.p (cLogg,
    'Subrutine: PosterData.'
    ).

  FIND FIRST ttSBudDag NO-ERROR.

  IF AVAILABLE ttSBudDag THEN
      FIND SBudHode NO-LOCK WHERE 
           SBudHode.SBudId = ttSbudDag.SBudId NO-ERROR.
  IF NOT AVAILABLE SBudHode THEN
  DO:
      RUN bibl_loggDbFri.p (cLogg,
          '** Feil: Budsjett ' + STRING(ttSBudDag.SBudId) + ' er slettet. Import avbrutt.'
          ).
      RETURN.
  END.
  ELSE 
      piSBudId = ttSBudDag.SBudId.

  /* fjerner gaml budsjett poster. */
  IF AVAILABLE ttSBudDag THEN
      FOR EACH SBudDag EXCLUSIVE-LOCK WHERE
          SBudDag.SBudId = ttSBudDag.SBudId:
          DELETE SBudDag.
      END.
  
  /* Legger inn linjene. */
  FOR EACH ttSBudDag TRANSACTION:

    FIND SBudDag EXCLUSIVE-LOCK WHERE 
        SBudDag.SBudId    = ttSBudDag.SBudId    AND
        SBudDag.AarMnd    = ttSBudDag.AarMnd    AND
        SBudDag.AarMndDag = ttSBudDag.AarMndDag 
        NO-ERROR.
    IF NOT AVAILABLE SBudDag THEN 
    DO:
      CREATE SBudDag.
      ASSIGN
          SBudDag.SBudId       = ttSBudDag.SBudId   
          SBudDag.AarMnd       = ttSBudDag.AarMnd   
          SBudDag.AarMndDag    = ttSBudDag.AarMndDag
      NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
      DO iLoop = 1 TO ERROR-STATUS:NUM-MESSAGES:
          ERROR-STATUS:GET-NUMBER(iLoop).
          cTekst = ERROR-STATUS:GET-MESSAGE(iLoop).    
          RUN bibl_loggDbFri.p (cLogg,
              '** Feil: ' + cTekst
              ).
      END.
    END.
    ASSIGN
        SBudDag.SalgBudsjett = ttSBudDag.SalgBudsjett 
        SBudDag.SalgProsent  = ttSBudDag.SalgProsent  
        SBudDag.DbBudsjett   = ttSBudDag.DbBudsjett   
        SBudDag.DbProsent    = ttSBudDag.DbProsent    
    NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO iLoop = 1 TO ERROR-STATUS:NUM-MESSAGES:
        ERROR-STATUS:GET-NUMBER(iLoop).
        cTekst = ERROR-STATUS:GET-MESSAGE(iLoop).    
        RUN bibl_loggDbFri.p (cLogg,
            '** Feil: ' + cTekst
            ).
    END.

    RELEASE SBudDag.
  END. /* TRANSACTION */

  ASSIGN
      plSalgBudsjett = 0
      plDbBudsjett   = 0
      .

  /* Summerer opp måneder. */ 
  FOR EACH SBudManed OF SBudHode TRANSACTION:
      ASSIGN
          SBudManed.SalgBudsjett = 0
          SBudManed.SalgProsent  = 0
          SBudManed.DbBudsjett   = 0
          SBudManed.DbProsent    = 0
          .
      FOR EACH SBudDag OF SBudManed NO-LOCK:
          ASSIGN
              SBudManed.SalgBudsjett = SBudManed.SalgBudsjett + SBudDag.SalgBudsjett 
              SBudManed.DbBudsjett   = SBudManed.DbBudsjett   + SBudDag.DbBudsjett  
              plSalgBudsjett         = plSalgBudsjett         + SBudDag.SalgBudsjett
              plDbBudsjett           = plDbBudsjett           + SBudDag.DbBudsjett  
              .
      END.
  END. /* TRANSACTION */

  DO TRANSACTION:
      FIND CURRENT SBudHode EXCLUSIVE-LOCK.
      ASSIGN
          SBudHode.SalgBudsjett = plSalgBudsjett
          SBudHode.DbBudsjett   = plDbBudsjett
          .
      FIND CURRENT SBudHode NO-LOCK.
  END. /* TRANSACTION */

  /* Regner ut prosenter på måned. */
  FOR EACH SBudManed OF SBudHode TRANSACTION:
      ASSIGN 
          SbudManed.SalgProsent = (SBudManed.SalgBudsjett * 100) / SbudHode.SalgBudsjett
          SbudManed.DbProsent   = (SBudManed.DbBudsjett * 100) / (
                                                                  SbudManed.SalgBudsjett - (
                                                                                            (SbudManed.SalgBudsjett * 20)/ 100
                                                                                           )
                                                                  )
          .
      IF SbudManed.SalgProsent = ? THEN
          SBudManed.SalgProsent = 0.
      IF SbudManed.DbProsent = ? THEN
          SBudManed.DbProsent = 0.
  END. /* TRANSACTION */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

