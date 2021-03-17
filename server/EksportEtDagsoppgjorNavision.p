
/*------------------------------------------------------------------------
    File        : EksportEtDagsoppgjorNavision.p
    Purpose     : 

    Syntax      :

    Description : Starter og kjører eksportklassen for Navision. eksporterer et dagsoppgjør.  

    Author(s)   : tomn
    Created     : Fredag 2/9-20
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE lBokForingsId AS DECIMAL FORMAT ">>>>>>>>>>>>>>>9" NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rEksportNavision AS CLASS cls.Navision.EksportNavision NO-UNDO.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).

IF SEARCH('tnc.txt') <> ? THEN 
  bTest = TRUE.

ASSIGN 
    cLogg = 'EksportEtDagsoppgjorNavision' + REPLACE(STRING(TODAY),'/','')
    .
rStandardFunksjoner:SkrivTilLogg(cLogg, 
    'Start'
    ). 
rStandardFunksjoner:SkrivTilLogg(cLogg, 
    '  icParam: ' + icParam
    ). 

ASSIGN     
  lBokForingsId = DEC(ENTRY(1,icParam,'|'))
  NO-ERROR.
IF ERROR-STATUS:ERROR THEN 
  RETURN.
  
rEksportNavision = NEW cls.Navision.EksportNavision( INPUT cLogg ).

/* Flagger at eksport skal gjøres selv om det er gjort tidligere. */
rEksportNavision:iReEksport = 1.

/* Setter parametre for behandling av loggfil for ekstrafeed. */
rEksportNavision:settParametre().

/* Er logging av ekstrafeed aktivert, kjøres preparering av loggfilen. */
IF rEksportNavision:iAktiv = 1 THEN 
  DO:
    FIND BokforingsBilag NO-LOCK WHERE 
      BokforingsBilag.BokforingsId = lbokforingsId NO-ERROR.
    IF AVAILABLE Bokforingsbilag THEN 
    DO:       
      obOk = FALSE.
      IF rEksportNavision:prepKatalog( ) THEN 
        DO:
          rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Eksport av dagsoppgjør GR ' + STRING(BokforingsBilag.OmsetningsDato) + 'for butikk ' + STRING(BokforingsBilag.ButikkNr) + '.'
            ).
          rEksportNavision:emptyTempFile( ).
          IF rEksportNavision:prepDagsrapp( 1, BokforingsBilag.ButikkNr, BokforingsBilag.OmsetningsDato ) /* Vanlige butikker */  THEN
              rEksportNavision:eksporterDagsrapp( 1, BokforingsBilag.ButikkNr ). 

          rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Eksport av dagsoppgjør OU ' + STRING(BokforingsBilag.OmsetningsDato) + 'for butikk ' + STRING(BokforingsBilag.ButikkNr) + '.'
            ).
          rEksportNavision:emptyTempFile( ).
          IF rEksportNavision:prepDagsrapp( 2, BokforingsBilag.ButikkNr, BokforingsBilag.OmsetningsDato ) /* Outlet butikker */ THEN   
              rEksportNavision:eksporterDagsrapp( 2, BokforingsBilag.ButikkNr ). 
          ASSIGN 
            obOk = TRUE.
        END.
      ELSE DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Feil ved eksport av dagsoppgjør ' + STRING(BokforingsBilag.OmsetningsDato) + 'for butikk ' + STRING(BokforingsBilag.ButikkNr) + '.'
          ).
      END.
    END.
  END.
ELSE DO:
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg, 
          'EksportNavision er ikke aktiv.'
          ). 
  END.

rStandardFunksjoner:SkrivTilLogg(cLogg, 
    'Slutt.'
    ). 
    