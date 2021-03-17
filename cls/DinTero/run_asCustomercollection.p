&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : run_asCustomercollection.p
    Purpose     : Henter kunder fra Dintero og oppretter dem i PRS.

    Syntax      :

    Description :

    Author(s)   : Tom Nøkleby
    Created     : 4/3-2021
    Notes       : Ved å kjøre via AppServer, spares antall oppkoblede 
                  klienter på server.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE hServer           AS HANDLE    NO-UNDO.
DEFINE VARIABLE cConnectionString AS CHARACTER NO-UNDO.
DEFINE VARIABLE lConnected        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE obOk              AS LOG       NO-UNDO.
DEFINE VARIABLE cIpAdr            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAppSrv           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cConfigFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReturn-Value     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSearch           AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAntall           AS INTEGER   NO-UNDO.
DEFINE VARIABLE cReturn           AS CHARACTER NO-UNDO.

DEFINE VARIABLE bTest             AS LOG       NO-UNDO.

DEFINE TEMP-TABLE ttConfig
  FIELD cHost      AS CHARACTER 
  FIELD cPort      AS CHARACTER 
  FIELD cAppServer AS CHARACTER
  .

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */
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
         HEIGHT             = 15.05
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ).
    
ASSIGN
  bTest       = IF SEARCH('tnc.txt') = ? THEN FALSE ELSE TRUE
  bTest       = TRUE 
  cLogg       = 'asCustomercollection.p' + REPLACE(STRING(TODAY),'/','')
  cConfigFile = 'konfig\asPRS.JSon'
  .

/* Oppretter config katalog. */
OS-COMMAND SILENT VALUE('md konfig').

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Starter.' 
  ).    

/* Henter parametrene. */
RUN hentOppkoblingsparametre.
FIND FIRST ttConfig NO-ERROR.
IF NOT AVAILABLE ttConfig THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Inifil ikke opprettet (Konfig\asPRS.JSon).' 
    ).    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Avslutter.' 
    ).    
  RETURN.
END. 

SERVERBLOKK:
DO:
  /* Mot asPRS appserveren. */
  IF AVAILABLE ttConfig THEN  
  DO:
    CREATE SERVER hServer.
    hServer:CONNECT("-H " + ttConfig.cHost + " -S " + ttConfig.cPort + " -AppService " + ttConfig.cAppServer + " -DirectConnect") NO-ERROR.
  END.
  IF NOT hServer:CONNECTED() THEN 
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  **Får ikke kontakt med AppServer ' + ttConfig.cAppServer + '. Applikasjonen avsluttes.' 
      ).
    LEAVE SERVERBLOKK.    
  END.
  ELSE
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  AppServer: '  + ttConfig.cAppServer + ' Oppkoblet: ' + STRING(hServer:CONNECTED()) + '.'
      ).

  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Starter asCustomercollection.p.'
    ).
  cReturn-Value = ''.
  RUN cls\Dintero\asCustomercollection.p ON SERVER hServer (cSearch, OUTPUT iAntall, OUTPUT cReturn)..
  cReturn-Value = RETURN-VALUE.
  IF cReturn-Value <> '' THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  RETURN-VALUE: '  + cReturn-Value + '.'
      ).

  /* Kobler ned AppServer. */
  IF hServer:CONNECTED( ) THEN 
    hServer:DISCONNECT().
  IF VALID-HANDLE(hServer) THEN 
    DELETE OBJECT hServer.
END. /* SERVERBLOKK */  

EMPTY TEMP-TABLE ttConfig.

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Avslutter.' 
  ).    

/* Avslutter */
QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-hentOppkoblingsparametre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hentOppkoblingsparametre Procedure
PROCEDURE hentOppkoblingsparametre:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  /* Laster config. hvis den finnes. */
  IF SEARCH(cConfigFile) <> ? THEN 
    TEMP-TABLE ttConfig:READ-JSON ("File",cConfigFile,"empty"). 
  FIND FIRST ttConfig NO-ERROR.

  /* Default parameteroppsett for utvikling og opprettelse av Configfile. */
  IF NOT CAN-FIND(FIRST ttConfig) THEN 
  DO:
    CREATE ttConfig.
    ASSIGN                             
      ttConfig.cHost      = OS-GETENV("COMPUTERNAME")
      ttConfig.cPort      = '3190'
      ttConfig.cAppServer = 'asPRS' 
      .
      
    TEMP-TABLE ttConfig:WRITE-JSON('file', cConfigFile, TRUE).
    FIND FIRST ttConfig NO-ERROR.
  END.

  RETURN.
  
END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

/* ************************  Function Implementations ***************** */
