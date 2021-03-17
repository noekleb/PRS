&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : run_asUtleggPricat.p
    Purpose     : Legger ut prisendringer for kommisjonsbutikker.

    Syntax      :

    Description :

    Author(s)   : Tom Nøkleby
    Created     : 16/1-2021
    Notes       : Ved å kjøre via AppServer, spares antall oppkoblede 
                  klienter på server.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF VAR hServer           AS HANDLE    NO-UNDO.
DEF VAR cConnectionString AS CHARACTER NO-UNDO.
DEF VAR lConnected        AS LOGICAL   NO-UNDO.
DEF VAR obOk              AS LOG       NO-UNDO.
DEF VAR cIpAdr            AS CHAR      NO-UNDO.
DEF VAR cAppSrv           AS CHAR      NO-UNDO.
DEF VAR cLogg             AS CHAR NO-UNDO.
DEFINE VARIABLE cConfigFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReturn-Value AS CHARACTER NO-UNDO.

DEFINE VARIABLE bTest AS LOG NO-UNDO.

DEFINE TEMP-TABLE ttConfig
  FIELD cHost AS CHARACTER 
  FIELD cPort AS CHARACTER 
  FIELD cAppServer AS CHARACTER
  .

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.clStandardFunksjoner NO-UNDO.

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

rStandardFunksjoner  = NEW cls.StdFunk.clStandardFunksjoner( cLogg ).
    
ASSIGN
    bTest        = IF SEARCH('tnc.txt') = ? THEN FALSE ELSE TRUE
    bTest        = TRUE 
    cLogg        = 'utleggPRICATfraArtPris' + REPLACE(STRING(TODAY),'/','')
    cConfigFile  = 'konfig\asPRS.JSon'
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

SERVERKALL:
DO:
  /* Oppkobling av asPRS appserveren. */
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
    LEAVE SERVERKALL.    
  END.
  ELSE
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        'AppServer: '  + ttConfig.cAppServer + ' Oppkoblet: ' + STRING(hServer:CONNECTED()) + '.'
        ).

  cReturn-Value = ''.
  /* Leser inn bongene. */
  RUN cls\kommisjon\utleggPRICATfraArtPris.p ON SERVER hServer.
  cReturn-Value = RETURN-VALUE.

  /* Kobler ned AppServer. */
  IF hServer:CONNECTED( ) THEN
  DO: 
    hServer:DISCONNECT().
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        'AppServer koblet ned: '  + ttConfig.cAppServer + ' Oppkoblet: ' + STRING(hServer:CONNECTED()) + '.'
        ).
  END.

END. /* SERVERKALL */  

IF VALID-HANDLE(hServer) THEN 
  DELETE OBJECT hServer.
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
