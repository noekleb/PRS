&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : fix-ryns_lagerkonvertering.p
    Purpose     :

    Syntax      :

    Description : Leser inn filen og omformer den til en INV<NNN..>.<ButNr> fil.

    Author(s)   : Tom Nøkleby
    Created     : 2/8-13
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE iAntLinjer  AS INT    NO-UNDO.

DEFINE VARIABLE iTotAntLinjer AS INT  NO-UNDO.
DEFINE VARIABLE cLinje        AS CHAR NO-UNDO.
DEFINE VARIABLE cFilNavn      AS CHAR NO-UNDO.
DEFINE VARIABLE cVPIFil       AS CHAR NO-UNDO.
DEFINE VARIABLE ctmpKatalog   AS CHAR NO-UNDO.
DEFINE VARIABLE pcLinje       AS CHAR NO-UNDO.
DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.

DEF VAR cGenEan       AS CHAR NO-UNDO.
DEFINE VARIABLE cDefVg AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
DEFINE VARIABLE cStrl AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKode AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cGenInterleave AS CHARACTER NO-UNDO.

DEF STREAM InnFil.
DEF STREAM UtVPI.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .
DEF TEMP-TABLE tmpVare
    FIELD VareNr AS CHAR.

DEFINE TEMP-TABLE ttInv
    /*  1 */ FIELD ProductNr AS CHARACTER 
    /*  2 */ FIELD Quantity AS CHARACTER 
    /*  3 */ FIELD dDate AS CHARACTER
    /*  4 */ FIELD WareHouseNo AS CHARACTER 
    /*  5 */ FIELD GTIN AS CHARACTER 
    /*  6 */ FIELD UserName AS CHARACTER 
    /*  7 */ FIELD Lokasjon AS CHARACTER 
    /*  8 */ FIELD Price AS CHARACTER 
    /*  9 */ FIELD TerminalId AS CHARACTER 
    /* 10 */ FIELD CompanyNumber AS CHARACTER 
    /* 11 */ FIELD TransactionType AS CHARACTER  
    
    FIELD Storl AS CHARACTER 
    FIELD Vg AS INTEGER 
    FIELD LopNr AS INTEGER 
    FIELD KortNavn AS CHARACTER 
    FIELD c2AV5Interleaved AS CHARACTER 
    FIELD ButikkNr AS INTEGER 
    FIELD iFlagg AS INTEGER 
    
    INDEX Inv iFlagg ButikkNr ProductNr GTIN
    INDEX Flagg iFlagg. 
    
DEF BUFFER bttInv FOR ttInv.
DEFINE BUFFER bStrKonv FOR StrKonv.

{windows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-FixChk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FixChk Procedure 
FUNCTION FixChk RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-getEAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getEAN Procedure
FUNCTION getEAN RETURNS CHARACTER 
	(  ) FORWARD.

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
         HEIGHT             = 19.05
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

ASSIGN
    cFilNavn = '.\kom\in\PRSpfxuser_storl.csv'.

{syspara.i 2 4 8 cGenEan}

ASSIGN
    ctmpKatalog = '.\kom\in\' /*SESSION:TEMP-DIRECTORY*/
    cVPIFil     = "PRSINV" + STRING(TODAY,"99-99-9999") + "-" + STRING(TIME,"HH:MM:SS") + ".csv"
    cVPIFil     = REPLACE(cVPIFil,":","-")
    .

RUN LesInnFil.

RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-EksportINVFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportINVFil Procedure 
PROCEDURE EksportInvFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cEkspFil AS CHARACTER NO-UNDO.

/* MESSAGE ctmpKatalog + cVPIUtFil SKIP   */
/*     iAntLinjer                         */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */

IF iAntLinjer > 0 THEN
DO:
    EKSPORTFIL:                    
    FOR EACH ttInv NO-LOCK WHERE 
        ttInv.iFlagg = 1
        BREAK BY ttInv.iFlagg
              BY ttInv.ButikkNr
              BY ttInv.ProductNr     
              BY ttInv.GTIN:

        IF FIRST-OF(ttInv.ButikkNr) THEN
          DO: 
            cEkspFil = REPLACE(ctmpKatalog + cVPIFil,'.csv','.' + ttInv.WareHouseNo). 
            OUTPUT STREAM UtVPI TO VALUE(cEkspFil) NO-ECHO.                    
          END.

        PUT STREAM UtVPI UNFORMATTED  
          ttInv.Lokasjon ';' 
          ttInv.ProductNr ';'
          ttInv.Quantity ';' 
          ttInv.dDate ';'
          ttInv.WareHouseNo ';' 
          ttInv.GTIN ';' 
          ttInv.UserName ';' 
          ttInv.Price ';' 
          ttInv.TerminalId ';' 
          ttInv.CompanyNumber ';' 
          ttInv.TransactionType
        SKIP.  

        IF LAST-OF(ttInv.ButikkNr) THEN 
          DO:
            OUTPUT STREAM UtVpi CLOSE.
            MESSAGE 'PRSInv fil er generert: ' + cEkspFil
              VIEW-AS ALERT-BOX.
          END.
    END. /* EKSPORTFIL */

END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ErrorLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ErrorLogg Procedure 
PROCEDURE ErrorLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  OUTPUT TO VALUE("Error.Txt").
    PUT UNFORMATTED
      "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      "Feil i fil: " + cFilNavn skip
      .
    FOR EACH tt_Error:
      PUT UNFORMATTED tt_Error.Tekst SKIP.
    END.
  OUTPUT CLOSE.
  IF SEARCH("Error.Txt") <> ? THEN
  DO:
    DEF VAR hInstance AS INT.

    RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH("Error.Txt"),
                                  "",
                                  1,
                                  OUTPUT hInstance).

  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       


------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr AS INT  NO-UNDO.
  DEF VAR piAntFeil AS INT  NO-UNDO.
  DEF VAR pcBkuFil  AS CHAR NO-UNDO.
  DEF VAR plVareNr  AS DEC  NO-UNDO.
  DEF VAR pcStr     AS CHAR NO-UNDO.
  DEF VAR piLoop    AS INT  NO-UNDO.
  DEFINE VARIABLE cKode AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cStrl AS CHARACTER NO-UNDO.      
  
  /* Tømmer feillogg. */
  FOR EACH tt_Error:
    DELETE tt_Error.
  END.
  /* Tømmer pricat */
  FOR EACH ttInv TRANSACTION:
      DELETE ttInv.
  END.
  RUN TellOppLinjer.

  ASSIGN
      piLinjeNr  = 1.
      iAntLinjer = 0
      .
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje.

    assign
      iAntLinjer = iAntLinjer + 1      
      .

    /* Skipper tomme linjer */
    IF pcLinje = "" THEN
        NEXT LESERLINJER.
        
    /* Skipper overskriftslinje fra mal. */
    IF pcLinje BEGINS "1;2;3;4" THEN
        NEXT LESERLINJER.
    
    /* Skipper overskrift og linjer med feil. */
    ASSIGN lDec = DECIMAL(ENTRY(3,pcLinje,';')) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        NEXT LESERLINJER.

    /* Skipper linjer med for få entries. */
    IF NUM-ENTRIES(pcLinje,";") < 7 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
        NEXT LESERLINJER.
    END.

    /* Tomme linjer fra Excel */
    IF pcLinje BEGINS ";;;;;:" THEN
        NEXT LESERLINJER.
    /* OVerskriftsrad 2 */
    IF pcLinje BEGINS "recnum" THEN
        NEXT LESERLINJER.

    CREATE ttInv.

    /* Setting av løpenr for varegruppen. */
    IF LENGTH(TRIM(ENTRY( 3,pcLinje,";"))) > 3 THEN 
      ttInv.LopNr = INT(SUBSTRING(TRIM(ENTRY( 3,pcLinje,";")),LENGTH(TRIM(ENTRY( 3,pcLinje,";"))) - 2,3)).
    /* Setting varegruppen. */
    IF LENGTH(TRIM(ENTRY( 3,pcLinje,";"))) > 3 THEN 
      ttInv.Vg = INT(SUBSTRING(TRIM(ENTRY( 3,pcLinje,";")),1,LENGTH(TRIM(ENTRY( 3,pcLinje,";"))) - 3)).
      
      ASSIGN
        ttInv.KortNavn        = TRIM(ENTRY( 6,pcLinje,";"))
        ttInv.ButikkNr        = INT(TRIM(ENTRY( 2,pcLinje,";")))
        ttInv.DDate           = STRING(YEAR(TODAY)) + '-' +
                                STRING(MONTH(TODAY)) + '-' +
                                STRING(DAY(TODAY)) + ' ' +
                                STRING(TIME,"HH:MM:SS")        
        ttInv.WareHouseNo     = TRIM(ENTRY( 2,pcLinje,";"))
        ttInv.CompanyNumber   = TRIM(ENTRY( 2,pcLinje,";"))
        ttInv.UserName        = USERID('SkoTex')
        ttInv.Lokasjon        = ''
        ttInv.Price           = ''
        ttInv.Lokasjon        = ''
        ttInv.TerminalId      = 'KONV'
        ttInv.TransactionType = '7'
        .
        
    FIND ArtBas NO-LOCK WHERE 
      ArtBas.Vg    = ttInv.Vg AND 
      ArtBas.LopNr = ttInv.LopNr NO-ERROR.

    IF NOT AVAILABLE ArtBas THEN 
        NEXT LESERLINJER.
    ELSE 
      ttInv.ProductNr = STRING(ArtBas.ArtikkelNr).    

    IF AVAILABLE StrType THEN RELEASE StrType.
    IF ttInv.KortNavn <> '' THEN
      FIND FIRST StrType NO-LOCK WHERE 
        StrType.KortNavn = ttInv.KortNavn NO-ERROR.
    IF NOT AVAILABLE StrType THEN 
      NEXT LESERLINJER.
      
    /* I lagerfilen ligger det først 6 kolonner med lagerinfo. Deretter det 30 kolonner pr butikk. */
    /* For 5 butikker blir dette 150 kolonner :)                                                   */
    LAGER_PR_STORRELSE:  
    DO piLoop = 7 TO 30:
      
      FIND FIRST StrType NO-LOCK WHERE StrType.KortNavn = ttInv.KortNavn NO-ERROR.
      IF NOT AVAILABLE StrType THEN 
        NEXT.
      FIND FIRST StrTStr NO-LOCK WHERE 
        StrTStr.StrTypeId = StrType.StrTypeId AND 
        StrTStr.SeqNr = (piLoop - 6) NO-ERROR.
      IF NOT AVAILABLE StrTStr THEN 
        NEXT.
      IF INT(ENTRY(piLoop,pcLinje,";")) = 0 THEN 
        NEXT.  
      ASSIGN cStrl = IF NUM-ENTRIES(StrTStr.SoStorl,".") = 2 
                       THEN TRIM(REPLACE(StrTStr.SoStorl,".","")) 
                       ELSE TRIM(StrTStr.SoStorl) + "0"
             cStrl = FILL("0",4 - LENGTH(cStrl)) + cStrl
             cKode = TRIM(STRING(ttInv.Vg,"999"))     +
                     TRIM(STRING(ttInv.LopNr,"9999")) +
                     "0" +
                     cStrl NO-ERROR.
      FIND FIRST Strekkode NO-LOCK WHERE 
        Strekkode.Bestillingsnummer = cKode NO-ERROR.  
      IF NOT AVAILABLE Strekkode THEN 
      DO:
        FIND StrKonv NO-LOCK WHERE 
          StrKonv.Storl = StrTStr.SoStorl NO-ERROR.
        IF NOT AVAILABLE StrKonv THEN 
        DO:
          FIND LAST bStrKonv NO-LOCK NO-ERROR.
          CREATE StrKonv.
          ASSIGN
            StrKonv.StrKode = IF AVAILABLE bStrKonv 
                                THEN bStrKonv.StrKode + 1
                                ELSE 1
            StrKonv.Storl   = StrTStr.SoStorl
            .          
        END.
        
        CREATE STrekkode.
        ASSIGN
          Strekkode.ArtikkelNr        = DEC(ttInv.ProductNr)
          Strekkode.Kode              = getEAN()
          Strekkode.StrKode           = StrKonv.StrKode
          Strekkode.Bestillingsnummer = cKode
          Strekkode.ERPNr             = TRIM(ENTRY(3,pcLinje,";"))
          .
      END.
      
      CREATE bttInv.
      BUFFER-COPY ttInv TO bttInv
        ASSIGN 
          bttInv.Storl            = StrTStr.SoStorl
          bttInv.c2AV5Interleaved = cKode
          bttInv.Quantity         = ENTRY(piLoop,pcLinje,";")
          bttInv.GTIN             = IF AVAILABLE Strekkode THEN Strekkode.Kode ELSE ''
          bttInv.iFlagg           = 1 
          .    
    END. /* LAGER_PR_STORRELSE */    
        
     STATUS DEFAULT "Lese linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  /* Eksporterer til VPI fil. */
  RUN EksportInvFil.

  IF CAN-FIND(FIRST tt_Error) THEN
    RUN ErrorLogg.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TellOppLinjer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TellOppLinjer Procedure 
PROCEDURE TellOppLinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      iTotAntLinjer = 0
      .
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  repeat:
    IMPORT STREAM InnFil UNFORMATTED cLinje.
    ASSIGN
        iTotAntLinjer = iTotAntLinjer + 1
        .
  END.
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-FixChk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FixChk Procedure 
FUNCTION FixChk RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/
      DEF VAR iCount1 AS INTE NO-UNDO.
      DEF VAR iMulti  AS INTE INIT 1 NO-UNDO.
      DEF VAR iSum AS INTE NO-UNDO.
        DO iCount1 = LENGTH(cKode) TO 1 BY -1:  
            ASSIGN iMulti = IF iMulti = 1 THEN 3 ELSE 1
                   iSum = iSum + INT(SUBSTR(cKode,iCount1,1)) * iMulti.
        END.
        RETURN cKode + string((10 - iSum MODULO 10) MODULO 10).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


&IF DEFINED(EXCLUDE-getEAN) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getEAN Procedure
FUNCTION getEAN RETURNS CHARACTER 
	(  ):
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/

    DEFINE VARIABLE result AS CHARACTER NO-UNDO.

        RUN hentEAN.p (13,2,OUTPUT result). 

    RETURN result.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


