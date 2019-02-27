&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : fix-ryns_salgkonvertering.p
    Purpose     :

    Syntax      :

    Description : Leser inn filen og omformer den til en PRSTran<NNN..>.<ButNr> fil.

    Author(s)   : Tom Nøkleby
    Created     : 12/8-13
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
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

DEF STREAM InnFil.
DEF STREAM UtVPI.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .

DEFINE TEMP-TABLE ttTranslogg LIKE TransLogg
    INDEX ttTransLogg1 Butik Vg LopNr Dato
    INDEX ttTransLogg2 Butik Dato Vg LopNr 
    . 
    
DEF BUFFER bttTranslogg FOR ttTranslogg.

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
    cFilNavn = '.\kom\in\PRSpfxuser_sales.csv'.

{syspara.i 2 4 8 cGenEan}

ASSIGN
    ctmpKatalog = '.\kom\in\' /*SESSION:TEMP-DIRECTORY*/
    cVPIFil     = "PRSTrans" + STRING(TODAY,"99-99-9999") + "-" + STRING(TIME,"HH:MM:SS") + ".csv"
    cVPIFil     = REPLACE(cVPIFil,":","-")
    .

RUN LesInnFil.

RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-EksportTransFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportTransFil Procedure 
PROCEDURE EksportTransFil :
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
    FOR EACH ttTranslogg NO-LOCK WHERE 
        BREAK BY ttTransLogg.Butik
              BY ttTransLogg.Vg     
              BY ttTransLogg.LopNr
              BY ttTranslogg.Dato:

        IF FIRST-OF(ttTransLogg.Butik) THEN
          DO: 
            cEkspFil = REPLACE(ctmpKatalog + cVPIFil,'.csv','.' + STRING(ttTransLogg.Butik)). 
            OUTPUT STREAM UtVPI TO VALUE(cEkspFil) NO-ECHO.                    
          END.

        PUT STREAM UtVPI UNFORMATTED  
          ttTransLogg.Butik ';'                  
          ttTransLogg.ForsNr ';'                 
          ttTransLogg.TTId ';'                   
          ttTransLogg.TBId ';'                   
          ttTransLogg.Vg ';'                     
          ttTransLogg.LopNr ';'                  
          ttTransLogg.ArtikkelNr ';'             
          ttTransLogg.BongTekst ';'              
          ttTransLogg.LevNr ';'                  
          ttTransLogg.BongId ';'                 
          ttTransLogg.BongLinjeNr ';'            
          ttTransLogg.KassaNr ';'                
          ttTransLogg.Storl ';'                  
          ttTransLogg.Antall ';'                 
          ttTransLogg.Pris ';'                   
          ttTransLogg.Mva ';'                    
          ttTransLogg.RabKr ';'                  
          ttTransLogg.Mva% ';'                   
          ttTransLogg.Varekost ';'               
          ttTransLogg.VVarekost ';'              
          ttTransLogg.SattVVareKost ';'          
          ttTransLogg.KalkylePris ';'            
          ttTransLogg.Plukket ';'                
          ttTransLogg.Dato ';'                   
          ttTransLogg.Tid ';'                    
          ttTransLogg.BestNr ';'                 
          ttTransLogg.OvButik ';'                
          ttTransLogg.OvTransNr ';'              
          ttTransLogg.TilStorl ';'               
          ttTransLogg.MedlemsNr ';'              
          ttTransLogg.KortNr ';'                 
          ttTransLogg.KortType ';'               
          ttTransLogg.KundNr ';'                 
          ttTransLogg.ProfilNr ';'               
          ttTransLogg.SelgerNr ';'               
          ttTransLogg.Kode ';'                   
          ttTransLogg.RefNr ';'                  
          ttTransLogg.RefTekst               
        SKIP.  

        IF LAST-OF(ttTransLogg.Butik) THEN 
          DO:
            OUTPUT STREAM UtVpi CLOSE.
            MESSAGE 'PRSTrans fil er generert: ' + cEkspFil
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
  
  DEFINE VARIABLE dDato AS DATE NO-UNDO.
  DEFINE VARIABLE cKode AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cStrl AS CHARACTER NO-UNDO.      
  
  /* Tømmer feillogg. */
  FOR EACH tt_Error:
    DELETE tt_Error.
  END.
  /* Tømmer pricat */
  FOR EACH ttTransLogg TRANSACTION:
      DELETE ttTransLogg.
  END.
  RUN TellOppLinjer.

  FIND FIRST PrisProfil NO-LOCK.

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
    IF NUM-ENTRIES(pcLinje,";") < 27 THEN
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

    /* Varegruppe/løpenr */
    ASSIGN cTekst = TRIM(ENTRY(2,pcLinje,';')) NO-ERROR.
    ASSIGN dDato  = DATE(TRIM(ENTRY(6,pcLinje,';'))) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        NEXT LESERLINJER.
    
    IF dDato = ? THEN 
        NEXT LESERLINJER.

    IF dDato >= 01/01/2011 AND dDato < 01/01/2013 THEN. /* Skal med */
    ELSE NEXT LESERLINJER.
      
    CREATE ttTranslogg.

    ASSIGN ttTransLogg.Butik        = INT(TRIM(ENTRY(3,pcLinje,';')))
           ttTransLogg.TransNr      = 0
           ttTransLogg.SeqNr        = 0
           ttTranslogg.BatchNr      = 0
           ttTranslogg.Vg           = INT(TRIM(ENTRY(17,pcLinje,';')))
           ttTranslogg.LopNr        = INT(SUBSTRING(cTekst,LENGTH(cTekst) - 2,3))
           ttTranslogg.TTId         = 1 /* Salg */
           ttTranslogg.TBId         = 1
           NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        NEXT LESERLINJER.
    FIND ArtBas NO-LOCK WHERE
      ArtBas.Vg    = ttTransLogg.Vg AND 
      ArtBas.LopNr = ttTransLogg.LopNr NO-ERROR.
    IF AVAILABLE ArtBas THEN 
    DO:
      FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
      ASSIGN 
        ttTransLogg.ArtikkelNr  = ArtBas.ArtikkelNr
        ttTranslogg.LevNr       = ArtBas.LevNr
        ttTransLogg.KalkylePris = DEC(TRIM(ENTRY(27,pcLinje,';')))
        ttTransLogg.KalkylePris = (IF AVAILABLE ArtPris AND ttTransLogg.KalkylePris = 0 THEN ArtPris.Pris[1] ELSE ttTransLogg.KalkylePris)
        ttTransLogg.Varekost    = (IF AVAILABLE ArtPris THEN ArtPris.Varekost[1] ELSE 0)
        .
    END.
    ELSE
      ASSIGN
        ttTransLogg.ArtikkelNr = 0.

    ASSIGN 
           ttTranslogg.Antall        = DEC(TRIM(ENTRY( 8,pcLinje,';')))
           ttTranslogg.Dato          = DATE(TRIM(ENTRY( 6,pcLinje,';')))
           ttTranslogg.Tid           = (INT(TRIM(ENTRY(10,pcLinje,';'))) * 3600) + (INT(TRIM(ENTRY(19,pcLinje,';'))) * 60)
           ttTranslogg.Pris          = ABS(DEC(TRIM(ENTRY( 9,pcLinje,';'))) / ABSOLUTE(ttTranslogg.Antall))
           ttTranslogg.Pris          = IF ttTranslogg.Pris = ?
                                       THEN 0
                                       ELSE ttTranslogg.Pris
           ttTranslogg.BongId        = INT(TRIM(ENTRY( 5,pcLinje,';')))
           ttTranslogg.BongLinjeNr   = 1
           ttTranslogg.ForsNr        = 1 /* Kasserer */
           ttTranslogg.Plukket       = TRUE
           ttTranslogg.SelgerNr      = INT(TRIM(ENTRY( 7,pcLinje,';')))
           ttTranslogg.Postert       = FALSE
           ttTranslogg.BongTekst     = TRIM(ENTRY(21,pcLinje,';'))
           ttTranslogg.VVareKost     = ABS(DEC(TRIM(ENTRY(25,pcLinje,';'))))
           ttTranslogg.VVareKost     = IF ttTranslogg.VVareKost = ? THEN 0 ELSE ttTranslogg.VVareKost
           ttTranslogg.SattVVarekost = IF ttTranslogg.VVareKost = 0 THEN FALSE ELSE TRUE
           ttTranslogg.Mva%          = 25  
           ttTranslogg.Mva           = ABS(ttTranslogg.Pris - (ttTranslogg.Pris / (1 + (ttTranslogg.Mva% / 100))))
           ttTranslogg.Mva           = (IF ttTranslogg.Mva = ? THEN 0 ELSE ttTranslogg.Mva)
           ttTransLogg.ProfilNr      = PrisProfil.ProfilNr
           ttTransLogg.Storl         = TRIM(ENTRY(14,pcLinje,';'))
           NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        DO: 
          IF AVAILABLE ttTransLogg THEN DELETE ttTransLogg.
          NEXT LESERLINJER.
        END.
        
    IF ttTranslogg.Storl = '' THEN 
      ttTransLogg.Storl = ' 1'.   
        
    /* Overstyrer for 002-Brekkasje, 005-Varekjøp, 006-Overføring og 011-Internt forbruk. */
    /* Disse transaksjonene skal håndteres til varekost.                  */
    IF CAN-DO('002,005,006,011',STRING(ttTransLogg.TTId,"999")) THEN 
    DO:
      ASSIGN
        TransLogg.Pris          = ttTransLogg.Varekost
        TransLogg.RabKr         = 0
        TransLogg.SubtotalRab   = 0
        TransLogg.VVareKost     = 0
        TransLogg.SattVVarekost = FALSE
        TransLogg.Mva           = 0
        TransLogg.Mva%          = 0
        .
    END.
        
    STATUS DEFAULT "Lese linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  /* Eksporterer til VPI fil. */
  RUN EksportTransFil.

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


