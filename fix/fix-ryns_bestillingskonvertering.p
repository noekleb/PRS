&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : fix-ryns_bestillingskonvertering.p
    Purpose     :

    Syntax      :

    Description : Leser inn bestillingene og omformer den til en pricat fil
                  og en pakkseddel fil.

    Author(s)   : Tom Nøkleby
    Created     : 10/8-13
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
DEFINE VARIABLE cBestFil      AS CHAR NO-UNDO.
DEFINE VARIABLE ctmpKatalog   AS CHAR NO-UNDO.
DEFINE VARIABLE pcLinje       AS CHAR NO-UNDO.
DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE lArtikkelNr   AS DECIMAL NO-UNDO.

DEF VAR cGenEan       AS CHAR NO-UNDO.
DEFINE VARIABLE cDefVg AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
DEFINE VARIABLE cStrl AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKode AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cGenInterleave AS CHARACTER NO-UNDO.
DEFINE VARIABLE iInt AS INTEGER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE dDate AS DATE NO-UNDO.

DEF STREAM InnFil.
DEF STREAM UtVPI.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .
DEF TEMP-TABLE tmpVare
    FIELD VareNr AS CHAR.

DEF TEMP-TABLE ttPkSdl
    FIELD OrdreNr AS CHAR
    FIELD ODato   AS CHAR 
    FIELD BrukerId AS CHAR 
    FIELD PkSdlNr AS CHAR 
    FIELD LevNr AS CHAR 
    FIELD LevNavn AS CHAR
    INDEX PkSdl PkSdlNr
    .
DEF TEMP-TABLE ttPkSdlLinje
    FIELD PkSdlNr AS CHAR 
    FIELD DokNr AS CHAR 
    FIELD StrTypeId AS CHAR 
    FIELD OrdreNr AS CHAR
    FIELD LevNr AS CHAR 
    FIELD SendtDato   AS CHAR 
    FIELD BestNr AS CHAR 
    FIELD LevKod AS CHAR 
    FIELD Beskr AS CHAR 
    FIELD LevDato AS CHAR 
    FIELD ValPris AS CHAR 
    FIELD InnPris AS CHAR 
    FIELD RabKr AS CHAR 
    FIELD Rab% AS CHAR 
    FIELD Pris AS CHAR 
    FIELD ButikkNr AS CHAR 
    FIELD Storl AS CHAR 
    FIELD Bestilt AS CHAR 
    FIELD Strekkode AS CHAR 
    FIELD Varekost AS CHAR 
    FIELD AntBestilt AS CHAR 
    FIELD AntLevert AS CHAR 
    INDEX PkSdlLinje LevNr PkSdlNr DokNr LevKod Beskr ButikkNr StrTypeId Storl InnPris Pris
    .

{ttpricat.i &NEW=" " &SHARED=" "}
DEFINE BUFFER bttPrikat FOR ttPrikat.
DEFINE BUFFER artPriKat FOR ttPrikat.

/* Nye artikler som leses inn som har ukjent varegruppe, tildeles default varegruppe. */
{syspara.i 50 15 25 cDefVg}.

/* Setter interleave strekkoden */
{syspara.i 2 4 17 cGenInterleave}

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
    cFilNavn = '.\kom\in\PRSpfxuser_best.csv'.

{syspara.i 2 4 8 cGenEan}

ASSIGN
    ctmpKatalog = '.\kom\in\' /*SESSION:TEMP-DIRECTORY*/
    cVPIFil     = "PRSPricatB" + '915' + "-" + STRING(TODAY,"99-99-9999") + "-" + STRING(TIME,"HH:MM:SS") + ".csv"
    cVPIFil     = REPLACE(cVPIFil,":","-")
    cBestFil    = cVPIFil
    cBestFil    = REPLACE(cBestFil,"PricatB","PkSdl")
    .

FIND LAST ArtBas NO-LOCK USE-INDEX ArtikkelNr NO-ERROR.
IF AVAILABLE ArtBas THEN 
DO:
  IF ArtBas.ArtikkelNr > 1000000 THEN 
    lArtikkelNr = ArtBas.ArtikkelNr + 1.
END.
ELSE lArtikkelNr = 1000000.
FIND LAST VPIArtBas NO-LOCK USE-INDEX ArtikkelNr NO-ERROR.
IF AVAILABLE VPIArtBas THEN 
DO:
  IF VPIArtBas.ArtikkelNr > lArtikkelNr THEN 
    lArtikkelNr = VPIArtBas.ArtikkelNr + 1.
END.

RUN LesInnFil.

RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-EksportVPIFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportVPIFil Procedure 
PROCEDURE EksportVPIFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR plFilId      AS DEC NO-UNDO.
DEF VAR pbOk         AS LOG NO-UNDO.
DEF VAR piAntLinjer  AS LOG NO-UNDO.
DEFINE VARIABLE i2Ant AS INTEGER NO-UNDO.

/* MESSAGE ctmpKatalog + cVPIUtFil SKIP   */
/*     iAntLinjer                         */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */

IF iAntLinjer > 0 THEN
DO:
    OUTPUT STREAM UtVPI TO VALUE(ctmpKatalog + cVPIFil) NO-ECHO.                    
    EKSPORTFIL:                    
    FOR EACH ttPrikat NO-LOCK
        BREAK BY ttPriKat.LevModellNr
              BY ttPriKat.VareTekst     
              BY ttPriKat.FargeTekst    
              BY ttPriKat.SeqNrStr           
              BY ttPriKat.MarkedsPris:
                
        IF ttPriKat.Str = "DUMMY" THEN 
            NEXT EKSPORTFIL.
             
        EXPORT STREAM UtVpi DELIMITER ";"
          /*  1 */ ttPriKat.R1            
          /*  2 */ ttPriKat.LevNr         
          /*  3 */ ttPriKat.LevModellNr   
          /*  4 */ ttPriKat.EANNr         
          /*  5 */ ttPriKat.VareTekst     
          /*  6 */ ttPriKat.FargeKode     
          /*  7 */ ttPriKat.FargeTekst    
          /*  8 */ ttPriKat.Str          
          /*  9 */ ttPriKat.StrTab        
          /* 10 */ ttPriKat.Varemerke     
          /* 11 */ ttPriKat.Enh           
          /* 12 */ ttPriKat.AntIEnh       
          /* 13 */ ttPriKat.LevPrisEngros 
          /* 14 */ ttPriKat.ValKod        
          /* 15 */ ttPriKat.forhRab%      
          /* 16 */ ttPriKat.suppRab%      
          /* 17 */ ttPriKat.VeilPris      
          /* 18 */ ttPriKat.PAKstru       
          /* 19 */ ttPriKat.LevUke1       
          /* 20 */ ttPriKat.LevUke2       
          /* 21 */ ttPriKat.LevUke3       
          /* 22 */ ttPriKat.LevUke4       
          /* 23 */ ttPriKat.VareGruppe    
          /* 24 */ ttPriKat.LevNavn       
          /* 25 */ ttPriKat.LevKod  
          /* 26 */ ttPriKat.nettoForh     
          /* 27 */ ttPriKat.kalkForh      
          /* 28 */ ttPriKat.BFforh        
          /* 29 */ ttPriKat.nettoSupp     
          /* 30 */ ttPriKat.kalkSupp      
          /* 31 */ ttPriKat.BFsupp        
          /* 32 */ ttPriKat.MarkedsPris   
          /* 33 */ ttPriKat.Sortiment     
          /* 34 */ ttPriKat.Sasong        
          /* 35 */ ttPriKat.VPIBildeKode
          /* 36 */ ttPriKat.Merknad     
          /* 37 */ ttPriKat.KjedeValutaPris 
          /* 38 */ ttPriKat.KjedeProdusent 
          /* 39 */ ttPriKat.ERPNr                
          /* 40 */ ttPriKat.SalgsEnhetsType
          /* 41 */ ttPriKat.AktivFraDato
          /* 42 */ ttPriKat.AktivTilDato
          /* 43 */ ttPriKat.Bongtekst
          /* 44 */ ttPriKat.Etikettekst1
          /* 45 */ ttPriKat.Funksjonskode
          /* 46 */ ttPriKat.Mva_Proc
          /* 47 */ ttPriKat.LinkVare
          /* 48 */ ttPriKat.PantBelop
          /* 49 */ ttPriKat.Filial
          /* 50 */ ttPriKat.Produsent
          /* 51 */ ttPriKat.Mengde
          /* 52 */ ttPriKat.JamforEnhet
          /* 53 */ ttPriKat.Kontrolleres
          /* 54 */ ttPriKat.ArtikkelNr
          /* 55 */ ttPriKat.OpprettArtikkel 
          /* 56 */ ttPriKat.PosterPrisending 
          /* 57 */ ttPriKat.KjedeRab% 
          /* 58 */ ttPriKat.KjedeSupRab% 
          /* 59 */ ''
          /* 60 */ ''
          /* 61 */ ''
          /* 62 */ ''
          /* 63 */ ''
          /* 64 */ ''
          /* 65 */ ''
          /* 66 */ ''
          /* 67 */ ''
          /* 68 */ ''
          /* 69 */ ''
          /* 70 */ ''
          /* 71 */ ''
          /* 72 */ ttPriKat.EkstStrTypeNavn 
          /* 73 */ ''
          /* 74 */ ''
          /* 75 */ ''
          /* 76 */ ''
          /* 77 */ ''
          /* 78 */ ''
          /* 79 */ ''
          /* 80 */ ttPriKat.KjedeInnkPris 
          /* 81 */ ttPriKat.KjedeSupInnkPris 
          /* 82 */ ''
          /* 83 */ ''
          /* 84 */ ''
          /* 85 */ ''
          /* 86 */ ''
          /* 87 */ ''
          /* 88 */ ''
          /* 89 */ ttPriKat.Lager 
          /* 90 */ ttPriKat.Etikett 
          /* 91 */ ttPriKat.Sortimentkoder 
          /* 92 */ ttPriKat.Lagerkoder 
          /* 93 */ ttPriKat.Gjennomfaktureres 
          /* 94 */ ttPriKat.KjedeVare 
          /* 95 */ ttPriKat.Kampanjeuker 
          /* 96 */ ttPriKat.Kampanjestotte
          /* 97 */ ttPriKat.BehStatus   
          /* 98 */ ttPriKat.Grunnsortiment 
          /* 99 */ ttPriKat.Opphav
          /*100 */ ttPriKat.RAvdNr 
          /*101 */ ttPriKat.OrgFilNavn 
          /*102 */ ttPriKat.LoggFilNavn 
          /*103 */ ttPriKat.Etikettekst2
          /*104 */ ttPriKat.ArtSlag 
          /*105 */ ttPriKat.OPris 
          /*106 */ ttPriKat.NON_Sale 
          /*107 */ ttPriKat.NegVare 
          /*108 */ ttPriKat.Pant 
          /*109 */ ttPriKat.Telefonkort 
          /*110 */ ttPriKat.WebButikkArtikkel 
          /*111 */ ttPriKat.PubliserINettbutikk 
          /*112 */ ttPriKat.HoyLav 
          /*113 */ ttPriKat.WebLeveringstid 
          /*114 */ ttPriKat.WebMinLager 
          /*115 */ ttPriKat.KampanjeKode 
          /*116 */ ttPriKat.LopNr         
          .
    END. /* EKSPORTFIL */
    OUTPUT STREAM UtVpi CLOSE.

    MESSAGE 'PRSPricat fil er generert: ' + ctmpKatalog + cVPIFil
    VIEW-AS ALERT-BOX.

    IF CAN-FIND(FIRST ttPkSdl) THEN
    DO:
        i2Ant = 0.
        OUTPUT STREAM UtVPI TO VALUE(ctmpKatalog + cBestFil) NO-ECHO.  
        PAKKSEDDELHODE:
        FOR EACH ttPkSdl:
            PUT STREAM UtVPI UNFORMATTED
                '99;'
                ttPkSdl.OrdreNr ";"
                ttPkSdl.ODato ";"   
                ttPkSdl.BrukerId ";" 
                ttPkSdl.PkSdlNr ";"  
                ttPkSdl.LevNr ";" 
                ttPkSdl.LevNavn 
                SKIP.
            i2Ant = i2Ant + 1.

            PAKKSEDDELLINJE:
            FOR EACH ttPkSdlLinje WHERE
                ttPkSdlLinje.PkSdlNr = ttPkSdl.PkSdlNr:

                PUT STREAM UtVPI UNFORMATTED
                    '1;'
                    ttPkSdlLinje.PkSdlNr    ';' 
                    ttPkSdlLinje.LevNr      ';' 
                                            ';' 
                    ttPkSdlLinje.OrdreNr    ';'
                                            ';' 
                    ttPkSdlLinje.SendtDato  ';' 
                    ttPkSdlLinje.BestNr     ';' 
                    ttPkSdlLinje.LevKod     ';' 
                    ttPkSdlLinje.Beskr      ';' 
                    ttPkSdlLinje.LevDato    ';' 
                    ttPkSdlLinje.ValPris    ';' 
                    ttPkSdlLinje.InnPris    ';' 
                    ttPkSdlLinje.RabKr      ';' 
                    ttPkSdlLinje.Rab%       ';' 
                    ttPkSdlLinje.Pris       ';' 
                                            ';' 
                    ttPkSdlLinje.ButikkNr   ';' 
                    ttPkSdlLinje.Storl      ';' 
                    ttPkSdlLinje.Bestilt    ';' 
                    ttPkSdlLinje.Strekkode  ';' 
                    ttPkSdlLinje.Varekost   ';' 
                    ttPkSdlLinje.AntBestilt ';' 
                    ttPkSdlLinje.AntLevert 
                    /*
                    ttPkSdlLinje.DokNr      ';' 
                    ttPkSdlLinje.StrTypeId  ';' 
                    */
                    SKIP.
                i2Ant = i2Ant + 1.
            END. /* PAKKSEDDELLINJE */
        END. /* PAKKSEDDELHODE */
        OUTPUT STREAM UtVpi CLOSE.
        
        MESSAGE 'PRSBest fil er generert: ' + ctmpKatalog + cBestFil
        VIEW-AS ALERT-BOX.
    END.
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
  
  /* Tømmer feillogg. */
  FOR EACH tt_Error:
    DELETE tt_Error.
  END.
  /* Tømmer pricat */
  FOR EACH ttPrikat TRANSACTION:
      DELETE ttPrikat.
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
    ASSIGN lDec = DECIMAL(ENTRY(1,pcLinje,';')) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        NEXT LESERLINJER.

    /* Tomme linjer fra Excel */
    IF pcLinje BEGINS ";;;;;:" THEN
        NEXT LESERLINJER.
    /* OVerskriftsrad 2 */
    IF pcLinje BEGINS "recnum" THEN
        NEXT LESERLINJER.

    IF NUM-ENTRIES(pcLinje,";") < 64 THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      NEXT LESERLINJER.
    END.

    /* Er bestillingen innlevert, skal den ikke leses inn. */
    IF TRIM(ENTRY(90,pcLinje,";"),'"') <> '' THEN
        NEXT LESERLINJER.
        
    cTekst = TRIM(ENTRY(15,pcLinje,";"),'"').
    /* Svensk */
    IF NUM-ENTRIES(cTekst,'-') = 3 THEN 
      dDate = DATE(INT(ENTRY(3,cTekst,'-')),
                   INT(ENTRY(2,cTekst,'-')),
                   INT(ENTRY(1,cTekst,'-'))).
    ELSE
      dDate = DATE(cTekst).
    /* Bare bestillinger fra 2013. */
    IF dDate < 1/1/2013 THEN 
        NEXT LESERLINJER.
    
    CREATE ttPriKat.
    ASSIGN
        ttPriKat.EkstVPILevNr  = 915
        ttPriKat.LinjeNr       = piLinjeNr
        piLinjeNr              = piLinjeNr  + 1
        lArtikkelNr            = lArtikkelNr + 1
        /*               */ ttPriKat.R1            = "R1"
        /*               */ ttPriKat.LevNr         = TRIM(ENTRY( 8,pcLinje,";"),'"')
        /* nArtno        */ ttPriKat.LevModellNr   = TRIM(ENTRY( 4,pcLinje,";"),'"')                  
        /* cEANCode      */ ttPriKat.EANnr         = ""
        /* rArtName      */ ttPriKat.VareTekst     = TRIM(ENTRY( 7,pcLinje,";"),'"')
        /* NColCode      */ ttPriKat.FargeKode     = "1" /* Farge er ikke i bruk */
        /* cColName      */ ttPriKat.FargeTekst    = ""
        /* cCode1        */ ttPriKat.Str           = "DUMMY"
        /* nOrder        */ ttPriKat.SeqNrStr      = 0 
        /* nSeason(3) + nSizeCode(3) */
                            ttPriKat.StrTab        = TRIM(ENTRY( 12,pcLinje,";"),'"')
        /* nMainGroup    */ ttPriKat.Varemerke     = ""
        /*               */ ttPriKat.Enh           = "St"
        /*               */ ttPriKat.AntIEnh       = '1'

        /* NWholeSaleNet */ ttPriKat.LevPrisEngros = replace(trim(trim(ENTRY(13,pcLinje,";"),'"'),"%"),' ','')
        /*               */ ttPriKat.forhRab%      = "" /*replace(trim(trim(ENTRY(62,pcLinje,";"),'"'),"%"),' ','')*/
        /*               */ ttPriKat.suppRab%      = ""
        /* nRetailPrice  */ ttPriKat.VeilPris      = replace(trim(trim(ENTRY(14,pcLinje,";"),'"'),"%"),' ','')
        /*               */ ttPriKat.PAKstru       = ""
        /*               */ ttPriKat.LevUke1       = ""
        /*               */ ttPriKat.LevUke2       = ""
        /*               */ ttPriKat.LevUke3       = ""
        /*               */ ttPriKat.LevUke4       = ""
        /* nArtgroup + nSubGroup */
                            ttPriKat.VareGruppe    = ENTRY(10,pcLinje,";") 
        /*               */ ttPriKat.LevNavn       = ""
        /* nArtno        */ ttPriKat.LevKod        = "" /*ttPriKat.LevModellNr*/                  
        /* NWholeSaleNet */ ttPriKat.nettoForh     = ""
        /*               */ ttPriKat.kalkForh      = ""
        /*               */ ttPriKat.BFforh        = ""
        /*               */ ttPriKat.nettoSupp     = ""
        /*               */ ttPriKat.kalkSupp      = ""
        /*               */ ttPriKat.BFsupp        = ""
        /* nRetailPrice  */ ttPriKat.MarkedsPris   = REPLACE(trim(trim(ENTRY(14,pcLinje,";"),'"'),"%"),' ','')
        /*               */ ttPriKat.Sortiment     = ""
        /* nSeason       */ ttPriKat.Sesong        = ENTRY(60,pcLinje,";")
                            ttPriKat.VPIBildeKode  = ""
        /*               */ ttPriKat.Merknad       = ENTRY(57,pcLinje,";")
        /*               */ ttPriKat.ERPNr         = ENTRY(74,pcLinje,";") /* DokNr */
        /*               */ ttPriKat.ValKod        = 'SEK' /*ENTRY(65,pcLinje,";")*/
        /*               */ ttPriKat.Kampanjekode  = ENTRY(17,pcLinje,";") /* note */
        /*               */ ttPriKat.ArtikkelNr    = lArtikkelNr
        .      

    /* Kobler om fargekoden og legger på varegruppetekst. */
    FIND Farg NO-LOCK WHERE 
      Farg.FarBeskr = ttPriKat.VareTekst NO-ERROR.
    IF AVAILABLE Farg THEN 
    DO:
      ASSIGN
        ttPriKat.FargeKode  = STRING(Farg.Farg)
        ttPriKat.FargeTekst = Farg.FarBeskr
        .
      FIND VarGr NO-LOCK WHERE 
        VarGr.Vg = INT(ttPriKat.Varegruppe) NO-ERROR.
        IF AVAILABLE VarGr THEN 
          ttPriKat.Varetekst = VarGr.VgBeskr.
    END.

    /* Konvertering av sesong. Heter sesong (char) i prikat + ekstra felt Sasong (Int)*/
    FIND FIRST Sasong NO-LOCK WHERE 
      Sasong.SasBeskr = ttPriKat.Sesong NO-ERROR.
    IF NOT AVAILABLE Sasong THEN 
    DO:
      FIND LAST Sasong NO-LOCK NO-ERROR.
      IF AVAILABLE Sasong 
        THEN iInt = Sasong.Sasong + 1.
        ELSE iInt = 1.
      CREATE Sasong.
      ASSIGN 
        Sasong.Sasong   = iInt
        Sasong.SasBeskr = ttPrikat.Sesong
        ttPriKat.Sasong = INT(STRING(Sasong.Sasong)).
    END.          
    ELSE 
      ttPriKat.Sasong = Sasong.Sasong.
    /*
    /* Kobler mot eksisterende artikkelnr. */
    FIND FIRST artPriKat WHERE 
        artPriKat.LevModellNr    = ttPriKat.LevModellNr AND 
        artPriKat.VareTekst      = ttPriKat.Varetekst AND 
        artPriKat.LevNr          = ttPriKat.LevNr AND 
        artPriKat.VareGruppe     = ttPriKat.Varegruppe AND 
        artPriKat.StrTab         = ttPriKat.StrTab AND 
        artPriKat.LevPrisEngros  = ttPriKat.LevPrisEngros AND 
        artPriKat.MarkedsPris    = ttPriKat.Markedspris AND 
        artPriKat.KampanjeKode   = ttPriKat.KampanjeKode
        NO-ERROR.
    IF AVAILABLE artPriKat THEN 
        ttPriKat.ArtikkelNr    = artPriKat.ArtikkelNr.
    ELSE 
        ttPriKat.ArtikkelNr    = lArtikkelNr.
    */
    
    /* Legger opp pakkseddelhode */
    IF NOT CAN-FIND(FIRST ttPkSdl WHERE
                    ttPkSdl.PkSdlNr = TRIM(ENTRY(3,pcLinje,";"),'"')) THEN
    DO:
        CREATE ttPkSdl.
        ASSIGN
            ttPkSdl.OrdreNr  = TRIM(ENTRY( 3,pcLinje,";"),'"')
            ttPkSdl.ODato    = STRING(dDate)
            ttPkSdl.BrukerId = ''
            ttPkSdl.PkSdlNr  = TRIM(ENTRY( 3,pcLinje,";"),'"')
            ttPkSdl.LevNr    = TRIM(ENTRY( 8,pcLinje,";"),'"')
            ttPkSdl.LevNavn  = ''
            .
    END.

    /* Henter størrelsestypen. */
    FIND FIRST StrType NO-LOCK WHERE
        StrType.KortNavn = TRIM(ENTRY(12,pcLinje,";"),'"') NO-ERROR.
    IF NOT AVAILABLE StrType THEN
        NEXT.
    /* Legger opp pakkseddel linje */
    PAKKSEDDELLINJE:
    DO piLoop = 25 TO 54:
        IF INT(TRIM(ENTRY(piLoop,pcLinje,";"),'"')) = 0 THEN
            NEXT PAKKSEDDELLINJE.
        FIND StrTStr NO-LOCK WHERE
            StrTStr.StrTypeId = StrType.StrTypeId AND
            StrTStr.SeqNr     = piLoop - 24 NO-ERROR.
        IF NOT AVAILABLE StrTStr THEN 
           NEXT  PAKKSEDDELLINJE.
           
        IF AVAILABLE Strekkode THEN RELEASE Strekkode.
        FIND FIRST StrKonv WHERE 
          StrKonv.Storl = StrTStr.SoStorl NO-LOCK NO-ERROR.
        IF ttPriKat.ArtikkelNr > 0 AND AVAILABLE StrKonv THEN 
        DO:
          FIND FIRST Strekkode NO-LOCK WHERE 
            Strekkode.ArtikkelNr = ttPriKat.ArtikkelNr AND
            Strekkode.StrKode    = StrKonv.StrKode NO-ERROR. 
        END.

        /* Det skal bare genereres bestillingsnr på de artikler som har løpenr. */
        /* Ellers skal feltet være blankt.                                      */
        cKode = ''.
        IF cGenInterleave = '1' AND INT(ttPriKat.LopNr) <> 0 AND ttPriKat.LopNr <> ? THEN
        GENINTERLEAVE: 
        DO:
          IF INT(ttPriKat.VareGruppe) <= 999 AND INT(ttPriKat.LopNr) <= 9999 THEN
          DO:
            IF NOT AVAIL StrKonv THEN
                  LEAVE GENINTERLEAVE.
              ASSIGN cStrl = IF NUM-ENTRIES(StrKonv.Storl,".") = 2 
                               THEN TRIM(REPLACE(StrKonv.Storl,".","")) 
                               ELSE TRIM(StrKonv.Storl) + "0"
                     cStrl = FILL("0",4 - LENGTH(cStrl)) + cStrl
                     cKode = TRIM(STRING(INT(ttPriKat.VareGruppe),"999"))     +
                             TRIM(STRING(INT(ttPriKat.LopNr),"9999")) +
                             "0" +
                             cStrl NO-ERROR.
          END.
          ELSE cKode = ''.
        END. /* GENINTERLEAVE */          
            
        CREATE bttPriKat.
        BUFFER-COPY ttPriKat TO bttPrikat
          ASSIGN
          bttPriKat.EANNr  = (IF AVAILABLE Strekkode THEN Strekkode.Kode ELSE getEAN()) /* ttPriKat.EANNr */         
          bttPriKat.Str    = StrTStr.SoStorl /*ttPriKat.Str*/           
          bttPriKat.LevKod = cKode   
          bttPriKat.ERPNr  = STRING(ttPriKat.VareGruppe) + TRIM(STRING(INT(ttPriKat.LopNr),">>>999"))                
          .
        /*    
        IF NOT CAN-FIND(FIRST ttPkSdlLinje WHERE
                        ttPkSdlLinje.LevNr     = TRIM(ENTRY( 8,pcLinje,";"),'"') AND 
                        ttPkSdlLinje.PkSdlNr   = TRIM(ENTRY( 3,pcLinje,";"),'"') AND 
                        ttPkSdlLinje.DokNr     = TRIM(ENTRY(74,pcLinje,";"),'"') AND 
                        ttPkSdlLinje.LevKod    = TRIM(ENTRY( 4,pcLinje,";"),'"') AND
                        ttPkSdlLinje.Beskr     = ttPriKat.Varetekst AND
                        ttPkSdlLinje.ButikkNr  = TRIM(ENTRY( 6,pcLinje,";"),'"') AND 
                        ttPkSdlLinje.StrTypeId = TRIM(ENTRY(12,pcLinje,";"),'"') AND 
                        ttPkSdlLinje.Storl     = StrTStr.SoStorl AND 
                        ttPkSdlLinje.InnPris   = TRIM(ENTRY(13,pcLinje,";"),'"') AND
                        ttPkSdlLinje.Pris      = TRIM(ENTRY(14,pcLinje,";"),'"') 
                        ) THEN
        */
        DO:
            CREATE ttPkSdlLinje.
            ASSIGN
                ttPkSdlLinje.PkSdlNr    = TRIM(ENTRY( 3,pcLinje,";"),'"')
                ttPkSdlLinje.DokNr      = TRIM(ENTRY(74,pcLinje,";"),'"')
                ttPkSdlLinje.StrTypeId  = TRIM(ENTRY(12,pcLinje,";"),'"')
                ttPkSdlLinje.OrdreNr    = TRIM(ENTRY( 3,pcLinje,";"),'"')
                ttPkSdlLinje.LevNr      = TRIM(ENTRY( 8,pcLinje,";"),'"')
                ttPkSdlLinje.SendtDato  = STRING(dDate)
                ttPkSdlLinje.BestNr     = ''
                ttPkSdlLinje.LevKod     = TRIM(ENTRY( 4,pcLinje,";"),'"')
                ttPkSdlLinje.Beskr      = ttPriKat.Varetekst
                ttPkSdlLinje.LevDato    = TRIM(ENTRY( 9,pcLinje,";"),'"')
                ttPkSdlLinje.ValPris    = TRIM(ENTRY(13 ,pcLinje,";"),'"')
                ttPkSdlLinje.InnPris    = TRIM(ENTRY(13,pcLinje,";"),'"')
                ttPkSdlLinje.RabKr      = ''
                ttPkSdlLinje.Rab%       = ''
                ttPkSdlLinje.Pris       = TRIM(ENTRY(14,pcLinje,";"),'"')
                ttPkSdlLinje.ButikkNr   = TRIM(ENTRY( 6,pcLinje,";"),'"')
                ttPkSdlLinje.Storl      = StrTStr.SoStorl
                ttPkSdlLinje.Bestilt    = ''
                ttPkSdlLinje.Strekkode  = bttPriKat.EANNr
                ttPkSdlLinje.Varekost   = TRIM(ENTRY(13,pcLinje,";"),'"')
                ttPkSdlLinje.AntBestilt = TRIM(ENTRY(piLoop,pcLinje,";"),'"')
                ttPkSdlLinje.AntLevert  = TRIM(ENTRY(piLoop,pcLinje,";"),'"')
                .
        END.

        IF LENGTH(ttPkSdlLinje.LevDato) = 3 THEN 
        DO:
          ttPkSdlLinje.LevDato = STRING(DATE(INT(SUBSTRING(ttPkSdlLinje.LevDato,2,2)),
                                      IF SUBSTRING(ttPkSdlLinje.LevDato,2,2) = '1' 
                                        THEN 15
                                        ELSE 28,  
                                       2013)).  
        END.
    END. /* PAKKSEDDELLINJE */


    STATUS DEFAULT "Lese linje " + 
                   STRING(iAntLinjer) + 

                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  /* Eksporterer til VPI fil. */
  RUN EksportVPIFil.

  IF CAN-FIND(FIRST tt_Error) THEN
    RUN ErrorLogg.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-settVaregruppe) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE settVaregruppe Procedure
PROCEDURE settVaregruppe:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
  IF NOT AVAILABLE ttPriKat 
      THEN RETURN.
  FIND Strekkode NO-LOCK WHERE 
    Strekkode.Kode = ttPriKat.EANnr NO-ERROR.
  IF AVAILABLE Strekkode THEN 
    FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
  IF AVAILABLE ArtBas THEN 
  DO:
    IF CAN-FIND(VarGr OF ArtBas) THEN 
      ttPriKat.VareGruppe = STRING(ArtBas.Vg).
    ELSE
      ttPriKat.VareGruppe = cDefVg.   
  END.
  ELSE
    ttPriKat.VareGruppe = cDefVg.   

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


