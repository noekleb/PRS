&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : eksportlager.p
    Purpose     : Eksport av lager fra butikk til hk.
                  Eksporterer alle lagerposter som har endret seg siste 10
                  døgn.

    Syntax      :

    Description :

    Author(s)   : Tom Nøkleby
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    
&ELSE
    
&ENDIF

DEFINE INPUT  PARAMETER dinpFraDato AS DATE NO-UNDO.
DEFINE INPUT  PARAMETER dinpTilDato AS DATE NO-UNDO.
DEFINE INPUT  PARAMETER bLagerSjekk AS LOG  NO-UNDO.
DEFINE OUTPUT PARAMETER ocRetur     AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  iAntEksport AS INTEGER    NO-UNDO.

DEF VAR iAntLinjer          AS INT     NO-UNDO.
DEF VAR iAlle               AS INT     NO-UNDO.
DEF VAR bStream             AS LOG     NO-UNDO.
DEFINE VARIABLE lDec        AS DECIMAL NO-UNDO.
DEFINE VARIABLE bManuell    AS LOG     NO-UNDO.
DEFINE VARIABLE dFraDato    AS DATE    NO-UNDO.
DEFINE VARIABLE dTilDato    AS DATE    NO-UNDO.
DEFINE VARIABLE pdLoopDato  AS DATE    NO-UNDO.
DEFINE VARIABLE lTid        AS INTEGER NO-UNDO.
DEFINE VARIABLE cLinje      AS CHARACTER NO-UNDO.

/* Filhåndtering */
DEF VAR cFilNavn   AS CHAR FORMAT "x(40)"     NO-UNDO.
DEFINE VARIABLE ctmpFilNavn AS CHARACTER NO-UNDO.
DEF VAR cKatalog   AS CHAR                    NO-UNDO.
DEF VAR cPrefix    AS CHAR                    NO-UNDO.
DEFINE VARIABLE cKopi    AS CHARACTER NO-UNDO.
DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst   AS CHARACTER NO-UNDO.
DEFINE VARIABLE bStreamApen  AS LOG NO-UNDO.

DEFINE STREAM Inn.
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

ASSIGN lTid = TIME.

IF dinpFraDato = ? OR dinpTilDato = ? THEN 
DO: 
  RUN SettFraTilDato.
  RUN bibl_logg.p ('eksporter_lager', 'eksporterLager.p: AUTO WinCheduler Periode: ' + string(dinpFraDato) + ' - ' + string(dinpTilDato) + ' ' + string(TIME,"HH:MM:SS")).
END.
ELSE DO:
  ASSIGN 
    dFraDato = dinpFraDato
    dTilDato = dinpTilDato
    bManuell = TRUE. /* Flagger at eksporten kjøres manuelt og at det ikke skal sendes eMail. */
  RUN bibl_logg.p ('eksporter_lager', 'eksporterLager.p: MANUELL Periode: ' + string(dinpFraDato) + ' - ' + string(dinpTilDato) + ' ' + string(TIME,"HH:MM:SS")).
END.

/* Legger ut data til fil. */
RUN Eksporter.

ocRetur = "OK," + String(iAntEksport) + cTekst.

lTid = TIME - lTid.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Eksporter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Eksporter Procedure 
PROCEDURE Eksporter :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEFINE VARIABLE iButikkNr       AS INT       NO-UNDO.
DEFINE VARIABLE cFilNavn        AS CHAR      NO-UNDO.
DEFINE VARIABLE iAnt            AS INT       NO-UNDO.
DEFINE VARIABLE cSistSolgtDato  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVVareKost      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lIkkeInnlevertAnt AS DECIMAL NO-UNDO.
DEFINE VARIABLE lIkkeInnlevertVerdi AS DECIMAL NO-UNDO.

{syspara.i 5 1 1 iButikkNr INT}

ASSIGN
    cKatalog    = RIGHT-TRIM(cKatalog,'\')
    cKopi       = cKatalog + "\purpleo"
    cFilNavn    = cKatalog + '\' + 'POSL' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.
    ctmpFilNavn = cKatalog + '\' + 'tmpPOSL' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.

BUTIKKLOOP:
FOR EACH Butiker NO-LOCK WHERE
  Butiker.ApningsDato     <> ? AND
  Butiker.NedlagtDato     = ?: 
  
OUTPUT STREAM Ut TO VALUE(ctmpFilNavn + STRING(Butiker.Butik)).

RUN bibl_logg.p ('eksporter_lager', 'eksporterLager.p Butikk: ' + string(Butiker.Butik)+ ' Fil: ' + string(cFilNavn) + STRING(Butiker.Butik)).

EXPORT STREAM Ut
    'Butik;'
    'VismaNr (ArtNr+StrKode);'
    'EAN;'
    'ERPNr;'
    'ModellNr;'
    'Varenavn;'
    'Farge;'
    'Størrelse;'
    'Varegruppenr;'
    'Leverandørnr;'
    'Leverandørnavn;'
    'Beholdning;'
    'Beholdningsdato;'
    'Sist solgtdato;'
    'VVarekost'
    /*
    'MinAnt;'
    'MaksAnt;'
    'BestillAnt;'
    'Ikke innlevert ant;'
    'Ikke innlevert verdi;'
    'UtPris'
    */
    SKIP.

UTLEGG_LAGER:
FOR EACH Lager NO-LOCK WHERE
    Lager.Butik = Butiker.Butik,
    FIRST ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = Lager.ArtikkelNr,
    FIRST ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = Lager.ArtikkelNr,
    EACH ArtLag NO-LOCK WHERE
         ArtLag.ArtikkelNr = Lager.ArtikkelNr AND
         ArtLag.Butik      = Lager.Butik:

    /* Tar bare med aktuelle lagerposter for artikkel. */
    IF bLagerSjekk THEN 
    DO:
      IF Lager.EDato < dFraDato OR 
        Lager.EDato > dTilDato THEN 
        NEXT UTLEGG_LAGER.
    END.
    
    FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
    FIND FIRST StrKonv NO-LOCK WHERE
        StrKonv.StrKode = ArtLag.StrKode NO-ERROR.

    /* Henter min/maks og bestillt antall */
    FIND ArtBestPkt NO-LOCK WHERE 
         ArtBestPkt.ArtikkelNr = ArtBas.ArtikkelNr AND 
         ArtBestPkt.ButikkNr   = Lager.Butik AND 
         ArtBestPkt.StrKode    = ArtLag.StrKode NO-ERROR.

    ASSIGN
      cSistSolgtDato  = ''.
      
    LOOPEN:
    FOR EACH TransLogg NO-LOCK WHERE
      TransLogg.ArtikkelNr = Lager.ArtikkelNr AND
      Translogg.Storl      = ArtLag.Storl AND
      TransLogg.Dato       <= dTilDato AND
      TransLogg.Tid        >= 0 AND
      TransLogg.Butik      = Lager.Butik AND
      TransLogg.TTId       = 1
      BY Translogg.ArtikkelNr
      BY TransLogg.Storl
      BY TransLogg.Dato DESCENDING:
  
      cSistSolgtDato = STRING(TransLogg.Dato).
      LEAVE LOOPEN.
    END. /* LOOPEN */

    ASSIGN 
        lIkkeInnlevertAnt   = 0
        lIkkeInnlevertVerdi = 0
        .
    
    FOR EACH PkSdlHode NO-LOCK WHERE
      PkSdlHode.PkSdlStatus = 10,
      EACH PkSdlLinje OF PkSdlHode NO-LOCK WHERE 
        PkSdlLinje.ArtikkelNr = ArtBas.ArtikkelNr AND 
        PkSdlLinje.StrKode    = ArtLag.StrKode AND 
        PkSdlLinje.ButikkNr   = ArtLag.Butik,
        FIRST pkSdlPris OF PkSdlHode NO-LOCK WHERE 
          PkSdlPris.ArtikkelNr = Artlag.ArtikkelNr:
        
        IF PkSdlHode.PkSdlStatus = 10 THEN
            ASSIGN 
              lIkkeInnlevertAnt   = lIkkeInnlevertAnt + PkSdlLinje.Antall
              lIkkeInnlevertVerdi = lIkkeInnlevertVerdi + (PkSdlLinje.Antall * PkSdlPris.NyVarekost)
              .
        ELSE 
            ASSIGN 
              lIkkeInnlevertAnt   = lIkkeInnlevertAnt + PkSdlLinje.AntLevert
              lIkkeInnlevertVerdi = lIkkeInnlevertVerdi + (PkSdlLinje.AntLevert * PkSdlPris.NyVarekost)
              .
    END.    
    /* Setter vektet varekost. */
    IF Lager.VVareKost = ? OR Lager.VVareKost <= 0 THEN 
      cVVarekost = STRING(ArtPris.VareKost[1]).
    ELSE cVVareKost = STRING(ROUND(Lager.VVareKost,2)).
    IF cVVareKost = ? OR dec(cVVareKost) <= 0 THEN 
      cVVareKost = '0'.
    
    FOR EACH Strekkode NO-LOCK WHERE
        Strekkode.ArtikkelNr = Lager.ArtikkelNr AND
        Strekkode.StrKode    = ArtLag.StrKode:

      PUT STREAM Ut UNFORMATTED
        ArtLag.Butik ';'
        STRING(ArtBas.ArtikkelNr) + TRIM(STRING(ArtLag.StrKode,">>>>>>>>>>999")) ';'
        Strekkode.Kode ';'
        Strekkode.ERPNr  ';'    
        REPLACE(ArtBas.LevKod,';',',') ';'
        REPLACE(ArtBas.Beskr,';',',') ';'
        REPLACE(ArtBas.LevFargKod,';',',') ';'
        TRIM((IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')) ';' 
        ArtBas.Vg ';'
        ArtBas.LevNr ';' 
        (IF AVAILABLE LevBas THEN LevBas.LevNamn ELSE '') ';'
        ArtLag.LagAnt ';' 
        TODAY ';'  
        cSistSolgtDato ';'  
        cVVarekost /*';'  
        (IF AVAILABLE ArtBestPkt THEN STRING(ArtBestPkt.MinAnt) ELSE '') ';'
        (IF AVAILABLE ArtBestPkt THEN STRING(ArtBestPkt.MaksAnt) ELSE '') ';'
        (IF AVAILABLE ArtBestPkt THEN STRING(ArtBestPkt.BestAnt) ELSE '') ';'
        lIkkeInnlevertAnt ';'
        lIkkeInnlevertVerdi ';'
        ArtPris.Pris[1]*/
        SKIP.
    END.

    iAnt = IAnt + 1.
    
END. /* UTLEGG_LAGER */
    
OUTPUT STREAM Ut CLOSE.

    /* Gir filen dens riktige navn og tar bort den temporære filen. */
    OS-COPY value(ctmpFilNavn + STRING(Butiker.Butik)) value(cFilNavn + STRING(Butiker.Butik)).
    IF SEARCH(cFilNavn + STRING(Butiker.Butik)) <> ? THEN
        OS-DELETE VALUE(ctmpFilNavn + STRING(Butiker.Butik)).

    /*
    /* Sikrer at backup katalog finnes. */
    OS-CREATE-DIR value(cKopi).

    /*
    /* Legger en kopi av filen i egen katalog. */
    OS-COPY value(cFilNavn + STRING(Butiker.Butik)) 
            value(cKopi).
    */        
    /* Appender en kopi av filen inn i en felles fil. */
    cTekst = ENTRY(NUM-ENTRIES(cFilNavn,'\'),cFilNavn,'\').
    IF NUM-ENTRIES(cTekst,'.') > 0 THEN
    DO: 
      ENTRY(2,cTekst,'.') = 'txt'.
      IF SEARCH(cKopi + '\' + cTekst) = ? THEN 
        OS-COMMAND SILENT VALUE('TYPE '+ cFilNavn + STRING(Butiker.Butik) + ' >> ' + cKopi + '\' + cTekst).
      ELSE DO:
        INPUT STREAM Inn FROM VALUE(cFilNavn + STRING(Butiker.Butik)) NO-ECHO.
        OUTPUT STREAM Ut TO VALUE(cKopi + '\' + cTekst) APPEND NO-ECHO.
        
        REPEAT:
          IMPORT STREAM Inn UNFORMATTED cLinje.
          IF cLinje BEGINS '"Butik;' THEN NEXT.
          ELSE PUT STREAM Ut UNFORMATTED cLinje SKIP.
        END.
        
        OUTPUT STREAM Ut CLOSE.
        INPUT STREAM Inn CLOSE.
      END.
      
    END.
    */
END. /* BUTIKKLOOP */
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SettFraTilDato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettFraTilDato Procedure 
PROCEDURE SettFraTilDato :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE pdDato AS DATE NO-UNDO.

{syspara.i 210 202 1 pcTekst}
ASSIGN
  pdDato = DATE(pcTekst) NO-ERROR.
IF ERROR-STATUS:ERROR OR pcTekst = '' 
THEN ASSIGN pdDato = TODAY.
ELSE ASSIGN pdDato = pdDato + 1.

ASSIGN
  dFraDato    = pdDato - 10
  dTilDato    = (IF pdDato > TODAY THEN pdDato ELSE TODAY)
  dinpFraDato = dFraDato
  dinpTilDato = dTilDato
  .

DO TRANSACTION:
  {setsyspara.i 210 101 1 STRING(dTilDato)}
  IF AVAILABLE SysPara THEN 
    RELEASE SysPara.
END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

