&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : eksportKomplettlager.p
    Purpose     : Eksport av komplett lager fra butikk til hk.

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

DEFINE OUTPUT PARAMETER ocRetur     AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  iAntEksport AS INTEGER    NO-UNDO.

DEF VAR iAntLinjer          AS INT     NO-UNDO.
DEF VAR iAlle               AS INT     NO-UNDO.
DEF VAR bStream             AS LOG     NO-UNDO.
DEFINE VARIABLE lDec        AS DECIMAL NO-UNDO.
DEFINE VARIABLE pdLoopDato  AS DATE    NO-UNDO.
DEFINE VARIABLE lTid        AS INTEGER NO-UNDO.
DEFINE VARIABLE cLinje      AS CHARACTER NO-UNDO.

/* Filhåndtering */
DEF VAR cFilNavn   AS CHAR FORMAT "x(40)"     NO-UNDO.
DEFINE VARIABLE ctmpFilNavn AS CHARACTER NO-UNDO.
DEF VAR cKatalog   AS CHAR                    NO-UNDO.
DEFINE VARIABLE ctmpKatalog AS CHARACTER NO-UNDO.
DEF VAR cPrefix    AS CHAR                    NO-UNDO.
DEFINE VARIABLE cKopi    AS CHARACTER NO-UNDO.
DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst   AS CHARACTER NO-UNDO.
DEFINE VARIABLE bStreamApen  AS LOG NO-UNDO.
DEFINE VARIABLE cEanLst AS CHARACTER NO-UNDO.

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

{syspara.i 1 1 51 cKatalog}
IF cKatalog = '' THEN
  cKatalog = 'c:\home\lindbak\sendes'.

ASSIGN lTid = TIME
ctmpKatalog = SESSION:TEMP-DIRECTORY.

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
DEFINE VARIABLE cSistKjoptDato  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVVareKost      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lIkkeInnlevertAnt AS DECIMAL NO-UNDO.
DEFINE VARIABLE lIkkeInnlevertVerdi AS DECIMAL NO-UNDO.
 
{syspara.i 5 1 1 iButikkNr INT}
 
ASSIGN
    cKatalog    = RIGHT-TRIM(cKatalog,'\')
    cFilNavn    = cKatalog + '\' + 'POSTOTN' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.
    ctmpFilNavn = ctmpKatalog + '\' + 'tmpPOSTOTN' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.

BUTIKKLOOP:
FOR EACH Butiker NO-LOCK WHERE
  Butiker.ApningsDato     <> ? AND
  Butiker.NedlagtDato     = ? AND 
  CAN-FIND(FIRST Lager WHERE Lager.Butik = Butiker.Butik): 
  
  OUTPUT STREAM Ut TO VALUE(ctmpFilNavn + STRING(Butiker.Butik)).

  RUN bibl_logg.p ('eksporter_lager', 'eksporterKomplettLager.p Butikk: ' + string(Butiker.Butik)+ ' Fil: ' + string(cFilNavn) + STRING(Butiker.Butik)).

  PUT STREAM Ut 
    'Butik;'
    'Dato;'
    'SE ArtikkelNr + StrKode;'
    'Artikkeltekst;'
    'LevNr;'
    'Farge;'
    'Størrelse;'
    'Modell;'
    'Varegruppe;'
    'Mellomgruppe;'
    'Hovedgruppe;'
    'Avdeling;'
    'Varemerke;'
    'Momskode;'
    'Gjennomfakturert;'
    'Kjedelevert;'
    'Siste bestillingspris Supplering;'
    'Siste bestillingspris Forhånd;'
    'Snittpris;'
    'Antall på lager;'
    'Sist solgt dato;'
    'Sist kjøpt dato;'
    'EAN liste;'    
    'MinAnt;'
    'MaksAnt;'
    'BestillAnt;'
    'Ikke innlevert ant;'
    'Ikke innlevert verdi;'
    'UtPris;'
    'Åpen pris'
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

    FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE VarGr THEN 
      FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
    FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
    FIND FIRST StrKonv NO-LOCK WHERE
        StrKonv.StrKode = ArtLag.StrKode NO-ERROR.
    FIND VareMerke OF Artbas NO-LOCK NO-ERROR.
    
    /* Henter min/maks og bestillt antall */
    FIND ArtBestPkt NO-LOCK WHERE 
         ArtBestPkt.ArtikkelNr = ArtBas.ArtikkelNr AND 
         ArtBestPkt.ButikkNr   = Lager.Butik AND 
         ArtBestPkt.StrKode    = ArtLag.StrKode NO-ERROR.

    ASSIGN
      cEANLst = ''.
    FOR EACH Strekkode NO-LOCK WHERE 
        Strekkode.ArtikkelNr = ArtBas.ArtikkelNr AND 
        Strekkode.StrKode    = ArtLag.StrKode:
        cEANLst = cEANLst + ',' + Strekkode.Kode.   
    END.
    cEANLst = LEFT-TRIM(cEANLst,',').
    
    ASSIGN   
      cSistSolgtDato  = ''.
    LOOPEN1:
    FOR EACH TransLogg NO-LOCK WHERE
      Translogg.ArtikkelNr  = ArtBas.ArtikkelNr AND
      TransLogg.Storl       = ArtLAg.Storl AND 
      TransLogg.Dato       <= TODAY AND
      TransLogg.Tid        >= 0 AND 
      TransLogg.Butik       = Butiker.Butik AND 
      TransLogg.TTId        = 1 
      BY TransLogg.ArtikkelNr
      BY Translogg.Storl
      BY TransLogg.Dato DESCENDING:
      cSistSolgtDato = STRING(Translogg.Dato).
      IF cSistSolgtDato = ? THEN 
          cSistSolgtDato = ''.
      LEAVE LOOPEN1.
    END. /* LOOPEN1 */
    
    ASSIGN
      cSistKjoptDato  = ''.
    LOOPEN2:
    FOR EACH TransLogg NO-LOCK WHERE
      Translogg.ArtikkelNr  = ArtBas.ArtikkelNr AND
      TransLogg.Storl       = ArtLAg.Storl AND 
      TransLogg.Dato       <= TODAY AND
      TransLogg.Tid        >= 0 AND 
      TransLogg.Butik       = Butiker.Butik AND 
      TransLogg.TTId        = 5 
      BY TransLogg.ArtikkelNr
      BY TransLogg.Storl
      BY TransLogg.Dato DESCENDING:
      cSistKjoptDato = STRING(Translogg.Dato).
      IF cSistKjoptDato = ? THEN 
          cSistKjoptDato = ''.
      LEAVE LOOPEN2.
    END. /* LOOPEN2 */

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
    ELSE cVVareKost = STRING(Lager.VVareKost).
    IF cVVareKost = ? OR dec(cVVareKost) <= 0 THEN 
      cVVareKost = '0'.
    
    PUT STREAM Ut UNFORMATTED
      ArtLag.Butik ';'
      REPLACE(STRING(TODAY),'/','.') ';'
      STRING(ArtBas.ArtikkelNr) + TRIM(STRING(ArtLag.StrKode,">>>>>>>>>>999")) ';'
      REPLACE(ArtBas.Beskr,';',',') ';'
      ArtBas.LevNr ';'
      REPLACE(ArtBas.LevFargKod,';',',') ';'
      TRIM((IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')) ';' 
      REPLACE(ArtBas.LevKod,';',',') ';'
      ArtBas.Vg ';'
      ';'
      (IF AVAILABLE VarGr THEN VarGr.Hg ELSE 0) ';' 
      (IF AVAILABLE HuvGr THEN HuvGr.AvdelingNr ELSE 0) ';' 
      (IF AVAILABLE Varemerke THEN Varemerke.Beskrivelse ELSE '') ';' 
      (IF AVAILABLE VarGr THEN VarGr.MomsKod ELSE 0) ';' 
      ArtBas.Gjennomfaktureres ';'
      ArtBas.KjedeVare ';'
      ArtPris.InnkjopsPris[1] - ROUND((ArtPris.InnkjopsPris[1] * ArtBas.SupRab%) / 100,2) ';'
      ArtPris.InnkjopsPris[1] - ROUND((ArtPris.InnkjopsPris[1] * ArtBas.ForhRab%) / 100,2) ';'
      cVVarekost ';'  
      (IF ArtBas.OPris THEN 999 ELSE ArtLag.LagAnt) ';' 
      cSistSolgtDato ';'
      cSistKjoptDato ';'
      cEANLst ';'
      (IF AVAILABLE ArtBestPkt THEN STRING(ArtBestPkt.MinAnt) ELSE '') ';'
      (IF AVAILABLE ArtBestPkt THEN STRING(ArtBestPkt.MaksAnt) ELSE '') ';'
      (IF AVAILABLE ArtBestPkt THEN STRING(ArtBestPkt.BestAnt) ELSE '') ';'
      lIkkeInnlevertAnt ';'
      lIkkeInnlevertVerdi ';'
      ArtPris.Pris[1] ';'
      ArtBas.OPris
      SKIP.

    iAnt = IAnt + 1.
    
  END. /* UTLEGG_LAGER */
    
  OUTPUT STREAM Ut CLOSE.

  /* Gir filen dens riktige navn og tar bort den temporære filen. */
  OS-COPY value(ctmpFilNavn + STRING(Butiker.Butik)) value(cFilNavn + STRING(Butiker.Butik)).
  IF SEARCH(cFilNavn + STRING(Butiker.Butik)) <> ? THEN
    OS-DELETE VALUE(ctmpFilNavn + STRING(Butiker.Butik)).

END. /* BUTIKKLOOP */
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

