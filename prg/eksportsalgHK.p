&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : eksporsalgHK.p
    Purpose     : Eksport av salg fra butikk til hk.
                  Eksporterer alle salg pr. dag pr. størrelse siste 10 dager. 

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

DEFINE BUFFER bufPOSS FOR POSS.

DEFINE TEMP-TABLE tmpSalgPrStr NO-UNDO 
    FIELD ButikkNr AS INTEGER    
    FIELD ArtikkelNr AS DECIMAL 
    FIELD StrKode AS INTEGER     
    FIELD Dato AS DATE        
    FIELD Antall AS DECIMAL     
    FIELD InnVerdiKr AS DECIMAL  
    FIELD MvaKr AS DECIMAL      
    FIELD UtverdiKr AS DECIMAL
    FIELD LevNr AS INTEGER  
    FIELD Tid AS INT 
    FIELD TTId AS INT FORMAT ">>9"
    FIELD BongId AS INT FORMAT ">>>>>>>9"
    FIELD BongLinjeNr AS INT 
    FIELD SelgerNr AS DEC FORMAT ">>>>>>>>>>>9"
    FIELD Storl AS CHAR FORMAT "x(10)"
    FIELD RabattKr AS DEC FORMAT "->>,>>>,>>9.99"
    FIELD MomsProc AS DEC FORMAT "->>>9.99"
    FIELD KasseNr AS INT 
    INDEX SalgIdx ButikkNr ArtikkelNr StrKode Dato Tid TTId BongId BongLinjeNr.

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
         HEIGHT             = 14.24
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

/*{syspara.i 1 1 51 cKatalog} Eksportkatalog */
/*{syspara.i 1 1 52 cKatalog} /* Import katalog */*/
IF cKatalog = '' THEN
  cKatalog = 'c:\tmp'. /* 'd:\pos_filer'*/

ASSIGN lTid = TIME.

IF dinpFraDato = ? OR dinpTilDato = ? THEN 
DO: 
  RUN SettFraTilDato.
  RUN bibl_logg.p ('eksporter_salg', 'eksportsalg.p: AUTO WinCheduler Periode: ' + string(dinpFraDato) + ' - ' + string(dinpTilDato) + ' ' + string(TIME,"HH:MM:SS")).
END.
ELSE DO:
  ASSIGN 
    dFraDato = dinpFraDato
    dTilDato = dinpTilDato
    bManuell = TRUE. /* Flagger at eksporten kjøres manuelt og at det ikke skal sendes eMail. */
  RUN bibl_logg.p ('eksporter_salg', 'eksportsalg.p: MANUELL Periode: ' + string(dinpFraDato) + ' - ' + string(dinpTilDato) + ' ' + string(TIME,"HH:MM:SS")).
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
DEFINE VARIABLE iButikkNr       AS INT  NO-UNDO.
DEFINE VARIABLE cFilNavn        AS CHAR NO-UNDO.
DEFINE VARIABLE iAnt            AS INT  NO-UNDO.
DEFINE VARIABLE cSistSolgtDato  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEAN            AS CHAR NO-UNDO.
DEFINE VARIABLE lPSTransId      AS DECIMAL NO-UNDO.

{syspara.i 5 1 1 iButikkNr INT}

ASSIGN
    cKatalog    = RIGHT-TRIM(cKatalog,'\')
    cKopi       = cKatalog + "\purpleo"
    cFilNavn    = cKatalog + '\' + 'POSS' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.
    ctmpFilNavn = cKatalog + '\' + 'tmpPOSS' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.

lPSTransId = 0.

BUTIKKLOOP:
FOR EACH Butiker NO-LOCK WHERE
  Butiker.harButikksystem = TRUE AND
  Butiker.ApningsDato     <> ?: 
  
OUTPUT STREAM Ut TO VALUE(ctmpFilNavn + STRING(Butiker.Butik)).

RUN bibl_logg.p ('eksporter_salg', 'eksportsalg.p Butikk: ' + string(Butiker.Butik)+ ' Fil: ' + string(cFilNavn) + STRING(Butiker.Butik)).

PUT STREAM Ut UNFORMATTED 
    'TTId;'
    'Butik;'
    'VismaNr (ArtNr+StrKode);'
    'EAN;'
    'ERPNr;'
    'Dato;'
    'Antall;'
    'InnVerdiKr;'
    'MvaKr;'
    'UtverdiKr;'
    'Levnr;'
    'Vg;'
    'Oms.eks mva;'
    'Varetekst;'
    /* Nye felt til Thommas */
    'Lev. fargekode;'
    'Modell;'
    'Mellomgrp;'
    'Kjedelevert;'
    'Gj.fakturert;'
    'Størrelse;'
    'VaremerkeId;'
    'Varemerke;'
    'Vareområde;'
    'Mva%;'
    'RabattKr;'
    'BongNr;'
    'BongLinjeNr;'
    'Tid;'
    'SelgerNr;'
    'SelgerNavn;'
    'KasseNr'
    SKIP.

LOOPEN:
FOR EACH TransLogg NO-LOCK WHERE
  TransLogg.Butik      = Butiker.Butik AND
  TransLogg.Dato       >= dFraDato AND
  TransLogg.Dato       <= dTilDato
  BREAK 
    BY Translogg.Butik
    BY TransLogg.Dato:
          
  FIND ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
  IF AVAILABLE ArtBas THEN
  DO: 
      FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
      FIND VarGr NO-LOCK WHERE VarGr.Vg = ArtBas.Vg NO-ERROR. 
      IF AVAILABLE VarGr THEN FIND FIRST Moms NO-LOCK WHERE Moms.MomsKod = VarGr.MomsKod NO-ERROR.
      FIND Varemerke NO-LOCK WHERE
          VareMerke.VmId = ArtBas.VmId NO-ERROR.
      FIND Regnskapsavdeling NO-LOCK WHERE
          Regnskapsavdeling.RAvdNr = ArtBas.RavdNr NO-ERROR.
  END.
  
  FIND Selger NO-LOCK WHERE
      Selger.SelgerNr = TransLogg.SelgerNr NO-ERROR.
  FIND BongLinje NO-LOCK WHERE
      BongLinje.ButikkNr = TransLogg.Butik AND
      BongLinje.GruppeNr = 1 AND
      BongLinje.KasseNr  = TransLogg.KassaNr AND
      BongLinje.Dato     = TransLogg.Dato AND
      BongLinje.BongNr   = TransLogg.BongId AND
      BongLinje.LinjeNr  = TransLogg.BongLinjeNr NO-ERROR.
  IF AVAILABLE Strekkode THEN RELEASE Strekkode.
  cEAN = ''.
  IF AVAILABLE BongLinje THEN
  DO:
      cEAN = BongLinje.Strekkode. 
      RUN bibl_chkean.p (INPUT-OUTPUT cEAN).
      FIND Strekkode NO-LOCK WHERE 
          Strekkode.Kode = cEAN NO-ERROR.
  END.

  FIND FIRST POSS WHERE
    POSS.Butik   = TransLogg.Butik AND 
    POSS.TransNr = TransLogg.TransNr AND 
    POSS.SeqNr   = TransLogg.SeqNr NO-ERROR.
  IF NOT AVAILABLE POSS THEN 
  DO:
    IF lPSTransId = 0 THEN 
    DO:
        FIND LAST bufPOSS USE-INDEX IdxPSSTransID NO-ERROR.
        IF AVAILABLE bufPOSS THEN 
          lPSTransId = bufPOSS.PSTransId + 1.
        ELSE 
          lPSTransId = 1.
    END.
    ELSE lPSTransId = lPSTransId + 1.
    CREATE POSS.
    ASSIGN
      POSS.PSTransId = lPSTransId
      POSS.Butik     = TransLogg.Butik 
      POSS.TransNr   = TransLogg.TransNr 
      POSS.SeqNr     = TransLogg.SeqNr.
  END. 
  
  ASSIGN
    POSS.TTId         = TransLogg.TTId                                                                                                          
    POSS.VismaNr      = STRING(TransLogg.ArtikkelNr) + TRIM(STRING(IF AVAILABLE StrKonv THEN StrKonv.StrKode ELSE 0,">>999"))                   
    POSS.EAN          = (IF AVAILABLE BongLinje THEN BongLinje.StrekKode ELSE '')                                                               
    POSS.ERPNr        = (IF AVAILABLE Strekkode THEN Strekkode.ERPNr ELSE '')                                                                   
    POSS.Dato         = TransLogg.Dato                                                                                                          
    POSS.Antall       = TransLogg.Antall                                                                                                        
    POSS.InnVerdiKr   = ROUND((Translogg.VVareKost * TransLogg.Antall),2)                                                                       
    POSS.MvaKr        = ROUND(TransLogg.Mva * TransLogg.Antall,2)                                                                               
    POSS.UtVerdiKr    = ROUND((TransLogg.Pris * TransLogg.Antall) - (TransLogg.RabKr * TransLogg.Antall),2)                                     
    POSS.LevNr        = Translogg.LevNr                                                                                                         
    POSS.Vg           = Translogg.Vg   
    POSS.OmsEksMva    = ROUND((TransLogg.Pris * TransLogg.Antall) - (TransLogg.RabKr * TransLogg.Antall) - (TransLogg.Mva * TransLogg.Antall),2)
    POSS.Varetekst    = REPLACE(Translogg.Bongtekst,';',' ')                                                                                    
    POSS.LevFargeKode = IF AVAILABLE ArtBas THEN REPLACE(ArtBas.LevFargKod,';',' ') ELSE ''                                                     
    POSS.Modell       = IF AVAILABLE ArtBas THEN REPLACE(ArtBas.LevKod,';',' ') ELSE ''                                                         
    POSS.MellomGrp    = IF AVAILABLE VarGr THEN VarGr.Hg ELSE 0                                                                        
    POSS.Kjedelevert  = IF AVAILABLE ArtBas THEN ArtBas.KjedeVare ELSE FALSE                                                                
    POSS.GjFakturert  = IF AVAILABLE ArtBas THEN ArtBas.Gjennomfaktureres ELSE FALSE                                                        
    POSS.Storrelse    = REPLACE(TransLogg.Storl,';',' ')                                                                                        
    POSS.VaremerkeId  = IF AVAILABLE ArtBas THEN ArtBas.VmId ELSE 0                                                                    
    POSS.Varemerke    = REPLACE((IF AVAILABLE VareMerke THEN VareMerke.Beskrivelse ELSE ''),';',' ')                                            
    POSS.Vareomrade   = IF AVAILABLE ArtBas THEN ArtBas.RAvdNr ELSE 0                                                                  
    POSS.Mva%         = (IF AVAILABLE Moms THEN Moms.MomsProc ELSE 0)                                                                           
    POSS.RabattKr     = (TransLogg.RabKr * TransLogg.Antall)                                                                                    
    POSS.BongNr       = TransLogg.BongId                                                                                                        
    POSS.BongLinjeNr  = TransLogg.BongLinjeNr                                                                                                   
    POSS.Tid          = TransLogg.Tid                                                                                                           
    POSS.SelgerNr     = TransLogg.SelgerNr                                                                                                      
    POSS.SelgerNavn   = (IF AVAILABLE Selger THEN Selger.Navn ELSE '')                                                                          
    POSS.KasseNr      = TransLogg.KassaNr                                                                                                            
    .
    
  PUT STREAM Ut UNFORMATTED
    TransLogg.TTId ';'
    TransLogg.Butik ';'
    STRING(TransLogg.ArtikkelNr) + TRIM(STRING(IF AVAILABLE StrKonv THEN StrKonv.StrKode ELSE 0,">>999")) ';'
    (IF AVAILABLE BongLinje THEN BongLinje.StrekKode ELSE '') ';'
    (IF AVAILABLE Strekkode THEN Strekkode.ERPNr ELSE '') ';'    
    TransLogg.Dato ';'
    TransLogg.Antall ';' 
    ROUND((Translogg.VVareKost * TransLogg.Antall),2) ';'  
    ROUND(TransLogg.Mva * TransLogg.Antall,2) ';'
    ROUND((TransLogg.Pris * TransLogg.Antall) - (TransLogg.RabKr * TransLogg.Antall),2) ';'
    Translogg.LevNr ';'
    Translogg.Vg ';'
    ROUND((TransLogg.Pris * TransLogg.Antall) - (TransLogg.RabKr * TransLogg.Antall) - (TransLogg.Mva * TransLogg.Antall),2) ';'
    REPLACE(Translogg.Bongtekst,';',' ') ';' 
    IF AVAILABLE ArtBas THEN REPLACE(ArtBas.LevFargKod,';',' ') ELSE '' ';'
    IF AVAILABLE ArtBas THEN REPLACE(ArtBas.LevKod,';',' ') ELSE '' ';'
    IF AVAILABLE VarGr THEN STRING(VarGr.Hg) ELSE '' ';'
    IF AVAILABLE ArtBas THEN STRING(ArtBas.KjedeVare) ELSE '' ';'
    IF AVAILABLE ArtBas THEN STRING(ArtBas.Gjennomfaktureres) ELSE '' ';'
    REPLACE(TransLogg.Storl,';',' ') ';'
    IF AVAILABLE ArtBas THEN STRING(ArtBas.VmId) ELSE '' ';'
    REPLACE((IF AVAILABLE VareMerke THEN VareMerke.Beskrivelse ELSE ''),';',' ') ';'
    IF AVAILABLE ArtBas THEN STRING(ArtBas.RAvdNr) ELSE '' ';'
    (IF AVAILABLE Moms THEN Moms.MomsProc ELSE 0) ';'
    (TransLogg.RabKr * TransLogg.Antall) ';'
    TransLogg.BongId ';'
    TransLogg.BongLinjeNr ';'
    TransLogg.Tid ';'
    TransLogg.SelgerNr ';'
    (IF AVAILABLE Selger THEN Selger.Navn ELSE '') ';'
    TransLogg.KassaNr
    SKIP.
END. /* LOOPEN */

iAnt = IAnt + 1.
    
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

