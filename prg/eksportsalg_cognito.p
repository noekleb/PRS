&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : eksporsalg_cognito.p
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

DEF VAR bTest AS LOG NO-UNDO.

ASSIGN 
    bTest = FALSE.

DEFINE STREAM Inn.
DEF STREAM Ut.

DEFINE TEMP-TABLE tmpSalgPrStr
    FIELD ButikkNr AS INTEGER 
    FIELD ButNavn AS CHARACTER    
    FIELD EkstKundeNr AS CHARACTER 
    FIELD ArtikkelNr AS DECIMAL 
    FIELD StrKode AS INTEGER     
    FIELD Varetekst AS CHARACTER 
    FIELD VareGr AS INTEGER 
    FIELD VareGrTekst AS CHARACTER 
    FIELD HovedGr AS INTEGER 
    FIELD HovedGrTekst AS CHARACTER 
    FIELD Avdeling AS INTEGER 
    FIELD AvdelingTekst AS CHARACTER 
    FIELD Dato AS DATE        
    FIELD Antall AS DECIMAL     
    FIELD LevNr AS INTEGER 
    FIELD LevNavn AS CHARACTER 
    FIELD RabKr AS DECIMAL 
    FIELD InnVerdiKr AS DECIMAL  
    FIELD UtverdiKr AS DECIMAL
    FIELD MvaKr AS DECIMAL
    FIELD VVarekost AS DECIMAL       
    INDEX SalgIdx ButikkNr EkstKundeNr ArtikkelNr StrKode Dato.

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

ASSIGN lTid = TIME.

DO:
  ASSIGN 
    dFraDato = dinpFraDato
    dTilDato = dinpTilDato
    bManuell = TRUE. /* Flagger at eksporten kjøres manuelt og at det ikke skal sendes eMail. */
  IF bTest THEN RUN bibl_logg.p ('eksporter_salg_cognito', 'eksportsalg_cognito.p: MANUELL Periode: ' + string(dinpFraDato) + ' - ' + string(dinpTilDato) + ' ' + string(TIME,"HH:MM:SS")).
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

{syspara.i 5 1 1 iButikkNr INT}

ASSIGN
    cKatalog    = RIGHT-TRIM(cKatalog,'\')
    cKatalog    = cKatalog + "\cognito"
    cFilNavn    = cKatalog + '\' + 'COGN' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.
    ctmpFilNavn = cKatalog + '\' + 'tmpCOGN' + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + REPLACE(STRING(TIME,"HH:MM"),':','') + '.'.

/* Sikrer at backup katalog finnes. */
OS-CREATE-DIR value(cKatalog).

OUTPUT STREAM Ut TO VALUE(ctmpFilNavn + 'txt').

IF bTest THEN RUN bibl_logg.p ('eksporter_salg_cognito', 'eksportsalg.p Butikk: Fil: ' + string(cFilNavn) + 'txt').

PUT STREAM Ut UNFORMATTED 
    "ButikkNr;" 
    "ButNavn;"    
    "EkstKundeNr;" 
    "ArtikkelNr;" 
    "StrKode;"     
    "Varetekst;" 
    "VareGr;" 
    "VareGrTekst;" 
    "HovedGr;" 
    "HovedGrTekst;" 
    "Avdeling;" 
    "AvdelingTeks;" 
    "Dato;"        
    "Antall;"     
    "LevNr;" 
    "LevNavn;" 
    "RabKr;" 
    "InnVerdiKr;"  
    "UtverdiKr;"
    "MvaKr;"
    "VVareKost"
    SKIP.      

ARTIKKELLOOP:
FOR EACH ArtBas NO-LOCK WHERE 
  CAN-FIND(FIRST TransLogg WHERE 
           TransLogg.ArtikkelNr  =  ArtBas.ArtikkelNr AND 
           TransLogg.Dato       >= dFraDato AND 
           TransLogg.Dato       <= dTilDato AND 
           CAN-DO('1,10',STRING(TransLogg.TTId)) AND 
           TransLogg.KundNr     >  0
           ),
    FIRST ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr:
    
    IF bTest THEN RUN bibl_logg.p ('eksporter_salg_cognito', 'eksportsalg.p Artikkel: ' + string(ArtBas.ArtikkelNr) + ' ' + ArtBas.Beskr).
           
BUTIKKLOOP:
FOR EACH Butiker NO-LOCK WHERE
  Butiker.harButikksystem = TRUE AND
  Butiker.ApningsDato     <> ? AND
  Butiker.NedlagtDato     = ?: 
  
/* Butikker det ikke skal legges ut salg for. */
IF CAN-DO('203,219,260,293,355,801',STRING(Butiker.Butik)) THEN 
  NEXT BUTIKKLOOP.
  
  IF bTest THEN RUN bibl_logg.p ('eksporter_salg_cognito', 'eksportsalg.p Butikk: ' + string(Butiker.Butik) + ' ' + Butiker.ButNamn).

UTLEGG_LAGER:
FOR EACH Lager NO-LOCK WHERE
    Lager.ArtikkelNr = ArtBas.ArtikkelNr AND 
    Lager.Butik      = Butiker.Butik,
    EACH ArtLag NO-LOCK WHERE
         ArtLag.ArtikkelNr = Lager.ArtikkelNr AND
         ArtLag.Butik      = Lager.Butik:

    /* Tømmer tmpTabell. */
    FOR EACH tmpSalgPrStr:
      DELETE tmpSalgPrStr.
    END.
    
    FIND FIRST StrKonv NO-LOCK WHERE
        StrKonv.StrKode = ArtLag.StrKode NO-ERROR.
    IF AVAILABLE StrKonv THEN 
    TRANSLOGG_LOOPEN:
    FOR EACH TransLogg NO-LOCK WHERE
      TransLogg.ArtikkelNr = Lager.ArtikkelNr AND
      Translogg.Storl      = ArtLag.Storl AND
      TransLogg.Dato       >= dFraDato AND
      TransLogg.Dato       <= dTilDato AND
      TransLogg.Tid        >= 0 AND
      TransLogg.Butik      = Lager.Butik AND
      CAN-DO('1,10',STRING(TransLogg.TTId)) AND
      TransLogg.KundNr     > 0  
      BREAK BY Translogg.ArtikkelNr
            BY TransLogg.Storl
            BY TransLogg.Dato DESCENDING:

      IF bTest THEN RUN bibl_logg.p ('eksporter_salg_cognito', 'eksportsalg.p Translogg: ' + 
               string(TransLogg.Butik) + ' ' + 
               Butiker.ButNamn  + ' ' +
               STRING(Translogg.ArtikkelNr)  + ' ' +
               Translogg.BongTekst  + ' ' +
               STRING(Translogg.Dato)  + ' ' +
               STRING(Translogg.BongId)
               ).
    
      /* Opprett tmpSalgPrStr hvis den ikke finnes fra før. */
      IF TransLogg.KundNr > 0 AND TransLogg.ArtikkelNr > 0 AND CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = TransLogg.ArtikkelNr) THEN
      LOGG_TRANS: 
      DO:
          FIND Kunde NO-LOCK WHERE
               Kunde.KundeNr = TransLogg.KundNr NO-ERROR.
          IF NOT AVAILABLE Kunde OR TRIM(Kunde.EksterntKundeNr) = '' THEN LEAVE LOGG_TRANS.
          
          FIND FIRST tmpSalgPrStr WHERE 
                tmpSalgPrStr.ButikkNr    = Lager.Butik AND 
                tmpSalgPrStr.EkstKundeNr = Kunde.EksterntKundeNr AND 
                tmpSalgPrStr.ArtikkelNr  = Lager.ArtikkelNr AND 
                tmpSalgPrStr.StrKode     = StrKonv.StrKode AND 
                tmpSalgPrStr.Dato        = TransLogg.Dato NO-ERROR.
          IF NOT AVAILABLE tmpSalgPrStr THEN 
            DO:              
              FIND LevBas NO-LOCK WHERE LevBas.LevNr = ArtBas.LevNr NO-ERROR.
              FIND VarGr NO-LOCK WHERE VarGr.Vg = ArtBas.Vg NO-ERROR.
              IF AVAILABLE VarGr THEN FIND HuvGr NO-LOCK WHERE HuvGr.Hg = VarGr.Hg NO-ERROR.
              IF AVAILABLE HuvGr THEN FIND Avdeling NO-LOCK WHERE Avdeling.AvdelingNr = HuvGr.AvdelingNr NO-ERROR.
              
              CREATE tmpSalgPrStr.
    
              /*INDEX SalgIdx ButikkNr EkstKundeNr ArtikkelNr StrKode Dato.*/
              ASSIGN
                tmpSalgPrStr.ButikkNr    = Lager.Butik
                tmpSalgPrStr.EkstKundeNr = Kunde.EksterntKundeNr
                tmpSalgPrStr.ArtikkelNr  = Lager.ArtikkelNr
                tmpSalgPrStr.StrKode     = StrKonv.StrKode
                tmpSalgPrStr.Dato        = TransLogg.Dato
                .
              ASSIGN 
                tmpSalgPrStr.LevNr        = ArtBas.LevNr
                tmpSalgPrStr.ButNavn      = (IF AVAILABLE Butiker THEN Butiker.ButNamn ELSE '')  
                tmpSalgPrStr.Varetekst    = (IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE '')
                tmpSalgPrStr.VareGr       = (IF AVAILABLE ArtBas THEN ArtBas.Vg ELSE 0)
                tmpSalgPrStr.VareGrTekst  = (IF AVAILABLE VarGr THEN VarGr.VgBeskr ELSE '')
                tmpSalgPrStr.HovedGr      = (IF AVAILABLE VarGr THEN VarGr.Hg ELSE 0)
                tmpSalgPrStr.HovedGrTekst = (IF AVAILABLE HuvGr THEN HuvGr.HgBeskr ELSE '')
                tmpSalgPrStr.Avdeling     = (IF AVAILABLE huvGr THEN HuvGr.AvdelingNr ELSE 0)
                tmpSalgPrStr.AvdelingTeks = (IF AVAILABLE Avdeling THEN Avdeling.AvdelingNavn ELSE '')
                tmpSalgPrStr.LevNr        = (IF AVAILABLE ArtBas THEN ArtBas.LevNr ELSE 0)
                tmpSalgPrStr.LevNavn      = (IF AVAILABLE LevBas THEN LevBas.LevNamn ELSE '')
                .
            END.
          /* Akkumulert pr. kunde, str m.m. */  
          ASSIGN
              tmpSalgPrStr.Antall     = tmpSalgPrStr.Antall + TransLogg.Antall
              tmpSalgPrStr.InnVerdiKr = tmpSalgPrStr.InnVerdiKr + (TransLogg.Pris * TransLogg.Antall) - (TransLogg.RabKr * TransLogg.Antall) - (TransLogg.Mva * TransLogg.Antall)
              tmpSalgPrStr.MvaKr      = tmpSalgPrStr.MvaKr + (TransLogg.Mva * TransLogg.Antall)
              tmpSalgPrStr.UtverdiKr  = tmpSalgPrStr.UtVerdiKr + (TransLogg.Pris * TransLogg.Antall) - (TransLogg.RabKr * TransLogg.Antall)
              tmpSalgPrStr.RabKr      = tmpSalgPrStr.RabKr + TransLogg.RabKr
              tmpSalgPrStr.VVareKost  = tmpSalgPrStr.VVareKost + (IF TransLogg.VVareKost <> ? THEN (TransLogg.VVareKost * TransLogg.Antall) ELSE 0)
              .
      END. /* LOGG_TRANS */  
    END. /* TRANSLOGG_LOOPEN */
    
    IF AVAILABLE tmpSalgPrStr THEN 
        PUT STREAM Ut UNFORMATTED
          tmpSalgPrStr.ButikkNr ';' 
          tmpSalgPrStr.ButNavn  ';'  
          tmpSalgPrStr.EkstKundeNr ';' 
          tmpSalgPrStr.ArtikkelNr ';'
          tmpSalgPrStr.StrKode ';' 
          tmpSalgPrStr.Varetekst ';' 
          tmpSalgPrStr.VareGr ';' 
          tmpSalgPrStr.VareGrTekst ';' 
          tmpSalgPrStr.HovedGr ';' 
          tmpSalgPrStr.HovedGrTekst ';' 
          tmpSalgPrStr.Avdeling ';' 
          tmpSalgPrStr.AvdelingTeks ';' 
          tmpSalgPrStr.Dato ';'       
          tmpSalgPrStr.Antall ';'     
          tmpSalgPrStr.LevNr ';'
          tmpSalgPrStr.LevNavn ';' 
          ROUND(tmpSalgPrStr.InnVerdiKr,2) ';'  
          ROUND(tmpSalgPrStr.UtverdiKr,2) ';'
          ROUND(tmpSalgPrStr.RabKr,2) ';'
          ROUND(tmpSalgPrStr.MvaKr,2) ';'
          ROUND(tmpSalgPrStr.VVareKost,2)
        SKIP.

    iAnt = IAnt + 1.
    
END. /* UTLEGG_LAGER */    
END. /* BUTIKKLOOP */
END. /* ARTIKKELLOOP */ 
OUTPUT STREAM Ut CLOSE.

    /* Gir filen dens riktige navn og tar bort den temporære filen. */
    OS-COPY value(ctmpFilNavn + 'txt') value(cFilNavn + 'txt').
    IF SEARCH(cFilNavn + 'txt') <> ? THEN
        OS-DELETE VALUE(ctmpFilNavn + 'txt').
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
  dFraDato    = pdDato - 5
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

