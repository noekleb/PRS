&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE TT_OvBuffer NO-UNDO LIKE OvBuffer.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        :  xoverforbong.p
    Purpose     :  Overfï¿½ring av bonger fra kasse.

    Syntax      :

    Description :  

    Author(s)   :  Tom Nï¿½kleby
    Created     :  24/10-01
    Notes       :  -
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF INPUT  PARAMETER lDataSettId  AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Telleverk  AS HANDLE NO-UNDO.
DEF INPUT  PARAMETER h_Logg       AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntBonger   AS INT    NO-UNDO.

DEF VAR cError          AS CHAR NO-UNDO.
DEF VAR piLoop1         AS INT  NO-UNDO.
DEFINE VARIABLE cDubl AS CHARACTER INITIAL 'DUBL' NO-UNDO.
DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE plFaktura_Id AS DECIMAL   FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
DEFINE VARIABLE bSkrivEtikett AS LOG NO-UNDO.
DEFINE VARIABLE cEtikett      AS CHARACTER NO-UNDO.
DEFINE VARIABLE bLoggNets     AS LOG NO-UNDO.
DEFINE VARIABLE iNettButLager AS INT NO-UNDO.
DEFINE VARIABLE cButPlussMinus AS CHARACTER NO-UNDO.
DEFINE VARIABLE iOverskLagerNettbutikk AS INTEGER   NO-UNDO.
DEFINE VARIABLE bBrukTBId2             AS LOG       NO-UNDO.
DEFINE VARIABLE lPkSdlId        AS DECIMAL NO-UNDO.
DEFINE VARIABLE cFilNavn AS CHARACTER NO-UNDO.
DEFINE VARIABLE ieCom AS INTEGER NO-UNDO.
DEFINE VARIABLE iLagereCom AS INTEGER NO-UNDO.
                 
DEFINE VARIABLE cReturn                AS CHARACTER NO-UNDO.
DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
DEFINE VARIABLE bOk     AS LOG NO-UNDO.                  

DEF VAR iButikkNr       AS INT  NO-UNDO.
DEF VAR iGruppeNr       AS INT  NO-UNDO.
DEF VAR iKasseNr        AS INT  NO-UNDO.
DEF VAR cBehKvittering  AS CHAR NO-UNDO.
DEF VAR iTotAntBonger   AS INT  NO-UNDO.
DEF VAR cDatoListe      AS CHAR NO-UNDO.
DEF VAR lFilId          AS CHAR NO-UNDO.
DEF VAR iStart          AS INT  NO-UNDO.
DEFINE VARIABLE iInt    AS INTEGER NO-UNDO.
DEF VAR iCl              AS INT  NO-UNDO.
DEF VAR lVVareKost       AS DEC  NO-UNDO.
DEF VAR idags_moms       AS INT  NO-UNDO.
DEF VAR cTekst           AS CHAR NO-UNDO.
DEF VAR iZNr             AS INT  NO-UNDO.
DEF VAR iBatchNr         AS INT  NO-UNDO.
DEF VAR iOvBatchNr       AS INT  NO-UNDO.
DEF VAR plDbFaktorBrutto AS DEC  NO-UNDO.
DEF VAR plDbFaktorNetto  AS DEC  NO-UNDO.
DEF VAR plMva%           AS DEC  NO-UNDO.
DEF VAR bMotpostert      AS LOG  NO-UNDO.
DEF VAR h_PrisKo         AS HANDLE NO-UNDO.
DEF VAR dVVareKost       AS DEC  NO-UNDO.
DEF VAR bPlukkliste      AS LOG  NO-UNDO.
DEF VAR bLoggPrisAvvik   AS LOG  NO-UNDO.
DEF VAR iTilgodeGyldig   AS INT  NO-UNDO.
DEFINE VARIABLE iGavekortGyldig AS INTEGER NO-UNDO.
DEF VAR bBestNr          AS LOG  NO-UNDO.
DEF VAR bInnkjopspris    AS LOG  NO-UNDO.
DEFINE VARIABLE bTransloggTilHk AS LOG NO-UNDO.
DEF VAR cSprak           AS CHAR NO-UNDO.
DEFINE VARIABLE bVarespes AS LOG NO-UNDO.
DEFINE VARIABLE bStatTilHK  AS LOG       NO-UNDO.
DEFINE VARIABLE iSentrallager AS INTEGER NO-UNDO.
DEFINE VARIABLE iOutlet AS INTEGER NO-UNDO.
DEFINE VARIABLE cOutletListe AS CHARACTER NO-UNDO.
DEFINE VARIABLE iGantAktiv AS INTEGER NO-UNDO. 

DEFINE VARIABLE icParam     AS CHAR NO-UNDO.
DEFINE VARIABLE ihBuffer    AS HANDLE NO-UNDO.
DEFINE VARIABLE icSessionId AS CHAR NO-UNDO.
DEFINE VARIABLE ocReturn    AS CHAR NO-UNDO.
DEFINE VARIABLE obOK        AS LOG NO-UNDO.

DEF VAR wEDB-System      AS CHARACTER  NO-UNDO.
DEF VAR wTabell          AS CHARACTER  NO-UNDO.

DEF BUFFER bKundeTrans    FOR KundeTrans.                
DEF BUFFER bKundeBetTrans FOR KundeBetTrans.                
DEF BUFFER bKundeSaldo    FOR KundeSaldo.

DEFINE STREAM Ut.

DEFINE BUFFER bufTilgode FOR Tilgode.
DEFINE BUFFER bufArtBas FOR ArtBas.
DEFINE BUFFER bufArtPris FOR ArtPris.
DEFINE BUFFER bufButiker FOR Butiker.
DEFINE BUFFER bufBongHode FOR BongHode.

DEF TEMP-TABLE ttKundeBut 
    FIELD KundeNr  LIKE Kunde.KundeNr
    FIELD ButikkNr AS   INT.

DEF TEMP-TABLE ttMedlemBut 
    FIELD MedlemsNr LIKE Medlem.MedlemsNr
    FIELD ButikkNr  AS   INT.

DEF TEMP-TABLE ttBongLinje 
    FIELD TTId     AS INT
    FIELD LinjeSum AS DEC.

DEFINE TEMP-TABLE tt2BongLinje LIKE BongLinje.

DEF TEMP-TABLE tt_Lager
  FIELD ArtikkelNr AS DEC
  FIELD ButikkNr   AS INT
  FIELD Storl      AS CHAR
  FIELD Antall     AS DEC.

DEF BUFFER bBokforingsbilag FOR Bokforingsbilag.
DEFINE BUFFER buf2Butiker FOR Butiker.

DEFINE TEMP-TABLE ttPkSdlHode
  FIELD PkSdlId   AS DECIMAL FORMAT ">>>>>>>>>>>>9"
  FIELD SendtDato AS DATE 
  FIELD PkSdlNr   AS CHARACTER 
  FIELD EkstId    AS CHARACTER 
  INDEX Pakkseddel PkSdlNr SendtDato
  . 
   
DEFINE TEMP-TABLE ttpkSdlLinje LIKE PkSdlLinje.
DEFINE TEMP-TABLE tt2pkSdlLinje LIKE PkSdlLinje.

{etikettlogg.i &NEW=NEW}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-FixStorl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FixStorl Procedure 
FUNCTION FixStorl RETURNS CHARACTER
  ( pcStorl AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Mva2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Mva2 Procedure 
FUNCTION Mva2 RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Mva2LinjeRab) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Mva2LinjeRab Procedure 
FUNCTION Mva2LinjeRab RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SjekkNonSale) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SjekkNonSale Procedure 
FUNCTION SjekkNonSale RETURNS INTEGER
        ( piArtikkelNr AS DECIMAL ) FORWARD.

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
   Temp-Tables and Buffers:
      TABLE: TT_OvBuffer T "NEW SHARED" NO-UNDO SkoTex OvBuffer
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 32.14
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEF BUFFER clButiker FOR Butiker.

FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
IF AVAIL bruker THEN
    cSprak = TRIM(Bruker.Lng).

{syspara.i 210 100 8 iGantAktiv INT}
{syspara.i 150 1 2 ieCom INT}
{syspara.i 150 1 3 iLagereCom INT}

{syspara.i 11 6 1 cTekst}
IF cTekst = '1' THEN 
    bBrukTBId2 = TRUE.
    
/* Skal det sendes artikkelstatistikk? */
    {syspara.i 3 4 1 cTekst}
IF (cTekst = "1" OR cTekst = "") THEN
    bStatTilHK = TRUE.
ELSE
    bStatTilHK = FALSE.

/* Logger bonger for overfï¿½ring til Nets - MainCard */
{syspara.i 50 28 1 cTekst}
IF (cTekst = "1" OR cTekst = "") THEN
    bLoggNets = TRUE.
ELSE
    bLoggNets = FALSE.

/* Utvidet varespes pï¿½ fakturalinje. */
{syspara.i 19 100 4 cTekst}
IF CAN-DO('1,Ja,J,Yes,Y,True',cTekst) 
  THEN bVarespes = TRUE.
  ELSE bVarespes = FALSE.

/* Skal det sendes translogg? */
{syspara.i 3 4 2 cTekst}
IF (cTekst = "1" OR cTekst = "") THEN
    bTransloggTilHk = TRUE.
ELSE
    bTransloggTilHk = FALSE.

cButPlussMinus = '848,849'.

FIND DataSett NO-LOCK WHERE
    DataSett.DataSettId = lDataSettId NO-ERROR.
IF NOT AVAILABLE Datasett THEN
    RETURN " ** Ukjent datasett (" + STRING(lDataSettId) + ").".

FIND Filer OF DataSett NO-LOCK NO-ERROR.
IF NOT AVAILABLE Filer THEN
    RETURN " ** Ukjent filkobling på datasett (" + STRING(lDataSettId) + ").".

/* Sjekker om butikken skal oppdateres for statistikk. */
SJEKKSTAT:
DO:
  FIND Butiker NO-LOCK WHERE
      Butiker.Butik = DataSett.Butik NO-ERROR.
  IF DataSett.Butik = 0 OR NOT AVAILABLE Butiker THEN
      RETURN " ** Ingen oppdatering gjøres på butikk 0 (" + STRING(lDataSettId) + ").".
  /* Markerer Datasettet som under overfï¿½ring/delhvis overfï¿½rt */
  IF Butiker.StatistikkOppdatering = FALSE THEN
  DO TRANSACTION:
      FIND CURRENT DataSett EXCLUSIVE-LOCK.
      ASSIGN
          DataSett.Behandlet = 5. /* Overfï¿½rt */
      FIND CURRENT DataSett NO-LOCK.
      RETURN "".
  END.
END. /* SJEKKSTAT */
   
/* Tï¿½mmer buffer for lagerkontroll */
FOR EACH tt_Lager:
  DELETE tt_Lager.
END.

{syspara.i 19 100 1 cTekst}
IF CAN-DO("Ja,yes,true,1",cTekst) THEN
    bBestNr = TRUE. 
ELSE
    bBestNr = FALSE. 

{syspara.i 19 100 3 cTekst}
IF CAN-DO("Ja,yes,true,1",cTekst) THEN
    bInnkjopspris = TRUE. 
ELSE
    bInnkjopspris = FALSE. 

{syspara.i 1 5 1 cTekst}
IF CAN-DO("Ja,yes,true,1",cTekst) THEN
    bLoggPrisAvvik = TRUE. 
ELSE
    bLoggPrisAvvik = FALSE. 

/* Gyldighetstid tilgodelapper */
{syspara.i 1 1 50 iTilgodeGyldig INT}
IF iTilgodeGyldig = 0 THEN
    iTilgodeGyldig = 90.

/* Gyldighetstid gavekort */
{syspar2.i 1 1 50 iGavekortGyldig INT}
IF iGavekortGyldig = 0 THEN
    iGavekortGyldig = 90.

/* Kode for lï¿½sing av artikkelnummer ved overfï¿½ring. */
{syspara.i 1 2 3 wEDB-System}
IF wEDB-System = "" THEN
  wEDB-System = "OVERFOR-LOCK".
/* {syspar2.i 1 2 3 wTabell} */
/* if wEDB-System = "" then  */
/*   wEDB-System = "ArtBas". */

{syspara.i 5 1 1 iCl INT}
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCl NO-ERROR.

{syspara.i 22 20 1 iSentrallager INT}
{syspar2.i 22 20 1 iOutlet INT}
{syspara.i 22 20 2 iOverskLagerNettbutikk INT}  
{syspar2.i 22 20 2 iNettButLager INT}
{syspara.i 22  5 2 cOutletListe}  
 
 
 
/* Avgjï¿½r om dagsrapporten skal posteres inklusive eller eksklusive mva */
idags_moms = 0.
{syspara.i 6 4 1 cTekst}
IF CAN-DO("Ja,yes,true,1",cTekst) THEN
    idags_moms = 1. /* Poster inkl. Mva */
ELSE
    iDags_Moms = 0. /* Poster eks.  Mva  */

/* Overføringsordre basert på varesalg */
{syspara.i 11 3 1 cTekst}
IF CAN-DO("Ja,yes,true,1",cTekst) THEN
    bPlukkliste = TRUE. /* Opprett */
ELSE
    bPlukkliste = FALSE. /* Ikke opprett */

/* Faktor for beregning av DB pï¿½ grunnlag av oms inkl. mva, hvis ikke kalkyle finnes pï¿½ artikkelen. */
plDbFaktorBrutto = 0.
{syspara.i 6 4 3 plDbFaktorBrutto DEC}
IF plDbFaktorBrutto = 0 THEN
    plDbFaktorBrutto = 0.24.

/* Faktor for beregning av DB pï¿½ grunnlag av oms ekskl. mva, hvis ikke kalkyle finnes pï¿½ artikkelen. */
plDbFaktorNetto = 0.
{syspara.i 6 4 4 plDbFaktorNetto DEC}
IF plDbFaktorNetto = 0 THEN
    plDbFaktorNetto = 0.30.

/* Faktor for beregning av DB pï¿½ grunnlag av oms ekskl. mva, hvis ikke kalkyle finnes pï¿½ artikkelen. */
plMva% = 0.
{syspara.i 6 4 2 plMva% DEC}
IF plMva% = 0 THEN
    plMva% = 0.30.

IF NOT VALID-HANDLE(h_PrisKo) THEN
    RUN PrisKo.p PERSISTENT SET h_PrisKo.

RUN TellOppBonger.

/* Batch for TransLogg */
RUN batchlogg.w (PROGRAM-NAME(1),
                 "Data fra kassene " +
                 string(TODAY) +
                 " " +
                 string(TIME,"HH:MM") +
                 " " +
                 USERID("dictdb"),
                 OUTPUT iBatchNr).

RUN OverforDatasett.

/* Flagger batchen klar for oppdatering. */
RUN batchstatus.p (iBatchNr, 2).

IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_PrisKo.

EMPTY TEMP-TABLE EtikettLogg. 
EMPTY TEMP-TABLE ttKundeBut. 
EMPTY TEMP-TABLE ttMedlemBut. 
EMPTY TEMP-TABLE ttBongLinje. 
EMPTY TEMP-TABLE tt2BongLinje.
EMPTY TEMP-TABLE tt_Lager.
EMPTY TEMP-TABLE ttPkSdlHode.
EMPTY TEMP-TABLE ttpkSdlLinje.
EMPTY TEMP-TABLE tt2pkSdlLinje.

RETURN cError.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Aktivitetsrapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Aktivitetsrapport Procedure 
PROCEDURE Aktivitetsrapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{Aktivitetsrapport.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EODLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EODLogg Procedure 
PROCEDURE EODLogg :
/*------------------------------------------------------------------------------
  Purpose:     Utskrift av Bokfï¿½ringsbilag og finansrapport.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iRapptype  AS INTEGER INIT ?  NO-UNDO.
DEFINE VARIABLE hRapport1  AS HANDLE  NO-UNDO.
DEFINE VARIABLE hRapport2  AS HANDLE  NO-UNDO.
DEFINE VARIABLE d31DecFgAr AS DATE    NO-UNDO.
DEFINE VARIABLE plSum      AS DEC     NO-UNDO.
DEFINE VARIABLE bSettEOD   AS LOG     NO-UNDO.
DEFINE VARIABLE bSkrivEOD  AS LOG     NO-UNDO.

DEF BUFFER bbKas_Rap FOR Kas_Rap.
DEF BUFFER finButiker FOR Butiker.

/* Sjekker om det er en EOD 074 post fra InfoPOS 8.0 */
/* 074 er det samme som 092 i PRS formatet.          */
BOKFORINGSBILAG:
FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.Makulert = FALSE AND
    CAN-DO("092",STRING(BongLinje.TTId,"999")):

    ASSIGN
        plSum     = 0
        bSkrivEOD = TRUE 
        .

    /* Logger EOD mottak for kasse. */
    FIND EODKasse EXCLUSIVE-LOCK WHERE
       EODKasse.ButikkNr = Bonglinje.ButikkNr AND
       EODKasse.GruppeNr = Bonglinje.GruppeNr AND 
       EODKasse.KasseNr  = BongLinje.KasseNr AND
       EODKasse.EODDato  = BongHode.Dato NO-ERROR.
    IF NOT AVAILABLE EODKasse THEN
    DO:
        CREATE EODKasse.
        ASSIGN 
           EODKasse.ButikkNr = Bonglinje.ButikkNr 
           EODKasse.GruppeNr = Bonglinje.GruppeNr  
           EODKasse.KasseNr  = BongLinje.KasseNr 
           EODKasse.EODDato  = BongHode.Dato 
           .
    END.
    /* Sjekker om EOD er mottatt fra alle kasser. */
    bSettEOD = TRUE.
    FOR EACH Kasse NO-LOCK WHERE 
        Kasse.ButikkNr = BongLinje.ButikkNr AND 
        Kasse.Aktiv    = TRUE AND 
        Kasse.KasseNr  <= 90:
        IF NOT CAN-FIND(EODKasse WHERE
                        EODKasse.ButikkNr = Kasse.ButikkNr AND
                        EODKasse.GruppeNr = Kasse.GruppeNr AND 
                        EODKasse.KasseNr  = Kasse.KasseNr AND
                        EODKasse.EODDato  = BongHode.Dato) 
            THEN bSettEOD = FALSE.
    END.
    /* Hvis alle kasser har levert EOD, skal bokfï¿½ringsbilaget flagges med EOD mottatt. */
    IF bSettEOD THEN 
    DO:
       FIND Bokforingsbilag EXCLUSIVE-LOCK WHERE
           Bokforingsbilag.OmsetningsDato = BongHode.Dato AND
           Bokforingsbilag.ButikkNr       = BongHode.ButikkNr NO-ERROR.
       IF AVAILABLE Bokforingsbilag THEN 
         ASSIGN 
             BokForingsbilag.EODDato    = BongHode.Dato
             BokForingsbilag.EODMottatt = TRUE.
       bSettEOD = FALSE. 
    END.

    /* Sjekker om det var noe bevegelse den dagen */
    FOR EACH bbKas_Rap WHERE
        bbKas_Rap.Dato   = BongHode.Dato AND
        bbKas_Rap.Butikk = BongHode.butikkNr:
        plSum = plSum + abs(Kas_rap.Kontant) +
                        abs(Kas_rap.Sjekk) +
                        abs(Kas_rap.Kort) +
                        abs(Kas_rap.Kredit) +
                        abs(Kas_rap.Kupong1) +
                        abs(Kas_rap.Kupong2) +
                        abs(Kas_rap.Tilgode) +
                        abs(Kas_rap.Gavekort) +
                        abs(Kas_rap.Rekvisisasjon) +
                        abs(Kas_rap.Dropp) +
                        abs(Kas_rap.Overfort) +
                        abs(Kas_rap.Veksel) +
                        abs(Kas_rap.Avrunding).
    END.
    FIND finbutiker NO-LOCK WHERE
        finButiker.Butik = BongHode.ButikkNr NO-ERROR.
    /* Skriver ut rapporten hvis det er noe ï¿½ skrive ut. */
    IF plSum > 0 AND finButiker.EODRapporter = TRUE THEN
    DO:
        /* Flyttet utenfor BOKFORINGSBILAG blokken 
        IF finButiker.EODFinansrapport THEN
        DO:
            lskrivrapp10 = TRUE.
            iRappType = 10.                                                     
            RUN w-rkassarapportx.w PERSISTENT SET hRapport1.                    
            RUN AutoInit IN hRapport1 (iRappType,BongHode.Butik,BongHode.Dato). 
        END.

        IF finButiker.EODBokforingsbilag THEN
        DO:
            lskrivrapp11 = TRUE.
            ASSIGN iRappType = 11.                                              
            RUN w-rkassarapportx.w PERSISTENT SET hRapport2.                    
            RUN AutoInit IN hRapport2 (iRappType,BongHode.Butik,BongHode.Dato). 
        END.

        IF finButiker.EDOJournal THEN
        DO:
            lskrivbongrp = TRUE.
            RUN skrivbongrap.p (BongHode.ButikkNr,BongHode.Dato,TRUE,TRUE). 
        END.
        */
    END.
    ELSE DO: /* ï¿½pningsskjemahantering */
      /* vi markerar butiken som stï¿½ngd i mottakskontrollen */
        FIND ApnSkjema WHERE ApnSkjema.ButikkNr = BongHode.ButikkNr AND
                             ApnSkjema.Ar       = YEAR(BongHode.Dato) NO-ERROR.
        IF AVAIL ApnSkjema THEN DO:
            ASSIGN d31DecFgAr = DATE(12,31,ApnSkjema.Ar - 1).
            IF ENTRY(BongHode.Dato - d31DecFgAr,ApnSkjema.OpenClosed) = "1" THEN
                ENTRY(BongHode.Dato - d31DecFgAr,ApnSkjema.OpenClosed) = "0" NO-ERROR.
            RELEASE ApnSkjema.
        END.
    END. /* ï¿½pnings.... SLUTT      */
END. /* BOKFORINGSBILAG */

/* Er det omsetning og EOD rapporter er satt opp, skal det skrives ut. */
IF bSkrivEOD THEN 
SKRIV_EOD:
DO:
  FIND finButiker NO-LOCK WHERE
    finButiker.Butik = BongHode.ButikkNr NO-ERROR.
  IF NOT AVAILABLE finButiker THEN 
    LEAVE SKRIV_EOD.
  IF finButiker.EODRapporter = FALSE THEN 
    LEAVE SKRIV_EOD.
    
  IF finButiker.EODFinansrapport = TRUE THEN DO:
    iRappType = 10.
/*    RUN w-rkassarapportx.w PERSISTENT SET hRapport1.                   */
/*    RUN AutoInit IN hRapport1 (iRappType,BongHode.Butik,BongHode.Dato).*/
    RUN bibl_logg.p ('xOverforBong-EOD', 'Finansrapp Start Butikk: ' + STRING(BongHode.Butik) + 
                                         ' Dato: ' + STRING(BongHode.Dato)).                          
    RUN dagsrapp_utskrift.p ("1", BongHode.Butik, BongHode.Dato, BongHode.Dato, TRUE, OUTPUT cFilnavn).
    RUN bibl_logg.p ('xOverforBong-EOD', 'Finansrapp Slutt Butikk: ' + STRING(BongHode.Butik) + 
                                         ' Dato: ' + STRING(BongHode.Dato) + 
                                         ' Filnavn: ' + cFilnavn).                          
  END.
  IF finButiker.EODBokforingsbilag = TRUE THEN DO:
    ASSIGN iRappType = 11.
/*    RUN w-rkassarapportx.w PERSISTENT SET hRapport2.                   */
/*    RUN AutoInit IN hRapport2 (iRappType,BongHode.Butik,BongHode.Dato).*/
    RUN bibl_logg.p ('xOverforBong-EOD', 'Bokføringsbilag Start Butikk: ' + STRING(BongHode.Butik) + 
                                         ' Dato: ' + STRING(BongHode.Dato)).                          
    RUN dagsrapp_utskrift.p ("2", BongHode.Butik, BongHode.Dato, BongHode.Dato, TRUE, OUTPUT cFilnavn).
    RUN bibl_logg.p ('xOverforBong-EOD', 'Bokføringsbilag Slutt Butikk: ' + STRING(BongHode.Butik) + 
                                         ' Dato: ' + STRING(BongHode.Dato) + 
                                         ' Filnavn: ' + cFilnavn).                          
  END.
  IF finButiker.EDOJournal = TRUE THEN DO:
    RUN skrivbongrap.p (BongHode.ButikkNr,BongHode.Dato,TRUE,TRUE).
  END.
END. /* SKRIV_EOD */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Etikettsjekk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Etikettsjekk Procedure 
PROCEDURE Etikettsjekk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bEtiBong AS LOG NO-UNDO.

{syspara.i 2 4 45 cTekst}

/* Er det en bong med varesalg pï¿½ etikett artikkel pï¿½, skal bongen makuleres. */
ETIKETTSJEKK:
FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.Makulert = FALSE AND
    CAN-DO("001,002,003,004,005,006,007,009,010,011,012",STRING(BongLinje.TTId,"999")):
    IF CAN-DO(cTekst,BongLinje.Strekkode) THEN
        ASSIGN
        bEtiBong = TRUE.

END. /*ETIKETTSJEKK */
/* Makulerer bongen.                                                                       */
/* Dette er nï¿½dvendig da selgerne ofte registrerer feil og fï¿½rst legger inn varesalg feil. */
IF bEtiBong THEN
    MAKULER:
    DO:
      BongHode.Makulert = 2.
      FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
          BongLinje.B_Id = BongHode.B_Id AND
          BongLinje.Makulert = FALSE:
          ASSIGN
              BongLinje.Makulert = TRUE.
      END. /*MAKULER */
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Hovedgrupperapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hovedgrupperapport Procedure 
PROCEDURE Hovedgrupperapport PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{Hovedgrupperapport.i}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Kassarapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kassarapport Procedure 
PROCEDURE Kassarapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Lokale variabler */
DEF VAR plSum      AS DEC  NO-UNDO.
DEF VAR piLoop     AS INT  NO-UNDO.
DEF VAR pcTTId     AS CHAR NO-UNDO.
DEF VAR piFlagg    AS INT  NO-UNDO.
DEF VAR piTTId     AS INT  NO-UNDO.
DEF VAR pbInnbet   AS LOG  NO-UNDO.
DEF VAR pbInternt  AS LOG  NO-UNDO.
DEF VAR pbKredit   AS LOG  NO-UNDO.

DEF BUFFER bBongLinje FOR BongLinje.
/* Legger alle finansrapporter for en dag inn pï¿½ samme ZNr. */
ASSIGN
    iZNr   = 1
    piTTId = 0.
FOR EACH ttBonglinje: DELETE ttBonglinje. END.
/* Henter kassarapporten og z_nummer. */
HENT:
DO:
    /* Henter kassarapporten. */
    FIND kas_rap WHERE 
        kas_rap.dato       = DataSett.Dato     AND
        kas_rap.butikk     = DataSett.ButikkNr AND
        kas_rap.kasse      = DataSett.KasseNr  AND
        kas_rap.KassererNr = int(BongHode.KassererNr) AND 
        kas_rap.z_nummer   = iZNr
        EXCLUSIVE-LOCK NO-ERROR.

    /* Skaper den hvis den ikke finnes. */
    IF NOT AVAILABLE kas_rap THEN
    DO:
      CREATE kas_rap.
      ASSIGN kas_rap.dato     = DataSett.Dato
             kas_rap.butikk   = DataSett.butik
             kas_rap.kasse    = DataSett.kassenr
             kas_rap.KassererNr = int(BongHode.KassererNr) 
             kas_rap.z_nummer = iZNr.
             
      ASSIGN Kas_Rap.MvaGrp[1] = 0      /* ghg 20130220 */
             Kas_Rap.MvaGrp[2] = 1      /* ghg */
             Kas_Rap.MvaGrp[3] = 2      /* ghg */
             Kas_Rap.MvaGrp[4] = 3      /* ghg */
             Kas_Rap.MvaGrp[5] = 4      /* ghg */
             Kas_Rap.MvaGrp[6] = 5      /* ghg */
             Kas_Rap.MvaGrp[7] = 6      /* ghg */
             Kas_Rap.MvaGrp[8] = 7      /* ghg */
             Kas_Rap.MvaGrp[9] = 8      /* ghg */
             Kas_Rap.MvaGrp[10] = 9.    /* ghg */
      ASSIGN Kas_rap.MvaKredGrp[1] = 0   /* ghg */
             Kas_rap.MvaKredGrp[2] = 1   /* ghg */
             Kas_rap.MvaKredGrp[3] = 2   /* ghg */
             Kas_rap.MvaKredGrp[4] = 3   /* ghg */
             Kas_rap.MvaKredGrp[5] = 4   /* ghg */
             Kas_rap.MvaKredGrp[6] = 5   /* ghg */
             Kas_rap.MvaKredGrp[7] = 6   /* ghg */
             Kas_rap.MvaKredGrp[8] = 7   /* ghg */
             Kas_rap.MvaKredGrp[9] = 8   /* ghg */
             Kas_rap.MvaKredGrp[10] = 9. /* ghg */               
    END.
END. /* HENT */
/* Posterer medlemssalg */
IF BongHode.MedlemsNr <> 0 OR BongHode.Medlemskort <> "" THEN
DO:
    ASSIGN
        Kas_Rap.Medlemssalg    = Kas_Rap.Medlemssalg    + BongHode.Belop
        Kas_Rap.AntMedlemssalg = Kas_Rap.AntMedlemssalg + 1.
END.

/* Sjekker om det er en innbetaling */
IF CAN-FIND(FIRST BongLinje NO-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.TTId = 61 AND
    BongLinje.Makulert = FALSE) THEN
  /* Flagger betalingsmiddel som er benyttet pï¿½ bongen */
  DO:
    FOR EACH BongLinje NO-LOCK WHERE 
      BongLinje.B_Id = BongHode.B_Id AND
      BongLinje.Makulert = FALSE:
      /* Flagger betalingsmiddel */
      IF CAN-DO("050,053,054,061,066,069,070",STRING(BongLinje.TTId,"999")) THEN
      DO:
        ASSIGN
            pbInnbet = TRUE
            piTTID   = BongLinje.TTId.
        IF BongLinje.TTId <> 61 THEN
        DO:
            CREATE ttBongLinje.
            ASSIGN
            ttBongLinje.TTId     = BongLinje.TTId
            ttBongLinje.LinjeSum = BongLinje.LinjeSum.
        END.
      END.
    END.
  END.

/* Sjekker om det er en bong med varetrasaksjoner */
IF CAN-FIND(FIRST BongLinje NO-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.TTId = 87 AND
    BongLinje.Makulert = FALSE) THEN
    pbInternt = TRUE.

/* Sjekker om det er kreditsalg pï¿½ bongen */
IF CAN-FIND(FIRST BongLinje NO-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.TTId = 65 AND
    BongLinje.Makulert = FALSE) THEN
    pbKredit = TRUE.

/* Leser kvittering i workfile. */
LES:
FOR EACH BongLinje NO-LOCK WHERE 
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.Makulert = FALSE:
    
    /* NonSale artikler behandles her. */
    IF CAN-DO('1,3,10',STRING(BongLinje.TTID)) AND
       NOT CAN-DO('0',STRING(SjekkNonSale(dec(BongLinje.ArtikkelNr)))) THEN
    NON_SALE_BEHANDLING:
    DO:
      IF AVAILABLE ArtBas THEN RELEASE ArtBas.
      IF dec(BongLinje.ArtikkelNr) > 0 AND CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = dec(BongLinje.ArtikkelNr)) THEN 
        FIND ArtBas NO-LOCK WHERE
             ArtBas.ArtikkelNr = dec(BongLinje.ArtikkelNr) NO-ERROR.
      IF AVAILABLE ArtBas AND ArtBas.NON_Sale THEN 
        DO:
           IF ArtBas.NegVare THEN
           DO: 
             ASSIGN
               Kas_Rap.Non_SaleNeg = Kas_Rap.Non_SaleNeg + ((BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab - BongLinje.MvaKr) * (IF Bonglinje.Antall > 0
                                                                                 THEN 1 ELSE -1))
               Kas_Rap.Non_SaleNegAnt = Kas_Rap.Non_SaleNegAnt + Bonglinje.Antall.
             /* Spes. av neg nonsale. */
             FIND Non_Sale_Spes EXCLUSIVE-LOCK WHERE
               Non_Sale_Spes.Butikk     = Kas_Rap.Butikk AND
               Non_Sale_Spes.Kasse      = Kas_Rap.Kasse AND
               Non_Sale_Spes.Dato       = Kas_Rap.Dato AND
               Non_Sale_Spes.KassererNr = int(Kas_Rap.KassererNr) AND 
               Non_Sale_Spes.Non_Sale_type = 2 AND
               Non_Sale_Spes.Kode       = BongLinje.Strekkode NO-ERROR.
             IF NOT AVAILABLE Non_Sale_Spes THEN 
             DO:
               CREATE Non_Sale_Spes.
               ASSIGN
                 Non_Sale_Spes.Butikk     = Kas_Rap.Butikk
                 Non_Sale_Spes.Kasse      = Kas_Rap.Kasse
                 Non_Sale_Spes.KassererNr = int(Kas_Rap.KassererNr) 
                 Non_Sale_Spes.Dato       = Kas_Rap.Dato
                 Non_Sale_Spes.Non_Sale_type = 2 
                 Non_Sale_Spes.Kode       = BongLinje.Strekkode
                 .
             END.
             ASSIGN
               Non_Sale_Spes.NON_SaleVerdi = Non_Sale_Spes.NON_SaleVerdi + ((BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab - BongLinje.MvaKr) * (IF Bonglinje.Antall > 0
                                                                                                   THEN 1 ELSE -1))
               Non_Sale_Spes.NON_SaleAntall = Non_Sale_Spes.NON_SaleAntall + Bonglinje.Antall.                                                                                                      
           END.
           ELSE DO: 
             ASSIGN
               Kas_Rap.Non_SalePos = Kas_Rap.Non_SalePos + ((BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab - BongLinje.MvaKr) * (IF Bonglinje.Antall > 0
                                                                                 THEN 1 ELSE -1))
               Kas_Rap.Non_SalePosAnt = Kas_Rap.Non_SalePosAnt + Bonglinje.Antall
               .   
             /* Spes. av pos nonsale. */
             FIND Non_Sale_Spes EXCLUSIVE-LOCK WHERE
               Non_Sale_Spes.Butikk     = Kas_Rap.Butikk AND
               Non_Sale_Spes.Kasse      = Kas_Rap.Kasse AND
               Non_Sale_Spes.Dato       = Kas_Rap.Dato AND
               Non_Sale_Spes.KassererNr = int(Kas_Rap.KassererNr) AND 
               Non_Sale_Spes.Non_Sale_type = 1 AND
               Non_Sale_Spes.Kode       = BongLinje.Strekkode NO-ERROR.
             IF NOT AVAILABLE Non_Sale_Spes THEN 
             DO:
               CREATE Non_Sale_Spes.
               ASSIGN
                 Non_Sale_Spes.Butikk = Kas_Rap.Butikk
                 Non_Sale_Spes.Kasse  = Kas_Rap.Kasse
                 Non_Sale_Spes.KassererNr = int(Kas_Rap.KassererNr) 
                 Non_Sale_Spes.Dato   = Kas_Rap.Dato
                 Non_Sale_Spes.Non_Sale_type = 1 
                 Non_Sale_Spes.Kode   = BongLinje.Strekkode
                 .
             END.
             ASSIGN
               Non_Sale_Spes.NON_SaleVerdi = Non_Sale_Spes.NON_SaleVerdi + ((BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab - BongLinje.MvaKr) * (IF Bonglinje.Antall > 0
                                                                       THEN 1 ELSE -1))
               Non_Sale_Spes.NON_SaleAntall = Non_Sale_Spes.NON_SaleAntall + Bonglinje.Antall.                                                                                                      
           END.         
        END.
      NEXT LES.
    END. /* NON_SALE_BEHANDLING */

    ASSIGN
        pcTTId    = STRING(BongLinje.TTId,"999").
    /* Postering av betalingstransaksjoner. */
    POSTER:
    DO:
        /* Reklamasjoner */
        IF int(pcTTId) =  3 THEN
            ASSIGN
                kas_rap.Reklamasjon    = kas_rap.Reklamasjon + (BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab)
                Kas_Rap.AntReklamasjon = Kas_Rap.AntReklamasjon + 1.
        /* Lagerjustering */
        IF int(pcTTId) =  7 THEN
            ASSIGN
                kas_rap.Lagerjustering    = kas_rap.Lagerjustering + (BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab)
                Kas_Rap.AntLagerjustering = Kas_Rap.AntLagerjustering + 1.
        /* Varemottak */
        IF int(pcTTId) =  5 THEN
            ASSIGN
                kas_rap.Varemottak    = kas_rap.Varemottak    + (BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab)
                Kas_Rap.AntVaremottak = Kas_Rap.AntVaremottak + 1.
        /* Returer */
        IF int(pcTTId) =  10 THEN
            ASSIGN
                kas_rap.Retur    = kas_rap.Retur    + (BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab)
                Kas_Rap.AntRetur = Kas_Rap.AntRetur + 1
                Kas_Rap.AntReturer = Kas_Rap.AntReturer + 1.
        /* Brekkasje */
        IF int(pcTTId) =  2 THEN
            ASSIGN
                kas_rap.Brekkasje    = kas_rap.Brekkasje + (BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab)
                Kas_Rap.AntBrekkasje = Kas_Rap.AntBrekkasje + 1.
        /* Internt forbruk */
        IF int(pcTTId) =  11 THEN
            ASSIGN
                kas_rap.InterntForbruk    = kas_rap.InterntForbruk + (BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab)
                Kas_Rap.AntInterntForbruk = Kas_Rap.AntInterntForbruk + 1.
        /* Kontant.                                                        */
        /* Se notat i toppen av filen for transkod 20, rabatt p} subtotal. */
        IF int(pcTTId) =  50 AND pbInternt = FALSE THEN
        DO:
            ASSIGN
                kas_rap.kontant           = kas_rap.kontant    + BongLinje.LinjeSum
                Kas_Rap.AntKontant        = Kas_Rap.AntKontant + (IF Bonglinje.Linjesum <> 0 THEN 1 ELSE 0)
                kas_rap.KontantBeholdning = kas_rap.KontantBeholdning + (BongLinje.LinjeSum).
        END.
        /* Kort */
        IF int(pcTTId) = 52 THEN
        DO:
          ASSIGN
          kas_rap.kort       =   kas_rap.kort    + (BongLinje.LinjeSum)
          Kas_Rap.AntKort    =   Kas_Rap.AntKort + 1.
          RUN PosterKortSpes.
        END.
        /* Gavekort */
        IF int(pcTTId) = 53 THEN
        DO:
            ASSIGN
            kas_rap.GavekortInn    = kas_rap.GaveKortInn    + (BongLinje.LinjeSum)
            Kas_Rap.AntGaveKortInn = Kas_Rap.AntGaveKortInn + 1
            .
            /* Spesifikasjon av mottat fra andre */
            IF NUM-ENTRIES(BongLinje.BongTekst) >= 3 AND 
               ENTRY(1,BongLinje.BongTekst) = "1" THEN
                ASSIGN
                Kas_Rap.AntGaveKortAndreInn = Kas_Rap.AntGaveKortAndreInn + 1
                Kas_Rap.GaveKortAndreInn    = Kas_Rap.GaveKortAndreInn + BongLinje.LinjeSum
                .
        END.
        /* Gavekort_Ut */
        IF int(pcTTId) = 134 THEN
            ASSIGN
            kas_rap.GavekortUt       = kas_rap.GaveKortUt       + (BongLinje.LinjeSum)
            Kas_Rap.AntGaveKortUt    = Kas_Rap.AntGaveKortUt    + 1
            kas_rap.GavekortRabatt   = kas_rap.GaveKortRabatt   + (BongLinje.LinjeRab)
            Kas_Rap.AntGaveKortRabUt = Kas_Rap.AntGaveKortRabUt + 1.
            .
        /* Sjekk */
        IF int(pcTTId) = 54 THEN
            ASSIGN
            kas_rap.sjekk           = kas_rap.sjekk    + (BongLinje.LinjeSum)
            Kas_Rap.AntSjekk        = Kas_Rap.AntSjekk + 1
            Kas_rap.Sjekkbeholdning = Kas_rap.Sjekkbeholdning + (BongLinje.LinjeSum).
        /* Rekvisisjon */
        IF int(pcTTId) = 55 THEN
            ASSIGN
            kas_rap.Rekvisisasjon      =   kas_rap.Rekvisisasjon    + (BongLinje.LinjeSum)
            Kas_Rap.AntRekvisisjon     =   Kas_Rap.AntRekvisisjon + 1.
        /* Bank som betalingsmiddel. */
        IF int(pcTTId) = 58 THEN
            ASSIGN
            kas_rap.Bank    =   kas_rap.Bank    + (BongLinje.LinjeSum)
            Kas_Rap.AntBank =   Kas_Rap.AntBank + 1.
        /* Dropp. */
        IF int(pcTTId) = 59 THEN
            ASSIGN
            kas_rap.Dropp             =   kas_rap.Dropp             + (BongLinje.LinjeSum)
            Kas_Rap.AntDropp          =   Kas_Rap.AntDropp + 1
            Kas_Rap.Kontant           =   Kas_Rap.Kontant           + Bonglinje.LinjeSum
            Kas_Rap.AntKontant        =   Kas_Rap.AntKontant        - 1.
        /* Tilgode som betalingsmiddel. */
        IF int(pcTTId) = 66 THEN
        DO:
            ASSIGN
            kas_rap.tilgode       =   kas_rap.tilgode       + (BongLinje.LinjeSum)
            Kas_Rap.AntTilgode    =   Kas_Rap.AntTilGode    + 1.
            /* Er belï¿½pet negativt, skal det legges som tilgode ut. */
            IF BongLinje.LinjeSum > 0 THEN DO:
                /* Spesifikasjon av mottat fra andre */
                IF BongLinje.BongTekst = "EXTERN" OR NUM-ENTRIES(BongLinje.BongTekst) >= 3 AND 
                   ENTRY(1,BongLinje.BongTekst) = "1" THEN
                    ASSIGN Kas_Rap.AntTilgodeAndre = Kas_Rap.AntTilgodeAndre + 1
                           Kas_Rap.TilgodeAndre    = Kas_Rap.TilgodeAndre + BongLinje.LinjeSum.
                ELSE
                ASSIGN Kas_Rap.TilgodeInn    =   Kas_Rap.TilgodeInn    + (BongLinje.LinjeSum)
                       Kas_Rap.AntTilgodeInn =   Kas_Rap.AntTilgodeInn + 1.
            END.
            ELSE
                ASSIGN
                    Kas_Rap.TilgodeUt    =   Kas_Rap.TilgodeUt    - (BongLinje.LinjeSum)
                    Kas_Rap.AntTilgodeUt =   Kas_Rap.AntTilgodeUt + 1.

        END.
/*         IF int(pcTTId) = 66 THEN                                                         */
/*         DO:                                                                              */
/*             ASSIGN                                                                       */
/*             kas_rap.tilgode       =   kas_rap.tilgode       + (BongLinje.LinjeSum)       */
/*             Kas_Rap.AntTilgode    =   Kas_Rap.AntTilGode    + 1.                         */
/*             /* Er belï¿½pet negativt, skal det legges som tilgode ut. */                   */
/*             IF BongLinje.LinjeSum > 0 THEN                                               */
/*                 ASSIGN                                                                   */
/*                 Kas_Rap.TilgodeInn    =   Kas_Rap.TilgodeInn    + (BongLinje.LinjeSum)   */
/*                 Kas_Rap.AntTilgodeInn =   Kas_Rap.AntTilgodeInn + 1.                     */
/*             ELSE                                                                         */
/*                 ASSIGN                                                                   */
/*                     Kas_Rap.TilgodeUt    =   Kas_Rap.TilgodeUt    - (BongLinje.LinjeSum) */
/*                     Kas_Rap.AntTilgodeUt =   Kas_Rap.AntTilgodeUt + 1.                   */
/*                                                                                          */
/*             /* Spesifikasjon av mottat fra andre */                                      */
/*             IF NUM-ENTRIES(BongLinje.BongTekst) >= 3 AND                                 */
/*                ENTRY(1,BongLinje.BongTekst) = "1" THEN                                   */
/*                 ASSIGN                                                                   */
/*                 Kas_Rap.AntTilgodeAndre = Kas_Rap.AntTilgodeAndre + 1                    */
/*                 Kas_Rap.TilgodeAndre    = Kas_Rap.TilgodeAndre + BongLinje.LinjeSum      */
/*                 .                                                                        */
/*         END.                                                                             */

        /* Cash-Back. */
        IF int(pcTTId) = 67 THEN
            ASSIGN
            kas_rap.CashBack    =   kas_rap.CashBack    + (BongLinje.LinjeSum) * -1
            Kas_Rap.AntCashBack =   Kas_Rap.AntCashBack + 1
            Kas_Rap.Bank        =   Kas_Rap.Bank        + BongLinje.LinjeSum
            Kas_Rap.KontantBeholdning = Kas_Rap.KontantBeholdning + BongLinje.LinjeSum
            .
        /* Konto.                                                */
        /* Det forutsettes at det kun kommer en 08 trans p} en   */
        /* kvittering. Det forutsettes videre at alle artikkler  */
        /* som er solgt p} kvitteringen, skal posteres p} konto. */
        IF int(pcTTId) = 65 THEN
        DO:
            ASSIGN
                kas_rap.kredit    = kas_rap.kredit    + (BongLinje.LinjeSum)
                Kas_Rap.AntKredit = Kas_Rap.AntKredit + 1.
            RUN PosterKonto.
        END.
        /* Inn og utbetalinger.                                        */
        /* Disse skal posteres for seg selv i tillegg.                 */
        /* Dette gj|res for } kunne finne netto salg og utg}ende moms. */
        /* ved kontering av kassarapporten.                            */
        IF int(pcTTId) = 61 OR /* Innbetaling */
           int(pcTTId) = 62 THEN /* Utbetaling */
        DO:
            IF int(pcTTId) = 61 THEN /* Innbetaling */
            DO:
                ASSIGN
                kas_rap.kont_Inn          = kas_rap.kont_Inn   + (BongLinje.LinjeSum)
                Kas_Rap.AntKont_Inn       = Kas_Rap.AntKont_Inn + 1.
            END.
            ELSE DO: /* Utbetaling */
                ASSIGN
                kas_rap.kont_ut          = kas_rap.kont_ut   + (BongLinje.LinjeSum)
                Kas_Rap.AntKont_ut       = Kas_Rap.AntKont_Ut + 1.
                
                FOR EACH ttBongLinje:
                    /* Korrigerer betalingsmiddel */
                    /* Mï¿½ trekkes ned her, for ï¿½ motvikre postering av betalingsmiddel. */
                    IF piTTId = 50 /* Kontant */ THEN
                        ASSIGN
                        Kas_Rap.AntKontant        = Kas_Rap.AntKontant - (IF Kas_Rap.AntKontant > 0
                                                                          THEN 1
                                                                          ELSE 0)
                        Kas_rap.Kontant           = Kas_rap.Kontant - (ttBongLinje.LinjeSum)
                        Kas_Rap.AntKontant        = Kas_Rap.AntKontant - 1.
                    IF piTTId = 54 THEN
                    DO:
                        ASSIGN
                        Kas_rap.Sjekk    = Kas_rap.Sjekk - (ttBongLinje.LinjeSum)
                        Kas_Rap.AntSjekk = Kas_Rap.AntSjekk - 1.
                    END.
                    IF CAN-DO("066,069",STRING(piTTId,"999")) THEN
                        ASSIGN
                        kas_rap.tilgode       =   kas_rap.tilgode       - (ttBongLinje.LinjeSum)
                        Kas_Rap.AntTilgode    =   Kas_Rap.AntTilGode    - 1.
                    IF piTTId = 70 THEN
                        ASSIGN
                        kas_rap.Kontant       =   kas_rap.Kontant       - (ttBongLinje.LinjeSum)
                        Kas_Rap.AntKontant    =   Kas_Rap.AntKontant    - 1.
                END.
                
            END.
        END.

        /* Overf|ringer. */
        IF int(pcTTId) = 06 THEN
        DO:
            ASSIGN Kas_Rap.OverfortUt    = Kas_Rap.OverfortUt    + BongLinje.LinjeSum
                Kas_Rap.AntOVerfortUt = Kas_Rap.AntOverfortUt + 1.
            RUN OpprettKontoPost.
        END.

        /* Layaway inn.                     */
        IF int(pcTTId) = 72 THEN
        KODE-18:
        DO:
            /* innbetalt bel|p p} layaway_inn */
            ASSIGN kas_rap.layaway_inn    = kas_rap.layaway_inn    + (BongLinje.LinjeSum * -1)
                Kas_Rap.AntLayAway_Inn = Kas_Rap.AntLAyaway_Inn + 1.
            RUN opprettKonto (999999998).
        END. /* KODE-18 */

        /* Layaway ut. */
        IF int(pcTTId) = 73 THEN DO:
            ASSIGN kas_rap.layaway_ut    = kas_rap.layaway_ut    + (BongLinje.LinjeSum)
                Kas_Rap.AntLayAway_Ut = Kas_Rap.AntLAyaway_Ut + 1.
            RUN opprettKonto (999999998).
        END.

        /* Tilgode ut. */
        IF int(pcTTId) = 69 THEN
            ASSIGN
            Kas_Rap.Tilgode      =   Kas_Rap.Tilgode      + BongLinje.LinjeSum
            Kas_Rap.AntTilgode   =   Kas_Rap.AntTilgode   + 1
            Kas_Rap.TilgodeUt    =   Kas_Rap.TilgodeUt    + (BongLinje.LinjeSum) * -1
            Kas_Rap.AntTilgodeUt =   Kas_Rap.AntTilgodeUt + 1.
        /* Veksel - Veksel er alltid betalingsmiddel kontant. */
        /* Dvs. betalingsmiddel kontant justeres her, og kontantbeholdning justeres. */
        IF int(pcTTId) = 70 THEN
            ASSIGN
            Kas_Rap.Veksel            = Kas_Rap.Veksel  + BongLinje.LinjeSum * -1
            Kas_Rap.AntVeksel         = Kas_Rap.AntVeksel + 1
            kas_rap.kontant           = kas_rap.kontant + BongLinje.LinjeSum
            Kas_rap.KontantBeholdning = kas_rap.KontantBeholdning + BongLinje.LinjeSum.
        /* Kupong 1 */
        IF int(pcTTId) = 56 THEN
            ASSIGN
            kas_rap.kupong1    = kas_rap.kupong1    + (BongLinje.LinjeSum)
            Kas_Rap.AntKupong1 = Kas_Rap.AntKupong1 + (IF BongLinje.Antall > 0 THEN BongLinje.Antall ELSE 1).
        /* Kupong 2 */
        IF int(pcTTId) = 71 THEN
            ASSIGN
            kas_rap.kupong2    = kas_rap.kupong2    + (BongLinje.LinjeSum)
            Kas_Rap.AntKupong2 = Kas_Rap.AntKupong2 + 1.
        /* Avrundinger. */
        IF int(pcTTId) = 78 THEN
            ASSIGN
            kas_rap.Avrunding    =   kas_rap.Avrunding    + BongLinje.LinjeSum
            Kas_Rap.AntAvrunding =   Kas_Rap.AntAvrunding + 1.
        /* Reservelï¿½sning Bank som betalingsmiddel. */
        IF int(pcTTId) = 79 THEN
            ASSIGN
            kas_rap.Reservelosning    =   kas_rap.Reservelosning    + (BongLinje.LinjeSum)
            Kas_Rap.AntReservelosning =   Kas_Rap.AntReservelosning + 1.
        /* Innbetaling kunde. */
        IF int(pcTTId) = 89 THEN
            ASSIGN
            kas_rap.InnbetaltKunde    =   kas_rap.InnbetaltKunde    + (BongLinje.LinjeSum)
            Kas_Rap.AntInnbetaltKunde =   Kas_Rap.AntInnbetaltKunde + 1.
        /* Mva regnskap Salg, kundereklamasjon og retur. */
        IF CAN-DO("001,003,010",pcTTId) THEN
        DO:
            /* Adderer en for ikke ï¿½ fï¿½ 0 i indeks */
            ASSIGN
            /*Kas_Rap.MvaGrp[BongLinje.MvaGr + 1]      = BongLinje.MvaGr*/
            Kas_Rap.MvaGrunnLag[BongLinje.MvaGr + 1] = Kas_Rap.MvaGrunnLag[BongLinje.MvaGr + 1] + ((BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab - BongLinje.MvaKr) * (IF Bonglinje.Antall > 0
                                                                                      THEN 1 ELSE -1))
            Kas_Rap.MvaBelop[BongLinje.MvaGr + 1]    = Kas_Rap.MvaBelop[BongLinje.MvaGr + 1] + (BongLinje.MvaKr * (IF Bonglinje.Antall > 0
                                                                                      THEN 1 ELSE -1)).
        END.
        /* Poster MVA regnskap for kreditsalg */
        IF pbKredit THEN RUN MvaKreditRegnskap(pcTTId).
        /* Rabattpostering */
        ASSIGN
            Kas_Rap.GenerellRabatt = Kas_Rap.GenerellRabatt + BongLinje.GenerellRabatt
            Kas_Rap.MedlemsRabatt  = Kas_Rap.MedlemsRabatt  + BongLinje.MedlemsRabatt
            Kas_Rap.Kunderabatt    = Kas_Rap.KundeRabatt    + BongLinje.KundeRabatt
            Kas_Rap.PersonalRabatt = Kas_Rap.PersonalRabatt + BongLinje.PersonalRabatt
            .
    END. /* POSTER */
END. /* LES */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Kundeordre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kundeordre Procedure 
PROCEDURE Kundeordre :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR bOk AS LOG NO-UNDO.

/* Sjekker om det er kundeordre. */
FIND FIRST BongLinje WHERE
           BongLinje.B_Id = BongHode.B_Id AND
           BongLinje.Makulert = FALSE AND
           CAN-DO("027",STRING(BongLinje.TTId,"999")) NO-LOCK NO-ERROR.
IF NOT AVAILABLE BongLinje THEN RETURN.

RUN opprettKundeordre.p (BongLinje.B_Id, OUTPUT bOk) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KundeSalg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KundeSalg Procedure 
PROCEDURE KundeSalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piTransNr     AS INT  NO-UNDO.
DEF VAR pbVareSalg    AS LOG  NO-UNDO.
DEF VAR pbInnBet      AS LOG  NO-UNDO.
DEF VAR pbKredit      AS LOG  NO-UNDO.

/* Det er sjekket pï¿½ forhï¿½nd at det er kundesalg pï¿½ denne bongen.   */
/* Dvs bongen skal posteres.                                        */ 
/* Det forutsettes her at det er kontrollert at kunden    finnes og */
/* Her posteres nï¿½ salg og betalinger.                              */

/* Finner ikke kunden, posteres ikke kundetransene.                 */
FIND Kunde NO-LOCK WHERE
    Kunde.KundeNr = BongHode.KundeNr NO-ERROR.
IF NOT AVAILABLE Kunde THEN
DO:
    RUN NyFilLogg IN h_Logg (INPUT Filer.FilId, STRING(TODAY) + " " + 
                    STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                    " - Ukjent kundenummer på bong (KortNr/BongNr/KundeNr: " + 
                    BongHode.KundeKort + "/" + 
                    STRING(BongHode.BongNr) + "/" + 
                    string(BongHode.KundeNr) + ")." + CHR(1) + "3").
    RETURN "".
END.
                 
ASSIGN
    bMotPostert   = TRUE 
    pbVareSalg    = FALSE
    pbInnBet      = FALSE
    pbKredit      = FALSE
    .
TRANSRAD-1:
FOR EACH BongLinje NO-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id:

    /* Logger peker. */
    IF NOT CAN-FIND(FIRST ttKundeBut WHERE
                          ttKundeBut.KundeNr  = BongHode.KundeNr AND
                          ttKundeBut.ButikkNr = BongHode.ButikkNr) THEN
    DO:
        CREATE ttKundeBut.
        ASSIGN
            ttKundeBut.KundeNr  = BongHode.KundeNr
            ttKundeBut.ButikkNr = BongHode.ButikkNr
            .
    END.

    /* Sjekker om det er kreditsalg eller rekvisisjon pï¿½ bongen. */
    IF CAN-DO("055,065",STRING(BongLinje.TTId,"999")) THEN
        ASSIGN
          pbKredit    = TRUE
          bMotPostert = FALSE 
          .
    /* Sjekker om det er varesalg pï¿½ bongen. */
    IF CAN-DO("001",STRING(BongLinje.TTId,"999")) THEN
        ASSIGN
          pbVareSalg = TRUE 
          .
    /* Sjekker om det er innbetaling pï¿½ bongen */
    IF CAN-DO("089",STRING(BongLinje.TTId,"999")) THEN
        ASSIGN
          pbInnBet = TRUE 
          .
    /* Flagger at varesalgslinjene skal markeres som motpostert.           */
    /* Hvis det er bï¿½de varesalg og innbetaling pï¿½ bongen, vil varelinjene */
    /* pï¿½ bongen alltid vï¿½re betalt. Rekvisisjon og kredit kommer aldri    */
    /* sammen med innbetaling.                                             */
    IF pbVaresalg = TRUE AND pbInnBet = TRUE THEN
        bMotPostert = TRUE.
END. /* TRANSRAD-1 */

/* Leser og posteres varesalgstranser. */
VARESALG:
FOR EACH BongLinje NO-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    (BongLinje.TTId < 50) AND
    BongLinje.Makulert = FALSE:

    /* Overfï¿½ringer skal ikke med */
    IF BongLinje.TTId = 6 THEN
        NEXT VARESALG.   

    /* Henter artikkel. */
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = dec(BongLinje.ArtikkelNr) NO-ERROR.

    /* Transaksjonen kan ha blitt oppdatert tidligere. */
    IF CAN-FIND(bKundeTrans WHERE
                bKundeTrans.KundeNr   = BongHode.KundeNr AND
                bKundeTrans.Butik     = BongLinje.ButikkNr    AND
                bKundeTrans.TransNr   = BongLinje.TransNr  AND
                bKundeTrans.SeqNr     = BongLinje.SeqNr) THEN
      RETURN "AVBRYT".

    /* Hï¿½ndterer varekost */
    ASSIGN
        dVVareKost = BongLinje.VVareKost.
    IF dVVareKost = 0 THEN
        RUN SetVVAreKost (INPUT BongLinje.ArtikkelNr,
                          INPUT BongLinje.ButikkNr,
                          INPUT BongLinje.LinjeSum / BongLinje.Antall,
                          OUTPUT dVVareKost).

    POSTERING-TRANS:
    DO TRANSACTION:
      CREATE bKundeTrans.
      ASSIGN
          bKundeTrans.KundeNr      = BongHode.KundeNr
          bKundeTrans.Butik        = BongLinje.ButikkNr
          bKundeTrans.TransNr      = BongLinje.TransNr
          bKundeTrans.SeqNr        = BongLinje.SeqNr
          .
      ASSIGN
        bKundeTrans.KassaNr        = BongLinje.KasseNr
        bKundeTrans.Dato           = BongLinje.TransDato
        bKundeTrans.Tid            = BongLinje.TransTid
        bKundeTrans.BongId         = BongLinje.BongNr
        bKundeTrans.BongLinjeNr    = BongLinje.LinjeNr

        bKundeTrans.BatchNr        = iBatchNr
        bKundeTrans.TTId           = BongLinje.TTId
        bKundeTrans.TBId           = BongLinje.TBId
        bKundeTrans.ArtikkelNr     = dec(BongLinje.ArtikkelNr)
        bKundeTrans.LevNr          = (IF AVAILABLE ArtBas
                                    THEN ArtBas.LevNr
                                    ELSE 0)
        bKundeTrans.Vg             = BongLinje.VareGr
        bKundeTrans.LopNr          = BongLinje.LopeNr
        bKundeTrans.Storl          = BongLinje.Storrelse
        bKundeTrans.Antall         = BongLinje.Antall
        bKundeTrans.Pris           = BongLinje.LinjeSum
        bKundeTrans.RabKr          = BongLinje.LinjeRab
        bKundeTrans.SubTotalRab    = BongLinje.SubTotalRab
        bKundeTrans.Mva            = BongLinje.MvaKr
        bKundeTrans.VVarekost      = dVVarekost
        bKundeTrans.SelgerNr       = BongHode.SelgerNr
        bKundeTrans.MeldemsNr      = BongHode.MedlemsNr
        bKundeTrans.BongTekst      = BongLinje.BongTekst
        bKundeTrans.SattVVareKost  = (IF BongLinje.VVareKost <> 0
                                      THEN TRUE 
                                      ELSE FALSE)
        bKundeTrans.KortNr         = IF BongHode.KortType = 3 /* Medlemskort */
                                     THEN BongHode.MedlemsKort
                                   ELSE IF BongHode.KortType = 2 
                                     THEN BongHode.KundeKort
                                   ELSE ""
        bKundeTrans.ForsNr         = BongHode.KassererNr
        bKundeTrans.MotPostert     = bMotPostert
        bKundeTrans.RefNr          = BongLinje.RefNr
        bKundeTrans.RefTekst       = BongLinje.RefTekst
        .

      /* Snur fortegn */
      /*
      IF CAN-DO("03,04,10",STRING(BongLinje.TTId)) THEN
        ASSIGN
          bKundeTrans.Antall = bKundeTrans.Antall * -1
          .
      */

      /* Logger peker. */
      IF NOT CAN-FIND(FIRST ttKundeBut WHERE
                            ttKundeBut.KundeNr  = BongHode.KundeNr AND
                            ttKundeBut.ButikkNr = BongHode.ButikkNr) THEN
      DO:
          CREATE ttKundeBut.
          ASSIGN
              ttKundeBut.KundeNr  = BongHode.KundeNr
              ttKundeBut.ButikkNr = BongHode.ButikkNr
              .
      END.

      POSTERING-SALDO:
      DO WHILE TRUE:
        FIND bKundeSaldo EXCLUSIVE-LOCK WHERE
            bKundeSaldo.KundeNr   = BongHode.KundeNr AND
            bKundeSaldo.Butik     = BongLinje.ButikkNr
            NO-ERROR NO-WAIT.
        /* Posten holdes av en annen, vi forsï¿½ker igjen. */
        IF LOCKED bKundeSaldo THEN
            NEXT POSTERING-SALDO.
        /* Oppretter ny post. */
        IF NOT AVAILABLE bKundeSaldo THEN
        DO:
            CREATE bKundeSaldo.
            ASSIGN
                bKundeSaldo.KundeNr   = BongHode.KundeNr
                bKundeSaldo.Butik     = BongLinje.ButikkNr
                .
        END.
        /* Posterer 1.gangs kjï¿½p. */
        IF bKundeSaldo.ForsteDato = ? THEN
          ASSIGN
            bKundeSaldo.ForsteDato = BongLinje.TransDato
            bKundeSaldo.ForsteTid  = BongLinje.TransTid
            .
        /* Posterer siste gangs kjï¿½p. */
        ASSIGN
            bKundeSaldo.DatoSiste  = BongLinje.TransDato
            bKundeSaldo.SisteTid   = BongLinje.TransTid
            .
        /* Oppdaterer saldo. */
        ASSIGN
            bKundeSaldo.Saldo      = bKundeSaldo.Saldo + bKundeTrans.Pris 
            bKundeSaldo.TotaltKjop = bKundeSaldo.TotaltKjop + bKundeTrans.Pris
            .
   
        LEAVE POSTERING-SALDO.    
      END. /* POSTERING-SALDO */
    END. /* POSTERING-TRANS */

END. /* VARESALG */

/* Finner siste TransNr. */
FIND LAST KundeBetTrans NO-LOCK WHERE
    KundeBetTrans.KundeNr = BongHode.BongNr AND
    KundeBetTrans.Butik   = BongHode.ButikkN
    USE-INDEX KundeBetTrans NO-ERROR.
IF AVAILABLE KundeBetTrans THEN
    piTransNr = KundeBetTrans.TransNr.
ELSE
    piTransNr = 0.
RELEASE KundeBetTrans.

/* Leser og posterer betalingstransaksjoner. */
BETALINGER:
FOR EACH BongLinje NO-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.Makulert = FALSE:

    /* Kun betalingstranser og utgï¿½ende gavekort skal med. */
    IF (BongLinje.TTId >= 50 AND BongLinje.TTId <  90) OR 
        can-do("134",STRING(BongLinje.TTId)) THEN. /* Gjï¿½r ingenting. */
    ELSE 
        NEXT BETALINGER.

    /* Transaksjoner som ikke skal posteres i betalingsregisteret. */
    /* CashBack, Kundeinfo.                                        */
    IF CAN-DO("067,088",STRING(BongLinje.TTId,"999")) THEN
        NEXT BETALINGER.

    ASSIGN
        piTransNr = piTransNr + 1
        .

    /* Transaksjonen kan ha blitt oppdatert tidligere. */
    FIND  bKundeBetTrans NO-LOCK WHERE
          bKundeBetTrans.KundeNr   = BongHode.KundeNr AND
          bKundeBetTrans.Butik     = BongLinje.ButikkNr    AND
          bKundeBetTrans.TransNr   = piTransNr  AND
          bKundeBetTrans.SeqNr     = BongLinje.SeqNr NO-ERROR.
    DO WHILE AVAILABLE bKundeBetTrans:
        ASSIGN
            piTransNr = piTransNr + 1
            .
        /* Transaksjonen kan ha blitt oppdatert tidligere. */
        FIND  bKundeBetTrans NO-LOCK WHERE
              bKundeBetTrans.KundeNr   = BongHode.KundeNr AND
              bKundeBetTrans.Butik     = BongLinje.ButikkNr    AND
              bKundeBetTrans.TransNr   = piTransNr  AND
              bKundeBetTrans.SeqNr     = BongLinje.SeqNr NO-ERROR.
    END.

    IF NOT AVAILABLE bKundeBetTrans THEN
    POSTERING-TRANS:
    DO TRANSACTION:
      CREATE bKundeBetTrans.
      ASSIGN
          bKundeBetTrans.KundeNr      = BongHode.KundeNr
          bKundeBetTrans.Butik        = BongLinje.ButikkNr
          bKundeBetTrans.TransNr      = piTransNr
          bKundeBetTrans.SeqNr        = BongLinje.SeqNr

          .
      ASSIGN
        bKundeBetTrans.KassaNr        = BongLinje.KasseNr
        bKundeBetTrans.Dato           = BongLinje.TransDato
        bKundeBetTrans.Tid            = BongLinje.TransTid
        bKundeBetTrans.BongId         = BongLinje.BongNr
        bKundeBetTrans.BongLinjeNr    = BongLinje.LinjeNr

        bKundeBetTrans.betButik       = BongLinje.ButikkNr 
        bKundeBetTrans.betKassaNr     = BongLinje.KasseNr
        bKundeBetTrans.betBongId      = BongLinje.BongNr

        bKundeBetTrans.BatchNr        = iBatchNr
        bKundeBetTrans.TTId           = BongLinje.TTId
        bKundeBetTrans.TBId           = BongLinje.TBId
        bKundeBetTrans.Belop          = BongLinje.LinjeSum
        bKundeBetTrans.OrgBelop       = BongLinje.LinjeSum
        bKundeBetTrans.SelgerNr       = BongHode.SelgerNr
        bKundeBetTrans.MedlemsNr      = BongHode.MedlemsNr
        bKundeBetTrans.KortNr         = IF BongHode.KortType = 3 /* Medlemskort */
                                     THEN BongHode.MedlemsKort
                                   ELSE IF BongHode.KortType = 2 
                                     THEN BongHode.KundeKort
                                   ELSE ""
        bKundeBetTrans.ForsNr         = BongHode.KassererNr
        bKundeBetTrans.MotPostert     = (IF (BongHode.KundeNr <> 0 and
                                             (BongLinje.TTId = 61 OR BongLinje.TTId = 89))
                                               THEN false
                                               ELSE bMotPostert)
        bKundeBetTrans.RefNr          = BongLinje.RefNr
        bKundeBetTrans.RefTekst       = BongLinje.RefTekst
        .
      /* Er det salg av gavekort (134), skal dette hï¿½ndteres spesielt.          */
      /* Den legges inn i betalingstranser, men blir regnet med i salget. */
      IF CAN-DO("134",STRING(bKundeBetTrans.TTID,"999")) THEN
          bKundeBetTrans.Motpostert = TRUE.

      /* Disse skal ikke pï¿½virke saldo. */
      IF CAN-DO("059,065,070,079",STRING(bKundeBetTrans.TTID,"999")) THEN
          bKundeBetTrans.Motpostert = TRUE.

      /* Disse skal alltid pï¿½virke saldo pï¿½ en bong med kreditsalg. */
      IF pbKredit AND can-do("050,051,052,053,054,055,056,058",STRING(bKundeBetTrans.TTID,"999")) THEN
          bKundeBetTrans.Motpostert = FALSE.
      ELSE IF pbKredit = FALSE AND can-do("050,051,052,053,054,055,056,058",STRING(bKundeBetTrans.TTID,"999")) THEN
          bKundeBetTrans.Motpostert = TRUE.

      /* Snur fortegn */
      /*
      IF CAN-DO("09",STRING(BongLinje.TTId)) THEN
        ASSIGN
          bKundeBetTrans.Belop          = bKundeBetTrans.Belop    * -1
          bKundeBetTrans.OrgBelop       = bKundeBetTrans.OrgBelop * -1
          .
      */
    END. /* POSTERING-TRANS */
END. /* BETALNGER */

IF AVAILABLE kunde THEN
DO:
    RUN beregnkundesaldo.p (Kunde.kundeNr, BongHode.ButikkNr).
    FIND CURRENT Kunde NO-LOCK.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-loggFinansEksport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loggFinansEksport Procedure 
PROCEDURE loggFinansEksport :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER lB_Id AS DECIMAL NO-UNDO.

DEFINE BUFFER trgEkstEDBSystem FOR EkstEDBSystem.
DEFINE BUFFER trgBongHode FOR BongHode.

FIND trgBongHode NO-LOCK WHERE
  trgBongHode.B_Id = lB_Id NO-ERROR.
IF NOT AVAILABLE trgBongHode THEN 
  RETURN.

FIND FIRST trgEkstEDBSystem WHERE 
    trgEkstEDBSystem.DataType = "KONTAUTO" AND 
    trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
IF AVAILABLE trgEkstEDBSystem THEN
ERPLOGG:
DO:
    IF CAN-FIND(FIRST BongLinje WHERE
        BongLinje.B_Id = trgBonghode.B_Id AND
        BongLinje.TTId = 1) OR /* Salg */ 
    CAN-FIND(FIRST BongLinje WHERE
        BongLinje.B_Id = trgBonghode.B_Id AND
        BongLinje.TTId = 3) OR /* Kundereklamasjon */
    CAN-FIND(FIRST BongLinje WHERE
        BongLinje.B_Id = trgBonghode.B_Id AND
        BongLinje.TTId = 10)  /* Retur */ THEN
    /* Endring som skal til ERP system */
    ERPUT:
    DO:
        FIND ELogg WHERE 
             ELogg.TabellNavn     = "Bonghode" AND
             ELogg.EksterntSystem = "KONTAUTO"    AND
             ELogg.Verdier        = STRING(trgBongHode.B_Id) NO-ERROR.
        IF NOT AVAIL Elogg THEN DO:
            CREATE Elogg.
            ASSIGN ELogg.TabellNavn     = "Bonghode"
                   ELogg.EksterntSystem = "KONTAUTO"   
                   ELogg.Verdier        = STRING(trgBonghode.B_Id).
        END.
        ASSIGN ELogg.EndringsType = 1 
               ELogg.Behandlet    = FALSE.
        RELEASE ELogg.
    END. /* ERPUT */

END. /* ERPLOGG */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-loggKuponginnlosen) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loggKuponginnlosen Procedure 
PROCEDURE loggKuponginnlosen :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEFINE VARIABLE cTransactionId AS CHARACTER FORMAT "x(20)" NO-UNDO.

  /* Spesifikasjoner for kuponginnlï¿½sens bonger. */
  IF NOT CAN-FIND(FIRST BongLinje NO-LOCK WHERE
      BongLinje.B_Id     = BongHode.B_Id AND
      BongLinje.Makulert = FALSE AND
      CAN-DO("205",STRING(BongLinje.TTId,"999"))) THEN 
      RETURN.
   
  /* KUPONGINNLï¿½SEN */
  LOOPEN:
  FOR EACH BongLinje NO-LOCK WHERE
      BongLinje.B_Id     = BongHode.B_Id AND
      BongLinje.Makulert = FALSE AND
      CAN-DO("205",STRING(BongLinje.TTId,"999")):
  
      FIND FIRST KupongEier NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KupongEier THEN 
      DO TRANSACTION:
          CREATE KupongEier.
          ASSIGN
              KupongEier.KEierNr = 1
              KupongEier.KENavn  = 'Kupongeier'.
          FIND CURRENT KupongEier NO-LOCK.
      END. /* TRANSACTION */
      
      /* Oppdaterer kupong registeret hvis ikke kuponginformasjonen finnes der. */
      FIND FIRST Kupong NO-LOCK WHERE
          Kupong.EANKode = Bonglinje.StrekKode AND
          Kupong.InterleaveKode = TRIM(BongLinje.OriginalData) NO-ERROR.

      FIND TransBeskr NO-LOCK  WHERE 
              TransBeskr.TTId = BongLinje.TTId AND 
              TransBeskr.TBId = BongLinje.TBId NO-ERROR.
      
      IF NOT AVAILABLE Kupong THEN
      OPPRETTKUPONG: 
      DO TRANSACTION:
          FIND TransBeskr NO-LOCK  WHERE 
              TransBeskr.TTId = BongLinje.TTId AND 
              TransBeskr.TBId = BongLinje.TBId NO-ERROR.
          
          CREATE Kupong.
          ASSIGN
              /* Id legges opp i trigger. */
              Kupong.EANKode = BongLinje.Strekkode
              Kupong.InterleaveKode = TRIM(BongLinje.OriginalData)
              
              Kupong.KupBeskrivelse = IF AVAILABLE TransBeskr 
                                        THEN TransBeskr.Beskrivelse 
                                      ELSE '* Kupongtype ikke definert i tabell TransBeskr.'
              Kupong.Belop          = 0
              Kupong.MaksBelop      = 0
              Kupong.MinBelop       = 0
              Kupong.GyldigFra      = TODAY 
              Kupong.GyldigTil      = ?
              Kupong.Aktiv          = TRUE 
              Kupong.IdKrav         = FALSE 
              Kupong.KTypeNr        = BongLinje.TBId
              Kupong.RabattVerdi    = 0
              Kupong.SisteinnlDato  = ?
              Kupong.TaVarePaKupong = TRUE 
              Kupong.KupongNotat    = '* Opprettet fra xoverforbong.p. ' + CHR(10) + ' But/Dato/Kasse/BongNr: '  +
                                      STRING(BongLinje.ButikkNr) + '/' + 
                                      STRING(BongLinje.Dato) + '/' + 
                                      STRING(BongLinje.KasseNr) + '/' + 
                                      STRING(BongLinje.BongNr)  
              Kupong.KEierNr        = IF AVAILABLE KupongEier THEN KupongEier.KEierNr ELSE 0
              .
          FIND CURRENT Kupong NO-LOCK NO-ERROR.
      END. /* OPPRETTKUPONG TRANSACTION */ 
  
      /* Posterer transaksjonen */
      KUPONGTRANSLOGG:
      DO TRANSACTION:
          CREATE KupongTransLogg.
          ASSIGN
              /* KTLoggId settes i TRIGGER. */
              KupongTransLogg.B_Id           = BongLinje.B_Id
              KupongTransLogg.ButikkNr       = BongLinje.ButikkNr
              KupongTransLogg.SalgsDato      = BongLinje.TransDato
              KupongTransLogg.SalgsTid       = BongLinje.TransTid
              KupongTransLogg.Avregnet       = ?
              KupongTransLogg.KupBeskrivelse = IF AVAILABLE TransBeskr THEN TransBeskr.Beskrivelse ELSE ''
              KupongTransLogg.BelopKnd       = BongLinje.LinjeSum
              KupongTransLogg.BelopBut       = BongLinje.PrisPrSalgsenhet
              KupongTransLogg.Kode           = BongLinje.Strekkode
              KupongTransLogg.Interleave     = ENTRY(2,BongLinje.BongTekst,'|')
              KupongTransLogg.KTypeNr        = BongLinje.TBId
              KupongTransLogg.BongTekst      = BongLinje.BongTekst
              KupongTransLogg.LinjeNr        = BongLinje.LinjeNr
              KupongTransLogg.KasseNr        = BongLinje.KasseNr
              KupongTransLogg.GruppeNr       = BongLinje.GruppeNr
              KupongTransLogg.BongNr         = BongLinje.BongNr
              .
          RELEASE KupongTransLogg.          
      END. /* TRANSACTION */
    END. /* KUPONGTRANSLOGG TRANSRAD. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-loggMayFlower) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loggMayFlower Procedure 
PROCEDURE loggMayFlower :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTransactionId AS CHARACTER FORMAT "x(20)" NO-UNDO.

        /* NETS */
        LOOPEN:
        FOR EACH BongLinje NO-LOCK WHERE
            BongLinje.B_Id     = BongHode.B_Id AND
            BongLinje.Makulert = FALSE AND
            CAN-DO("001,003,010",STRING(BongLinje.TTId,"999")):
  
            FIND ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = DEC(BongLinje.ArtikkelNr) NO-ERROR.
            IF AVAILABLE ArtBas AND ArtBas.OnLineLevNr = 3 THEN 
            LOGG_ELOGG:
            DO TRANSACTION:
                FIND ELogg WHERE 
                    ELogg.TabellNavn     = "Bonghode" AND
                    ELogg.EksterntSystem = "MAYFLOWER"    AND
                    ELogg.Verdier        = STRING(BongHode.B_Id) NO-ERROR.
                IF NOT AVAIL Elogg THEN 
                DO:
                    CREATE Elogg.
                    ASSIGN 
                        ELogg.TabellNavn     = "Bonghode"
                        ELogg.EksterntSystem = "MAYFLOWER"   
                        ELogg.Verdier        = STRING(Bonghode.B_Id).
                END.
                ASSIGN 
                    ELogg.EndringsType = 1 
                    ELogg.Behandlet    = FALSE.
                RELEASE ELogg.
            END. /* TRANSACTION LOGG_ELOGG*/  
        END. /* TRANSRAD. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-loggNets) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loggNets Procedure 
PROCEDURE loggNets :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEFINE VARIABLE cTransactionId AS CHARACTER FORMAT "x(20)" NO-UNDO.

  IF NOT bLoggNets THEN  
    RETURN.
    
  /* Er det tidsregistrering, skal det ikke logges her. */
  IF CAN-FIND(FIRST BongLinje NO-LOCK WHERE
      BongLinje.B_Id     = BongHode.B_Id AND
      BongLinje.Makulert = FALSE AND
      CAN-DO("096,097",STRING(BongLinje.TTId,"999"))) THEN 
      RETURN.
  ELSE 
  /* NETS */
  LOOPEN:
  FOR EACH BongLinje NO-LOCK WHERE
      BongLinje.B_Id     = BongHode.B_Id AND
      BongLinje.Makulert = FALSE AND
      CAN-DO("095",STRING(BongLinje.TTId,"999")):
  
      IF Bonglinje.BongTekst BEGINS 'Ref.:' OR 
         BongLinje.BongTekst BEGINS 'REF:' THEN 
      LOGG_NETS:
      DO:
        /* Gammelt format */
        IF Bonglinje.BongTekst BEGINS 'Ref.:' THEN 
        DO:
          ASSIGN lDec = DECIMAL(TRIM(ENTRY(2,Bonglinje.BongTekst,' '))) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN 
            NEXT LOOPEN.
          
          ASSIGN lDec = DECIMAL(TRIM(ENTRY(3,Bonglinje.BongTekst,' '))) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN 
            NEXT LOOPEN.
          ASSIGN cTransactionId = TRIM(ENTRY(2,Bonglinje.BongTekst,' ')) + ' ' + TRIM(ENTRY(3,Bonglinje.BongTekst,' ')).
        END.
        
        /* Nytt format */
        ELSE IF LENGTH(TRIM(ENTRY(2,Bonglinje.BongTekst,' '))) = 12 THEN
        DO:
          ASSIGN lDec = DECIMAL(TRIM(ENTRY(2,Bonglinje.BongTekst,' '))) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN 
            NEXT LOOPEN.
          ASSIGN cTransactionId = SUBSTRING(TRIM(ENTRY(2,Bonglinje.BongTekst,' ')),1,6) + ' ' + SUBSTRING(TRIM(ENTRY(2,Bonglinje.BongTekst,' ')),7,6).
        END.
        
        /* Denne skal vi ikke ha med. */
        ELSE NEXT LOOPEN.
          
        IF cTransactionID > '' THEN 
        DO TRANSACTION:
          FIND FIRST Nets NO-LOCK WHERE
            Nets.B_Id = BongHode.B_Id AND 
            Nets.TransactionId = cTransactionId NO-ERROR.
          IF NOT AVAILABLE Nets THEN 
          DO:
            CREATE Nets.
            ASSIGN
              Nets.B_Id           = BongHode.B_Id
              Nets.TransactionId  = cTransactionId
              Nets.iJBoxCompanyId = iCL
              Nets.ButikkNr       = BongLinje.ButikkNr
              Nets.Dato           = BongHode.Dato
              .
            RELEASE Nets.  
          END.
          LEAVE LOOPEN.
        END. /* TRANSACTION */
      END. /* LOGG_NETS */
  END. /* TRANSRAD. */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-loggTransloggEksport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loggTransloggEksport Procedure 
PROCEDURE loggTransloggEksport :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER lB_Id AS DECIMAL NO-UNDO.

DEFINE BUFFER trgBongHode FOR BongHode.

/* Skal det sendes translog til HK (Eksporteres med eksportbutikk.p) */
IF bTransloggTilHk = FALSE THEN 
  RETURN.
ELSE DO:  
    FIND trgBongHode NO-LOCK WHERE
      trgBongHode.B_Id = lB_Id NO-ERROR.
    IF NOT AVAILABLE trgBongHode THEN 
      RETURN.
    
    ERPLOGG:
    DO:
        FOR EACH BongLinje WHERE
            BongLinje.B_Id = trgBonghode.B_Id AND
            BongLinje.TransNr > 0:
          /* Endring som skal til ERP system */
          ERPUT:
          DO:
            FIND ELogg WHERE 
                 ELogg.TabellNavn     = "Translogg" AND
                 ELogg.EksterntSystem = "HK"    AND
                 ELogg.Verdier        = STRING(BongLinje.ButikkNr) + CHR(1) + STRING(BongLinje.TransNr) + CHR(1) + STRING(BongLinje.SeqNr)  NO-ERROR.
            IF NOT AVAIL Elogg THEN DO:
                CREATE Elogg.
                ASSIGN ELogg.TabellNavn     = "Translogg"
                       ELogg.EksterntSystem = "HK"   
                       ELogg.Verdier        = STRING(BongLinje.ButikkNr) + CHR(1) + STRING(BongLinje.TransNr) + CHR(1) + STRING(BongLinje.SeqNr).
            END.
            ASSIGN ELogg.EndringsType = 1 
                   ELogg.Behandlet    = FALSE.
            RELEASE ELogg.
          END. /* ERPUT */
        END.
    END. /* ERPLOGG */
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Medlemssalg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Medlemssalg Procedure 
PROCEDURE Medlemssalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
                
/* Det er sjekket pï¿½ forhï¿½nd at det er medlemssalg pï¿½ denne bongen. */
/* Dvs bongen skal posteres.                                        */ 
/* Det forutsettes her at det er kontrollert at medlemmet finnes og */
/* Her posteres nï¿½ salg og betalinger.                              */

DEF BUFFER bMedTrans FOR MedTrans.                
DEF BUFFER bMedlemSaldo FOR MEdlemSaldo.
                
/* Finner ikke medlemmet, posteres ikke medlemstransene.            */
FIND Medlem NO-LOCK WHERE
    Medlem.MedlemsNr = BongHode.MedlemsNr NO-ERROR.
IF NOT AVAILABLE Medlem THEN
DO:
    RUN NyFilLogg IN h_Logg (INPUT Filer.FilId, STRING(TODAY) + " " + 
                    STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                    " - Ukjent medlemsnummer på bong (KortNr/BongNr/MedlemsNr: " + 
                    BongHode.MedlemsKort + "/" + 
                    STRING(BongHode.BongNr) + "/" + 
                    string(BongHode.MedlemsNr) + ")." + CHR(1) + "3").
    RETURN "".
END.
                 
/* Leser og posteres varesalgstranser. */
VARESALG:
FOR EACH BongLinje NO-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.TTId < 50 AND
    BongLinje.Makulert = FALSE:

    /* Overfï¿½ringer skal ikke med */
    IF BongLinje.TTId = 6 THEN
        NEXT VARESALG.

    /* Henter artikkel. */
    FIND ArtBas NO-LOCK WHERE
        ArtBas.Vg    = BongLinje.VareGr AND
        ArtBas.LopNr = BongLinje.LopeNr NO-ERROR.

    /* Transaksjonen kan ha blitt oppdatert tidligere. */
    IF CAN-FIND(bMedTrans WHERE
                bMedTrans.MedlemsNr = BongHode.MedlemsNr AND
                bMedTrans.Butik     = BongLinje.ButikkNr    AND
                bMedTrans.TransNr   = BongLinje.TransNr  AND
                bMedTrans.SeqNr     = BongLinje.SeqNr) THEN
      RETURN "AVBRYT".

    /* Hï¿½nterer varekost */
    ASSIGN
        dVVareKost = BongLinje.VVareKost.
    IF dVVareKost = 0 THEN
        RUN SetVVAreKost (INPUT BongLinje.ArtikkelNr,
                          INPUT BongLinje.ButikkNr,
                          INPUT BongLinje.LinjeSum / BongLinje.Antall,
                          OUTPUT dVVareKost).

    POSTERING-TRANS:
    DO FOR bMedTrans TRANSACTION:
      CREATE bMedTrans.
      ASSIGN
          bMedTrans.MedlemsNr      = BongHode.MedlemsNr
          bMedTrans.Butik          = BongLinje.ButikkNr
          bMedTrans.TransNr        = BongLinje.TransNr
          bMedTrans.SeqNr          = BongLinje.SeqNr

          .
      ASSIGN
        bMedTrans.KassaNr        = BongLinje.KasseNr
        bMedTrans.Dato           = BongLinje.TransDato
        bMedTrans.Tid            = BongLinje.TransTid
        bMedTrans.BongId         = BongLinje.BongNr
        bMedTrans.BongLinjeNr    = BongLinje.LinjeNr

        bMedTrans.BatchNr        = iBatchNr
        bMedTrans.TTId           = BongLinje.TTId
        bMedTrans.TBId           = BongLinje.TBId
        bMedTrans.ArtikkelNr     = dec(BongLinje.ArtikkelNr)
        bMedTrans.LevNr          = (IF AVAILABLE ArtBas
                                    THEN ArtBas.LevNr
                                    ELSE 0)
        bMedTrans.Vg             = BongLinje.VareGr
        bMedTrans.LopNr          = BongLinje.LopeNr
        bMedTrans.Storl          = BongLinje.Storrelse
        bMedTrans.Antall         = BongLinje.Antall
        bMedTrans.Pris           = BongLinje.LinjeSum
        bMedTrans.RabKr          = BongLinje.LinjeRab
        bMedTrans.SubTotalRab    = BongLinje.SubTotalRab
        bMedTrans.Mva            = BongLinje.MvaKr
        bMedTrans.VVarekost      = dVVarekost
        bMedTrans.SelgerNr       = BongHode.SelgerNr
        bMedTrans.BongTekst      = BongLinje.BongTekst
        bMedTrans.SattVVareKost  = (IF BongLinje.VVareKost <> 0
                                      THEN TRUE 
                                      ELSE FALSE)
        bMedTrans.KortNr         = IF BongHode.KortType = 3 /* Medlemskort */
                                     THEN BongHode.MedlemsKort
                                   ELSE IF BongHode.KortType = 2 
                                     THEN BongHode.KundeKort
                                   ELSE ""
        bMedTrans.ForsNr         = BongHode.KassererNr
        bMedTrans.RefNr          = BongLinje.RefNr
        bMedTrans.RefTekst       = BongLinje.RefTekst
        .

       /* Snur fortegn */
       /*
       IF CAN-DO("03,04,10",STRING(BongLinje.TTId)) THEN
         ASSIGN
           bMedTrans.Antall = bMedTrans.Antall * -1
           .
       */

       /* Logger peker. */
       IF NOT CAN-FIND(FIRST ttMedlemBut WHERE
                             ttMedlemBut.MedlemsNr = BongHode.MedlemsNr AND
                             ttMedlemBut.ButikkNr  = BongHode.ButikkNr) THEN
       DO:
           CREATE ttMedlemBut.
           ASSIGN
               ttMedlemBut.MedlemsNr = BongHode.MedlemsNr
               ttMedlemBut.ButikkNr  = BongHode.ButikkNr
               .
       END.
 
      POSTERING-SALDO:
      DO FOR bMedlemSaldo WHILE TRUE:
        FIND bMedlemSaldo EXCLUSIVE-LOCK WHERE
            bMedlemSaldo.MedlemsNr = BongHode.MedlemsNr AND
            bMedlemSaldo.Butik     = BongLinje.ButikkNr
            NO-ERROR NO-WAIT.
        /* Posten holdes av en annen, vi forsï¿½ker igjen. */
        IF LOCKED bMedlemSaldo THEN
            NEXT POSTERING-SALDO.
        /* Oppretter ny post. */
        IF NOT AVAILABLE bMedlemSaldo THEN
        DO:
            CREATE bMedlemSaldo.
            ASSIGN
                bMedlemSaldo.MedlemsNr = BongHode.MedlemsNr
                bMedlemSaldo.Butik     = BongLinje.ButikkNr
                .
        END.
        /* Posterer 1.gangs kjï¿½p. */
        IF bMedlemSaldo.ForsteDato = ? THEN
          ASSIGN
            bMedlemSaldo.ForsteDato = BongLinje.TransDato
            bMedlemSaldo.ForsteTid  = BongLinje.TransTid
            .
        /* Posterer siste gangs kjï¿½p. */
        ASSIGN
            bMedlemSaldo.DatoSiste  = BongLinje.TransDato
            bMedlemSaldo.SisteTid   = BongLinje.TransTid
            .
        /* Oppdaterer saldo. */
        ASSIGN
            bMedlemSaldo.Saldo      = bMedlemSaldo.Saldo + bMedTrans.Pris 
            bMedlemSaldo.TotaltKjop = bMedlemSaldo.TotaltKjop + bMedTrans.Pris
            .
   
        LEAVE POSTERING-SALDO.    
      END. /* POSTERING-SALDO */
    END. /* POSTERING-TRANS */

END. /* VARESALG */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-MvaKreditRegnskap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MvaKreditRegnskap Procedure 
PROCEDURE MvaKreditRegnskap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pc2TTId AS CHAR NO-UNDO.

  /* Mva regnskap */
  IF CAN-DO("001,003,010",pc2TTId) THEN
  DO:

    /* Adderer en for ikke ï¿½ fï¿½ 0 i indeks */
    ASSIGN
    /*Kas_Rap.MvaKredGrp[BongLinje.MvaGr + 1]      = BongLinje.MvaGr*/
    Kas_Rap.MvaKredGrunnLag[BongLinje.MvaGr + 1] = Kas_Rap.MvaKredGrunnLag[BongLinje.MvaGr + 1] 
                                                      + ((BongLinje.LinjeSum 
                                                      - BongLinje.LinjeRab
                                                      - BongLinje.SubtotalRab
                                                      - BongLinje.MvaKr) * (IF Bonglinje.Antall > 0
                                                                              THEN 1
                                                                              ELSE -1))
    Kas_Rap.MvaKredBelop[BongLinje.MvaGr + 1]    = Kas_Rap.MvaKredBelop[BongLinje.MvaGr + 1]    
                                                      + (BongLinje.MvaKr * (IF Bonglinje.Antall > 0
                                                                              THEN 1
                                                                              ELSE -1))
    .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettGavekort) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettGavekort Procedure 
PROCEDURE OpprettGavekort :
/*------------------------------------------------------------------------------
  Purpose:     Posterer salg og bruk av gavekort. 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR plWork     AS DEC  NO-UNDO.
DEF VAR pcTekst    AS CHAR NO-UNDO.
DEF VAR piTransNr  AS INT  NO-UNDO.
DEF VAR pcTTId     AS CHAR NO-UNDO.
DEF VAR piTTId     AS INT  NO-UNDO.
DEF VAR piTBId     AS INT  NO-UNDO.
DEF VAR piButikkNr AS INT NO-UNDO.

DEF VAR pcIdentNr    AS CHAR NO-UNDO.                      
DEF VAR pdGyldigDato AS DATE NO-UNDO.

DEF VAR piLoop AS INT NO-UNDO.

/* Posterer gavekort. Salg av gavekort og brukte gavekort. */
SKAPELSEN:
FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.Makulert = FALSE AND
    CAN-DO("053,057,134",STRING(BongLinje.TTId,"999")):

    ASSIGN
        lDec   = 0
        piTTId = BongLinje.TTId
        piTBId = BongLinje.TBId
        pcTTId  = STRING(BongLinje.TTId,"999")
        .
    /* Salg av gavekort. */
    IF BongLinje.TTId = 134 THEN
    SALG-GAVEKORT:
    DO:
      
         ASSIGN pdGyldigDato = ?.
         ASSIGN pcIdentNr    = BongLinje.Strekkode.
         ASSIGN pdGyldigDato = DATE(BongLinje.BongTekst) NO-ERROR.
         IF pdGyldigDato = ? THEN 
           pdGyldigDato = BongLinje.TransDato + iGavekortGyldig.
            
        /* Er det ikke angitt en ident, returnerer kassen '0'. Det erstatter vi med dato og bongnr. */       
        ASSIGN  lDec = DEC(pcIdentNr) NO-ERROR.
        IF lDec = 0 THEN 
            pcIdentNr = STRING(BongLinje.TransDato) + '-' + STRING(BongLinje.KasseNr) + '-' + STRING(BongLinje.BongNr) + '-' + STRING(BongLinje.LinjeNr).
            
        /* Her skal vi ikke finne noe kort fra fï¿½r. */
        FIND GaveKort EXCLUSIVE-LOCK WHERE
            Gavekort.ButNr   = BongLinje.ButikkNr AND
            Gavekort.IdentNr = pcIdentNr NO-ERROR.
        IF NOT AVAILABLE Gavekort OR GaveKort.BongNr > 0 THEN
        OPPRETT:
        DO ON ERROR UNDO, RETRY:
            /* Finner vi alikevel et kort, opprettes et nytt id. */
            IF AVAILABLE GaveKort THEN
            ERRORLOOP:
            DO WHILE TRUE:
                piLoop = piLoop + 1.
                FIND GaveKort WHERE
                    GaveKort.ButNr    = BongLinje.ButikkNr AND
                    GaveKort.IdentNr  = pcIdentNr + "-DUBL" + string(piLoop) NO-ERROR.
                IF AVAILABLE Gavekort THEN
                    NEXT ERRORLOOP.
                /* Nytt Id */
                pcIdentNr = pcIdentNr + "-DUBL" + string(piLoop).
                LEAVE ERRORLOOP.
            END. /* ERRORLOOP */        
            DO:
                CREATE Gavekort.
                ASSIGN
                    Gavekort.ButNr      = BongLinje.ButikkNr
                    Gavekort.IdentNr    = pcIdentNr
                    Gavekort.IdentType  = 1 /* Alltid 1 nï¿½r det opprettes fra kassen */
                    NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    UNDO OPPRETT, RETRY OPPRETT.
            END.
        END. /* OPPRETT */
        IF AVAILABLE GaveKort THEN
        DO:
            FIND ButikkForsalj NO-LOCK WHERE
                ButikkForsalj.Butik  = BongLinje.ButikkNr AND
                ButikkForsalj.ForsNr = int(BongHode.KassererNr) NO-ERROR.
            ASSIGN
            Gavekort.Dato       = BongLinje.Dato
            Gavekort.Tid        = BongLinje.TransTid
            Gavekort.KasseNr    = BongLinje.KasseNr
            Gavekort.SelgerNr   = BongHode.SelgerNr
            GaveKort.KassNr     = (IF AVAILABLE ButikkForsalj
                                     THEN ButikkForsalj.KassererId
                                     ELSE 0)
            Gavekort.BongNr     = BongLinje.BongNr
            Gavekort.GyldigDato = IF pdGyldigDato <> ?
                                    THEN pdGyldigDato
                                    ELSE Gavekort.GyldigDato
            Gavekort.Belop      = BongLinje.LinjeSum
            GaveKort.RabKr      = BongLinje.LinjeRab            
            GaveKort.FraB_Id    = BongLinje.B_Id
            Gavekort.Eget       = TRUE 
            .
        END.
    END. /* SALG-GAVEKORT */
    /* Betaling med gavekort. */
    IF BongLinje.TTId = 53 THEN
    SALG-GAVEKORT:
    DO:
        RELEASE Gavekort.
        ASSIGN
            lDec         = 0
            pcIdentNr    = BongLinje.Strekkode
            piButikkNr   = BongLinje.Antall
            .

        /* Er det ikke angitt en ident, returnerer kassen '0'. Det erstatter vi med dato og bongnr. */
        ASSIGN lDec = DEC(pcIdentNr) NO-ERROR.
        IF lDec = 0 THEN 
            pcIdentNr = STRING(BongLinje.TransDato) + '-' + STRING(BongLinje.KasseNr) + '-' + STRING(BongLinje.BongNr) + '-' + STRING(BongLinje.LinjeNr).
        /* Vi mï¿½ste fï¿½rst hitta mha piButikkNr som ï¿½r den som stï¿½llt ut vid vï¿½rt gavekortsnr */
        IF LENGTH(Bonglinje.strekkode) = 22 THEN DO:
            FIND GaveKort EXCLUSIVE-LOCK WHERE
                Gavekort.ButNr   = piButikkNr AND
                Gavekort.IdentNr = pcIdentNr NO-ERROR.
            /* detta ï¿½r onï¿½digt */
            IF NOT AVAIL Gavekort THEN
                FIND FIRST GaveKort EXCLUSIVE-LOCK WHERE
                    Gavekort.IdentNr = pcIdentNr NO-ERROR.
        END.
        /* om vi inte hittat skal vi finne et kort fra fï¿½r. */
        IF NOT AVAIL Gavekort THEN
            FIND GaveKort EXCLUSIVE-LOCK WHERE
                Gavekort.ButNr   = (IF BongLinje.MButikk > 0
                                   THEN BongLinje.MButikk
                                   ELSE BongLinje.ButikkNr) AND
                Gavekort.IdentNr = pcIdentNr NO-ERROR.
        OPPRETT:
        DO ON ERROR UNDO, RETRY:
            /* Finner vi kortet og det er brukt fra fï¿½r, mï¿½ nytt kort opprettes. */
            IF AVAILABLE GaveKort AND GaveKort.BruktDato <> ? THEN
            ERRORLOOP:
            DO WHILE TRUE:
                piLoop = piLoop + 1.
                FIND GaveKort WHERE
                    GaveKort.ButNr    = (IF BongLinje.MButikk > 0
                                         THEN BongLinje.MButikk
                                         ELSE BongLinje.ButikkNr) AND
                    GaveKort.IdentNr  = pcIdentNr + "-DUBL" + string(piLoop) NO-ERROR.
                IF AVAILABLE Gavekort THEN
                    NEXT ERRORLOOP.
                /* Nytt Id */
                pcIdentNr = pcIdentNr + "-DUBL" + string(piLoop).
                LEAVE ERRORLOOP.
            END. /* ERRORLOOP */        
            IF NOT AVAILABLE Gavekort THEN
            DO:
                CREATE Gavekort.
                ASSIGN
                    Gavekort.ButNr      = (IF BongLinje.MButikk > 0
                                           THEN BongLinje.MButikk
                                           ELSE BongLinje.ButikkNr) /* Utstedt i butikk */
                    Gavekort.IdentNr    = pcIdentNr
                    Gavekort.IdentType  = IF NUM-ENTRIES(BongLinje.BongTekst) = 4
                                          THEN int(ENTRY(4,BongLinje.BongTekst))
                                          ELSE 1
                    NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    UNDO OPPRETT, RETRY OPPRETT.
            END.
        END. /* OPPRETT */
        IF AVAILABLE Gavekort THEN
        DO:
            FIND ButikkForsalj NO-LOCK WHERE
                ButikkForsalj.Butik  = BongLinje.ButikkNr AND /* Brukt butikk */
                ButikkForsalj.ForsNr = int(BongHode.KassererNr) NO-ERROR.
            /* Legger pï¿½ brukt informasjonen. */
            ASSIGN
                GaveKort.BruktB_Id       = BongLinje.B_Id
                GaveKort.BruktButNr      = BongLinje.ButikkNr
                Gavekort.BruktDato       = BongLinje.TransDato
                Gavekort.BruktTid        = BongLinje.TransTid
                Gavekort.BruktKasseNr    = BongLinje.KasseNr
                Gavekort.BruktSelgerNr   = BongHode.SelgerNr
                GaveKort.BruktKassNr     = (IF AVAILABLE ButikkForsalj
                                         THEN ButikkForsalj.KassererId
                                         ELSE 0)
                Gavekort.BruktBongNr     = BongLinje.BongNr
                Gavekort.Belop      = IF Gavekort.Belop = 0
                                         THEN BongLinje.LinjeSum
                                         ELSE Gavekort.Belop
                Gavekort.Utgatt          = TRUE
                Gavekort.UtgattDato      = BongLinje.TransDato
                Gavekort.UtgattTid       = BongLinje.TransTid
                Gavekort.GyldigDato      = IF Gavekort.GyldigDato = ?
                                             THEN Gavekort.UtgattDato
                                             ELSE Gavekort.GyldigDato
                Gavekort.UtgattRegAv     = USERID('SkoTex')
                Gavekort.Eget            = IF BongLinje.MButikkNr = BongLinje.butikkNr
                                             THEN TRUE
                                             ELSE FALSE
                .
        END.
    END. /* BETALING-GAVEKORT */

    /* Legger pï¿½ kundeinformasjon */
    IF AVAILABLE GaveKort AND
       Gavekort.KundeNr = 0 AND BongHode.KundeNr > 0 THEN
    KUNDE:
    DO:
        FIND Kunde NO-LOCK WHERE
            Kunde.KundeNr = BongHode.KundeNr NO-ERROR.
        IF AVAILABLE Kunde THEN
            ASSIGN
            GaveKort.KundeNr   = Kunde.KundeNr
            GaveKort.KNavn     = Kunde.Navn
            GaveKort.KAdresse1 = Kunde.Adresse1
            Gavekort.KPostNr   = Kunde.PostNr
            GaveKort.KTelefon  = Kunde.Telefon               
            .
    END. /* KUNDE */

    /* Legger pï¿½ medleminformasjon */
    IF AVAILABLE GaveKort AND
       Gavekort.MedlemsNr = 0 AND BongHode.MedlemsNr > 0 THEN
    MEDLEM:
    DO:
        FIND Kunde NO-LOCK WHERE
            Kunde.KundeNr = BongHode.KundeNr NO-ERROR.
        IF AVAILABLE Kunde THEN
            ASSIGN
            GaveKort.MedlemsNr   = Medlem.MedlemsNr
            GaveKort.MFornavn    = Medlem.ForNavn
            GaveKort.MEtternavn  = Medlem.EtterNavn
            Gavekort.MAdresse1   = Medlem.Adresse1
            GaveKort.MPostNr     = Medlem.PostNr
            GaveKort.MTelefon    = Medlem.Telefon
            .
    END. /* MEDLEM */


END. /* SKAPELSEN */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettKampanje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettKampanje Procedure 
PROCEDURE OpprettKampanje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR plKampId         AS DEC NO-UNDO.
DEF VAR piKampTilbArtSeq AS INT NO-UNDO.

DEF BUFFER bufKampanjeTilbArtikkel FOR KampanjeTilbArtikkel.

/*

/* Legger opp transaksjonene i translogg. Kun salgstransaksjoner. */
/* TANSNR og SEQNR pï¿½fï¿½rs bonglinjene her.                     */
BONGLINJE:
FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.Makulert = FALSE AND
    CAN-DO("001,003,010",STRING(BongLinje.TTId,"999")):

    /* Kontoll av kampanjeEier */
    IF (BongLinje.KampEierId > 0 AND
        NOT CAN-FIND(KampanjeEier WHERE
                    KampanjeEier.KampEierId = BongLinje.KampEierId)) THEN
    KAMPANJEEIER:
    DO:
        CREATE KampanjeEier.
        ASSIGN
            KampanjeEier.KampEierId   = BongLinje.KampEierId
            KampanjeEier.KampEierNavn = "Butikk " + STRING(BongLinje.KampEierId)
            .
    END. /* KAMPANJEEIER */

    ASSIGN
        plKampId = BongLinje.KampId /*dec(string(BongLinje.KampEierId) + string(BongLinje.KampId,"9999"))*/
        .

    /* Kontroll av kampanjeMixMatch */
    IF (BongLinje.KampId > 0 AND
        NOT CAN-FIND(KampanjeMixMatch WHERE
                     KampanjeMixMatch.KampId = plKampId)) THEN
    KAMPANJEMIXMATCH:
    DO:
        CREATE KampanjeMixMatch.
        ASSIGN
            KampanjeMixMatch.KampEierId = BongLinje.KampEierId
            KampanjeMixMatch.KampId     = plKampId
            KampanjeMixMatch.KampNavn   = "Lokal kampanje " + STRING(plKampId)
            .
        IF NOT CAN-FIND(Kampanjetilbud WHERE
                        KampanjeTilbud.KampId     = plKampId AND
                        Kampanjetilbud.KampTilbId = BongLinje.KampTilbId) THEN
        DO:
            CREATE KampanjeTilbud.
            ASSIGN
                KampanjeTilbud.KampId     = plKampId
                Kampanjetilbud.KampTilbId = BongLinje.KampTilbId
                .
        END.
    END. /* KAMPANJEMIXMATCH */

    /* Kontroll av kampanjeartikkel */
    IF (plKampId > 0 AND dec(BongLinje.ArtikkelNr) > 0 AND
        NOT CAN-FIND(KampanjeTilbArtikkel WHERE
                     KampanjeTilbArtikkel.KampId        = plKampId AND
                     KampanjeTilbArtikkel.KamptilbId    = BongLinje.KampTilbId AND
                     KampanjeTilbArtikkel.KampTilbArtId = dec(BongLinje.ArtikkelNr))) THEN
    KAMPANJEARTIKKEL:
    DO:
        RUN genkamptilbartseq.p (BongLinje.ButikkNr,"0",OUTPUT piKampTilbArtSeq).

        CREATE KampanjeTilbArtikkel.
        ASSIGN
            KampanjeTilbArtikkel.KampId         = plKampId
            KampanjeTilbArtikkel.KamptilbId     = BongLinje.KampTilbId
            KampanjeTilbArtikkel.KampTilbArtId  = dec(BongLinje.ArtikkelNr)
            KampanjeTilbArtikkel.KampTilbArtSeq = piKampTilbArtSeq
            .
    END. /* KAMPANJEARTIKKEL */


END. /* BONGLINJE */
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-opprettKonto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprettKonto Procedure 
PROCEDURE opprettKonto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER plKonto AS DEC NO-UNDO.

  CREATE konto.
  /* Setter opp index. */
  ASSIGN konto.butikk      = BongLinje.ButikkNr
         konto.kasse       = BongLinje.kassenr
         konto.dato        = BongLinje.dato
         konto.vg          = BongLinje.VareGr
         konto.lopnr       = BongLinje.lopenr
         konto.storl       = BongLinje.Storrelse
         konto.antall      = BongLinje.Antall
         konto.kontonummer = plKonto
         konto.pris        = BongLinje.LinjeSum
         konto.kvitto      = BongLinje.BongNr
         konto.forsnr      = BongHode.KassererNr.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettKontoPost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettKontoPost Procedure 
PROCEDURE OpprettKontoPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Skaper konto post. */
  CREATE konto.

  /* Setter opp index. */
  ASSIGN konto.butikk      = BongLinje.ButikkNr
         konto.kasse       = BongLinje.kassenr
         konto.dato        = BongLinje.dato
         konto.kontonummer = BongLinje.MButikkNr.

  /* Legger over informasjonen. */
  ASSIGN konto.Vg          = BongLinje.VareGr
         konto.lopnr       = BongLinje.lopenr
         konto.storl       = BongLinje.Storrelse
         konto.pris        = BongLinje.LinjeSum
         konto.antal       = BongLinje.Antall
         konto.kvitto      = BongLinje.BongNr
         konto.ForsNr      = BongHode.KassererNr
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettKundeMedlem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettKundeMedlem Procedure 
PROCEDURE OpprettKundeMedlem :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER dKundeNr AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER cKort    AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE bOk    AS LOG       NO-UNDO.
  DEFINE VARIABLE cMsgs  AS CHARACTER NO-UNDO.
  
  /* Sendes det inn et kortnr, skal det benyttes. Hvis ikke benyttes det som stï¿½r i bonghode. */
  IF cKort = '' THEN
      cKort = BongHode.KundeKort.
  
  OPPRETT_KUNDE_OG_MEDLEM: 
  DO:
    FIND Kunde EXCLUSIVE-LOCK WHERE
        Kunde.KundeNr = dKundeNr NO-ERROR.
    IF AVAILABLE Kunde THEN 
        FIND KundeGruppe OF Kunde NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Kunde THEN 
    DO:
      FIND FIRST KundeGruppe NO-LOCK WHERE
         KundeGruppe.GruppeId > 0 NO-ERROR.
      CREATE Kunde.
      ASSIGN
        Kunde.Navn            = "Automatisk opprettet " + STRING(TODAY)
        Kunde.BetType         = 1 /* Kontantkunde - skal stoppes i kassen. */
        Kunde.ButikkNr        = BongHode.ButikkNr /* Kobler kunden til butikken. */
        Kunde.SamleFaktura    = FALSE  
        Kunde.Fakturagebyr    = FALSE 
        Kunde.Purregebyr      = TRUE 
        Kunde.EksterntKundeNr = BongHode.KundeKort
        Kunde.GruppeId        = IF AVAILABLE KundeGruppe THEN KundeGruppe.GruppeId ELSE 1
        .
      /* Tar bort kundekort som opprettes i trigger da vi har fï¿½tt inn kundekort fra kassen. */
      IF cKort <> '' THEN
        FOR EACH KundeKort OF Kunde:
          DELETE KundeKort.
        END.
    END.
    /* Oppdaterer bonginfo. */
    ASSIGN 
      BongHode.KundeNr      = Kunde.KundeNr
      BongHode.KortType     = 2 /* Kundekort. */
      .
      
    /* Bare medlem skal opprettes. */
    RUN genkundeMedlem.p (BongHode.ButikkNr,
                          (IF AVAILABLE KundeGruppe THEN KundeGruppe.GruppeId ELSE 1),
                          INPUT-OUTPUT Kunde.KundeNr,
                          OUTPUT BongHode.MedlemsNr,
                          OUTPUT bOk,
                          OUTPUT cMsgs).
    /* Kundekortene legges opp pï¿½ samme kunde, men unike medlemskort */
    /* legges pï¿½ separate medlemmer.                              */
    RUN genkundekort_og_medlem.p (BongHode.ButikkNr,
                                  Kunde.KundeNr,
                                  BongHode.MedlemsNr,
                                  INT(cKort),
                                  INT(cKort),
                                  999,
                                  OUTPUT bOk,
                                  OUTPUT cMsgs).
  END. /* OPPRETT_KUNDE_OG_MEDLEM */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettReklamasjon) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettReklamasjon Procedure 
PROCEDURE OpprettReklamasjon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER plReklamasjonsNr AS DEC NO-UNDO. 

  DEF BUFFER bufReklamasjonslogg FOR Reklamasjonslogg.
      
  DO FOR bufReklamasjonslogg TRANSACTION:
      FIND LevBas OF ArtBas NO-LOCK NO-ERROR.

      FIND LAST Reklamasjonslogg NO-ERROR.

      CREATE bufReklamasjonslogg.
      ASSIGN
          bufReklamasjonslogg.ReklamasjonsNr = IF AVAILABLE Reklamasjonslogg
                                                 THEN Reklamasjonslogg.ReklamasjonsNr + 1
                                                 ELSE 1
          .
      ASSIGN
        plReklamasjonsNr                       = bufReklamasjonsLogg.ReklamasjonsNr
        bufReklamasjonslogg.LevNr              = ArtBas.LevNr
        bufReklamasjonslogg.KundeNr            = BongHode.KundeNr
        bufReklamasjonslogg.BetalesAv          = BongLinje.NotatKode
        bufReklamasjonslogg.AkseptertKunde     = 2 /* Akseptert i kasse */
        bufReklamasjonslogg.ReklamStatus       = IF bufReklamasjonslogg.AkseptertKunde = 1
                                                     THEN 2
                                                   ELSE IF bufReklamasjonslogg.AkseptertKunde = 2
                                                     THEN 4
                                                   ELSE 5
        bufReklamasjonslogg.AkseptertDato      = TODAY
        bufReklamasjonslogg.AkseptertAv        = USERID("SkoTex")
        bufReklamasjonslogg.AkseptertBesluttet = TRUE

        bufReklamasjonslogg.OppdLager           = TRUE
        bufReklamasjonslogg.OppdLagerDato       = TODAY
        bufReklamasjonslogg.OppdLagerAv         = USERID('SkoTex')
        bufReklamasjonslogg.Frist-Dato          = TODAY + 14
        .
      IF bufReklamasjonslogg.KundeNr <> 0 THEN
      DO:
          FIND Kunde NO-LOCK WHERE
              Kunde.KundeNr = bufReklamasjonslogg.KundeNr NO-ERROR.
          IF AVAILABLE Kunde THEN
              ASSIGN
              bufReklamasjonslogg.KundeNavn    = Kunde.Navn
              bufReklamasjonslogg.KundeAdresse = Kunde.Adresse1
              bufReklamasjonslogg.PostNr       = Kunde.PostNr
              bufReklamasjonslogg.KundeTelefon = Kunde.Telefon 
              bufReklamasjonslogg.KundeMobil   = Kunde.MobilTlf 
              bufReklamasjonslogg.KundeE-Mail  = ePostAdresse
              .

      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettTilgode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettTilgode Procedure 
PROCEDURE OpprettTilgode :
/*------------------------------------------------------------------------------
  Purpose:     Posterer utlevering og bruk av tilgodelapper. 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR plWork    AS DEC  NO-UNDO.
DEF VAR pcTekst   AS CHAR NO-UNDO.
DEF VAR piTransNr AS INT  NO-UNDO.
DEF VAR pcTTId     AS CHAR NO-UNDO.
DEF VAR piTTId    AS INT  NO-UNDO.
DEF VAR piTBId    AS INT  NO-UNDO.
DEFINE VARIABLE pdecLoop AS DECIMAL NO-UNDO.

DEF VAR pcIdentNr    AS CHAR NO-UNDO.                      
DEF VAR pdGyldigDato AS DATE NO-UNDO.

DEF VAR piLoop AS INT NO-UNDO.
DEF VAR lExtern AS LOG NO-UNDO.
DEFINE VARIABLE iTilgodeButnr AS INTEGER     NO-UNDO.
/* Posterer Tilgode. Utlevering av og bruk av Tilgode. */
SKAPELSEN:
FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.Makulert = FALSE AND
    CAN-DO("066,069",STRING(BongLinje.TTId,"999")):

    ASSIGN
        lDec    = 0
        piTTId  = BongLinje.TTId
        piTBId  = BongLinje.TBId
        pcTTId  = STRING(BongLinje.TTId,"999")
        .
    /* Utlevering av Tilgode. */
    IF BongLinje.TTId = 69 THEN
    UTLEVER-TILGODE:
    DO:
        ASSIGN
            pcIdentNr    = TRIM(BongLinje.Strekkode)
            pdGyldigDato = BongLinje.TransDato + iTilgodeGyldig
            .
        /* Er det ikke angitt en ident, returnerer kassen '0'. Det erstatter vi med dato og bongnr. */
        ASSIGN lDec = DEC(pcIdentNr) NO-ERROR.
        IF lDec = 0 THEN 
            pcIdentNr = STRING(BongLinje.TransDato) + '-' + STRING(BongLinje.KasseNr) + '-' + STRING(BongLinje.BongNr) + '-' + STRING(BongLinje.LinjeNr).
            
        RUN bibl_logg.p ('xOverforBong', 'Opprett tilgode 69: IndentNr: ' + STRING(pcIdentNr) + 
            ' Dato: ' + STRING(pdGyldigDato) + ' BongNr: ' + STRING(BongHode.BongNr)).        
                        
        /* Her skal vi ikke finne noe kort fra fï¿½r. */
        OPPRETT:
        DO ON ERROR UNDO, RETRY:
            IF LENGTH(pcIdentNr) > 6 
            THEN FIND FIRST Tilgode NO-LOCK WHERE
                Tilgode.IdentNr = pcIdentNr NO-ERROR.
            ELSE FIND FIRST Tilgode NO-LOCK WHERE
                Tilgode.ButNr   = BongLinje.ButikkNr AND
                Tilgode.IdentNr = pcIdentNr NO-ERROR.
            /* Finner vi alikevel et kort, opprettes et nytt id.           */
            /* Er bongnummer satt, er det tilgodelappen lest inn som brukt */
            /* fï¿½r bongen hvor det er utstedt.                             */
            pdecLoop = 0.
            IF AVAILABLE Tilgode AND Tilgode.BongNr > 0 THEN
            ERRORLOOP:
            DO WHILE TRUE:
                pdecLoop = pdecLoop + 1.
                FIND bufTilgode NO-LOCK WHERE
                    bufTilgode.ButNr    = BongLinje.ButikkNr AND
                    bufTilgode.IdentNr  = pcIdentNr + "-DUBL" + string(pdecLoop) NO-ERROR.
                IF AVAILABLE bufTilgode THEN
                DO:
                    RUN bibl_logg.p ('xOverforBong', 'Opprett tilgode 69 LOOP: IndentNr: ' + pcIdentNr + "-DUBL" + string(pdecLoop)).        
                    NEXT ERRORLOOP.
                END.
                RUN bibl_logg.p ('xOverforBong', 'Opprett tilgode 69 LOOP OK ID: IndentNr: ' + pcIdentNr + "-DUBL" + string(pdecLoop)).        
                /* Nytt Id */
                pcIdentNr = pcIdentNr + "-DUBL" + STRING(pdecLoop).
                LEAVE ERRORLOOP.
            END. /* ERRORLOOP */        
            IF NOT AVAILABLE Tilgode THEN
            DO:
                CREATE Tilgode.
                ASSIGN
                    Tilgode.ButNr      = BongLinje.ButikkNr
                    Tilgode.IdentNr    = pcIdentNr
                    NO-ERROR.
                /* Oppstï¿½r det feil, tar vi en sving til. */
                IF ERROR-STATUS:ERROR THEN
                DO:
                    RUN bibl_logg.p ('xOverforBong', 'Opprett tilgode 69 CREATE ERROR: IndentNr: ' + pcIdentNr + ' Butikk:' + STRING(BongLinje.ButikkNr)).        
                    pcIdentNr = STRING(BongLinje.TransDato) + '-' + STRING(BongLinje.KasseNr) + '-' + STRING(BongLinje.BongNr) + '-' + STRING(BongLinje.LinjeNr) + '_' + STRING(TIME).
                    UNDO OPPRETT, RETRY OPPRETT.
                END.
            END.
            ELSE IF AVAILABLE Tilgode THEN 
                FIND CURRENT Tilgode EXCLUSIVE-LOCK.
        END. /* OPPRETT */
        IF AVAILABLE Tilgode THEN
        DO:
            FIND ButikkForsalj NO-LOCK WHERE
                ButikkForsalj.Butik  = BongLinje.ButikkNr AND
                ButikkForsalj.ForsNr = int(BongHode.KassererNr) NO-ERROR.
            ASSIGN
            Tilgode.Dato       = BongLinje.TransDato
            Tilgode.Tid        = BongLinje.TransTid
            Tilgode.KasseNr    = BongLinje.KasseNr
            Tilgode.SelgerNr   = BongHode.SelgerNr
            Tilgode.KassNr     = (IF AVAILABLE ButikkForsalj
                                     THEN ButikkForsalj.KassererId
                                     ELSE 0)
            Tilgode.BongNr     = BongLinje.BongNr
            Tilgode.GyldigDato = pdGyldigDato
            Tilgode.Belop      = ABSOLUTE(BongLinje.LinjeSum)
            Tilgode.FraB_Id    = BongLinje.B_Id
            Tilgode.Eget       = TRUE 
            .
        END.
    END. /* UTLEVER-TILGODE */

    /* Bruk av Tilgode. */
    IF BongLinje.TTId = 66 THEN
    BRUK-TILGODE:
    DO:
        ASSIGN
            pcIdentNr    = BongLinje.Strekkode
            lExtern      = BongLinje.BongTekst = "EXTERN".
        RELEASE Tilgode.
        IF NOT lExtern THEN DO:
            /* Er det ikke angitt en ident, returnerer kassen '0'. Det erstatter vi med dato og bongnr. */
            ASSIGN lDec = DEC(pcIdentNr) NO-ERROR.
            IF lDec = 0 THEN 
                pcIdentNr = STRING(BongLinje.TransDato) + '-' + STRING(BongLinje.KasseNr) + '-' + STRING(BongLinje.BongNr) + '-' + STRING(BongLinje.LinjeNr).

            RUN bibl_logg.p ('xOverforBong', 'Bruk tilgode 66: IndentNr: ' + STRING(pcIdentNr) + 
                 ' Dato: ' + STRING(pdGyldigDato) + ' BongNr: ' + STRING(BongHode.BongNr)).

            /* Sjekker om vi finne et kort fra fï¿½r. */
            IF LENGTH(pcIdentNr) > 6 
            THEN FIND FIRST Tilgode NO-LOCK WHERE
                Tilgode.IdentNr = pcIdentNr NO-ERROR.
            ELSE FIND Tilgode NO-LOCK WHERE
                Tilgode.ButNr   = (IF BongLinje.MButikk > 0
                                   THEN BongLinje.MButikk
                                   ELSE BongLinje.ButikkNr) AND
                Tilgode.IdentNr = pcIdentNr NO-ERROR.
        END.
        OPPRETT:
        DO ON ERROR UNDO, RETRY:
            /* Finner vi kortet og det er brukt fra fï¿½r, mï¿½ nytt kort opprettes. */
            IF AVAILABLE Tilgode AND Tilgode.BruktDato <> ? THEN
            ERRORLOOP:
            DO WHILE TRUE:
                piLoop = piLoop + 1.
                FIND bufTilgode NO-LOCK WHERE
                    bufTilgode.ButNr    = (IF BongLinje.MButikk > 0
                                         THEN BongLinje.MButikk
                                         ELSE BongLinje.ButikkNr) AND
                    bufTilgode.IdentNr  = pcIdentNr + "-DUBL" + string(piLoop) NO-ERROR.
                IF AVAILABLE bufTilgode THEN
                DO:
                    RUN bibl_logg.p ('xOverforBong', 'Opprett tilgode 66 LOOP: IndentNr: ' + pcIdentNr + "-DUBL" + string(piLoop)).        
                    pcIdentNr = STRING(BongLinje.TransDato) + '-' + STRING(BongLinje.KasseNr) + '-' + STRING(BongLinje.BongNr) + '-' + STRING(BongLinje.LinjeNr) + '_' + STRING(TIME).
                    NEXT ERRORLOOP.
                END.
                /* Nytt Id */
                pcIdentNr = pcIdentNr + "-DUBL" + STRING(piLoop).
                LEAVE ERRORLOOP.
            END. /* ERRORLOOP */
            IF NOT AVAILABLE Tilgode THEN
            DO:
/*                 iTilgodeButnr = IF lExtern THEN INT(ENTRY(1,pcIdentNr,"-")) ELSE */
                iTilgodeButnr = IF lExtern THEN INT(Bonglinje.Antall) ELSE
                                      (IF BongLinje.MButikk > 0
                                           THEN BongLinje.MButikk
                                           ELSE BongLinje.ButikkNr).
                CREATE Tilgode.
                ASSIGN
                    Tilgode.ButNr = iTilgodeButnr
/*                     Tilgode.ButNr      = (IF BongLinje.MButikk > 0  */
/*                                            THEN BongLinje.MButikk   */
/*                                            ELSE BongLinje.ButikkNr) */
                    Tilgode.IdentNr    = pcIdentNr
                    Tilgode.IdentType  = BongLinje.Antall
                    Tilgode.Dato       = BongLinje.TransDato
                    NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                DO:
                    RUN bibl_logg.p ('xOverforBong', 'Opprett tilgode 66 CREATE ERROR: IndentNr: ' + pcIdentNr + ' Butikk:' + STRING(BongLinje.ButikkNr)).        
                    UNDO OPPRETT, RETRY OPPRETT.
                END.
            END.
            ELSE IF AVAILABLE Tilgode THEN 
              FIND CURRENT Tilgode EXCLUSIVE-LOCK.
        END. /* OPPRETT */

        FIND ButikkForsalj NO-LOCK WHERE
             ButikkForsalj.Butik  = BongLinje.ButikkNr AND
             ButikkForsalj.ForsNr = int(BongHode.KassererNr) NO-ERROR.
        
        /* Legger pï¿½ brukt informasjonen. */
        IF AVAILABLE Tilgode THEN 
        ASSIGN
            Tilgode.BruktB_Id       = BongLinje.B_Id
            Tilgode.BruktButNr      = BongLinje.ButikkNr
            Tilgode.BruktDato       = BongLinje.TransDato
            Tilgode.BruktTid        = BongLinje.TransTid
            Tilgode.BruktKasseNr    = BongLinje.KasseNr
            Tilgode.BruktSelgerNr   = BongHode.SelgerNr
            Tilgode.BruktKassNr     = (IF AVAILABLE ButikkForsalj
                                     THEN ButikkForsalj.KassererId
                                     ELSE 0)
            Tilgode.BruktBongNr     = BongLinje.BongNr
            Tilgode.Belop      = IF Tilgode.Belop = 0
                                     THEN BongLinje.LinjeSum
                                     ELSE Tilgode.Belop
            Tilgode.Utgatt          = TRUE
            Tilgode.UtgattDato      = BongLinje.TransDato
            Tilgode.UtgattTid       = BongLinje.TransTid
            Tilgode.GyldigDato      = IF Tilgode.GyldigDato = ?
                                         THEN Tilgode.UtgattDato
                                         ELSE Tilgode.GyldigDato
            Tilgode.UtgattRegAv     = USERID('SkoTex')
            Tilgode.Eget            = IF BongLinje.MButikkNr = BongLinje.butikkNr
                                         THEN TRUE
                                         ELSE FALSE
            .

    END. /* BETALING-Tilgode */

    /* Legger pï¿½ kundeinformasjon */
/*     IF AVAILABLE Tilgode AND                                 */
/*        Tilgode.KundeNr = 0 AND BongHode.KundeNr > 0 THEN     */
/*     KUNDE:                                                   */
/*     DO:                                                      */
/*         FIND Kunde NO-LOCK WHERE                             */
/*             Kunde.KundeNr = BongHode.KundeNr NO-ERROR.       */
/*         IF AVAILABLE Kunde THEN                              */
/*             ASSIGN                                           */
/*             Tilgode.KundeNr   = Kunde.KundeNr                */
/*             Tilgode.KNavn     = Kunde.Navn                   */
/*             Tilgode.KAdresse1 = Kunde.Adresse1               */
/*             Tilgode.KPostNr   = Kunde.PostNr                 */
/*             Tilgode.KTelefon  = Kunde.Telefon                */
/*             .                                                */
/*     END. /* KUNDE */                                         */
/*                                                              */
/*     /* Legger pï¿½ medleminformasjon */                        */
/*     IF AVAILABLE Tilgode AND                                 */
/*        Tilgode.MedlemsNr = 0 AND BongHode.MedlemsNr > 0 THEN */
/*     MEDLEM:                                                  */
/*     DO:                                                      */
/*         FIND Kunde NO-LOCK WHERE                             */
/*             Kunde.KundeNr = BongHode.KundeNr NO-ERROR.       */
/*         IF AVAILABLE Kunde THEN                              */
/*             ASSIGN                                           */
/*             Tilgode.MedlemsNr   = Medlem.MedlemsNr           */
/*             Tilgode.MFornavn    = Medlem.ForNavn             */
/*             Tilgode.MEtternavn  = Medlem.EtterNavn           */
/*             Tilgode.MAdresse1   = Medlem.Adresse1            */
/*             Tilgode.MPostNr     = Medlem.PostNr              */
/*             Tilgode.MTelefon    = Medlem.Telefon             */
/*             .                                                */
/*     END. /* MEDLEM */                                        */


END. /* SKAPELSEN */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettVaretelling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettVaretelling Procedure 
PROCEDURE OpprettVaretelling :
/*------------------------------------------------------------------------------
                        Purpose: Oppretter lokasjonsliste med varetransaksjoner av transtype 9                                                                                                                                            
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEFINE VARIABLE iTelleNr AS INTEGER NO-UNDO.
DEFINE VARIABLE ocReturn AS CHAR NO-UNDO.
DEFINE VARIABLE obOK     AS LOG NO-UNDO.

FIND LAST TelleHode NO-LOCK USE-INDEX TelleHode NO-ERROR.
IF AVAILABLE TelleHode 
    THEN iTelleNr = TelleHode.TelleNr + 1.
    ELSE iTelleNr = 1. 

IF AVAILABLE TelleHode THEN 
    RELEASE TelleHode.

BONGLINJE:
FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.Makulert = FALSE AND
    CAN-DO("009",STRING(BongLinje.TTId,"999")):

    FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = DEC(BongLinje.ArtikkelNr) NO-ERROR.
      
    IF NOT AVAILABLE ArtBas OR
      DEC(BongLinje.ArtikkelNr) = 0 THEN 
      NEXT BONGLINJE.  
    FIND Farg OF ArtBas NO-LOCK NO-ERROR.

    IF NOT AVAILABLE TelleHode THEN 
    DO:
      CREATE TelleHode.
      ASSIGN
        TelleHode.TelleNr     = iTelleNr
        TelleHode.Beskrivelse = "Telling fra kasse " + STRING(BongLinje.KasseNr) + ' ' + STRING(BongLinje.TransDato) + ' BongNr. ' + STRING(BongLinje.BongNr)
        TelleHode.ButikkListe = STRING(BongLinje.ButikkNr)
        TelleHode.TTId        = BongLinje.TTId
        TelleHode.TBId        = BongLinje.TBId
        TelleHode.StartDato   = BongLinje.Dato
        TelleHode.TelleType   = 2 /* Lokasjonsliste */        
        .
    END.
    
    FIND TelleLinje EXCLUSIVE-LOCK WHERE
        TelleLinje.TelleNr    = TelleHode.TelleNr AND
        TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr AND
        TelleLinje.Butik      = BongLinje.ButikkNr AND
        TelleLinje.Storl      = BongLinje.Storrelse NO-ERROR.
    IF NOT AVAILABLE TelleLinje THEN 
    DO:    
    
      CREATE TelleLinje.
      ASSIGN
        TelleLinje.TelleNr    = TelleHode.TelleNr 
        TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr
        Tellelinje.Beskr      = ArtBas.Beskr
        TelleLinje.butik      = BongLinje.ButikkNr
        TelleLinje.Vg         = ArtBas.Vg
        TelleLinje.LopNr      = ArtBas.LopNr
        TelleLinje.Storl      = BongLinje.Storrelse
        TelleLinje.Kode       = BongLinje.Strekkode
        TelleLinje.VgLopNr    = STRING(ArtBas.Vg,">>>>>9") + "/" + 
                                TRIM(STRING(ArtBas.LopNr,">>>>>9"))
        TelleLinje.LevKod     = ArtBas.LevKod
        TelleLinje.LevFargKod = IF ArtBas.LevFargKod <> ""
                                  THEN ArtBas.LevFargKod
                                ELSE IF AVAILABLE Farg 
                                  THEN Farg.FarBeskr
                                ELSE ""
        TelleLinje.LevNr      = ArtBas.LevNr
        TelleLinje.Sasong     = ArtBas.SaSong
        TelleLinje.Farg       = ArtBas.Farg
        TelleLinje.MatKod     = ArtBas.MatKod
        .
    END.
    FIND ArtLag NO-LOCK WHERE
        ArtLag.Artikkelnr = TelleLinje.artikkelnr AND
        ArtLag.Butik      = TelleLinje.Butik AND
        ArtLag.Storl      = BongLinje.Storrelse NO-ERROR.

    /* ï¿½vrig informasjon. */ 
    ASSIGN
        TelleLinje.AntallPar  = IF AVAILABLE ArtLag 
                                   THEN Artlag.Lagant
                                ELSE 0
        TelleLinje.AntallTalt    = TelleLinje.AntallTalt + BongLinje.Antall
        TelleLinje.OpprAntalTalt = TelleLinje.AntallTalt
        TelleLinje.VVareKost     = BongLinje.VVAreKost
        TelleLinje.NedSkrevet    = BongLinje.VVAreKost
        TelleLinje.OpprVerdi     = TelleLinje.AntallPar  * BongLinje.VVAreKost
        TelleLinje.OpptVerdi     = TelleLinje.AntallTalt * BongLinje.VVAreKost
        TelleLinje.AntallDiff    = TelleLinje.AntallPar  - TelleLinje.AntallTalt
        TelleLinje.OpptVerdi     = TelleLinje.AntallTalt * TelleLinje.VVareKost
        TelleLinje.OpprVerdi     = TelleLinje.AntallPar  * TelleLinje.VVareKost
        TelleLinje.RabKr         = 0
        TelleLinje.VerdiDiff     = TelleLinje.AntallDiff * TelleLinje.VVareKost.
        .    
END. /* BONGLINJE */
IF AVAILABLE TelleHode THEN 
  DO:  
    RUN tellehode_oppdatersum.p (STRING(ROWID(TelleHode)),?,cTekst,OUTPUT ocReturn,OUTPUT obOk).
    RELEASE TelleHode.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OverforDatasett) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforDatasett Procedure 
PROCEDURE OverforDatasett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr      AS INT   NO-UNDO.
  DEF VAR piBongNr       AS INT   NO-UNDO.
  DEF VAR piOldBongNr    AS INT   NO-UNDO.
  DEF VAR pbDobbel       AS LOG   NO-UNDO.
  DEF VAR piBongLinje    AS INT   NO-UNDO.
  DEF VAR piLoop1        AS INT   NO-UNDO.
  DEF VAR prBongRowId    AS ROWID NO-UNDO.
  DEF VAR pcError        AS CHAR  NO-UNDO.
  DEF VAR pcFilError     AS CHAR  NO-UNDO.
  DEF VAR plLinjeSum     AS DEC   NO-UNDO.
  DEF VAR piBokforingsNr AS INT   NO-UNDO.
  DEF VAR ocReturn      AS CHAR NO-UNDO.
  DEF VAR obOk          AS LOG  NO-UNDO.


  /* Tï¿½mmer temp-file */
  FOR EACH ttKundeBut:
      DELETE ttKundeBut.
  END.
  FOR EACH ttMedlemBut:
      DELETE ttMedlemBut.
  END.

  ASSIGN
      iAntBonger  = 0
      iStart      = TIME
      pbDobbel    = FALSE
      prBongRowId = ?
      .

  /* Markerer Datasettet som under overfï¿½ring/delhvis overfï¿½rt */
  DO TRANSACTION:
      FIND CURRENT DataSett EXCLUSIVE-LOCK.
      ASSIGN
          DataSett.Behandlet = 4
          .
      /* Setter Z-Nummer for kassarapporten. Det blir */
      /* En kassarapport pr. datasett.                */
      RUN SettZNummer.
  END.
  FIND CURRENT DataSett NO-LOCK.

  /* Leser alle bongene pï¿½ datasettet som ikke er overfï¿½rt fra fï¿½r. */
  BONGHODER-TRANSACTION:
  FOR EACH BongHode OF DataSett EXCLUSIVE-LOCK WHERE
      BongHode.BongStatus >= 5 AND
      BongHode.BongStatus <= 6:
      /* AND
      BongHode.Gradering  <  3 */ 

      /* Oppretter bokfï¿½ringsbilag og setter nummer.                    */
      /* Det skal finnes et bilag for butikk og dato. Gjï¿½r det ikke det */
      /* plukkes et ledig nummer fra nummerserien.                      */
      /* Dvs at nummerne tildeles ut fra nuimmerserie, men kan komme i  */
      /* uorden i forhold til dato.                                     */
      IF NOT CAN-FIND(FIRST Bokforingsbilag WHERE
                      Bokforingsbilag.ButikkNr = BongHode.ButikkNr AND
                      Bokforingsbilag.OmsetningsDato = BongHode.Dato) THEN
      DO:
        FIND LAST bBokforingsbilag NO-LOCK WHERE
            bBokforingsbilag.ButikkNr = BongHode.ButikkNr AND
            bBokforingsbilag.Aar      = YEAR(BongHode.Dato) 
            USE-INDEX BokforingsBilag NO-ERROR.
        IF AVAILABLE bBokforingsbilag THEN
            piBokforingsNr = bBokforingsbilag.BokforingsNr + 1.
        ELSE
            piBokforingsNr = 1.

        CREATE Bokforingsbilag.
        ASSIGN
            Bokforingsbilag.ButikkNr           = BongHode.ButikkNr
            Bokforingsbilag.Aar                = YEAR(BongHode.Dato)
            Bokforingsbilag.OmsetningsDato     = BongHode.Dato
            BokforingsBilag.BokforingsNr       = piBokforingsNr
            .
      END.

      /* Flagger den som under overfï¿½ring. */
      ASSIGN
          BongHode.BongStatus = 6 /* Delhvis overfï¿½rt. */
          iAntBonger          = iAntBonger + 1
          .

      /* Formidler til bruker hva som skjer. */
      IF iAntBonger MODULO 5 = 0 THEN
      DO:
        RUN Telleverk IN h_Telleverk ("Datasett: " + 
                                   STRING(DataSett.DataSettId) + 
                                   " Overfï¿½rt: " +
                                   STRING(iAntBonger) +
                                   " av " +
                                   string(iTotAntBonger) + " bonger. Vent litt...") NO-ERROR.
      END.

      /* Etikettsjekk */
      RUN Etikettsjekk.
      
      /* Posterer kassarapporten */
      RUN Kassarapport.

      /* Oppdaterer Dagsrapport (Dags_rap) som er grunnlag for hovedgrupperapporten */
      RUN Hovedgrupperapport.

      /* Oppdaterer Aktivitetersrapport. */
      RUN Aktivitetsrapport.

      /* Spesifiserer returer pï¿½ kassarapporten. */
      RUN SpesAvReturer.

      /* Postering av reklamasjonsrabatter.     */
      /* Alle poster hvor rabattkode er angitt. */
      /* Reklamasjoner posteres uansett.        */
      RUN Reklamasjonslogg.

      /* Varemottak i kassen som legges opp som pakkseddel. */
      RUN Varemottak.
      
      /* Kundeordre registrert pï¿½ kasse */
      RUN Kundeordre.
      
      /* Poster kreditsalg som faktura.                                                             */
      /* plFaktura_Id settes her. Det benyttes til ï¿½ koble faktura og pakkseddel i Overforingslogg. */
      IF (BongHode.KundeNr <> 0 OR BongHode.KundeKort <> "" OR CAN-FIND(FIRST BongLinje WHERE
                    BongLinje.B_Id = BongHode.B_id AND
                    CAN-DO("006,065,087",STRING(BongLinje.TTId,"999")))) THEN
          RUN posterFaktura.

      /* Postering av overfï¿½ringer i overfï¿½ringsordre */
      RUN Overforingslogg.

      /* Oppdaterer TransLogg.                                            */
      /* NB: Denne mï¿½ kjï¿½res fï¿½r logging av medlems og kundesalg kanskje. */
      /*     TransNr og SeqNr pï¿½fï¿½res bonglinjene her.                    */
      /* NB: Her flagges ogsï¿½ om bongen er ferdig motpostert.             */
      RUN TransLogg.

      /* Oppretter varetelling fra kassene */
      RUN OpprettVaretelling.
      
      /* Oppretter kampanjeregistre hvis det kommer kampanjer fra butikkene. */
      RUN OpprettKampanje.
      
      /* Sjekker at kunde og medlemmer finnes. Bonger som kommer inn fra InfoPOS 8.0 har alltid */
      /* kunde og medlem opprettet. Men bonger som kommer inn fra PRS POS og andre kasser, har  */
      /* ikke alltid det. Derfor denne dobbelkontrollen her.                                    */
      IF BongHode.KundeNr > 0 /*AND NOT CAN-FIND(Kunde WHERE Kunde.KundeNr = BongHode.KundeNr) */ THEN                                     
      RUN SjekkKundeMedlem.

      /* Poster medlemskjï¿½p - Grunnlag for bonusberegning. */
      IF BongHode.MedlemsNr <> 0 OR BongHode.Medlemskort <> "" THEN
        RUN posterMedlemsKjop.

      /* Logge medlemssalg. */
      IF BongHode.MedlemsNr <> 0 OR BongHode.Medlemskort <> "" THEN
        RUN Medlemssalg. 

      /* Logge kundesalg. */
      IF BongHode.KundeNr <> 0 OR BongHode.KundeKort <> "" THEN
        RUN KundeSalg.

      /* Logge aKontoinnbetaling. */
      IF BongHode.KundeNr <> 0 OR BongHode.KundeKort <> "" THEN
          RUN posterKundereskontro.

      /* Posterer gavekort. */
      RUN OpprettGavekort.

      /* Posterer tilgode. */
      RUN OpprettTilgode.

      /* Logger prisavvik */
      IF bLoggPrisAvvik THEN
        RUN PrisAvviksLogg.p (BongHode.ButikkNr,BongHode.b_id,IF AVAIL clButiker THEN clButiker.ProfilNr ELSE ?).

      /* Skriver finans og bokfï¿½ringsbilag */
      RUN EODLogg /*SkrivFinans*/.

      /* Skriver ut reservasjonsbilag.                                 */
      /* Dette skal skrives ut nï¿½r nettbutikk overfï¿½rer varer til seg. */
      RUN reservasjonsBilag.

      /* Logger bonger i Nets. */
      RUN loggNets.
      
      /* Logger bonger fra Kuponginnlï¿½sen */
      RUN loggKuponginnlosen.
      
      /* Logger bonger til MayFlower */
      RUN loggMayFlower.
      
      /* Flagger den ferdig overfï¿½rt. */
      ASSIGN
          BongHode.BongStatus = 7 
          .
  
      /* Logger data for eksport til finans. */
      RUN loggFinansEksport (BongHode.B_Id).
      
      /* Logger translogg for eksport til HK. */
      RUN loggTransloggEksport (BongHode.B_Id).
  END. /* BONGHODER-TRANSACTION */

  /* Markerer Datasettet som overfï¿½rt */
  DO TRANSACTION:
/*       IF NOT CAN-FIND(FIRST BongHode OF DataSett WHERE */
/*                       BongHode.BongStatus < 7) THEN    */
      DO:
          FIND CURRENT DataSett EXCLUSIVE-LOCK.
          ASSIGN
              DataSett.Behandlet = 5 /* Overfï¿½rt. */
              .
      END.
  END.
  FIND CURRENT DataSett NO-LOCK.

  /* Ajourfï¿½rer kundesaldi */
  FOR EACH ttKundeBut:
      ASSIGN
          ocReturn = ""
          obOk     = FALSE
          .
     /* Oppdaterer kundesaldo. */
/*       DYNAMIC-FUNCTION("runproc","beregn_kunde_saldo.p","idlist|" + STRING(ttKundeBut.KundeNr),?). */
      RUN beregn_kunde_saldo.p ("idlist|" + STRING(ttKundeBut.KundeNr),
                              ?,
                              "",
                              OUTPUT ocReturn,
                              OUTPUT obOk).

      /* Gammel saldoberegning - skal tas bort nï¿½r alle kunder har fï¿½tt ny fakturarutine. */
      RUN beregnkundesaldo.p (ttKundeBut.KundeNr, ttKundeBut.ButikkNr).
  END.

  /* Ajourfï¿½rer MedlemsSaldi */
  FOR EACH ttMedlemBut:
      RUN beregnmedlemsaldo.p (ttMedlemBut.MedlemsNr, ttMedlemBut.ButikkNr).
  END.

  RUN Telleverk IN h_Telleverk (" ") NO-ERROR.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Overforingslogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Overforingslogg Procedure 
PROCEDURE Overforingslogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Det er fï¿½rst opprettet en faktura. plFaktura_Id peker pï¿½ den faktura 
               som er utstedt som fï¿½lge av overfï¿½ringen. 
------------------------------------------------------------------------------*/
  DEF VAR pcTTId           AS CHAR NO-UNDO.
  DEF VAR piLinjeNr        AS INT  NO-UNDO.
  DEFINE VARIABLE iMButikkNr AS INTEGER NO-UNDO.
  DEF VAR plMinusButikk   AS LOG NO-UNDO.
  DEF VAR iBuntNr         AS INTEGER NO-UNDO.
  DEFINE VARIABLE vVareKost AS DECIMAL NO-UNDO.
  DEFINE VARIABLE cPkSdlNr      AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE lReklamasjonsNr AS DEC NO-UNDO.
  DEFINE VARIABLE iOvBuntFinnes   AS INTEGER NO-UNDO.

  DEFINE BUFFER ovButiker    FOR Butiker.
  DEFINE BUFFER bufBatchLogg FOR BatchLogg.
  DEFINE BUFFER bufOvBunt    FOR OvBunt.
  
  FIND buf2Butiker NO-LOCK WHERE 
      buf2Butiker.Butik = BongHode.ButikkNr NO-ERROR.
  
  /* Tï¿½mmer */
  FOR EACH TT_OvBuffer:
      DELETE TT_OvBuffer. /* ev metod empty-temp-table */
  END.
  ASSIGN
      lPkSdlId  = 0
      piLinjeNr = 1.
  /* Logger overfï¿½ringer fra kassen. */
  TRANSRAD:
  FOR EACH BongLinje NO-LOCK WHERE
      BongLinje.B_Id = BongHode.B_Id AND
      BongLinje.Makulert = FALSE AND
      CAN-DO("006",STRING(BongLinje.TTId,"999")):

      ASSIGN
          pcTTId = STRING(BongLinje.TTId,"999")
          .

      /* Ukjente artikler kan ikke behandles */
      IF TRIM(BongLinje.ArtikkelNr) = "" THEN
          NEXT TRANSRAD.
  
      /* Henter Artikkel */
      FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = DECIMAL(BongLinje.ArtikkelNr) NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
          NEXT TRANSRAD.
      FIND Lager NO-LOCK WHERE 
        Lager.ArtikkelNr = DECIMAL(BongLinje.ArtikkelNr) AND 
        Lager.Butik = BongLinje.ButikkNr NO-ERROR.
      IF AVAILABLE Lager THEN 
        vVareKost = Lager.vVarekost.
      IF vVareKost = ? THEN vVareKost = 0.
        
      /* TN 28/10-01 Valutapris ikke omregnet legges inn her. Ref. Gï¿½ran hos JF. */
      IF AVAILABLE ArtBas THEN
        FIND ArtPris NO-LOCK WHERE
          Artpris.ArtikkelNr = ArtBas.ArtikkelNr AND
          ArtPris.ProfilNr   = buf2Butiker.ProfilNr NO-ERROR.
      IF AVAILABLE ArtBas AND NOT AVAILABLE ArtPris THEN
        FIND ArtPris NO-LOCK WHERE
          Artpris.ArtikkelNr = ArtBas.ArtikkelNr AND
          ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
      IF NOT AVAILABLE ArtPris THEN 
        FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.

      /* Henter butikk det overfï¿½res til. */
      FIND ovButiker NO-LOCK WHERE
          ovButiker.Butik = INT(BongLinje.MButikkNr) NO-ERROR.
      IF AVAILABLE ovButiker THEN
          plMinusButikk = ovButiker.MinusButikk.
      ELSE
          plMinusButikk = FALSE.
      
      /* Logger mottagende butikk for bongen */    
      iMButikkNr = INT(BongLinje.MButikkNr).

      DEFINE VARIABLE lKalkvarekost AS DECIMAL NO-UNDO.
      /* Varekost ved overfï¿½ring, skal hentes fra gjeldende kalkyle.             */
      /* Er mottagende butikk sentrallageret, skal varekost fï¿½r rabatt benyttes. */
      IF iSentrallager = iMButikkNr THEN 
          ASSIGN 
              lKalkVarekost = ArtPris.InnkjopsPris[1]
              . 
      ELSE  
          ASSIGN 
              lKalkVarekost = ArtPris.InnkjopsPris[1] - ROUND(((ArtPris.InnkjopsPris[1] * ArtPris.Rab1%[1]) / 100),2)
              . 

      /* Logger overfï¿½ringstransaksjonen */
      CREATE TT_OvBuffer.
      ASSIGN TT_OvBuffer.BuntNr      = 999 /* dummy, kan vara vad som helst */
             TT_OvBuffer.LinjeNr     = piLinjeNr
             TT_OvBuffer.ArtikkelNr  = dec(Bonglinje.ArtikkelNr)
             TT_OvBuffer.Vg          = BongLinje.VareGr   
             TT_OvBuffer.LopNr       = BongLinje.LopeNr
             TT_OvBuffer.Antall      = BongLinje.Antall
             TT_OvBuffer.Merknad     = "Kasse"
             TT_OvBuffer.Storl       = BongLinje.Storrelse
             TT_OvBuffer.TilStorl    = BongLinje.Storrelse
          TT_OvBuffer.Varekost    = (IF bInnkjopsPris THEN lKalkVarekost ELSE vVareKost) /* ï¿½ndrat 17-09-10 frï¿½n raderna nedan */
             TT_OvBuffer.Varekost    = (IF TT_OvBuffer.Varekost = 0 THEN lKalkVarekost ELSE TT_OvBuffer.Varekost) /* Vektet kostpris fra db */
/*              TT_OvBuffer.Varekost    = (IF bInnkjopsPris THEN lKalkVarekost ELSE BongLinje.VVarekost) /* Vektet kostpris fra kassen */        */
/*              TT_OvBuffer.Varekost    = (IF TT_OvBuffer.Varekost = 0 THEN vVareKost ELSE TT_OvBuffer.Varekost) /* Vektet kostpris fra db */    */
/*              TT_OvBuffer.Varekost    = (IF TT_OvBuffer.Varekost = 0 THEN lKalkVarekost ELSE TT_OvBuffer.Varekost) /* Kostpris fra kalkyle. */ */
             piLinjeNr               = piLinjeNr + 1
             /* Setter datoinfo i registrert dato og tid. */
             TT_OvBuffer.RegistrertDato = BongLinje.TransDato
             TT_OvBuffer.RegistrertTid  = BongLinje.TransTid
             TT_OvBuffer.RegistrertAv   = USERID("SkoTex")
             .
      /* Vanlig overfï¿½ring */
      IF plMinusbutikk = FALSE THEN
          ASSIGN
            TT_OvBuffer.ButikkNrFra = INT(BongLinje.ButikkNr)
            TT_OvBuffer.ButikkNrTil = INT(BongLinje.MButikkNr)        
            .
      /* Overfï¿½ring fra kassen som markerer for lite mottatte varer.       */
      /* Her snur vi retningen pï¿½ overfï¿½ringen. Butikken skal motta varer. */
      ELSE DO:
          ASSIGN
            TT_OvBuffer.ButikkNrFra = INT(BongLinje.MButikkNr)        
            TT_OvBuffer.ButikkNrTil = INT(BongLinje.ButikkNr)
            .
      END.
      
      IF iOvbuntFinnes = 0 AND BongLinje.RefTekst BEGINS 'OvBuntNr:' THEN
          DO:
              IF CAN-FIND(OvBunt WHERE 
                          OvBunt.BuntNr = BongLinje.RefNr) THEN 
                  iOvbuntFinnes = BongLinje.RefNr.
          END. 
          
  END. /* TRANSRAD. */

  IF CAN-FIND(FIRST TT_OvBuffer) THEN 
  DO:
      /* Henter record for å få på plass riktig butikknr også ved +/- butikk overfï¿½ringer. */
      FIND FIRST TT_OvBuffer NO-ERROR.
      IF AVAILABLE tt_Ovbuffer THEN 
      DO:
          IF bBrukTBId2 THEN 
          DO:
              RUN opprettPakkseddlerOutlet.p (INPUT TABLE tt_OvBuffer, TT_OvBuffer.ButikkNrTil, TT_OvBuffer.ButikkNrTil, cPkSdlNr, plFaktura_Id, 4, OUTPUT lPkSdlId) NO-ERROR.
              RUN sendFakturaEMail.p ( plFaktura_Id ).
              FIND PkSdlHode NO-LOCK WHERE 
                  PkSdlHode.PkSdlId = lPkSdlId NO-ERROR.
              IF AVAILABLE PkSdlHode THEN 
                cPksdlNr = PkSdlHode.PkSdlNr.
              ELSE 
                cPkSdlNr = ''.

              IF NOT CAN-DO(cButPlussMinus,STRING(iMButikkNr)) THEN
              DO: 
                  RUN bibl_logg.p ('xOverforBong-PkSdlUtskrift', 'Butikk start(1) PkSdlNr: ' + cPkSdlNr + ' PkSdlId: ' + STRING(lPkSdlId) + ' Skriver: ' + buf2Butiker.RapPrinter).                          
                  RUN skrivpakkseddel.p (STRING(lPkSdlId) + "|",TRUE,buf2Butiker.RapPrinter,1,"",1).
                  RUN bibl_logg.p ('xOverforBong-PkSdlUtskrift', 'Butikk Ferdig PkSdlId: ' + STRING(lPkSdlId)).                          
              END.
              IF iMButikkNr = iSentrallager THEN 
              DO:
                  RUN bibl_logg.p ('xOverforBong-PkSdlUtskrift', 'Sentrallager start(2) PkSdlNr: ' + cPkSdlNr + ' PkSdlId: ' + STRING(lPkSdlId) + ' Skriver: ' + buf2Butiker.RapPrinter).                          
                  RUN skrivpakkseddel.p (STRING(lPkSdlId) + "|",TRUE,buf2Butiker.RapPrinter,1,"",1).
                  RUN bibl_logg.p ('xOverforBong-PkSdlUtskrift', 'Sentrallager Ferdig PkSdlId: ' + STRING(lPkSdlId)).                          
              END.
                  
              /* Overføres det til sentrallageret (But 20), oversk.lager nettbutikk (but 50) eller +/- butikker, skal pakkseddelen innleveres umiddelbart. */
              IF (iSentrallager = iMButikkNr) OR 
                 (iOverskLagerNettbutikk = iMButikkNr) OR 
                 CAN-DO(cButPlussMinus,STRING(iMButikkNr))
                 THEN
              INNLEVER_PAKKSEDDEL: 
              DO:
                  FIND PkSdlHode NO-LOCK WHERE 
                      PkSdlHode.PkSdlId = lPkSdlId NO-ERROR.
                  IF AVAILABLE PkSdlHode THEN 
                  DO:
                      EMPTY TEMP-TABLE tt2PkSdlLinje.
                      FOR EACH pkSdlLinje NO-LOCK WHERE
                          PkSdlLinje.PkSdlId = PkSdlHode.PkSdlId:
                          CREATE tt2PkSdlLinje.
                            BUFFER-COPY PkSdlLinje TO tt2PkSdlLinje.
                      END.
                      hBuffer = TEMP-TABLE tt2PkSdlLinje:DEFAULT-BUFFER-HANDLE.
                      RUN pksdl_innlever.p ('', hBuffer, '', OUTPUT cReturn, OUTPUT bOk).                      
                      EMPTY TEMP-TABLE tt2PkSdlLinje.   
                  END.
              END. /* INNLEVER_PAKKSEDDEL */
              
          END.
          
          /* Kommer bongen fra bakrom ved oppdatering av overføingsordre, skal det ikke opprettes ny ordre. */
          IF iOvbuntFinnes = 0 THEN
          OPPRETTOVBUNT: 
          DO:
              ASSIGN iBuntNr = -2. /* -2 = En overfï¿½ringsordre pr. bong. Og de markeres som oppdatert. */
              RUN LagraOvBuffer.p (INPUT-OUTPUT iBuntNr,
                                   0,
                                   "N" + CHR(1) + "Overføring kasse " + STRING(TODAY) + STRING(TIME,"HH:MM") + CHR(1) + "N",
                                   wEDB-System,
                                   wTabell,
                                   7).
          END. /* OPPRETTOVBUNT */
          ELSE iBuntNr = iOvbuntFinnes.
              
          DO FOR bufOvBunt TRANSACTION:
            FIND bufOvBunt EXCLUSIVE-LOCK WHERE 
              bufOvBunt.BuntNr = iBuntNr NO-ERROR.
            IF AVAILABLE bufOvBunt THEN 
            DO:
              ASSIGN 
                bufOvBunt.Faktura_Id = plfaktura_Id
                bufOvBunt.BatchNr    = iBatchNr.
              RELEASE bufOvBunt.
            END.
          END.
          
          /* Overføres det til sentrallager (But 20), og det finnes en outlet, skal det legges opp en pakksedel. */
          /* Dette skal gjøres uavhengig av bBrukTBId2 parameteren.                                              */
          IF (iSentrallager = iMButikkNr AND iOutlet > 0) THEN 
              RUN opprettPakkseddlerOutlet.p (INPUT TABLE tt_OvBuffer, iSentrallager, iOutlet, cPkSdlNr, 0, 5, OUTPUT lPkSdlId) NO-ERROR.
              
          /* Overfres det til overskuddslager (But 50) fra nettbutikkens lager, skal det legges opp en pakksedel på nettbutikkens lager. */
          /* Dette skal gjøres uavhengig av bBrukTBId2 parameteren.                                                                       */
          IF (iOverskLagerNettbutikk = iMButikkNr AND iNettButLager > 0) THEN 
              RUN opprettPakkseddlerOutlet.p (INPUT TABLE tt_OvBuffer, iOverskLagerNettbutikk, iNettButLager, cPkSdlNr, 0, 6, OUTPUT lPkSdlId) NO-ERROR.
      END.
      EMPTY TEMP-TABLE TT_OvBuffer NO-ERROR.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-posterFaktura) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE posterFaktura Procedure 
PROCEDURE posterFaktura :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR piLinjeNr     AS INT  NO-UNDO.
DEF VAR pcVaretran    AS CHAR NO-UNDO.
DEF VAR pcBetaling    AS CHAR NO-UNDO.
DEF VAR pcRefNr       AS CHAR NO-UNDO.
DEF VAR pcRefTekst    AS CHAR NO-UNDO.
DEF VAR pcTekst       AS CHAR NO-UNDO.
DEF VAR ocReturn      AS CHAR NO-UNDO.
DEF VAR obOk          AS LOG  NO-UNDO.
DEF VAR pbOverforing  AS LOG NO-UNDO.
DEF VAR pcVareNr      AS CHAR NO-UNDO.
DEFINE VARIABLE lMva% AS DECIMAL NO-UNDO.

ASSIGN
    pcBetaling   = "50,51,52,53,54,55,56,57,58,61,66,69,70,71,72,73,78,79"
    pcVaretran   = "1,3,4,6,10,134"
    pbOverforing = FALSE
    plfaktura_Id = 0
    pcRefNr      = ''
    pcRefTekst   = ''
    .
    
/* Bonger som allerede er fakturert fra kundeordre, skal ikke faktureres her. */
IF BongHode.KOrdre_Id > 0 THEN
    RETURN.

/* Henter kunden som er satt inn pï¿½ bongen. */
FIND Kunde NO-LOCK WHERE
    Kunde.KundeNr = BongHode.KundeNr NO-ERROR.

FIND Butiker NO-LOCK WHERE
    Butiker.Butik = BongHode.Butik NO-ERROR.

/* NY PRS POS Flagger at det er en overføring og setter kundenr. for mottagende butikk. */
IF CAN-FIND(FIRST BongLinje WHERE
                  BongLinje.B_Id = BongHode.B_id AND
                CAN-DO("006",STRING(BongLinje.TTId,"999"))) THEN
DO:
    FIND FIRST BongLinje WHERE BongLinje.B_Id = BongHode.B_id AND
        CAN-DO("006",STRING(BongLinje.TTId,"999")) NO-LOCK NO-ERROR.
   
    /* Overfï¿½ringer mellom nettbutikk og nettbutikks ventelager, skal ikke generere faktura. */
    /* Er det fra til eller til fra. GANT GANt..... */
    IF iGantAktiv = 1 THEN 
    DO:
        IF (BongLinje.ButikkNr = iNettButLager AND BongLinje.MButikkNr = iOverskLagerNettbutikk) OR 
           (BongLinje.ButikkNr = iOverskLagerNettbutikk AND BongLinje.MButikkNr = iNettButLager) THEN 
            RETURN.
    END.
        
    /* Henter mottagende butikk. */    
    FIND bufButiker NO-LOCK WHERE 
      bufButiker.Butik = BongLinje.MButik AND bufButiker.KundeNr > 0 NO-ERROR.
    IF NOT AVAILABLE bufbutiker THEN 
        RETURN.  
      
    IF AVAILABLE bufButiker AND BongHode.KundeNr = 0 THEN 
      DO TRANSACTION:
        FIND bufBongHode EXCLUSIVE-LOCK WHERE RECID(bufBongHode) = RECID(BongHode).
        ASSIGN 
          bufBongHode.KundeNr = bufButiker.KundeNr.
          
        /* Henter kunden som er koblet til mottagende butikk.                             */
        /* Denne er normalt satt inn i bonghode, men det gjï¿½res hvis dette ikke er gjort. */  
        FIND Kunde NO-LOCK WHERE
          Kunde.KundeNr = bufButiker.KundeNr NO-ERROR.
        IF AVAILABLE bufBongHode THEN RELEASE bufBongHode. 
      END. /* TRANSACTION */       
    pbOverforing = TRUE.
    
    /* Ved overføring må internfakturering vï¿½re satt pï¿½ begge butikker for at faktura skal opprettes. */
    IF Butiker.IntFaktOverforing = FALSE OR 
       bufButiker.IntFaktOverforing = FALSE THEN 
        RETURN.
                
    /* Ved overfï¿½ring mï¿½ ogsï¿½ kunde pï¿½ mottagende butikk vï¿½re opprettet og koblet til butikken. */ 
    IF NOT AVAILABLE Kunde THEN 
        RETURN.
END.

/* Leser og posterer betalingstransaksjoner. */
IF AVAILABLE Kunde THEN
POSTER-FAKTURA:
DO:
    /* Er det en overfï¿½ring og kunden representerer en Outlet, skal ikke faktura utstedes her.  */
    /* Faktura utstedes da fï¿½rst ved varemottaket, samtidig med at overskuddslager trekkes ned. */
    IF pbOverforing AND CAN-DO(cOutletListe,STRING(bufButiker.Butik)) THEN 
        LEAVE POSTER-FAKTURA.

    /* Innlevering av pakkseddel fra outlet. Da er pakkseddelnr satt i RefTekst. */
    FIND FIRST BongLinje NO-LOCK WHERE 
        BongLinje.b_id = BongHode.b_id AND 
        BongLinje.RefTekst MATCHES '*PkSdlNr*' NO-ERROR.
    IF AVAILABLE BongLinje THEN 
    DO:
        ASSIGN
            pcRefNr    = '1'
            pcRefTekst = BongLinje.RefTekst 
            NO-ERROR.
    END.
    IF pcRefTekst = '' THEN 
    DO:
        FIND FIRST BongLinje NO-LOCK WHERE
            BongLinje.B_Id = Bonghode.B_Id AND
            BongLinje.TTId = 88 NO-ERROR.
        IF AVAILABLE BongLinje THEN
            ASSIGN
            pcRefNr    = STRING(BongLinje.RefNr)
            pcRefTekst = BongLinje.RefTekst
            .
    END.

    /* Er det ikke kredittsalg eller overfï¿½ringer pï¿½ bongen skal den ikke behandles her. */
    IF NOT CAN-FIND(FIRST BongLinje WHERE
                    BongLinje.B_Id = BongHode.B_id AND
                    CAN-DO("006,065,087",STRING(BongLinje.TTId,"999"))) THEN
        LEAVE POSTER-FAKTURA.
    /* Flagger at det er en varetransaksjon. Skal ha tilsvarende behandling som overfï¿½ring med hensyn til betalingstransaksjonen. */
    IF CAN-FIND(FIRST BongLinje WHERE
                      BongLinje.B_Id = BongHode.B_id AND
                    CAN-DO("087",STRING(BongLinje.TTId,"999"))) THEN
            pbOverforing = TRUE.
        
    /* Oppretter faktura, setter kundenr m.m. og returnerer faktura_id. */
    IF BongHode.Belop >= 0 THEN
        RUN getfaktura_id.p (Kunde.KundeNr,BongHode.ButikkNr,1,YES,BongHode.Dato,OUTPUT plFaktura_Id).
    ELSE
    /* For negative bonger skal det opprpettes separat faktura - Kreditnota. */
        RUN getfaktura_id.p (Kunde.KundeNr,BongHode.ButikkNr,999,YES,BongHode.Dato,OUTPUT plFaktura_Id).

    IF NOT CAN-FIND(FakturaHode WHERE FakturaHode.Faktura_Id = plFaktura_Id) THEN DO:
/*        MESSAGE                                                              */
/*        PROGRAM-NAME(1)  SKIP                                                */
/*        "FEIL - Hentet Id pï¿½ ukjent faktura: " RETURN-VALUE SKIP plFaktura_Id*/
/*        VIEW-AS ALERT-BOX INFO BUTTONS OK.                                   */
        LEAVE POSTER-FAKTURA.
    END.

    IF CAN-FIND(FakturaHode WHERE FakturaHode.Faktura_Id = plFaktura_Id) THEN
    FAKTURAINFO:
    DO:
        FIND FakturaHode NO-LOCK WHERE
            FakturaHode.Faktura_Id = plFaktura_Id.
        /* Initierer faktura med kundeinfo m.m. */
        IF FakturaHode.Navn = "" AND FakturaHode.Adresse1 = "" THEN
        DO TRANSACTION:
            RUN update_fakturahode.p (plfaktura_Id,"INIT","",1).
            /* Vi mï¿½ her overstyre bilagstype, som er satt tilbake til 1 i overstï¿½ende INIT */
            RUN update_fakturahode.p (plfaktura_Id,"Butikksalg,TotalRabatt%,Leveringsdato,LevFNr,Leveringsdato,Utsendelsesdato,BilagsType,KOrdre_Id",
                                      "Yes" + chr(1) + 
                                       STRING(Kunde.TotalRabatt%) + CHR(1) + 
                                       STRING(BongHode.Dato) + CHR(1) + 
                                       "1" + CHR(1) + 
                                       STRING(BongHode.Dato) + CHR(1) + 
                                       STRING(BongHode.Dato) + CHR(1) +
                                       (IF BongHode.Belop >= 0 
                                           THEN '1' 
                                           ELSE '2') + CHR(1) +
                                       (IF BongHode.KOrdre_Id > 0 THEN STRING(BongHode.KOrdre_Id) ELSE ''),
                                       1). 
            FIND CURRENT FakturaHode NO-LOCK.
        END.
    END. /* FAKTURAINFO */

    /* Setter linjenr */
    piLinjeNr = 0.
    FOR EACH FakturaLinje NO-LOCK OF FakturaHode
        BY FakturaLinje.Faktura_Id
        BY FakturaLinje.FakturaLinjeNr:
        piLinjeNr = FakturaLinje.FakturaLinjeNr.
    END.
    piLinjeNr = piLinjeNr + 1.

    /* Posterer bonglinjene pï¿½ faktura. */
    KREDITSALG:
    FOR EACH BongLinje NO-LOCK WHERE
        BongLinje.B_Id = BongHode.B_Id AND
        BongLinje.Makulert = FALSE:

        /* Setter mva%. Er kunden satt som mvafri, skal denne settes til 0. */
        lMva% = BongLinje.Mva%.
        IF Kunde.MvaFri THEN 
            lMva% = 0.

        /* Er det overfï¿½ring, skal ikke betalingstransaksjonen med */
        IF CAN-DO(pcVaretran + "," + (IF pbOverforing = FALSE
                                        THEN pcBetaling
                                        ELSE ""),STRING(BongLinje.TTId)) THEN
        OPPRETT-FAKTURA:
        DO:
            CREATE FakturaLinje.
            ASSIGN
                FakturaLinje.Faktura_Id     = FakturaHode.Faktura_Id
                FakturaLinje.FakturaLinjeNr = piLinjeNr
                piLinjeNr                   = piLinjeNr + 1
                FakturaLinje.Opphav         = 1 /* Artikkel */
                FakturaLinje.Leveringsdato  = BongLinje.TransDato
                FakturaLinje.TTId           = BongLinje.TTId
                FakturaLinje.TBId           = BongLinje.TBId
                FakturaLinje.B_Id           = BongLinje.B_Id
                FakturaLinje.BongLinjeNr    = BongLinje.LinjeNr
                FakturaLinje.Normalpris     = BongLinje.Normalpris
                .
            IF pcRefNr <> '' THEN 
                ASSIGN 
                    FakturaLinje.EkstRefId      = pcRefNr
                    FakturaLinje.EkstRefTekst   = pcRefTekst
                .
            ELSE 
                ASSIGN 
                FakturaLinje.EkstRefId    = STRING(BongLinje.RefNr)
                FakturaLinje.EkstRefTekst = BongLinje.RefTekst
               . 
                         
            IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
              ASSIGN
                FakturaLinje.Notat          = "Kvitto: "  +
                                              "Butik: "   + STRING(BongLinje.ButikkNr) + "/" +
                                              " Kassa: "  + STRING(BongLinje.KasseNr) + "/" +
                                              " Datum: " + STRING(BongLinje.TransDato) + "/" +
                                              " Nr: "   + STRING(BongLinje.BongNr) + "/" + 
                                              " Rad: "   + STRING(BongLinje.LinjeNr)
                .
            ELSE ASSIGN
                FakturaLinje.Notat          = "Kvittering: "  +
                                              "Butikk: "   + STRING(BongLinje.ButikkNr) + "/" +
                                              " Kasse: "  + STRING(BongLinje.KasseNr) + "/" +
                                              " Dato: " + STRING(BongLinje.TransDato) + "/" +
                                              " Nr: "   + STRING(BongLinje.BongNr) + "/" + 
                                              " Rad: "   + STRING(BongLinje.LinjeNr)
                   .
            /* Varelinjer */
            IF CAN-DO(pcVaretran,STRING(BongLinje.TTId)) THEN
            VARELINJE:
            DO:
                /* Henter artikkelen */
                IF AVAILABLE ArtBas THEN
                    RELEASE ArtBas.
                IF bBestNr THEN
                    FIND ArtBas WHERE
                    ArtBas.ArtikkelNr = dec(BongLinje.ArtikkelNr) NO-ERROR.
                ASSIGN
                    pcVareNr = IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE STRING(BongLinje.ArtikkelNr)
                    pcVareNr = IF pcVareNr = "" THEN STRING(BongLinje.ArtikkelNr) ELSE pcVareNr
                    .

                FIND FIRST Moms NO-LOCK WHERE
                    Moms.MomsProc = lMva% NO-ERROR.
                IF bInnkjopsPris THEN DO:
                    FIND ArtPris NO-LOCK WHERE
                        ArtPris.ArtikkelNr = dec(BongLinje.ArtikkelNr) AND
                        ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
                    IF NOT AVAILABLE ArtPris THEN
                        FIND ArtPris NO-LOCK WHERE
                            ArtPris.ArtikkelNr = dec(BongLinje.ArtikkelNr) AND
                            ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
                END.                

                ASSIGN
                    FakturaLinje.Antall        = BongLinje.Antall
                    FakturaLinje.VareNr        = pcVareNr
                    FakturaLinje.ArtikkelNr    = IF AVAILABLE ArtBas
                                                   THEN ArtBas.ArtikkelNr
                                                   ELSE dec(BongLinje.ArtikkelNr)
                    Fakturalinje.LevFargKod    = IF AVAILABLE ArtBas
                                                   THEN ArtBas.LevFargKod
                                                   ELSE ""
                    FakturaLinje.Varetekst     = IF FakturaLinje.TTId = 134
                                                   THEN "Gavekort pålydende kr: " + STRING(BongLinje.LinjeSum / BongLinje.Antall) 
                                                   ELSE BongLinje.BongTekst
                    FakturaLinje.LinjeRabattKr = (BongLinje.LinjeRab + BongLinje.SubtotalRab) / (BongLinje.Antall)
                    FakturaLinje.TotalrabattKr = (BongLinje.LinjeRab + BongLinje.SubtotalRab)
                    FakturaLinje.LinjeRab%     = ABS((FakturaLinje.LinjeRabattKr * BongLinje.Antall) / BongLinje.LinjeSum) * 100
                    FakturaLinje.LinjeRab%     = IF FakturaLinje.LinjeRab% = ? THEN 0 ELSE FakturaLinje.LinjeRab%
                    FakturaLinje.TotRab%       = FakturaLinje.LinjeRab%

                    FakturaLinje.NettoPris     = IF (pbOverforing AND bInnkjopspris = TRUE AND AVAILABLE ArtPris)
                                                   THEN ArtPris.Innkjopspris[1]
                                                 ELSE IF (pbOverforing AND bInnkjopspris = false) 
                                                     THEN BongLinje.VVarekost / BongLinje.Antall
                                                 ELSE (BongLinje.LinjeSum - (BongLinje.LinjeRab + BongLinje.SubtotalRab) - BongLinje.MvaKr) / BongLinje.Antall

                    FakturaLinje.Pris          = IF (pbOverforing AND bInnkjopsPris = TRUE AND AVAILABLE ArtPris)
                                                   THEN ArtPris.InnkjopsPris[1]
                                                 ELSE IF (pbOverforing AND bInnkjopsPris = FALSE) 
                                                     THEN BongLinje.VVarekost / BongLinje.Antall
                                                 ELSE (BongLinje.LinjeSum / (1 + (lMva% / 100))) / BongLinje.Antall

                    FakturaLinje.MvaKr         = IF (pbOverforing AND bInnkjopsPris = TRUE AND AVAILABLE ArtPris)
                                                   THEN ((ArtPris.InnkjopsPris[1] * lMva% / 100) * BongLinje.Antall) /* KO BongLinje.Antall */
                                                 ELSE IF (pbOverforing AND bInnkjopsPris = FALSE)
                                                   THEN (((FakturaLinje.NettoPris * lMva%) / 100) * BongLinje.Antall) /* KO BongLinje.Antall */
                                                 ELSE BongLinje.MvaKr * (IF BongLinje.Antall < 0 THEN -1 ELSE 1)                                  
                    FakturaLinje.NettoLinjeSum = FakturaLinje.NettoPris * abs(BongLinje.Antall)
                    FakturaLinje.Linjesum      = FakturaLinje.NettoPris * abs(BongLinje.Antall) + FakturaLinje.MvaKr

                    FakturaLinje.Mva%          = lMva%
                    FakturaLinje.MomsKod       = IF AVAILABLE Moms
                                                   THEN Moms.MomsKod
                                                   ELSE 0
                    FakturaLinje.Leveringsdato = BongLinje.TransDato
                    FakturaLinje.Storl         = BongLinje.Storrelse
                    FakturaLinje.DbKr          = (FakturaLinje.NettoPris - (IF BongLinje.Antall < 0 THEN BongLinje.VVArekost * -1 ELSE BongLinje.VVarekost))
                    FakturaLinje.Db%           = ABS(FakturaLinje.DbKr / FakturaLinje.NettoLinjeSum) * 100
                    FakturaLinje.Db%           = IF FakturaLinje.Db% = ? THEN 0 ELSE FakturaLinje.Db%
                    FakturaLinje.TTId          = BongLinje.TTId
                    FakturaLinje.TBId          = BongLinje.TBId
                    .
                    IF bVarespes THEN 
                    DO:
                      IF AVAILABLE ArtBas THEN 
                        FIND Farg OF ArtBas NO-LOCK NO-ERROR.
                      IF AVAILABLE Farg THEN 
                        Fakturalinje.Varespesifikasjon = STRING(Farg.Farg) + ' ' + Farg.FarBeskr + 
                                                         (IF Fakturalinje.Varespesifikasjon <> ''
                                                           THEN CHR(10) + Fakturalinje.Varespesifikasjon
                                                           ELSE '').
                    END.
            END. /* VARELINJE */
            /* Betalingstransaksjoner */
            IF CAN-DO(pcBetaling,STRING(BongLinje.TTId)) THEN
            BETALINGSLINJE:
            DO:
                FIND TransType NO-LOCK WHERE
                    TransType.TTId = BongLinje.TTId NO-ERROR.
                ASSIGN
                    FakturaLinje.Antall = 1
                    FakturaLinje.Varetekst     = (IF AVAILABLE TransType
                                                    THEN TransType.Beskrivelse
                                                    ELSE BongLinje.BongTekst)
                    FakturaLinje.NettoPris     = BongLinje.LinjeSum * -1
                    FakturaLinje.NettoLinjeSum = BongLinje.LinjeSum * -1
                    FakturaLinje.Linjesum      = BongLinje.LinjeSum * -1
                    .
            END. /* BETALINGSLINJE */

            FIND CURRENT FakturaLinje NO-LOCK.
        END. /* OPPRETT-FAKTURA */
    END. /* KREDITSALG */

    /* Summerer opp fakturahode. */
    IF plFaktura_Id <> 0 THEN
    FAKTURASUM:
    DO:
        /* Legger pï¿½ fakturagebyr pï¿½ vanlige faktura. Ikke pï¿½ kreditnota. */
        IF BongHode.Belop >= 0 AND Kunde.Fakturagebyr THEN
            RUN update_fakturahode.p (plfaktura_Id,"Fakturagebyr","",1).
        ELSE 
            RUN update_fakturahode.p (plfaktura_Id,"Dato",STRING(BongHode.Dato),1).
        /* Pï¿½fï¿½rer og beregner rabatt. Rabatt pr. linje tildeles automatisk nï¿½r totalrabatt <> 0 blir satt. */
        RUN update_fakturahode.p (plfaktura_Id,"KalkulerTotaler","",1).
    END. /* FAKTURASUM */
    
    /* direkte utskrift av faktura. */
    IF Kunde.Samlefaktura = FALSE AND
        Butiker.dirFakturaUtskrift = TRUE THEN
    DO:
        RUN faktura_produksjon.p ("idlist|" + STRING(plfaktura_Id),
                                ?,
                                "",
                                OUTPUT ocReturn,
                                OUTPUT obOk).
        IF obOk THEN
        DO:
            IF iGantAktiv = 1 THEN 
            DO TRANSACTION:
                FIND FakturaHode EXCLUSIVE-LOCK WHERE FakturaHode.Faktura_Id = plFaktura_Id NO-ERROR.
                IF AVAILABLE FakturaHode THEN 
                DO:         
                    /* Dette er tilfelle ved innlevering av pakkseddel pï¿½ outlet hvor outlet faktureres fra sentrallager. */  
                    IF pcRefTekst MATCHES '*PksdlNr*' THEN 
                    DO:
                        FIND LAST PkSdlHode EXCLUSIVE-LOCK WHERE 
                            PkSdlHode.PkSdlNr = TRIM(TRIM(ENTRY(2,pcRefTekst,':'),' '),'.') NO-ERROR.
                        IF AVAILABLE PkSdlHode THEN 
                            ASSIGN 
                            PkSdlHode.FakturaNr = FakturaHode.FakturaNr
                            PkSdlHode.EkstId    = STRING(FakturaHode.Faktura_Id)
                            FakturaHode.PkSdlNr = PkSdlHode.PkSdlNr
                            FakturaHode.FNotat  = FakturaHode.FNotat + 
                                                  (IF FakturaHode.FNotat <> '' THEN CHR(10) ELSE '') + 
                                                  'Pakkseddel: ' + STRING(PkSdlHode.PkSdlNr) + '.'
                            .
                    END.
                    IF AVAILABLE PkSdlHode THEN RELEASE PkSdlHode.
                    FIND CURRENT FakturaHode NO-LOCK.
                END.
            END. /* TRANSACTION */        
            
            FAKTURA_UTSKRIFT:
            DO:
                /* eCom skal ikke ha faktura utskrift når det overføres fra overskudslager(16) til eCom (15) eller tilbake. */
                IF iGantAktiv = 1 AND 
                   (
                    (Butiker.Butik = iLagereCom AND Kunde.ButikkNr = ieCom) OR 
                    (Butiker.Butik = ieCom AND Kunde.ButikkNr = iLagereCom)  
                   ) THEN 
                    LEAVE FAKTURA_UTSKRIFT. 
                
                RUN faktura_fakturaskriver.p (STRING(BongHode.ButikkNr) + "|1|" + STRING(BongHode.KasseNr),
                                        ?,
                                        "",
                                        OUTPUT ocReturn,
                                        OUTPUT obOk).
                IF obOk THEN DO:
                    pcTekst = ocReturn.
                    IF pcTekst <> "" THEN DO:
                        RUN skrivfaktura.p (STRING(plfaktura_Id) + "|",ENTRY(1,pcTekst,"|"),ENTRY(2,pcTekst,"|"),ENTRY(3,pcTekst,"|"),ENTRY(4,pcTekst,"|"),ENTRY(5,pcTekst,"|")). 
                        /* Ekstra kopi til butikk? */
                        IF Butiker.FaktKopiRappskriver AND Butiker.RapPrinter <> "" THEN
                            RUN skrivfaktura.p (STRING(plfaktura_Id) + "|",ENTRY(1,pcTekst,"|"),Butiker.RapPrinter,"1",ENTRY(4,pcTekst,"|"),ENTRY(5,pcTekst,"|")). 
                    END.
                END.
            END. /* FAKTURA_UTSKRIFT */
            
            /* Faktura utskrift på Outlet. Outlet skal ha en egen kopi av faktura skrevet ut på skriver i butikk. */
            IF CAN-DO(cOutletListe,STRING(Kunde.ButikkNr)) THEN 
            DO:
                RUN faktura_fakturaskriver.p (STRING(Kunde.ButikkNr) + "|1|99",
                    ?,
                    "",
                    OUTPUT ocReturn,
                    OUTPUT obOk).
                IF obOk AND ocReturn <> '' THEN 
                DO:
                    RUN skrivfaktura.p (STRING(plfaktura_Id) + "|",ENTRY(1,ocReturn,"|"),ENTRY(2,ocReturn,"|"),ENTRY(3,ocReturn,"|"),ENTRY(4,ocReturn,"|"),ENTRY(5,ocReturn,"|")).
                END. 
            END.            
        END.
    END.

END. /* POSTER-FAKTURA */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PosterKonto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PosterKonto Procedure 
PROCEDURE PosterKonto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bBongLinje FOR BongLinje.

  /* Legger alle salgstransaksjoner inn p} konto */
  FOR EACH bBongLinje WHERE 
      bBongLinje.B_Id = BongHode.B_Id AND
      bBongLinje.Makulert = FALSE AND                    
  can-do("012,056,061,052",STRING(bBongLinje.TTId,"999")) NO-LOCK:
      /* Skaper konto post. */
      CREATE konto.

      /* Setter opp index. */
      ASSIGN konto.butikk      = bBongLinje.ButikkNr
             konto.kasse       = bBongLinje.kassenr
             konto.dato        = bBongLinje.dato
             konto.kontonummer = BongHode.kundenr.

      /* Legger over informasjonen. */
      ASSIGN konto.Vg          = bBongLinje.VareGr
             konto.lopnr       = bBongLinje.lopenr
             konto.storl       = bBongLinje.Storrelse
             konto.pris        = bBongLinje.LinjeSum - bBongLinje.LinjeRab - bBongLinje.SubtotalRab
             konto.antal       = bBongLinje.Antall
             konto.kvitto      = bBongLinje.BongNr.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PosterKortSpes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PosterKortSpes Procedure 
PROCEDURE PosterKortSpes :
/*------------------------------------------------------------------------------
  Purpose:    Spesifikasjon av kortbetalinger.  
  Parameters:  <none>
  Notes:       Kas_Rap og BongLinje er tilgjengelige nï¿½r rutine kalles.
------------------------------------------------------------------------------*/

  FIND Kort_Spes EXCLUSIVE-LOCK WHERE
    Kort_Spes.Dato       = Kas_Rap.Dato AND
    Kort_Spes.Butikk     = Kas_Rap.Butikk AND
    Kort_Spes.Kasse      = Kas_Rap.Kasse AND
    Kort_Spes.KassererNr = Kas_Rap.KassererNr AND 
    Kort_Spes.z_nummer   = Kas_Rap.z_nummer AND
    Kort_Spes.KortType   = int(BongLinje.Antall) NO-ERROR.
  IF NOT AVAILABLE Kort_Spes THEN
  DO:
    CREATE Kort_Spes.
    ASSIGN
      Kort_Spes.Dato       = Kas_Rap.Dato 
      Kort_Spes.Butikk     = Kas_Rap.Butikk 
      Kort_Spes.Kasse      = Kas_Rap.Kasse 
      Kort_Spes.KassererNr = Kas_Rap.KassererNr 
      Kort_Spes.z_nummer   = Kas_Rap.z_nummer 
      Kort_Spes.KortType   = int(BongLinje.Antall)
      .
  END.
  ASSIGN
    Kort_Spes.AntKort = Kort_Spes.AntKort + 1
    Kort_Spes.Belop   = Kort_Spes.Belop   + BongLinje.LinjeSum
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-posterKundereskontro) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE posterKundereskontro Procedure 
PROCEDURE posterKundereskontro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR plBelop     AS DEC  NO-UNDO.
DEF VAR plFakturaNr AS DEC  NO-UNDO.
DEF VAR pcReturn    AS CHAR NO-UNDO.
DEF VAR ocReturn      AS CHAR NO-UNDO.
DEF VAR obOk          AS LOG  NO-UNDO.

ASSIGN
    plFakturaNr = ?
    .

/* Leser og posterer betalingstransaksjoner. */
IF AVAILABLE Kunde THEN
POSTER-RESKONTRO:
DO:
  /* Er det innbetaling pï¿½ bongen. */
  IF NOT CAN-FIND(FIRST BongLinje WHERE
                  BongLinje.B_Id = BongHode.B_id AND
                  CAN-DO("089",STRING(BongLinje.TTId,"999"))) THEN
      LEAVE POSTER-RESKONTRO.

  INNBETALING:
  FOR EACH BongLinje NO-LOCK WHERE
        BongLinje.B_Id = Bonghode.B_Id AND
        BongLinje.Makulert = FALSE AND
        BongLinje.TTId = 89:

    ASSIGN
        plBelop = (BongLinje.Linjesum)
        .
    IF plBelop NE 0 THEN 
    DO:
        FIND Butiker NO-LOCK WHERE
            Butiker.Butik = BongHode.Butik NO-ERROR.

        RUN kunderes_innbet.p (STRING(Kunde.KundeNr)
                           + "|" + ""
                           + "|" + "" 
                           + "|" + string(Bonghode.Dato)
                           + "|" + STRING(plBelop)
                           + "|Bong: "  +
                             "But: "   + STRING(BongLinje.ButikkNr) + "/" +
                             " Kas: "  + STRING(BongLinje.KasseNr) + "/" +
                             " Dato: " + STRING(BongLinje.TransDato) + "/" +
                             " Nr: "   + STRING(BongLinje.BongNr) + "/" + 
                             " Ln: "   + STRING(BongLinje.LinjeNr)
                           + "|" /* KID */ 
                           + "|" + STRING(BongHode.B_Id)
                           + "|" + STRING(BongLinje.LinjeNr),
                            ?,
                            "",
                            OUTPUT ocReturn,
                            OUTPUT obOk).
    END.
    /* Oppdaterer kundesaldo. */
    RUN beregn_kunde_saldo.p ("idlist|" + STRING(Kunde.KundeNr),
                            ?,
                            "",
                            OUTPUT ocReturn,
                            OUTPUT obOk).
  END. /* INNBETALING */
  IF AVAILABLE Kunde THEN FIND CURRENT Kunde NO-LOCK.
END. /* POSTER-RESKONTRO */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-posterMedlemsKjop) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE posterMedlemsKjop Procedure 
PROCEDURE posterMedlemsKjop :
/*------------------------------------------------------------------------------
                        Purpose: Postering av medlemskjï¿½p. Danner grunnlag for beregning
                                 av medlemsbonus.                                                                                                                                         
                        Notes:   Rutinene MedlemsSalg og Kundesalg som posterer i 
                                 MedTrans og KundeTrans er overflï¿½dige. De er erstattet
                                 av indeks pï¿½ kunde og medlem som er lagt inn pï¿½ Translogg.                                                                                                                               
        ------------------------------------------------------------------------------*/
DEFINE VARIABLE piDummy AS INTEGER NO-UNDO.

/* Finner ikke medlemmet, posteres ikke medlemstransene.            */
FIND Medlem NO-LOCK WHERE
    Medlem.MedlemsNr = BongHode.MedlemsNr NO-ERROR.
IF NOT AVAILABLE Medlem THEN
DO:
    RUN NyFilLogg IN h_Logg (INPUT Filer.FilId, STRING(TODAY) + " " + 
                    STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                    " - Ukjent medlemsnummer på bong (KortNr/BongNr/MedlemsNr: " + 
                    BongHode.MedlemsKort + "/" + 
                    STRING(BongHode.BongNr) + "/" + 
                    string(BongHode.MedlemsNr) + ")." + CHR(1) + "3").
    RETURN "".
END.

/* Er bongen postert tidligere, avbrytes posteringen */
IF CAN-FIND(FIRST MedKjop WHERE MedKjop.B_Id = BongHode.B_Id) THEN 
  DO:
    RUN NyFilLogg IN h_Logg (INPUT Filer.FilId, STRING(TODAY) + " " + 
                    STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                    " - Bong er postert i MedKjop tidligere: " + 
                    BongHode.MedlemsKort + "/" + 
                    STRING(BongHode.BongNr) + "/" + 
                    string(BongHode.MedlemsNr) + ")." + CHR(1) + "3").
    RETURN "".
  END.

  /* posterer kjï¿½pet */
  RUN posterMedlemsKjop.p (BongHode.B_Id, h_Prisko, INPUT-OUTPUT piDummy).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Reklamasjonslogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reklamasjonslogg Procedure 
PROCEDURE Reklamasjonslogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTTId           AS CHAR NO-UNDO.
  DEF VAR piLinjeNr        AS INT  NO-UNDO.
  DEF VAR plReklamasjonsNr AS DEC  NO-UNDO.
  DEF VAR ocValue          AS CHAR NO-UNDO.
  DEF VAR obOk             AS LOG NO-UNDO.
  DEF VAR dVVarekost       AS DEC  NO-UNDO.

  /* Logger reklamasjoner og reklamasjonsrabatter */
  TRANSRAD:
  FOR EACH BongLinje NO-LOCK WHERE
      BongLinje.B_Id = BongHode.B_Id AND
      BongLinje.Makulert = FALSE AND
      CAN-DO("001,003,004",STRING(BongLinje.TTId,"999")):
  
      ASSIGN
          pcTTId = STRING(BongLinje.TTId,"999")
          plReklamasjonsNr = 0 /* En reklamasjonslogg pr. linje. */
          .
      /* Vanlig salg skal bare logges nï¿½r det er registrert reklamasjonskode. */
      IF STRING(BongLinje.TTId,"999") = "001" THEN
      DO:
/*           IF BongLinje.FeilKode = 0 THEN */
              NEXT TRANSRAD.
      END.

      /* Ukjente artikler kan ikke behandles */
      IF BongLinje.ArtikkelNr = "" THEN
          NEXT TRANSRAD.
  
      /* Ikke alle reklamasjonskoder. */
      IF BongLinje.FeilKode > 99 THEN
          NEXT TRANSRAD.

      /* Henter Artikkel */
      FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = dec(BongLinje.ArtikkelNr) NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
          NEXT TRANSRAD.

      /* TN 28/10-01 Valutapris ikke omregnet legges inn her. Ref. Gï¿½ran hos JF. */
      IF AVAILABLE ArtBas THEN
        FIND ArtPris NO-LOCK WHERE
          Artpris.ArtikkelNr = ArtBas.ArtikkelNr AND
          ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
      IF AVAILABLE ArtBas THEN
          FIND Lager NO-LOCK WHERE
            Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
            Lager.Butik      = BongLinje.ButikkNr NO-ERROR.
      dVVarekost = 0.
      IF AVAILABLE Lager THEN
          dVVarekost = Lager.VVarekost.
      IF (dVVArekost = 0 OR dVVarekost = ?) AND AVAILABLE ArtPris THEN
          dVVarekost = ArtPris.Varekost[1].
      IF dVVareKost = ? THEN dVVArekost = 0.

      /* Skaper reklamasjonshode. */
      IF plReklamasjonsNr = 0 THEN
          RUN OpprettReklamasjon (OUTPUT plReklamasjonsNr).
      /* Henter reklamasjonshode. */
      FIND Reklamasjonslogg NO-LOCK WHERE
          Reklamasjonslogg.ReklamasjonsNr = plReklamasjonsNr NO-ERROR.

      /* Linjeteller */
      IF piLinjeNr = 0 THEN
      DO:
          FIND LAST ReklamasjonsLinje OF Reklamasjonslogg NO-ERROR.
          IF AVAILABLE Reklamasjonslinje THEN
              piLinjeNr = Reklamasjonslinje.LinjeNr + 1.
          ELSE
              piLinjeNr = 1.
      END.
      ELSE
          piLinjeNr = piLinjeNr + 1.

      DO TRANSACTION:
          /* Skaper Reklamasjonslinje */
          CREATE Reklamasjonslinje.
          ASSIGN
              Reklamasjonslinje.ReklamasjonsNr = Reklamasjonslogg.ReklamasjonsNr
              Reklamasjonslinje.LinjeNr        = piLinjeNr
              .
          ASSIGN
              Reklamasjonslinje.Butik          = BongHode.ButikkNr
              Reklamasjonslinje.ArtikkelNr     = dec(BongLinje.ArtikkelNr)
              Reklamasjonslinje.TTId           = BongLinje.TTId
              Reklamasjonslinje.BongId         = BongHode.BongNr
              Reklamasjonslinje.BongLinjeNr    = BongLinje.LinjeNr
              Reklamasjonslinje.KassaNr        = BongHode.KasseNr
              Reklamasjonslinje.Storl          = BongLinje.Storrelse
              Reklamasjonslinje.Dato           = BongLinje.TransDato 
              Reklamasjonslinje.Tid            = BongLinje.TransTid
              Reklamasjonslinje.SeqNr          = BongLinje.SeqNr
              Reklamasjonslinje.ForsNr         = Bonghode.KassererNr
              Reklamasjonslinje.SelgerNr       = BongHode.SelgerNr
              Reklamasjonslinje.SolgtIButikk   = 0
              Reklamasjonslinje.SolgtBongId    = 0
              Reklamasjonslinje.SolgtForsNr    = 0
              Reklamasjonslinje.SolgtDato      = ?
              Reklamasjonslinje.FeilKode       = BongLinje.FeilKode
              Reklamasjonslinje.Antall         = ABS(BongLinje.Antall)
              Reklamasjonslinje.Pris           = IF BongLinje.TTId = 3
                                                   THEN BongLinje.LinjeSum
                                                   ELSE BongLinje.LinjeRab
              Reklamasjonslinje.SubtotalRab    = IF BongLinje.TTId = 3
                                                   THEN BongLinje.SubtotalRab
                                                   ELSE 0
              Reklamasjonslinje.RabKr          = IF BongLinje.TTId = 3
                                                   THEN (BongLinje.LinjeRab + BongLinje.SubTotalRab)
                                                   ELSE 0
              Reklamasjonslinje.Mva            = BongLinje.MvaKr /* Regnes senere om for 001 */
              Reklamasjonslinje.VVareKost      = dVVareKost
              .

          /* Beregner Mva av rabattbelï¿½pet for reklamasjonsrabatt */
          IF Bonglinje.TTId = 1 THEN
              ASSIGN
                  Reklamasjonslinje.Mva = dec(DYNAMIC-FUNCTION('Mva2LinjeRab':U))
                  .
          ASSIGN
              Reklamasjonslinje.ReklamVerdi    = IF CAN-DO('3,4',STRING(BongLinje.TTId))
                                                   THEN (BongLinje.LinjeSum - 
                                                         BongLinje.SubTotalRab -
                                                         BongLinje.LinjeRab) * ABS(BongLinje.Antall)                                                          
                                                   ELSE (Bonglinje.LinjeRab -
                                                         Reklamasjonslinje.Mva)
              Reklamasjonslinje.ReklamUtgifter = 0
              Reklamasjonslinje.ReklamTotal    = Reklamasjonslinje.VVarekost * ABS(BongLinje.Antall)
              Reklamasjonslinje.AkseptertVerdi = Reklamasjonslinje.ReklamVerdi
              Reklamasjonslinje.Vg             = ArtBas.Vg
              Reklamasjonslinje.LopNr          = ArtBas.LopNr
              Reklamasjonslinje.Varetekst      = ArtBas.Beskr
              Reklamasjonslinje.LevKod         = ArtBas.LevKod
              Reklamasjonslinje.FeilNotat      = BongLinje.NotatKodeTekst
              Reklamasjonslinje.TransNr        = BongLinje.TransNr
              /* Reklamasjoner fra kasse er allerede oppdatert mot lager og statistikker */
              .

          RELEASE Reklamasjonslinje.
      END. /* TRANSACTION */

      /* Setter summer */
      IF plReklamasjonsNr > 0 THEN
          RUN reklamasjonslogg_recalc.p (STRING(plReklamasjonsNr),?,'',OUTPUT ocValue,OUTPUT obOk).

  END. /* TRANSRAD. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-reservasjonsBilag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reservasjonsBilag Procedure 
PROCEDURE reservasjonsBilag :
/*------------------------------------------------------------------------------
  Purpose:     Posterer salg og bruk av gavekort. 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Overfï¿½ringstransaksjon fra nettbutikk, med neg. antall, er reservasjon. */
IF CAN-FIND(FIRST BongLinje WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.Makulert = FALSE AND
    CAN-DO("006",STRING(BongLinje.TTId,"999")) AND
    BongLinje.Antall < 0) THEN
DO:
    /* Sjekker at kassen er av type nettbutikk. */
    FIND FIRST Kasse NO-LOCK WHERE
        Kasse.ButikkNr = BongHode.ButikkNr AND
        Kasse.GruppeNr = BongHode.GruppeNr AND
        Kasse.KasseNr  = BongHode.KasseNr NO-ERROR.
    IF AVAILABLE Kasse THEN
    DO:
        /* Er det nettbutikk, skal reservasjon sendes pï¿½ eMail. */
        IF Kasse.ModellNr = 11 THEN
            RUN webreservasjonPDF.p (INPUT BongHode.B_Id).
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SettZNummer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettZNummer Procedure 
PROCEDURE SettZNummer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN
        iZNr = 0
        .

    /* Her settes nytt z-nummer for kassarapportene. */
    Z_NR:
    DO TRANSACTION:
        FIND z_nummer WHERE 
            z_nummer.butikk = DataSett.ButikkNr AND
                z_nummer.kasse  = DataSett.KasseNr
                EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE z_nummer THEN
        DO:
            /* Skaper og initialiserer filen. */
            CREATE z_nummer.
            ASSIGN 
                z_nummer.butikk   = DataSett.ButikkNR
                    z_nummer.kasse    = DataSett.KasseNr
                    z_nummer.z_nummer = 1
                .
        END.
        ELSE 
            ASSIGN
                z_nummer.z_nummer = z_nummer.z_nummer + 1
                .
        
        /* Kontrollerer om nummeret har g}tt over grensen. */
        IF z_nummer.z_nummer > 9999 THEN 
            z_nummer = 1.
        
        /* setter z_nummer i variabelen. */
        ASSIGN
            iZNr = z_nummer.z_nummer
            .
        IF AVAILABLE z_nummer THEN
            RELEASE z_nummer.
    END. /* Z_NR TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetVVareKost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetVVareKost Procedure 
PROCEDURE SetVVareKost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER pdArtikkelNr AS DEC                   NO-UNDO.
  DEF INPUT  PARAMETER piButikkNr   AS INT                   NO-UNDO.
  DEF INPUT  PARAMETER pdPris       AS DEC                   NO-UNDO.
  DEF OUTPUT PARAMETER pdVVareKost  LIKE BongLinje.VVareKost NO-UNDO. 


  /* Henter varekost i butikken det overfï¿½res fra. */      
  /* Dette er pris eExMva.                         */
  FIND Lager NO-LOCK WHERE
      Lager.ArtikkelNr = pdArtikkelNr AND
      Lager.Butik      = piButikkNr NO-ERROR.
  IF AVAILABLE Lager THEN
      pdVVarekost = Lager.VVareKost.
  ELSE 
      pdVVareKost = 0.

  /* Sjekker om varekost er satt.                                       */
  /* Er det ikke satt noen varekost, merkes transaksjonen med feilkode. */
  IF pdVVareKost = 0 THEN /* or wBrutto% *** Skal ogsï¿½ utfï¿½res for brutto% artikkler */
    DO:
      IF VALID-HANDLE(h_PrisKo) THEN
        RUN HentVareKost IN h_PrisKo (INPUT  pdArtikkelNr, 
                                      INPUT  piButikkNr, 
                                      INPUT  pdPris, 
                                      OUTPUT pdVVareKost).
    END.
  IF pdVVareKost = ? THEN
      pdVVareKost = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SjekkKundeMedlem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkKundeMedlem Procedure 
PROCEDURE SjekkKundeMedlem :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.
  DEFINE VARIABLE lokMedlemsNr AS DECIMAL NO-UNDO.
  
  /* Kundekort */
  IF BongHode.KundeKort <> '' AND NOT CAN-FIND(Kunde WHERE Kunde.KundeNr = BongHode.KundeNr) THEN 
  DO:
      /* TN Her opprettes kunder som ikke finnes fra fï¿½r. */
      IF NOT CAN-FIND(FIRST KundeKort WHERE
                      KundeKort.KortNr = BongHode.KundeKort)
        /* Er medlemskortet lagt pï¿½ plass, legges det inn her. Hvis ikke pï¿½fï¿½res det senere i SjekkMedlem. */ 
        THEN RUN OpprettKundeMedlem (0,BongHode.KundeKort).
      ELSE DO:
        FIND FIRST KundeKort NO-LOCK WHERE
            KundeKort.KortNr = BongHode.KundeKort NO-ERROR.
        IF AVAILABLE KundeKort THEN 
          FIND Kunde OF KundeKort NO-LOCK NO-ERROR.
        IF AVAILABLE Kunde THEN 
          ASSIGN 
          BongHode.KundeNr   = Kunde.KundeNr
          BongHode.KundeNavn = Kunde.Navn.
      END.                  
  END.
  
  /* PRS POS og bare kunde er angitt. */
  ELSE IF BongHode.KundeNr > 0 THEN 
  DO:
      FIND Kunde NO-LOCK WHERE Kunde.KundeNr = BongHode.KundeNr NO-ERROR.
      IF AVAILABLE Kunde THEN 
      DO:
          FIND FIRST KundeKort OF Kunde NO-LOCK NO-ERROR.
          ASSIGN 
              BongHode.KundeNr   = Kunde.KundeNr
              BongHode.KundeNavn = Kunde.Navn.
          IF AVAILABLE KundeKort THEN 
            BongHode.KundeKort = KundeKort.KortNr.
      END.    
  END.
  
  /* Medlemskort */
  IF BongHode.MedlemsKort <> '' THEN 
  DO:
      /* Sjekker om medlemskortet finnes og om det er det samme medlemmet som er lagt opp pï¿½ bongen fra fï¿½r. */
      /* Medlemsnr. er blitt pï¿½fï¿½rt i transtype 62 hvis kunden ble opprettet.                                */
      FIND FIRST Medlemskort NO-LOCK WHERE
        MedlemsKort.KortNr = BongHode.MedlemsKort NO-ERROR.
      IF AVAILABLE MedlemsKort THEN 
        DO:
          /* Finnes medlemmet og medlemsnr er forskjellig fra det som er lagt pï¿½ ved behandling av transtype 62, skal */
          /* medlemsnummeret byttes ut. Er det ikke lagt inn medlemsnr, skal det pï¿½fï¿½res.                             */
          IF BongHode.MedlemsNr <> MedlemsKort.MedlemsNr THEN 
            DO:
              FIND Medlem OF MedlemsKort NO-LOCK NO-ERROR.
              IF AVAILABLE Medlem THEN 
              ASSIGN 
                BongHode.MedlemsNr  = Medlem.MedlemsNr
                BongHode.MedlemNavn = Medlem.ForNavn + Medlem.Etternavn
                .
            END.
        END.
      /* Ukjent medlemskort.                                                  */                                                   
      ELSE DO:
        /* Er det lagt opp en kunde, skal det legges opp nytt medlem pï¿½ kunden. */
        IF BongHode.Kundenr > 0 THEN
          RUN OpprettKundeMedlem(BongHode.KundeNr, BongHode.MedlemsKort).
        /* Finnes ikke en kunde, skal det genereres kunde og medlem. */
        ELSE
          RUN OpprettKundeMedlem(0, BongHode.MedlemsKort).
      END.  
  END.

  lokMedlemsNr = 0.
  IF BongHode.KundeNr > 0 AND BongHode.MedlemsNr = 0 THEN 
  DO:
      FOR EACH Medlem NO-LOCK WHERE 
        Medlem.KundeNr = BongHode.KundeNr:
        IF lokMedlemsNr = 0 THEN lokMedlemsNr = Medlem.MedlemsNr.
          iAnt = iAnt + 1.
      END.
      IF iAnt = 1 THEN 
          BongHode.MedlemsNr = lokMedlemsNr. 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SpesAvReturer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SpesAvReturer Procedure 
PROCEDURE SpesAvReturer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

TRANSRAD:
FOR EACH BongLinje NO-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.Makulert = FALSE AND
    CAN-DO("010",STRING(BongLinje.TTId,"999")):
    /* Skaper konto post. */
    CREATE konto.

    /* Setter opp index. */
    ASSIGN konto.butikk      = BongLinje.ButikkNr
           konto.kasse       = BongLinje.KasseNr
           konto.dato        = BongLinje.TransDato
           konto.kontonummer = 999999999.

    /* Legger over informasjonen. */
    ASSIGN konto.vg          = BongLinje.VareGr
           konto.lopnr       = BongLinje.LopeNr
           konto.storl       = BongLinje.Storrelse
           konto.pris        = BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab
           konto.antal       = BongLinje.Antall
           konto.kvitto      = BongLinje.BongNr
           konto.forsnr      = BongHode.KassererNr.
END. /* TRANSRAD */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TellOppBonger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TellOppBonger Procedure 
PROCEDURE TellOppBonger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      iTotAntBonger = 0
      iStart        = TIME
      .
  FOR EACH BongHode NO-LOCK WHERE
    BongHode.DataSettId = lDataSettId:  
    ASSIGN
        iTotAntBonger = iTotAntBonger + 1
        .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TransLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TransLogg Procedure 
PROCEDURE TransLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR plWork        AS DEC  NO-UNDO.
DEF VAR pcTekst       AS CHAR NO-UNDO.
DEF VAR piTransNr     AS INT  NO-UNDO.
DEF VAR pcTTId        AS CHAR NO-UNDO.
DEF VAR piTTId        AS INT  NO-UNDO.
DEF VAR piTBId        AS INT  NO-UNDO.
DEF VAR piButikkNr    AS INT  NO-UNDO.
DEF VAR piMButikkNr   AS INT  NO-UNDO.
DEF VAR piEtikettBut  AS INT  NO-UNDO.
DEF VAR bEtikettKasse AS LOG  NO-UNDO.
DEF VAR bEtiBong      AS LOG  NO-UNDO.
DEFINE VARIABLE lMvaKr AS DECIMAL NO-UNDO.
DEFINE VARIABLE vVarekost AS DECIMAL NO-UNDO.

DEFINE VARIABLE iSeq AS INTEGER    NO-UNDO.
DEFINE        VARIABLE  cInfoRad1 AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cInfoRad2 AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cInfoRad3 AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cInfoRad4 AS CHARACTER  NO-UNDO.

DEF BUFFER ovButiker FOR Butiker.

DEF VAR flVaremottak AS LOG NO-UNDO.
DEFINE BUFFER anbefaltPris FOR artpris.
/* Tï¿½mmer etikettlogg */
FOR EACH EtikettLogg:
    DELETE Etikettlogg.
END.
                                               
/* KonvReg */
pcTekst = "".
{syspara.i 1 2 2 pcTekst} /* RESTPAR */

/* Sjekker om det er pakkseddel varemottak som kommer fra kassen. */
/* Pakkseddel varemottak fra kasse skal ikke behandles her.       */
IF CAN-FIND(FIRST BongLinje WHERE
                BongLinje.B_Id = BongHode.B_Id AND
                BongLinje.Makulert = FALSE AND
                CAN-DO("026",STRING(BongLinje.TTId,"999"))) THEN 
  RETURN.

/* Sjekker om det er kundeordre fra kassen. Det skal ikke behandles her. */
/* Disse varene skal inn i kundeordre, og fï¿½rst trekkes fra lager nï¿½r    */
/* de faktureres.                                                        */
IF CAN-FIND(FIRST BongLinje WHERE 
                BongLinje.B_Id = BongHode.B_Id AND
                BongLinje.Makulert = FALSE AND
                CAN-DO("027",STRING(BongLinje.TTId,"999"))) THEN 
  RETURN.

ASSIGN
    flVaremottak = FALSE
    .
/* Er det en bong med etiketter pï¿½, skal allt pï¿½ bongen som ikke  */
/* er etiketter ignoreres.                                        */
/* Dette gjï¿½res hvis etikett er registrert som vanlig vareslag pï¿½ */
/* samme bongen.                                                  */
ETIKETTSJEKK:
FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.Makulert = FALSE AND
    CAN-DO("024,025",STRING(BongLinje.TTId,"999")):
    ASSIGN
        bEtiBong = TRUE.
END. /*ETIKETTSJEKK */

/* Makulerer allt annet pï¿½ bongen.                                                         */
/* Dette er nï¿½dvendig da selgerne ofte registrerer feil og fï¿½rst legger inn varesalg feil. */
IF bEtiBong THEN
    MAKULER:
    FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
        BongLinje.B_Id = BongHode.B_Id AND
        BongLinje.Makulert = FALSE AND
        CAN-DO("001,002,003,004,005,006,007,009,010,011,012",STRING(BongLinje.TTId,"999")):
        ASSIGN
            BongLinje.Makulert = TRUE.
    END. /*MAKULER */

bEtikettKasse = FALSE.

/* Legger opp transaksjonene i translogg. Kun salgstransaksjoner. */
/* TRANSNR og SEQNR pï¿½fï¿½rs bonglinjene her.                       */
SKAPELSEN:
FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
    BongLinje.B_Id = BongHode.B_Id AND
    BongLinje.Makulert = FALSE AND
    /* 009 - Svinntransaksjoner skal ikke tas med her. De blir lagt inn i en lokasjonliste i varetellingsmodulen */
    CAN-DO("001,002,003,004,005,006,007,010,011,012,024,025,109",STRING(BongLinje.TTId,"999")):

    IF CAN-DO("005,024,025",STRING(BongLinje.TTId,"999")) THEN
        ASSIGN
        piEtikettBut = BongLinje.ButikkNr
        flVaremottak = TRUE
        .

    IF AVAILABLE ArtBas  THEN RELEASE ArtBas. 
    IF AVAILABLE ArtPris THEN RELEASE ArtPris.
    IF AVAILABLE Lager   THEN RELEASE Lager.
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = BongLinje.ButikkNr NO-ERROR.
    IF NOT AVAILABLE Butiker THEN
        NEXT SKAPELSEN.

    ASSIGN
        piTTId      = BongLinje.TTId
        piTBId      = BongLinje.TBId
        pcTTId      = STRING(BongLinje.TTId,"999")
        piButikkNr  = BongLinje.ButikkNr
        piMButikkNr = BongLinje.MButikkNr
        vVarekost   = 0
        .

    /* Setter transaksjonsnummer  */
    IF piTransNr = 0 THEN
      DO:
        FIND LAST TransLogg WHERE
          TransLogg.Butik = BongLinje.ButikkNr
          USE-INDEX TransLogg NO-ERROR.
        IF AVAILABLE TransLogg THEN
          piTransNr = TransLogg.TransNr + 1.
        ELSE
          piTransNr = 1.
      END.
    ELSE
      piTransNr = piTransNr + 1.

    IF DECIMAL(BongLinje.ArtikkelNr) > 0 THEN 
      FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = DECIMAL(BongLinje.ArtikkelNr) NO-ERROR.
    IF AVAILABLE ArtBas THEN
      ASSIGN
        BongLinje.VareGr = ArtBas.VG
        BongLinje.LopeNr = ArtBas.LopNr.
    IF AVAILABLE ArtBas THEN
      DO:
        /* Sjekker om artikkelen har vï¿½rt nullstilt. */
        /* Har den det, skal Vg/LopNr byttes ut.     */
        IF pcTekst <> "" THEN
          DO:
            FIND FIRST KonvReg NO-LOCK WHERE
                 KonvReg.EDB-System = pcTekst AND
                 KonvReg.Tabell     = "" AND
                 KonvReg.EkstId     = string(ArtBas.ArtikkelNr) NO-ERROR.
            IF AVAILABLE KonvReg THEN
              DO:
                FIND ArtBas NO-LOCK WHERE
                  ArtBas.ArtikkelNr = DEC(KonvReg.InterntId) NO-ERROR.
                IF AVAILABLE ArtBas THEN
                  ASSIGN
                    BongLinje.VareGr = ArtBas.VG
                    BongLinje.LopeNr = ArtBas.LopNr.
              END.
          END.
        FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
      END.

    vVarekost = BongLinje.VVareKost.
    IF AVAILABLE ArtBas AND (vVareKost = 0 OR vVareKost = ?) THEN 
        FIND Lager NO-LOCK WHERE 
             Lager.ArtikkelNr = DECIMAL(BongLinje.ArtikkelNr) AND 
             Lager.Butik = BongLinje.ButikkNr NO-ERROR.
    IF AVAILABLE Lager AND (vVareKost = 0 OR vVareKost = ?) THEN 
        vVareKost = Lager.vVarekost.
    IF vVareKost = ? THEN vVareKost = 0.

    IF AVAILABLE ArtBas THEN 
    DO:
        FIND ArtPris OF ArtBas NO-LOCK WHERE
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
          FIND FIRST ArtPris OF ArtBas NO-ERROR.
        FIND FIRST anbefaltPris OF Artbas NO-LOCK NO-ERROR.
    END.
    IF AVAILABLE ArtPris AND 
        (vVareKost = 0 OR vVareKost = ?) 
      THEN vVareKost = ArtPris.VareKost[1].

    /* Sjekker om det er overfï¿½ring til minusbutikk.         */
    /* Overf. fra kasse til minusbutikk skal gï¿½ andre veien. */
    IF BongLinje.TTId = 6 AND BongLinje.MButikkNr > 0 THEN
    DO:
        FIND ovButiker NO-LOCK WHERE
            ovButiker.Butik = BongLinje.MButikkNr NO-ERROR.
        IF AVAILABLE ovButiker AND ovButiker.Minusbutikk THEN
            ASSIGN
            piButikkNr  = BongLinje.MButikkNr
            piMButikkNr = BongLinje.ButikkNr
            .
    END.
    /* Bestilling av enkeltetiketter fra kassen */
    IF CAN-DO("024",STRING(BongLinje.TTId,"999")) THEN
    ETIKETTER:
    DO:
        IF bEtikettKasse = FALSE THEN
        DO: /* Oppretter SLUTT etikett (Den skrives ut fï¿½rst). */
            ASSIGN cInfoRad1 = "Etiketter fra kasse"
                   cInfoRad3 = IF AVAIL Butiker THEN Butiker.butnamn ELSE ""
                   cInfoRad4 = "SLUTT"
                   iSeq      = iSeq + 1.
            CREATE EtikettLogg.
            ASSIGN
            EtikettLogg.Butik     = BongLinje.ButikkNr /* Det skal skrives ut i seqnr ordning. */
            EtikettLogg.Vg        = 0   
            EtikettLogg.LopNr     = 0
            EtikettLogg.Ant       = 0
            EtikettLogg.Storl     = "INFO"
            EtikettLogg.Bongtekst = cInfoRad1 + CHR(1) + cInfoRad2 + CHR(1) + cInfoRad3 + CHR(1) + cInfoRad4
            EtikettLogg.Pris      = 0
            EtikettLogg.Pris2     = 0
            EtikettLogg.SeqNr     = iSeq.
        END.
        ASSIGN bEtikettKasse = TRUE.
        IF dec(BongLinje.ArtikkelNr) = 0 THEN
            NEXT.
        IF NOT CAN-FIND(Strekkode WHERE StrekKode.Kode = BongLinje.Strekkode) THEN
            NEXT.
        IF NOT AVAIL ArtBas THEN
            NEXT.
        IF NOT AVAIL ArtPris THEN
            NEXT.

        ASSIGN iSeq = iSeq + 1.
        CREATE EtikettLogg.
        ASSIGN EtikettLogg.Butik     = BongLinje.ButikkNr
               EtikettLogg.Vg        = ArtBas.Vg
               EtikettLogg.LopNr     = ArtBas.LopNr
               EtikettLogg.Ant       = BongLinje.Antall
               EtikettLogg.Storl     = BongLinje.StrekKode
               EtikettLogg.Bongtekst = ArtBas.BongTekst
               EtikettLogg.Pris      = ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
               EtikettLogg.Pris2     = IF AVAIL anbefaltPris THEN anbefaltPris.Pris[1] ELSE ArtBas.AnbefaltPris
               /*EtikettLogg.Pris2     = IF ArtPris.Tilbud THEN ArtPris.Pris[1] ELSE 0*/
               EtikettLogg.SeqNr     = iSeq.

    END. /* ETIKETTER */
    /* Mottat av elektronisk pakkseddel og etikettutskrift fra denne. */
    /* Er pakkseddelen ikke mottatt fra fï¿½r, innleveres den. Mens det */
    /* alltid skrives ut etiketter pï¿½ hele pakkseddelen.              */
    ELSE IF CAN-DO("025",STRING(BongLinje.TTId,"999")) THEN
    ETIKETTER:
    DO:
      ASSIGN 
         BongLinje.Antall = DEC(BongLinje.LinjeSum) * 10
         bSkrivEtikett    = TRUE. /* Default er at etikett skal skrives ut */
      IF NUM-ENTRIES(BongLinje.Bongtekst,' ') = 3 THEN 
        bSkrivEtikett = ENTRY(3,BongLinje.Bongtekst,' ') = 'J'. /* Bruker har angitt om etiket skal skrives ut i kassen */  
      RUN asPakkseddel.p (BongLinje.ButikkNr, BongLinje.Antall, bEtikettKasse, bSkrivEtikett, OUTPUT obOk, OUTPUT ocReturn).
      RUN bibl_loggDbFri.p ('PakkseddelInnlevFraKasse', 'xoverforbong.p: Translogg: Etiketter og innlever pakkseddel' 
                          + ' Butikk: '     + STRING(BongLinje.ButikkNr)
                          + ' Pakkseddel: ' + STRING(BongLinje.Antall)
                          + ' Etikett: '    + STRING(bSkrivEtikett)
                          + ' Ok: '         + STRING(obOk)    
                          ).
      
    END. /* ETIKETTER */
    ELSE
    TRANSLOGGEN:
    DO:
        /* Oppretter TransLogg */    
        CREATE TransLogg.
        NYTRANSLOGG:
        DO WHILE TRUE ON ERROR UNDO, RETRY:
            ASSIGN TransLogg.Butik        = piButikkNr
                   TransLogg.TransNr      = piTransNr
                   TransLogg.SeqNr        = 1
                   /* Setter inn pekere pï¿½ transaksjonene */
                   BongLinje.TransNr      = piTransNr
                   BongLinje.SeqNr        = 1
                   NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                piTransNr = piTransNr + 1.
            ELSE LEAVE NYTRANSLOGG.
        END. /* NYTRANSLOGG */

        IF (BongLinje.VVareKost = 0 OR BongLinje.VVareKost = ?) THEN
            ASSIGN 
                BongLinje.VVareKost     = vVareKost * ABS(BongLinje.Antall)
                TransLogg.SattVVarekost = TRUE. 
        ASSIGN
               TransLogg.BatchNr      = iBatchNr
               TransLogg.TTId         = BongLinje.TTId
               TransLogg.TBId         = BongLinje.TBId
               TransLogg.ArtikkelNr   = IF AVAILABLE ArtBas
                                          THEN ArtBas.ArtikkelNr
                                          ELSE 0
               TransLogg.Vg           = BongLinje.VareGr
               TransLogg.LopNr        = BongLinje.LopeNr
               TransLogg.Antall       = BongLinje.Antall
               TransLogg.Pris         = BongLinje.LinjeSum / ABSOLUTE(BongLinje.Antall)
               TransLogg.Pris         = IF TransLogg.Pris = ?
                                          THEN 0
                                          ELSE TransLogg.Pris
               TransLogg.RabKr        = (BongLinje.LinjeRab + BongLinje.SubtotalRab) / absolute(BongLinje.Antall)
               TransLogg.RabKr        = IF TransLogg.RabKr = ?
                                          THEN 0
                                          ELSE TransLogg.RabKr
               TransLogg.KundNr       = BongHode.KundeNr

               TransLogg.LevNr        = IF AVAILABLE ArtBas
                                          THEN ArtBas.LevNr
                                          ELSE 0
               TransLogg.OvButik      = IF BongLinje.TTId = 6 THEN piMButikkNr ELSE 0
               TransLogg.OvTransNr    = IF BongLinje.TTId = 6 THEN TransLogg.TransNr ELSE 0
               TransLogg.BongId       = BongLinje.BongNr
               TransLogg.BongLinjeNr  = BongLinje.LinjeNr
               TransLogg.KassaNr      = BongLinje.KasseNr
               TransLogg.ForsNr       = BongHode.KassererNr
               TransLogg.Plukket      = IF BongLinje.TTId = 6 THEN TRUE ELSE FALSE
               TransLogg.Dato         = BongLinje.TransDato
               TransLogg.Tid          = BongLinje.TransTid
               TransLogg.SelgerNr     = BongHode.SelgerNr
               TransLogg.BestNr       = 0
               TransLogg.Postert      = FALSE
               TransLogg.KortNr       = (IF BongHode.KortType = 2
                                           THEN BongHode.KundeKort
                                           ELSE BongHode.MedlemsKort)
               TransLogg.KundNr       = BongHode.KundeNr
               TransLogg.MedlemsNr    = BongHode.MedlemsNr
               TransLogg.KortType     = BongHode.KortType
               TransLogg.RefNr        = BongLinje.RefNr
               TransLogg.RefTekst     = BongLinje.RefTekst
               Translogg.Kode         = Bonglinje.Strekkode
               Translogg.BongTekst    = BongLinje.BongTekst
               TransLogg.VVareKost    = BongLinje.VVareKost / ABS(BongLinje.Antall)
               TransLogg.VVareKost    = IF TransLogg.VVareKost = ? THEN 0 ELSE Translogg.VVareKost
               TransLogg.SattVVarekost = (IF AVAILABLE ArtBas AND ArtBas.Lager = TRUE 
                                            THEN FALSE 
                                          ELSE IF CAN-DO("1,3,10",STRING(Translogg.TTId))
                                            THEN TRUE /* Skal ikke regnes om ved opp. av statistikker. */
                                          ELSE FALSE)
               TransLogg.KalkylePris  = IF AVAILABLE ArtPris
                                          THEN ArtPris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                          ELSE Translogg.KalkylePris
               TransLogg.Varekost     = IF AVAILABLE ArtPris
                                          THEN ArtPris.Varekost[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                          ELSE TransLogg.Varekost
               TransLogg.Pris         = (IF BongLinje.TTId = 5 
                                          THEN  TransLogg.Varekost 
                                          ELSE TransLogg.Pris)                                          
               TransLogg.Mva          = (dec(BongLinje.MvaKr / ABSOLUTE(BongLinje.Antall)))
               Translogg.Mva          = (IF Translogg.Mva = ? THEN 0 ELSE Translogg.Mva)
               TransLogg.Mva%         = (IF AVAILABLE ArtPris
                                           THEN ArtPris.Mva%[IF ArtPris.Tilbud THEN 2 ELSE 1]
                                           ELSE (TransLogg.Mva / (TransLogg.Pris - TransLogg.Mva - TransLogg.RabKr)) * 100)
               Translogg.Mva%         = (IF Translogg.Mva% = ? THEN 0 ELSE Translogg.Mva%)
               Translogg.Generellrabatt         = BongLinje.Generellrabatt / absolute(BongLinje.Antall)
               Translogg.Tilbudsrabatt          = BongLinje.Tilbudsrabatt / absolute(BongLinje.Antall)
               Translogg.MixMatchRabatt         = BongLinje.MixMatchRabatt / absolute(BongLinje.Antall)
               Translogg.Medlemsrabatt          = BongLinje.Medlemsrabatt / absolute(BongLinje.Antall)
               Translogg.Kunderabatt            = BongLinje.Kunderabatt / absolute(BongLinje.Antall)
               Translogg.Personalrabatt         = BongLinje.Personalrabatt / absolute(BongLinje.Antall)
               Translogg.AlternativPrisRabatt   = BongLinje.AlternativPrisRabatt / absolute(BongLinje.Antall)
               Translogg.ManuelEndretPrisRabatt = BongLinje.ManuelEndretPrisRabatt / absolute(BongLinje.Antall)
               .
        /* Overstyrer for 002-Brekkasje, 005-Varekjï¿½p, 006-Overfï¿½ring og 011-Internt forbruk. */
        /* Disse transaksjonene skal hï¿½ndteres til varekost.                  */
        IF CAN-DO('002,005,006,011',STRING(BongLinje.TTId,"999")) THEN  
        DO:
          ASSIGN
            TransLogg.Pris          = TransLogg.vVarekost
            TransLogg.RabKr         = 0
            TransLogg.SubtotalRab   = 0
            TransLogg.Mva           = 0
            TransLogg.Mva%          = 0
            .
        END.
        
        /* TN 9/10-08 Kun registrerte kampanjer skal posteres. */
        IF BongLinje.KampId > 0 AND 
           CAN-FIND(KampanjeMixMatch WHERE
                    KampanjeMixMatch.KampId = BongLinje.KampId) THEN
        KAMPANJESTATISTIKK:
        DO:
            ASSIGN               
                   Translogg.KampId       = BongLinje.KampId /*dec(string(BongLinje.KampEierId) + string(BongLinje.KampId,"9999"))*/
                   Translogg.KampEier     = BongLinje.KampEier
                   Translogg.KampTilbId   = BongLinje.KampTilbId
                   .
        END. /* KAMPANJESTATISTIKK */

        ASSIGN TransLogg.Storl        = FixStorl(BongLinje.Storrelse)
               TransLogg.TilStorl     = TransLogg.Storl
               .
    END. /* TRANSLOGGEN */
END. /* SKAPELSEN */

/* Etiketter for varemottak */
IF flVaremottak OR bEtikettKasse THEN
ETIKETT:
DO:
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = piEtikettBut NO-ERROR.
    IF NOT AVAILABLE Butiker THEN
        LEAVE ETIKETT.
  
    IF CAN-FIND(FIRST EtikettLogg) THEN
    DO: /* Oppretter START etikett. Den skrives ut sist. */
        ASSIGN cInfoRad4 = "START"
               iSeq      = iSeq + 1.
        CREATE EtikettLogg.
        ASSIGN
          EtikettLogg.Butik     = piEtikettBut /* Det skal skrives ut i seqnr ordning. */
          EtikettLogg.Vg        = 0   
          EtikettLogg.LopNr     = 0
          EtikettLogg.Ant       = 0
          EtikettLogg.Storl     = "INFO"
          EtikettLogg.Bongtekst = cInfoRad1 + CHR(1) + cInfoRad2 + CHR(1) + cInfoRad3 + CHR(1) + cInfoRad4
          EtikettLogg.Pris      = 0
          EtikettLogg.Pris2     = 0
          EtikettLogg.SeqNr     = iSeq.

        /* Starter etikettutskrift. */
        RUN x-etikettstd.w (Butiker.BELayout,Butiker.BEPrinter,Butiker.BETerminalklient).
    END.

    ASSIGN
        flVaremottak = FALSE
        piEtikettBut = 0
        .
END. /* ETIKETT */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Varemottak) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Varemottak Procedure 
PROCEDURE Varemottak :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:          Her hï¿½ndteres varemottak i kasse. Bongen posteres som en 
                                  pakkseddel. I kassen er det regsitrert ordre og pakkseddelnr, 
                                  samt at alle linjene er skannet inn som varelinjer pï¿½ bongen.
                                  
                                  Denne rutinen skal ikke forveksles med rutinen som 
                                  tar hï¿½ndt om transkode 25 i subrutine Translogg. Der er det bare
                                  pakkseddelnr. som registreres i kassen, og selve pakkseddelen
                                  er mottatt elektronisk.
                                                                                                                                                                  
        ------------------------------------------------------------------------------*/
DEFINE VARIABLE iSeq          AS INTEGER   NO-UNDO.
DEFINE VARIABLE cInfoRad1     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInfoRad2     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInfoRad3     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInfoRad4     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEkstId       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPkSdlNr      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPkSdlId      AS DECIMAL   NO-UNDO.

/* Tï¿½mmer tmp-tabell */
FOR EACH ttPkSdlHode:
  DELETE ttPkSdlHode.
END.
FOR EACH ttpkSdlLinje:
  DELETE ttpkSdlLinje.
END.
/* Tï¿½mmer etikettlogg */
FOR EACH EtikettLogg:
    DELETE Etikettlogg.
END.

FOR EACH tt2BongLinje:
    DELETE tt2BongLinje.
END.
/* Legger opp ttBongLinje for overfï¿½ring til varemottaksrutine. */
FOR EACH BongLinje WHERE
         BongLinje.B_Id = BongHode.B_Id AND
         BongLinje.Makulert = FALSE:
    CREATE tt2BongLinje.
    BUFFER-COPY BongLinje TO tt2BongLinje.
END.

/* Sjekker om det er varemottak. */
FIND FIRST BongLinje WHERE
           BongLinje.B_Id = BongHode.B_Id AND
           BongLinje.Makulert = FALSE AND
           CAN-DO("026",STRING(BongLinje.TTId,"999")) NO-LOCK NO-ERROR.
IF NOT AVAILABLE BongLinje THEN RETURN.
ASSIGN
  cEkstId       = TRIM(ENTRY(1,BongLinje.BongTekst,'|'))
  cPkSdlNr      = TRIM(ENTRY(2,BongLinje.BongTekst,'|'))
  cEtikett      = IF NUM-ENTRIES(BongLinje.BongTekst,'|') > 2 THEN 
                    TRIM(ENTRY(3,BongLinje.BongTekst,'|'))
                  ELSE ''
  cInfoRad2     = cEkstId + '/' + cPkSdlNr
  iSeq          = 0
  .
FIND Butiker NO-LOCK WHERE
    Butiker.Butik = BongLinje.ButikkNr NO-ERROR.
IF NOT AVAILABLE Butiker THEN
    RETURN.

IF AVAILABLE ArtBas  THEN RELEASE ArtBas. 
IF AVAILABLE ArtPris THEN RELEASE ArtPris.

/* Behandler varemottak. Oppretter pakkseddel, posterer varemottak, skriver ut etiketter og gjï¿½r innleveranse. */
RUN asOpprettPakkseddlerInnlever.p (INPUT TABLE tt2BongLinje, cEkstID, cPkSdlNr, FALSE,OUTPUT lPkSdlId, OUTPUT cTekst).
 
FIND PkSdlHode NO-LOCK WHERE
  PkSdlHode.PkSdlId = lPkSdlId NO-ERROR.
  
IF NOT AVAILABLE PkSdlHode THEN 
  RETURN.

/* Kjï¿½rer varemottak. */
IF PkSdlHode.PkSdlStatus = 10 THEN 
DO:
  FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
    CREATE ttpkSdlLinje.              
    BUFFER-COPY pkSdlLinje TO ttpkSdlLinje.
  END.
  ihBuffer = BUFFER ttpkSdlLinje:HANDLE.              
  RUN pksdl_opprett_ordre.p ('', ihBuffer,'' ,OUTPUT ocReturn, OUTPUT obOk).
  RUN pksdl_innlever.p (USERID('SkoTex'), ihBuffer,'' ,OUTPUT ocReturn, OUTPUT obOk).
END.

IF CAN-DO('1,J,Ja,Yes,True',cEtikett) THEN 
ETIKETTER:
DO:
    ASSIGN cInfoRad1 = "Pakkseddel fra kasse"
           cInfoRad3 = IF AVAIL Butiker THEN Butiker.butnamn ELSE ""
           cInfoRad4 = "SLUTT"
           iSeq      = iSeq + 1.
    CREATE EtikettLogg.
    ASSIGN
    EtikettLogg.Butik     = BongLinje.ButikkNr /* Det skal skrives ut i seqnr ordning. */
    EtikettLogg.Vg        = 0   
    EtikettLogg.LopNr     = 0
    EtikettLogg.Ant       = 0
    EtikettLogg.Storl     = "INFO"
    EtikettLogg.Bongtekst = cInfoRad1 + CHR(1) + cInfoRad2 + CHR(1) + cInfoRad3 + CHR(1) + cInfoRad4
    EtikettLogg.Pris      = 0
    EtikettLogg.Pris2     = 0
    EtikettLogg.SeqNr     = iSeq.
  
    SKAPELSEN:
    FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
        FIND ArtBas WHERE Artbas.ArtikkelNr = DECIMAL(PkSdlLinje.ArtikkelNr) NO-LOCK NO-ERROR.
        IF NOT AVAIL ArtBas THEN
            NEXT.
        FIND FIRST ArtPris OF ArtBas WHERE 
            ArtPris.ProfilNr = Butiker.ProfilNr NO-LOCK NO-ERROR.
        FIND PkSdlPris NO-LOCK WHERE
          PkSdlPris.PkSdlId    = PkSdlLinje.PkSdlId AND 
          PkSdlPris.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
    
        ASSIGN iSeq = iSeq + 1.
        CREATE EtikettLogg.
        ASSIGN EtikettLogg.Butik     = BongLinje.ButikkNr
               EtikettLogg.Vg        = ArtBas.Vg
               EtikettLogg.LopNr     = (IF ArtBas.LopNr = ? THEN 0 ELSE ArtBas.LopNr)
               EtikettLogg.Ant       = PkSdlLinje.AntLevert
               EtikettLogg.Storl     = PkSdlLinje.Kode
               EtikettLogg.Bongtekst = PkSdlLinje.Beskr
               EtikettLogg.Pris      = PksdlPris.NyPris
               EtikettLogg.Pris2     = ArtBas.AnbefaltPris
               EtikettLogg.SeqNr     = iSeq.                         
    END. /* SKAPELSEN */

    ASSIGN cInfoRad4 = "START"
           iSeq      = iSeq + 1.
    CREATE EtikettLogg.
    ASSIGN
      EtikettLogg.Butik     = BongLinje.ButikkNr /* Det skal skrives ut i seqnr ordning. */
      EtikettLogg.Vg        = 0   
      EtikettLogg.LopNr     = 0
      EtikettLogg.Ant       = 0
      EtikettLogg.Storl     = "INFO"
      EtikettLogg.Bongtekst = cInfoRad1 + CHR(1) + cInfoRad2 + CHR(1) + cInfoRad3 + CHR(1) + cInfoRad4
      EtikettLogg.Pris      = 0
      EtikettLogg.Pris2     = 0
      EtikettLogg.SeqNr     = iSeq.

    /* Starter etikettutskrift. */
    RUN x-etikettstd.w (Butiker.BELayout,Butiker.BEPrinter,Butiker.BETerminalklient).
END. /* ETIKETTER */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-FixStorl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FixStorl Procedure 
FUNCTION FixStorl RETURNS CHARACTER
  ( pcStorl AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

 ASSIGN
    pcStorl = TRIM(pcStorl)
    pcStorl = CAPS(pcStorl)
    pcStorl = IF (LENGTH(pcStorl) = 1 OR 
                 LENGTH(pcStorl) = 3
                 ) 
                then " " + pcStorl
                else pcStorl.          

  /* Bytter ut eventuelle comma med punkt. */
  IF INDEX(pcStorl,",") <> 0 THEN
    OVERLAY(pcStorl, INDEX(pcStorl,","), 1, "CHARACTER") = ".".

  RETURN pcStorl.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Mva2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Mva2 Procedure 
FUNCTION Mva2 RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR plWork AS DEC NO-UNDO.

  plWork = ((BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab) / BongLinje.Antall) -
          (((BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab) / BongLinje.Antall) / (1 + (BongLinje.Mva% / 100))).
  IF plWork = ? 
      THEN plWork = 0.

  RETURN plWork.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Mva2LinjeRab) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Mva2LinjeRab Procedure 
FUNCTION Mva2LinjeRab RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR plWork AS DEC NO-UNDO.

  plWork = ((BongLinje.LinjeRab) / BongLinje.Antall) -
          (((BongLinje.LinjeRab) / BongLinje.Antall) / (1 + (BongLinje.Mva% / 100))).
  IF plWork = ? 
      THEN plWork = 0.

  RETURN plWork.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SjekkNonSale) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SjekkNonSale Procedure 
FUNCTION SjekkNonSale RETURNS INTEGER
        ( piArtikkelNr AS DECIMAL ):

/*------------------------------------------------------------------------------
                Purpose:                                                                                                                                          
                Notes:                                                                                                                                            
------------------------------------------------------------------------------*/
  DEFINE VARIABLE piResult AS INTEGER NO-UNDO.
  DEFINE BUFFER trgArtBas FOR ArtBas.
 
  piResult = 0.
  IF piArtikkelnr > 0 THEN 
  SJEKK: 
  DO:
    FIND trgArtBas NO-LOCK WHERE
      trgArtBas.ArtikkelNr = piArtikkelNr NO-ERROR.
    IF NOT AVAILABLE trgArtBas THEN 
      LEAVE SJEKK.
    IF trgArtBas.Non_Sale = TRUE AND trgArtBas.NegVare = FALSE THEN 
      piResult = 1.
    ELSE  IF trgArtBas.Non_Sale = TRUE AND trgArtBas.NegVare = TRUE THEN 
      piResult = 2.
  END. /* SJEKK */
  
  RETURN piResult.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

