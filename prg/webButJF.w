&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : webButikk.w 
    Purpose     : Eksport av varer til web butikk
    
    Syntax      :

    Description :

    Author(s)   : Tom Nøkleby
    Created     : 18 jun 08
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT  PARAMETER cEDBSystem AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocRetur    AS CHARACTER NO-UNDO.

DEFINE VAR oc2Retur                AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWB                AS CHARACTER INITIAL 'WB' NO-UNDO.

/* NB NB NB Denne skal skrives om til å håndtere medlemsdata til Web */

DEF VAR cTmpFilNavn AS CHAR NO-UNDO.
DEF VAR cFilNavn    AS CHAR NO-UNDO.
DEF VAR cOrgTmpFilNavn AS CHAR NO-UNDO.
DEF VAR cOrgFilNavn    AS CHAR NO-UNDO.
DEF VAR cGetFilnavn AS CHAR NO-UNDO.
DEFINE VARIABLE cUtfilNavn AS CHARACTER NO-UNDO.
DEF VAR iAntEksport AS INTEGER    NO-UNDO.
DEF VAR iCl      AS INT  NO-UNDO.
DEFINE VARIABLE iwbBut1 AS INTEGER NO-UNDO.
DEFINE VARIABLE iwbBut2 AS INTEGER NO-UNDO.
DEF VAR iKode       AS INT INITIAL 1 NO-UNDO.
DEF VAR piLoop      AS INT NO-UNDO.
DEF VAR iTotAntEksport AS INT  NO-UNDO.
DEF VAR clogFilNavn    AS CHAR NO-UNDO.
DEF VAR bNegLager   AS LOG NO-UNDO.
DEF VAR cTekst      AS CHAR NO-UNDO.
DEFINE VARIABLE bKopierBilder AS LOG NO-UNDO.
DEFINE VARIABLE cLagerLst     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNettButLager AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSumButikk    AS INTEGER NO-UNDO.
DEFINE VARIABLE bCopy         AS LOG NO-UNDO.

DEFINE VARIABLE hBuf        AS HANDLE     NO-UNDO.
DEFINE VARIABLE iAntRec     AS INTEGER    NO-UNDO.
DEFINE VARIABLE cOmmit      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iAkkumLager AS INTEGER NO-UNDO.

DEFINE VARIABLE cForNavn   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEtternavn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPostSted  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFodtDato  AS CHARACTER NO-UNDO.
DEFINE VARIABLE hSAXWriter AS HANDLE NO-UNDO.
DEFINE VARIABLE lOK AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE TT_ELogg  NO-UNDO LIKE ELogg.
DEFINE TEMP-TABLE tt2_ELogg NO-UNDO LIKE ELogg.
DEFINE BUFFER   bTT_Elogg FOR TT_Elogg.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr   AS INT
  FIELD Tekst     AS CHAR
  FIELD Gradering AS INT
  .
DEFINE VARIABLE cWebButiker AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iWebbut AS INTEGER     NO-UNDO.
DEFINE VARIABLE iButCount AS INTEGER     NO-UNDO.
DEF STREAM Ut.
DEF STREAM FilLogg.

{windows.i}
{webbut.i}

DEFINE TEMP-TABLE exp_webArtikkel LIKE tt_webArtikkel.
DEFINE TEMP-TABLE exp_webLager    LIKE tt_webLager.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fixChkEAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fixChkEAN Procedure 
FUNCTION fixChkEAN RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBildefil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBildefil Procedure 
FUNCTION getBildefil RETURNS CHARACTER
  ( INPUT ipBildNr AS INTEGER, 
    INPUT iType    AS INTEGER)  FORWARD.

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
         HEIGHT             = 30.38
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{incl\devmode.i}
{incl\custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEFINE BUFFER clButiker FOR Butiker.
DEFINE BUFFER wbButiker FOR Butiker.

bCopy = FALSE.
cUtfilNavn = 'webbutstd'.

{runlib.i}

{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE 
  clButiker.Butik = iCL NO-ERROR.
IF NOT AVAILABLE clButiker THEN 
  DO:
    MESSAGE 'Sentrallager butikk er ikke satt opp. Kontatk systemansvarlig.' SKIP(1)
            'Melding fra program webButikk.p ved eksport av data til nettbutikk.' SKIP
            'Kontakt systemansvarlig.'
    VIEW-AS ALERT-BOX.
    RETURN.
  END.

/* Skal bilder kopieres til nettbutikk? */
{syspara.i 150 1 11 cTekst}
IF CAN-DO('1,true,yes,ja,y,j',cTekst) THEN
    bKopierBilder = TRUE.
ELSE
    bKopierBilder = FALSE.

/* Nettbutikken skal ha -1000 i antall når artikkelen ikke skal publiseres. */
{syspara.i 150 1 7 cTekst}
IF CAN-DO('1,true,yes,ja,y,j',cTekst) THEN
    bNegLager = TRUE.
ELSE
    bNegLager = FALSE.

/* Legge ut lager til nettbutikk akkumulert for alle butikker. */
{syspara.i 150 1 9 iAkkumLager INT}
IF iAkkumLager = 0 THEN 
    iAkkumLager = 1.

/* Henter nettbutikk. Er ikke nettbutikk lagt opp, velges sentrallageret. */
/* Uansett er det buffer wbButiker som benyttes for nettbutikk.           */
{syspara.i 150 1 2 iwbBut1 INT}
FIND wbButiker NO-LOCK WHERE 
  wbButiker.Butik = iwbBut1 NO-ERROR.
IF NOT AVAILABLE wbButiker THEN 
DO:
    FIND wbButiker NO-LOCK WHERE 
        wbButiker.Butik = icL NO-ERROR.
END.
cWebButiker = STRING(iwbBut1).
{syspar2.i 150 1 2 iwbBut2 INT}
IF iwbBut2 > 0 THEN DO:
    FIND Butiker NO-LOCK WHERE 
         Butiker.Butik = iwbBut2 NO-ERROR.
    IF AVAILABLE Butiker THEN
        cWebButiker = cWebButiker + "," + STRING(iwbBut2).
END.

/* Nettbutikkens primærlager. */
{syspara.i 150 1 3 cTekst}
cNettButLager = TRIM(TRIM(cTekst),',').
/* Nettbutikkens sumlager. */
{syspar2.i 150 1 17 cTekst}
iSumbutikk = INTEGER(TRIM(TRIM(cTekst),',')).
IF iSumButikk = 0 THEN 
    ASSIGN iSumButikk = INTEGER(cNettButLager). 
/* Liste med lager. Samt at primærlager legges inn først i listen. */
{syspara.i 150 1 17 cTekst}
IF NOT CAN-DO(TRIM(TRIM(cTekst),','),cLagerLst) THEN 
    cLagerLst = cLagerLst + ',' + TRIM(TRIM(cTekst),',').
/* Plukker bort eventuelle overflødige komma. */
cLagerLst = RIGHT-TRIM(TRIM(cLagerLst),',').
   
FIND FIRST EkstEDBSystem WHERE 
    EkstEDBSystem.EDBSystem = cEDBSystem AND 
    EkstEDBSystem.DataType  = 'WebBut' AND 
    EkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
IF NOT AVAIL EkstEDBSystem THEN DO:
    ocRetur = "ERROR - Ingen webbutikkeksport-rutine aktiv".
    RETURN.
END.

/* Navn på fillogg */
ASSIGN
    clogFilNavn = 'webFilLog' + replace(STRING(TODAY),'/','-') + '.csv'.

SUBSCRIBE TO 'webbutikkeksporterr' ANYWHERE.

MOTTAGER:
DO:
    IF DYNAMIC-FUNCTION("runproc","get_ekstedbsys_filnavn.p",EkstEDBSystem.EDBSystem,?) THEN 
        ASSIGN cGetFilnavn = DYNAMIC-FUNCTION("getTransactionMessage").

    IF NOT NUM-ENTRIES(cGetFilnavn,"|") = 3 THEN DO:
        ocRetur = "ERROR-" + cGetFilnavn.
        RETURN.
    END.
/*     ELSE DO:                                                                                                   */
/*         ASSIGN cTmpFilNavn = RIGHT-TRIM(ENTRY(1,cGetFilnavn,"|"),"\") + "\" + "TMP" + ENTRY(2,cGetFilnavn,"|") */
/*                cFilNavn    = ENTRY(2,cGetFilnavn,"|")                                                          */
/*                cWB         = ENTRY(3,cGetFilNavn,"|").                                                         */
/*     END.                                                                                                       */
    
    /* Tar vare på orginalfilnavnet. Dette vil bli benyttet for hele filsettet. */
/*     ASSIGN                                                */
/*         cTmpFilNavn    = REPLACE(cTmpFilNavn,"csv","xml") */
/*         cFilNavn       = REPLACE(cFilNavn,"csv","xml")    */
/*         cOrgTmpFilNavn = cTmpFilNavn                      */
/*         cOrgFilNavn    = cFilNavn                         */
/*         .                                                 */

    /* Tømmer buffer før ny sortering bygges opp. */
    FOR EACH TT_ELogg:
        DELETE TT_Elogg.
    END.
    /* Leser alle loggede ordre og logger berørte artikler. */
    RUN KopierElogg.

    /* Nå legger vi ut VPI'en. */
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = iCl NO-ERROR.
    
    DO iButCount = 1 TO NUM-ENTRIES(cWebButiker):
        iWebbut = INT(ENTRY(iButCount,cWebButiker)).
        FIND wbButiker NO-LOCK WHERE 
            wbButiker.Butik = iWebbut NO-ERROR.
        ASSIGN cTmpFilNavn = RIGHT-TRIM(ENTRY(1,cGetFilnavn,"|"),"\") + "\web" + STRING(iButCount) + "\TMP" + ENTRY(2,cGetFilnavn,"|")
               cFilNavn    = ENTRY(2,cGetFilnavn,"|")
               cWB         = ENTRY(3,cGetFilNavn,"|").
        ASSIGN
            cTmpFilNavn    = REPLACE(cTmpFilNavn,"csv","xml")
            cFilNavn       = REPLACE(cFilNavn,"csv","xml")
            cOrgTmpFilNavn = cTmpFilNavn
            cOrgFilNavn    = cFilNavn
            .
        ASSIGN
            cTmpFilNavn = IF cWB = 'MA' THEN REPLACE(cOrgTmpFilNavn,cWB,'PublishEComArticlePrice')
                          ELSE REPLACE(cOrgTmpFilNavn,cWB,cWB + 'ART')
            cFilNavn    = IF cWB = 'MA' THEN REPLACE(cOrgFilNavn,cWB,'PublishEComArticlePrice')
                          ELSE REPLACE(cOrgFilNavn,cWB,cWB + 'ART').
        RUN ByggTmpTabellArtikkel.
        RUN EksporterArtikkelFil.

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'AVD') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'AVD').   */
/*         RUN ByggTmpTabellAvdeling.                                */
/*         RUN EksporterAvdelinglFil.                                */

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'HGR') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'HGR').   */
/*         RUN ByggTmpTabellHuvGr.                                   */
/*         RUN EksporterHuvGrFil.                                    */

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'VGR') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'VGR').   */
/*         RUN ByggTmpTabellVarGr.                                   */
/*         RUN EksporterVarGrFil.                                    */

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'KAT') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'KAT').   */
/*         RUN ByggTmpTabellKategori.                                */
/*         RUN EksporterKategoriFil.                                 */

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'AKT') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'AKT').   */
/*         RUN ByggTmpTabellAktivitet.                               */
/*         RUN EksporterAktivitetFil.                                */

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'LEV') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'LEV').   */
/*         RUN ByggTmpTabellLevBas.                                  */
/*         RUN EksporterLevBasFil.                                   */

        ASSIGN
            cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'PRO')
            cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'PRO').
        RUN ByggTmpTabellProdusent.
        RUN EksporterProdusentFil.

        ASSIGN
            cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'VMI')
            cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'VMI').
        RUN ByggTmpTabellVaremerke.
        RUN EksporterVaremerkeFil.

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'SAS') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'SAS').   */
/*         RUN ByggTmpTabellSasong.                                  */
/*         RUN EksporterSasongFil.                                   */

        ASSIGN
            cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'MAT')
            cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'MAT').
        RUN ByggTmpTabellMaterial.
        RUN EksporterMaterialFil.

        ASSIGN
            cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'FAR')
            cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'FAR').
        RUN ByggTmpTabellFarg.
        RUN EksporterFargFil.

        ASSIGN
            cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'HKAT')
            cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'HKAT').
        RUN ByggTmpTabellHovedkategori.
        RUN EksporterHovedkategoriFil.

        ASSIGN
            cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'UKAT')
            cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'UKAT').
        RUN ByggTmpTabellUnderkategori.
        RUN EksporterUnderkategoriFil.

        ASSIGN
            cTmpFilNavn = IF cWB = 'MA' THEN REPLACE(cOrgTmpFilNavn,cWB,'PublishEComStocklevel')
                          ELSE REPLACE(cOrgTmpFilNavn,cWB,cWB + 'LAG')
            cFilNavn    = IF cWB = 'MA' THEN REPLACE(cOrgFilNavn,cWB,'PublishEComStocklevel')
                          ELSE REPLACE(cOrgFilNavn,cWB,cWB + 'LAG').
        RUN ByggTmpTabellLager.
        RUN EksporterLagerFil.

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'MVA') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'MVA').   */
/*         RUN ByggTmpTabellMoms.                                    */
/*         RUN EksporterMomsFil.                                     */

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'STR') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'STR').   */
/*         RUN ByggTmpTabellStrKonv.                                 */
/*         RUN EksporterStrKonvFil.                                  */

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'STT') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'STT').   */
/*         RUN ByggTmpTabellStrTStr.                                 */
/*         RUN EksporterStrTStrFil.                                  */

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'VAK') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'VAK').   */
/*         RUN ByggTmpTabellVgAkt.                                   */
/*         RUN EksporterVgAktFil.                                    */

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'VKA') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'VKA').   */
/*         RUN ByggTmpTabellVgKat.                                   */
/*         RUN EksporterVgKatFil.                                    */

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'RAB') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'RAB').   */
/*         RUN ByggTmpTabellVgKundeGrpRabatt.                        */
/*         RUN EksporterVgKundeGrpRabattFil.                         */

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'KGR') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'KGR').   */
/*         RUN ByggTmpTabellKundeGruppe.                             */
/*         RUN EksporterKundeGruppeFil.                              */

        ASSIGN
            cTmpFilNavn = IF cWB = 'MA' THEN REPLACE(cOrgTmpFilNavn,cWB,'UpdateEComCustomer')
                          ELSE REPLACE(cOrgTmpFilNavn,cWB,cWB + 'KUN')
            cFilNavn    = IF cWB = 'MA' THEN REPLACE(cOrgFilNavn,cWB,'UpdateEComCustomer')
                          ELSE REPLACE(cOrgFilNavn,cWB,cWB + 'KUN').
        RUN ByggTmpTabellKunde.
        RUN EksporterKundeFil.

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'KKO') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'KKO').   */
/*         RUN ByggTmpTabellKundeKort.                               */
/*         RUN EksporterKundeKortFil.                                */

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'KSA') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'KSA').   */
/*         RUN ByggTmpTabellKundeSaldo.                              */
/*         RUN EksporterKundeSaldoFil.                               */

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'MED') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'MED').   */
/*         RUN ByggTmpTabellMedlem.                                  */
/*         RUN EksporterMedlemFil.                                   */

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'MGR') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'MGR').   */
/*         RUN ByggTmpTabellMedlemsGruppe.                           */
/*         RUN EksporterMedlemsGruppeFil.                            */

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'MKO') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'MKO').   */
/*         RUN ByggTmpTabellMedlemsKort.                             */
/*         RUN EksporterMedlemsKortFil.                              */

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'MSA') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'MSA').   */
/*         RUN ByggTmpTabellMedlemSaldo.                             */
/*         RUN EksporterMedlemSaldoFil.                              */

        ASSIGN /* Retur håndteres via negativ kundeordre. Derfor håndterees også UpdateEComReturnRequest her */
            cTmpFilNavn = IF cWB = 'MA' THEN REPLACE(cOrgTmpFilNavn,cWB,'PublishEComShippingInfo')
                          ELSE REPLACE(cOrgTmpFilNavn,cWB,cWB + 'KOR')
            cFilNavn    = IF cWB = 'MA' THEN REPLACE(cOrgFilNavn,cWB,'PublishEComShippingInfo')
                          ELSE REPLACE(cOrgFilNavn,cWB,cWB + 'KOR').
        RUN ByggTmpTabellKOrdreHode.
        RUN EksporterKOrdreHodeFil.

        ASSIGN
            cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'KLA')
            cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'KLA').
        RUN ByggTmpTabellKlack.
        RUN EksporterKlackFil.

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'HAN') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'HAN').   */
/*         RUN ByggTmpTabellHandtering.                              */
/*         RUN EksporterHandteringFil.                               */

        ASSIGN
            cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'INN')
            cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'INN').
        RUN ByggTmpTabellInnersula.
        RUN EksporterInnersulaFil.

        ASSIGN
            cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'OVA')
            cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'OVA').
        RUN ByggTmpTabellOvandel.
        RUN EksporterOvandelFil.

        ASSIGN
            cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'SLI')
            cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'SLI').
        RUN ByggTmpTabellSlitSula.
        RUN EksporterSlitSulaFil.

        ASSIGN
            cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'LAS')
            cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'LAS').
        RUN ByggTmpTabellLast-Sko.
        RUN EksporterLast-SkoFil.

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'ANV') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'ANV').   */
/*         RUN ByggTmpTabellAnv-Kod.                                 */
/*         RUN EksporterAnv-KodFil.                                  */

/*         ASSIGN                                                    */
/*             cTmpFilNavn = REPLACE(cOrgTmpFilNavn,cWB,cWB + 'REG') */
/*             cFilNavn    = REPLACE(cOrgFilNavn,cWB,cWB + 'REG').   */
/*         RUN ByggTmpTabellRegnskapsavdeling.                       */
/*         RUN EksporterRegnskapsavdelingFil.                        */

        ASSIGN
            iTotAntEksport = iTotantEksport + iantEksport
            iAntEksport    = 0
            .
    END.
END. /* MOTTAGER */

FIND FIRST tt_error NO-ERROR.
IF AVAILABLE tt_Error THEN
    RUN ErrorLogg.

ocRetur = "OK," + String(iTotAntEksport).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggTmpTabellAktivitet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellAktivitet Procedure 
PROCEDURE ByggTmpTabellAktivitet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webAktivitet:
    DELETE tt_webAktivitet.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "Aktivitet": 
    FIND Aktivitet NO-LOCK WHERE
        Aktivitet.AktNr = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE Aktivitet THEN
    DO:
        CREATE tt_webAktivitet.
        ASSIGN
            tt_webAktivitet.iRecType    = tt_ELogg.EndringsType
            tt_webAktivitet.AktNr       = Aktivitet.AktNr
            tt_webAktivitet.Beskrivelse = Aktivitet.Beskrivelse
            .
    END.
END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellAnv-Kod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellAnv-Kod Procedure 
PROCEDURE ByggTmpTabellAnv-Kod :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

    DEF VAR iRecType AS INT NO-UNDO.

    FOR EACH tt_webAnv-Kod:
        DELETE tt_webAnv-Kod.
    END.

    WEBBUT:
    FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "Anv-Kod": 
        FIND Anv-Kod NO-LOCK WHERE
            Anv-Kod.Anv-Id = INTEGER(ENTRY(1,TT_Elogg.Verdier,'|')) NO-ERROR.
            
        IF AVAILABLE Anv-Kod THEN
        DO:
            CREATE tt_webAnv-Kod.
            ASSIGN
                tt_webAnv-Kod.iRecType = tt_ELogg.EndringsType
                tt_webAnv-Kod.Anv-Id   = Anv-Kod.Anv-Id
                tt_webAnv-Kod.AnvBeskr = Anv-Kod.AnvBeskr
                .
        END.
    END. /* WEBBUT */   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellArtikkel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellArtikkel Procedure 
PROCEDURE ByggTmpTabellArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.
DEF VAR cNotat1      AS CHAR NO-UNDO.
DEF VAR cNotat2      AS CHAR NO-UNDO.
DEF VAR cNotat3      AS CHAR NO-UNDO.
DEF VAR cEAN         AS CHAR NO-UNDO.
DEF VAR cEANprefix   AS CHAR NO-UNDO.
DEF VAR cBildeFil1   AS CHAR NO-UNDO.
DEF VAR cBildeFil2   AS CHAR NO-UNDO.
DEF VAR cBildeFil1B  AS CHAR NO-UNDO.
DEF VAR cBildeFil2B  AS CHAR NO-UNDO.
DEFINE VARIABLE iLopNr AS INTEGER NO-UNDO.

FOR EACH tt_webArtikkel:
    DELETE tt_webArtikkel.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "ArtBas": 
    iRectype = tt_ELogg.EndringsType.
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = DEC(TT_Elogg.Verdier) NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
        NEXT WEBBUT.
/*         LEAVE WEBBUT. */
    ELSE DO:
          IF ArtBas.LopNr = ? THEN
          DO:
              iLopNr = 0.
              RUN SettLopNr.p (ArtBas.Vg,'F',OUTPUT iLopNr).
              IF iLopNr <> 0 THEN DO:
                  RUN SettLopNrIArtOgLager.p (ArtBas.ArtikkelNr, iLopNr).
                  FIND ArtBas NO-LOCK WHERE
                      ArtBas.ArtikkelNr = DEC(TT_Elogg.Verdier) NO-ERROR.
              END.
          END.
    END.
    FIND artbut WHERE artbut.artikkelnr = ArtBas.artikkelnr AND artbut.butik = wbButiker.butik NO-ERROR.
    IF NOT AVAIL artbut THEN
         NEXT WEBBUT.
    ELSE DO:
        IF TT_ELogg.endringstype = 3 THEN
            DELETE artbut NO-ERROR.
        ELSE IF artbut.deleted THEN DO:
            iRecType = 3.
            DELETE artbut NO-ERROR.
        END.
    END.
    FOR EACH Strekkode OF ArtBas NO-LOCK:
/*         IF CAN-FIND(FIRST tt_webArtikkel WHERE tt_webArtikkel.ArtikkelNr = artbas.artikkelnr) THEN */
/*             NEXT.                                                                                  */
        IF TRIM(Strekkode.Kode) = '' THEN NEXT.
         
        IF AVAILABLE ArtBas THEN
        BYGG:
        DO:
            /* Henter nettbutikkens prisprofil. Finnes ikke denne, benyttes sentrallagerets prisprofil. */
            FIND FIRST ArtPris OF ArtBas NO-LOCK WHERE  
              ArtPris.ProfilNr = wbButiker.ProfilNr NO-ERROR.
            IF NOT AVAILABLE ArtPris THEN 
              FIND FIRST ArtPris OF ArtBas NO-LOCK WHERE  
                ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.
            IF NOT AVAILABLE ArtPris THEN LEAVE BYGG.

            FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
            IF AVAILABLE VarGr THEN
                FIND Moms OF VarGr NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Moms THEN LEAVE BYGG.

            FIND StrKonv OF Strekkode NO-LOCK NO-ERROR.
            IF NOT AVAILABLE StrKonv THEN LEAVE BYGG.

            IF bKopierBilder THEN 
            DO:
              /* Legger ut bildefil. */
              cBildeFil1 = getbildefil(ArtBas.bildnr,1).
              cBildeFil2 = getbildefil(ArtBas.bildnr,3).
              ASSIGN 
                cBildeFil1B = cOrgTmpFilNavn
                cBildeFil2B = cOrgTmpFilNavn.
              IF SEARCH(cBildeFil1) <> ? THEN  
              DO:
                  ENTRY(NUM-ENTRIES(cBildeFil1B,'\'),cBildeFil1B,'\') = ENTRY(NUM-ENTRIES(cBildeFil1,'\'),cBildeFil1,'\').
                  OS-COPY VALUE(cBildeFil1) VALUE(cBildeFil1B).
              END.
              IF SEARCH(cBildeFil2) <> ? THEN
              DO:
                  ENTRY(NUM-ENTRIES(cBildeFil2B,'\'),cBildeFil2B,'\') = ENTRY(NUM-ENTRIES(cBildeFil2,'\'),cBildeFil2,'\').
                  OS-COPY VALUE(cBildeFil2) VALUE(cBildeFil2B).
                  IF SEARCH(cBildefil1b) <> ? THEN
                      RUN w-KonverterBilde.w (cBildeFil2B,400,0).
              END.
              /*
              MESSAGE 'cBildeFil1' cBildeFil1 cBildeFil1B SKIP
                      'cBildeFil2' cBildeFil2 cBildeFil2B SKIP(1)
                       'cOrgTmpFilNavn' cOrgTmpFilNavn SKIP 
                       'cOrgFilNavn' cOrgFilNavn                     
              VIEW-AS ALERT-BOX.
              */
            END.
            ELSE ASSIGN
                   cBildeFil1 = '\'
                   cBildeFil2 = '\'.

            CREATE tt_webArtikkel.
            ASSIGN
                tt_webArtikkel.iRecType        = iRectype
                tt_webArtikkel.ArtikkelNr      = DECIMAL(tt_Elogg.Verdier)
                tt_webArtikkel.LevKod          = ArtBas.LevKod
                tt_webArtikkel.Beskr           = ArtBas.Beskr
                tt_webArtikkel.LevFargKod      = ArtBas.LevFargKod
                tt_webArtikkel.Vg              = ArtBas.Vg
                tt_webArtikkel.VgKat           = ArtBas.VgKat
                tt_webArtikkel.StrTypeId       = ArtBas.StrTypeId
                tt_webArtikkel.StrKode         = Strekkode.StrKode
                tt_webArtikkel.Storl           = IF AVAILABLE StrKonv THEN TRIM(StrKonv.Storl) ELSE ""
                tt_webArtikkel.Kode            = Strekkode.Kode
                tt_webArtikkel.InnkjopsPris    = ArtPris.InnkjopsPris[IF ArtPris.tilbud THEN 2 ELSE 1]
                tt_webArtikkel.Rab1_Proc       = ArtPris.Rab1%[IF ArtPris.tilbud THEN 2 ELSE 1]
                tt_webArtikkel.Varekost        = ArtPris.Varekost[IF ArtPris.tilbud THEN 2 ELSE 1]
                tt_webArtikkel.MvaKode         = Moms.Momskod
                tt_webArtikkel.Mva_Proc        = ArtPris.Mva%[IF ArtPris.tilbud THEN 2 ELSE 1]
                tt_webArtikkel.Db_Proc         = ArtPris.Db%[IF ArtPris.tilbud THEN 2 ELSE 1]
                tt_webArtikkel.Pris            = ArtPris.Pris[IF ArtPris.tilbud THEN 2 ELSE 1]
                tt_webArtikkel.Tilbud          = ArtPris.Tilbud
                tt_webArtikkel.AktiveringsDato = IF ArtPris.Tilbud THEN ArtPris.TilbudFraDato ELSE ArtPris.AktivFraDato 
                tt_webArtikkel.AktiveringsDato = IF tt_webArtikkel.AktiveringsDato <> ? THEN tt_webArtikkel.AktiveringsDato ELSE TODAY
                tt_webArtikkel.AktiveringsTid  = IF ArtPris.Tilbud THEN STRING(ArtPris.TilbudFraTid,"HH:MM:SS") ELSE STRING(ArtPris.AktivFraTid,"HH:MM:SS")
                tt_webArtikkel.AvsluttDato     = IF ArtPris.Tilbud THEN ArtPris.TilbudTilDato ELSE TODAY + 360 
                tt_webArtikkel.AvsluttDato     = IF tt_webArtikkel.AvsluttDato <> ? THEN tt_webArtikkel.AvsluttDato ELSE TODAY + 360
                tt_webArtikkel.AvsluttTid      = STRING(ArtPris.TilbudTilTid,"HH:MM:SS")
                tt_webArtikkel.AntIPakn        = ArtBas.AntIPakn
                tt_webArtikkel.SalgsEnhet      = ArtBas.SalgsEnhet
                tt_webArtikkel.ModellFarge     = ArtBas.ModellFarge
                tt_webArtikkel.VmId            = ArtBas.VMId
                /* TN 6/5-09 Nye felt som skal legges ut */
                tt_WebArtikkel.VPIBildeKode    = ArtBas.VPIBildeKode
                tt_WebArtikkel.Varefakta       = ArtBas.VareFakta
                tt_WebArtikkel.PostVekt        = ROUND(ArtBas.PostVekt * 1000,0)
                tt_WebArtikkel.PostLengde      = ArtBas.PostLengde
                tt_WebArtikkel.PostBredde      = ArtBas.PostBredde
                tt_WebArtikkel.PostHoyde       = ArtBas.PostHoyde
                tt_WebArtikkel.WebMinLager     = ArtBAs.WebMinLager
                tt_WebArtikkel.KampanjeKode    = ArtBas.KampanjeKode
                tt_WebArtikkel.WebLeveringstid = ArtBas.WebLeveringstid
                tt_WebArtikkel.VareType        = ArtBas.VareType
                tt_WebArtikkel.VareTypeTekst   = ENTRY(ArtBas.VareType,'Lagervare,Suppleringsvare,Skaffevare')
                tt_WebArtikkel.Leveringstid    = ArtBas.Leveringstid
                tt_webArtikkel.Bonus_givende   = ArtBas.Bonus_Givende
                tt_webArtikkel.PubliserINettbutikk = ArtBas.PubliserINettbutikk
                
                /* TN 25/5-09 Farge skal også legges med ut (Ref. eMail fra Ronny). */
                tt_webArtikkel.Farg            = ArtBas.Farg
                /*tt_WebArtikkel.UtvidetSok      = ArtBas.UtvidetSok */   
                /* Ordinær pris feltene. */
                tt_webArtikkel.OrdInnkjopsPris = ArtPris.InnkjopsPris[1]
                tt_webArtikkel.OrdRab1_Proc    = ArtPris.Rab1%[1]
                tt_webArtikkel.OrdVarekost     = ArtPris.Varekost[1]
                tt_webArtikkel.OrdMvaKode      = Moms.Momskod
                tt_webArtikkel.OrdMva_Proc     = ArtPris.Mva%[1]
                tt_webArtikkel.OrdDb_Proc      = ArtPris.Db%[1]
                tt_webArtikkel.OrdPris         = ArtPris.Pris[1]
                /* Førpris eller veilendende pris */
                tt_webArtikkel.AnbefaltPris    = ArtBas.AnbefaltPris
                /* Jamførenhet */
                tt_webArtikkel.JamforEnhet     = ArtBas.JamforEnhet 
                tt_webArtikkel.Mengde          = ArtBas.Mengde
                tt_webArtikkel.Bestillingsnummer = Strekkode.Bestillingsnummer
                tt_webArtikkel.HovedKatNr      = ArtBas.HovedKatNr
                tt_webArtikkel.Link_Til_Nettside = ArtBas.Link_Til_Nettside
                /* 18/4-12 TN */
                tt_webArtikkel.Lokasjon        = ArtBas.Lokasjon
                tt_webArtikkel.LevNr           = ArtBas.LevNr
                tt_webArtikkel.ProdNr          = ArtBas.ProdNr
                tt_webArtikkel.Sasong          = ArtBas.Sasong
                tt_webArtikkel.MatKod          = ArtBas.MatKod
                tt_webArtikkel.Anv-Id          = ArtBas.Anv-Id
                /* 31/1-13 TN Ref. JF */
                tt_webArtikkel.Klack           = ArtBas.Klack 
                tt_webArtikkel.Inner_Id        = ArtBas.Inner-id
                tt_webArtikkel.Ov_Id           = ArtBas.Ov-Id
                tt_webArtikkel.Slit_Id         = ArtBas.Slit-Id
                tt_webArtikkel.Last-Id         = ArtBas.Last-Id
                tt_webArtikkel.BehKode         = ArtBas.BehKode
                tt_webArtikkel.RAvdNr          = ArtBas.RAvdNr
                tt_webArtikkel.DivInfo1        = ArtBas.DivInfo[1]
                tt_webArtikkel.DivInfo2        = ArtBas.DivInfo[2]
                tt_webArtikkel.DivInfo3        = ArtBas.DivInfo[3]
                tt_webArtikkel.DivInfo4        = ArtBas.DivInfo[4]
                tt_webArtikkel.DivInfo5        = ArtBas.DivInfo[5]
                tt_webArtikkel.DivInfo6        = ArtBas.DivInfo[6]
                tt_webArtikkel.DivInfo7        = ArtBas.DivInfo[7]
                tt_webArtikkel.DivInfo8        = ArtBas.DivInfo[8] 
                tt_webArtikkel.LopNr           = ArtBas.LopNr 
                .
                
                IF bKopierBilder THEN
                ASSIGN 
                  tt_WebArtikkel.BildeStort      = ENTRY(NUM-ENTRIES(cBildeFil2,'\'),cBildeFil2,'\')
                  tt_WebArtikkel.BildeLite       = ENTRY(NUM-ENTRIES(cBildeFil1,'\'),cBildeFil1,'\') NO-ERROR.
                  
                FOR EACH ArtBasUnderkategori OF ArtBas NO-LOCK:
                  tt_webArtikkel.UnderKatListe = tt_webArtikkel.UnderKatListe + 
                                                 (IF tt_webArtikkel.UnderKatListe <> ''
                                                    THEN ','
                                                    ELSE '') +
                                                 STRING(ArtBasUnderKategori.UnderKatNr).   
                END.

        END. /* BYGG */

        ASSIGN
            cNotat1  = REPLACE (cNotat1,CHR(13),"|")
            cNotat1  = REPLACE (cNotat1,CHR(10),"|")
            cNotat2  = REPLACE (cNotat2,CHR(13),"|")
            cNotat2  = REPLACE (cNotat2,CHR(10),"|")
            cNotat3  = REPLACE (cNotat3,CHR(13),"|")
            cNotat3  = REPLACE (cNotat3,CHR(10),"|")
            .
    END.
END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellAvdeling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellAvdeling Procedure 
PROCEDURE ByggTmpTabellAvdeling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webAvdeling:
    DELETE tt_webAvdeling.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "Avdeling": 
    FIND Avdeling NO-LOCK WHERE
        Avdeling.Avdelingnr = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE Avdeling THEN
    DO:
        CREATE tt_webAvdeling.
        ASSIGN
            tt_webAvdeling.iRecType     = tt_ELogg.EndringsType
            tt_webAvdeling.AvdelingNr   = Avdeling.AvdelingNr
            tt_webAvdeling.AvdelingNavn = Avdeling.AvdelingNavn
            .
    END.
END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellFarg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellFarg Procedure 
PROCEDURE ByggTmpTabellFarg :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webFarg:
    DELETE tt_webFarg.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "Farg": 
    FIND Farg NO-LOCK WHERE
        Farg.Farg = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE Farg THEN
    DO:
        CREATE tt_webFarg.
        ASSIGN
            tt_webFarg.iRecType    = tt_ELogg.EndringsType
            tt_webFarg.Farg        = Farg.Farg
            tt_webFarg.FArBeskr    = Farg.FarBeskr
            .
    END.
END. /* WEBBUT */   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-byggTmpTabellHandtering) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE byggTmpTabellHandtering Procedure 
PROCEDURE byggTmpTabellHandtering :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webHandtering:
    DELETE tt_webHandtering.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "Handtering": 
    FIND Handtering NO-LOCK WHERE
        Handtering.HandKode = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE Handtering THEN
    DO:
        CREATE tt_webHandtering.
        ASSIGN
            tt_webHandtering.iRecType    = tt_ELogg.EndringsType
            tt_webHandtering.HandKode    = Handtering.HandKode
            tt_webHandtering.Beskrivelse = Handtering.Beskrivelse
            .
    END.
END. /* WEBBUT */   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellHovedkategori) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellHovedkategori Procedure 
PROCEDURE ByggTmpTabellHovedkategori :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webHovedkategori:
    DELETE tt_webHovedkategori.
END.

WEBHKAT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "Hovedkategori": 
    FIND Hovedkategori NO-LOCK WHERE
        Hovedkategori.HovedKatNr = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE HovedKategori THEN
    DO:
        CREATE tt_webHovedkategori.
        ASSIGN
            tt_webHovedkategori.iRecType      = tt_ELogg.EndringsType
            tt_webHovedkategori.HovedKatNr    = Hovedkategori.HovedKatNr
            tt_webHovedkategori.HovedKatTekst = Hovedkategori.HovedKatTekst
            .
    END.
END. /* WEBHKAT */   


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellHuvGr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellHuvGr Procedure 
PROCEDURE ByggTmpTabellHuvGr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webHuvGr:
    DELETE tt_webHuvGr.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "HuvGr": 
    FIND HuvGr NO-LOCK WHERE
        HuvGr.Hg = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE HuvGr THEN
    DO:
        CREATE tt_webHuvGr.
        ASSIGN
            tt_webHuvGr.iRecType   = tt_ELogg.EndringsType
            tt_webHuvGr.Hg         = HuvGr.Hg
            tt_webHuvGr.HgBeskr    = HuvGr.HgBeskr
            tt_webHuvGr.AvdelingNr = HuvGr.AvdelingNr
            .
    END.
END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-byggTmpTabellInnersula) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE byggTmpTabellInnersula Procedure 
PROCEDURE byggTmpTabellInnersula :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webInnersula:
    DELETE tt_webInnersula.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "Innersula": 
    FIND Innersula NO-LOCK WHERE
        InnerSula.Inner-Id = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE InnerSula THEN
    DO:
        CREATE tt_webInnerSula.
        ASSIGN
            tt_webInnerSula.iRecType   = tt_ELogg.EndringsType
            tt_webInnerSula.Inner-Id   = Innersula.Inner-Id
            tt_webInnerSula.InnerBeskr = InnerSula.InnerBeskr
            .
    END.
END. /* WEBBUT */   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellKategori) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellKategori Procedure 
PROCEDURE ByggTmpTabellKategori :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webKategori:
    DELETE tt_webKategori.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "Kategori": 
    FIND Kategori NO-LOCK WHERE
        Kategori.KatNr = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE Kategori THEN
    DO:
        CREATE tt_webKategori.
        ASSIGN
            tt_webKategori.iRecType    = tt_ELogg.EndringsType
            tt_webKategori.KatNr       = Kategori.KatNr
            tt_webKategori.Beskrivelse = Kategori.Beskrivelse
            .
    END.
END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-byggTmpTabellKlack) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE byggTmpTabellKlack Procedure 
PROCEDURE byggTmpTabellKlack :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webKlack:
    DELETE tt_webKlack.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "Klack": 
    FIND Klack NO-LOCK WHERE
        Klack.Klack-Id = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE Klack THEN
    DO:
        CREATE tt_webKlack.
        ASSIGN
            tt_webKlack.iRecType    = tt_ELogg.EndringsType
            tt_webKlack.Klack-Id    = Klack.Klack-Id
            tt_webKlack.Beskrivning = Klack.Beskrivning
            .
    END.
END. /* WEBBUT */   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellKOrdreHode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellKOrdreHode Procedure 
PROCEDURE ByggTmpTabellKOrdreHode :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEFINE VARIABLE iRecType AS INT  NO-UNDO.
DEFINE VARIABLE iAnt     AS INTEGER NO-UNDO.

FOR EACH tt_webKOrdreHode:
    DELETE tt_webKOrdreHode.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "KOrdreHode":
    FIND KOrdreHode NO-LOCK WHERE
      KOrdreHode.KOrdre_Id = DECIMAL(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE KOrdreHode AND KOrdreHode.butikknr = wbButiker.butik THEN
    BLOKKEN:
    DO:
        /* Magento tåler ikke utlegg av makulerte ordre eller returer. */
        IF CAN-DO('MAGENTO',cEDBSystem) AND (KOrdreHode.LevStatus = '60' OR KOrdreHode.EkstOrdreNr MATCHES '*RETUR*' OR KOrdreHode.ProduksjonsDato <> ?) THEN 
          LEAVE BLOKKEN. 
          
        iAnt = 0.
        FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK:
          IF TRIM(KOrdreLinje.VareNr) <> '' AND CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = DEC(KOrdreLinje.VareNr)) THEN 
            iAnt = iAnt + KOrdreLinje.Antall.  
        END.
        
        CREATE tt_webKOrdreHode.
        BUFFER-COPY KOrdreHode TO tt_webKOrdreHode
          ASSIGN
            tt_webKOrdreHode.iRecType = tt_ELogg.EndringsType
            tt_webKOrdreHode.Totalt   = iAnt.    
    END.
END. /* WEBBUT */   


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellKunde) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellKunde Procedure 
PROCEDURE ByggTmpTabellKunde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webKunde:
    DELETE tt_webKunde.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "Kunde": 
    FIND Kunde NO-LOCK WHERE
        Kunde.KundeNr = DECIMAL(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE Kunde AND Kunde.butikknr = wbButiker.butik THEN
    KUNDEBLOKK:
    DO:
        /* Bare nettbutikkunder skal eksporteres til nettbutikk */
        IF Kunde.WebKunde = FALSE THEN 
          LEAVE KUNDEBLOKK.
          
        CREATE tt_webKunde.
        ASSIGN
            tt_webKunde.iRecType       = tt_ELogg.EndringsType
            tt_webKunde.KundeNr        = Kunde.KundeNr
            tt_webKunde.Navn           = Kunde.Navn
            tt_webKunde.EDato          = Kunde.EDato
            tt_webKunde.ETid           = Kunde.ETid
            tt_webKunde.BrukerID       = Kunde.BrukerID
            tt_webKunde.RegistrertDato = Kunde.RegistrertDato
            tt_webKunde.RegistrertTid  = Kunde.RegistrertTid
            tt_webKunde.RegistrertAv   = Kunde.RegistrertAv
            tt_webKunde.TypeId         = Kunde.TypeId
            tt_webKunde.GruppeId       = Kunde.GruppeId
            tt_webKunde.Adresse1       = Kunde.Adresse1
            tt_webKunde.Adresse2       = Kunde.Adresse2
            tt_webKunde.PostNr         = Kunde.PostNr
            tt_webKunde.Telefon        = Kunde.Telefon
            tt_webKunde.Telefaks       = Kunde.Telefaks
            tt_webKunde.MobilTlf       = Kunde.MobilTlf
            tt_webKunde.KontNavn       = Kunde.KontNavn
            tt_webKunde.KontTelefon    = Kunde.KontTelefon
            tt_webKunde.Stilling       = Kunde.Stilling
            tt_webKunde.KontTelefaks   = Kunde.KontTelefaks.

        ASSIGN
            tt_webKunde.KontMobilTlf  = Kunde.KontMobilTlf
            tt_webKunde.LevAdresse1   = Kunde.LevAdresse1
            tt_webKunde.LevAdresse2   = Kunde.LevAdresse2
            tt_webKunde.LevPostNr     = Kunde.LevPostNr
            tt_webKunde.LevLand       = Kunde.LevLand
            tt_webKunde.Land          = Kunde.Land
            tt_webKunde.MaksKredit    = Kunde.MaksKredit
            tt_webKunde.KreditSperret = Kunde.KreditSperret
            tt_webKunde.Opphort       = Kunde.Opphort
            tt_webKunde.ButikkNr      = Kunde.ButikkNr
            tt_webKunde.BydelsNr      = Kunde.BydelsNr
            tt_webKunde.ePostAdresse  = Kunde.ePostAdresse
            tt_webKunde.KontE-Post    = Kunde.KontE-Post
            tt_webKunde.OrgNr         = Kunde.OrgNr
            tt_webKunde.TotalRabatt%  = Kunde.TotalRabatt%
            tt_webKunde.BankKonto     = Kunde.BankKonto
            tt_webKunde.Postgiro      = Kunde.Postgiro
            tt_webKunde.BetBet        = Kunde.BetBet
            tt_webKunde.Etablert      = Kunde.Etablert
            tt_webKunde.SamleFaktura  = Kunde.SamleFaktura.

        ASSIGN
            tt_webKunde.PrivatTlf    = Kunde.PrivatTlf
            tt_webKunde.Kjon         = Kunde.Kjon
            tt_webKunde.FodtDato     = Kunde.FodtDato
            tt_webKunde.Alder        = Kunde.Alder
            tt_webKunde.FaktAdresse1 = Kunde.FaktAdresse1
            tt_webKunde.FaktAdresse2 = Kunde.FaktAdresse2
            tt_webKunde.FaktPostNr   = Kunde.FaktPostNr
            tt_webKunde.FaktTekstNr  = Kunde.FaktTekstNr
            tt_webKunde.DeresRef     = Kunde.DeresRef
            tt_webKunde.Privat       = Kunde.Privat
            tt_webKunde.FaktLand     = Kunde.FaktLand
            tt_webKunde.ValKod       = Kunde.ValKod
            tt_webKunde.BetType      = Kunde.BetType
            tt_webKunde.KundeSaldo   = Kunde.KundeSaldo
            tt_webKunde.ForsteKjop   = Kunde.ForsteKjop
            tt_webKunde.SisteKjop    = Kunde.SisteKjop
            tt_webKunde.Purregebyr   = Kunde.Purregebyr
            tt_webKunde.Fakturagebyr = Kunde.Fakturagebyr
            tt_webKunde.WebKunde     = Kunde.WebKunde
            tt_webKunde.Aktiv        = Kunde.Aktiv.

        ASSIGN
            tt_webKunde.Hovedkunde          = Kunde.Hovedkunde
            tt_webKunde.KobletTilKunde      = Kunde.KobletTilKunde
            tt_webKunde.Faktureringsperiode = Kunde.Faktureringsperiode
            tt_webKunde.Kilde               = Kunde.Kilde
            tt_webKunde.TilgKilde           = Kunde.TilgKilde
            tt_webKunde.EksterntKundeNr     = Kunde.EksterntKundeNr
            tt_webKunde.Momskod             = Kunde.Momskod.
            .
        FIND FIRST NumLandKode NO-LOCK WHERE
          NumLandKode.Land = tt_webKunde.FaktLand NO-ERROR.
        IF AVAILABLE NumLandKode THEN 
          FIND FIRST AlfaLandKode NO-LOCK WHERE
            AlfaLandKode.NumLandKode = NumLandKode.NumLandKode NO-ERROR.
        IF AVAILABLE AlfaLandKode THEN 
            tt_webKunde.FaktLand = AlfaLandKode.AlfaKode2.
        
        FIND FIRST NumLandKode NO-LOCK WHERE
          NumLandKode.Land = tt_webKunde.LevLand NO-ERROR.
        IF AVAILABLE NumLandKode THEN 
          FIND FIRST AlfaLandKode NO-LOCK WHERE
            AlfaLandKode.NumLandKode = NumLandKode.NumLandKode NO-ERROR.
        IF AVAILABLE AlfaLandKode THEN 
            tt_webKunde.LevLand = AlfaLandKode.AlfaKode2.
            
        FIND FIRST NumLandKode NO-LOCK WHERE
          NumLandKode.Land = tt_webKunde.Land NO-ERROR.
        IF AVAILABLE NumLandKode THEN 
          FIND FIRST AlfaLandKode NO-LOCK WHERE
            AlfaLandKode.NumLandKode = NumLandKode.NumLandKode NO-ERROR.
        IF AVAILABLE AlfaLandKode THEN 
            tt_webKunde.Land = AlfaLandKode.AlfaKode2.
          
    END. /* KUNDEBLOKK */
END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellKundeGruppe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellKundeGruppe Procedure 
PROCEDURE ByggTmpTabellKundeGruppe :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      
    ------------------------------------------------------------------------------*/

    DEF VAR iRecType AS INT NO-UNDO.

    FOR EACH tt_webKundeGruppe:
        DELETE tt_webKundeGruppe.
    END.

    WEBBUT:
    FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "KundeGruppe": 
        FIND KundeGruppe NO-LOCK WHERE
            KundeGruppe.GruppeId = int(TT_Elogg.Verdier) NO-ERROR.
        IF AVAILABLE KundeGruppe THEN
        DO:
            CREATE tt_webKundeGruppe.
            ASSIGN
                tt_webKundeGruppe.iRecType       = tt_ELogg.EndringsType
                tt_webKundeGruppe.GruppeId       = KundeGruppe.GruppeId
                tt_webKundeGruppe.Beskrivelse    = KundeGruppe.Beskrivelse
            .
        END.
    END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellKundeKort) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellKundeKort Procedure 
PROCEDURE ByggTmpTabellKundeKort :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      
    ------------------------------------------------------------------------------*/

    DEF VAR iRecType AS INT NO-UNDO.

    FOR EACH tt_webKundeKort:
        DELETE tt_webKundeKort.
    END.

    WEBBUT:
    FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "KundeKort": 
        FIND KundeKort NO-LOCK WHERE
                               KundeKort.KortNr = (TT_Elogg.Verdier) NO-ERROR.
        IF AVAILABLE KundeKort THEN
        DO:
            CREATE tt_webKundeKort.
            ASSIGN
                tt_webKundeKort.iRecType = tt_ELogg.EndringsType
                tt_webKundeKort.KundeNr        = KundeKort.KundeNr
                tt_webKundeKort.EDato          = KundeKort.EDato
                tt_webKundeKort.ETid           = KundeKort.ETid
                tt_webKundeKort.BrukerID       = KundeKort.BrukerID
                tt_webKundeKort.RegistrertDato = KundeKort.RegistrertDato
                tt_webKundeKort.RegistrertTid  = KundeKort.RegistrertTid
                tt_webKundeKort.RegistrertAv   = KundeKort.RegistrertAv
                tt_webKundeKort.KortNr         = KundeKort.KortNr
                tt_webKundeKort.Merknad        = KundeKort.Merknad
                tt_webKundeKort.AktivertDato   = KundeKort.AktivertDato
                tt_webKundeKort.UtgarDato      = KundeKort.UtgarDato
                tt_webKundeKort.Sperret        = KundeKort.Sperret
                tt_webKundeKort.Innehaver      = KundeKort.Innehaver
                tt_webKundeKort.MedlemsNr      = KundeKort.MedlemsNr
                tt_webKundeKort.InterntKKortId = KundeKort.InterntKKortId
                .
        END.
    END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellKundeSaldo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellKundeSaldo Procedure 
PROCEDURE ByggTmpTabellKundeSaldo :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      
    ------------------------------------------------------------------------------*/

    DEF VAR iRecType AS INT NO-UNDO.

    FOR EACH tt_webKundeSaldo:
        DELETE tt_webKundeSaldo.
    END.

    WEBBUT:
    FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "KundeSaldo": 
        FIND KundeSaldo NO-LOCK WHERE
                               KundeSaldo.KundeNr  = dec(ENTRY(1,TT_Elogg.Verdier,'|')) AND 
                               KundeSaldo.ButikkNr = int(ENTRY(2,TT_Elogg.Verdier,'|')) 
                               NO-ERROR.
        IF AVAILABLE KundeSaldo THEN
        DO:
            CREATE tt_webKundeSaldo.
            ASSIGN
                tt_webKundeSaldo.iRecType = tt_ELogg.EndringsType
                tt_webKundeSaldo.KundeNr        = KundeSaldo.KundeNr
                tt_webKundeSaldo.EDato          = KundeSaldo.EDato
                tt_webKundeSaldo.ETid           = KundeSaldo.ETid
                tt_webKundeSaldo.BrukerID       = KundeSaldo.BrukerID
                tt_webKundeSaldo.RegistrertDato = KundeSaldo.RegistrertDato
                tt_webKundeSaldo.RegistrertTid  = KundeSaldo.RegistrertTid
                tt_webKundeSaldo.RegistrertAv   = KundeSaldo.RegistrertAv
                tt_webKundeSaldo.ButikkNr       = KundeSaldo.ButikkNr
                tt_webKundeSaldo.ForsteDato     = KundeSaldo.ForsteDato
                tt_webKundeSaldo.DatoSiste      = KundeSaldo.DatoSiste
                tt_webKundeSaldo.ForsteTid      = KundeSaldo.ForsteTid
                tt_webKundeSaldo.SisteTid       = KundeSaldo.SisteTid
                tt_webKundeSaldo.Saldo          = KundeSaldo.Saldo
                tt_webKundeSaldo.TotaltKjop     = KundeSaldo.TotaltKjop.
                .
        END.
    END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellLager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellLager Procedure 
PROCEDURE ByggTmpTabellLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/

DEFINE VARIABLE iRecType AS INT       NO-UNDO.
DEFINE VARIABLE cKode    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOldArtikkelnr AS CHARACTER   NO-UNDO.

FOR EACH tt_webLager:
    DELETE tt_webLager.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "Lager":
    
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = DEC(ENTRY(1,TT_Elogg.Verdier,CHR(1))) NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
        NEXT WEBBUT.
    FIND artbut WHERE artbut.artikkelnr = ArtBas.artikkelnr AND artbut.butik = wbButiker.butik NO-LOCK NO-ERROR.
    IF NOT AVAIL artbut THEN
        NEXT WEBBUT.
    /* Legges akkumulert lager ut, skal bare den første lager - butikk elogg posten behandles. De andre skippes. */
    IF iAkkumLager = 2 AND 
        CAN-FIND(FIRST tt2_ELogg WHERE  
                       tt2_ELogg.EksterntSystem = tt_Elogg.EksterntSystem AND 
                       tt2_ELogg.TabellNavn     = tt_Elogg.TabellNavn AND
                       tt2_ELogg.Verdier        = ENTRY(1,tt_Elogg.Verdier,CHR(1)) AND 
                       tt2_Elogg.EndringsType   = 1) THEN 
        NEXT WEBBUT. 
    /* Henter nettbutikkens prisprofil. Finnes ikke denne, benyttes sentrallagerets prisprofil. */
    FIND FIRST ArtPris OF ArtBas NO-LOCK WHERE  
      ArtPris.ProfilNr = wbButiker.ProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
      FIND FIRST ArtPris OF ArtBas NO-LOCK WHERE  
        ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN NEXT WEBBUT.
    FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE VarGr THEN
        FIND Moms OF VarGr NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Moms THEN NEXT WEBBUT.
       
    IF iAkkumLager = 1 THEN 
    DO: 
    BYGG:
    FOR EACH Lager WHERE
        Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
        Lager.Butik      = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1))) AND  
        CAN-DO(cLagerLst,ENTRY(2,TT_Elogg.Verdier,CHR(1))) NO-LOCK:
          
        FOR EACH ArtLag NO-LOCK WHERE
            ArtLag.ArtikkelNr = Lager.ArtikkelNr AND
            ArtLag.Butik      = Lager.butik:

            cKode = ''.
            /* Henter strekkode for størrelsen */
            STREK1:
            FOR EACH Strekkode NO-LOCK WHERE
              Strekkode.ArtikkelNr   = ArtLag.ArtikkelNr AND
              Strekkode.StrKode      = ArtLag.StrKode AND
              LENGTH(Strekkode.Kode) = 13 AND               
              SUBSTRING(Strekkode.Kode,1,2) <> '02'
              BREAK BY Strekkode.ArtikkelNr
                    BY Strekkode.EDato DESCENDING:
              cKode = Strekkode.Kode.
              IF cKode <> '' THEN LEAVE STREK1.
            END.     
            
            /* Da tar vi interngenerert. */
            IF cKode = '' THEN 
            STREK2:
            FOR EACH Strekkode NO-LOCK WHERE
              Strekkode.ArtikkelNr   = ArtLag.ArtikkelNr AND
              Strekkode.StrKode      = ArtLag.StrKode AND
              LENGTH(Strekkode.Kode) = 13:
              cKode = Strekkode.Kode.
              IF cKode <> '' THEN LEAVE STREK2.
            END.      
            /* Da tar vi plunr. */
            IF cKode = '' THEN 
            PLU:
            FOR EACH Strekkode NO-LOCK WHERE
              Strekkode.ArtikkelNr   = ArtLag.ArtikkelNr AND
              Strekkode.StrKode      = ArtLag.StrKode:
              cKode = Strekkode.Kode.
              IF cKode <> '' THEN LEAVE PLU.
            END.      
            /* Legger bare ut lager der hvor det finnes strekkode */
            IF cKode <> '' THEN 
            DO:
              CREATE tt_webLager.
              ASSIGN
                  tt_webLager.iRecType        = 1
                  tt_webLager.ArtikkelNr      = dec(ENTRY(1,TT_Elogg.Verdier,CHR(1)))
                  tt_webLager.Butik           = Lager.butik
                  tt_webLager.LevKod          = ArtBas.LevKod
                  tt_webLager.Beskr           = ArtBas.Beskr
                  tt_webLager.LevFargKod      = ArtBas.LevFargKod
                  tt_webLager.StrTypeId       = ArtBas.StrTypeId
                  tt_webLager.StrKode         = ArtLag.StrKode
                  tt_webLager.Storl           = ArtLag.Storl
                  tt_webLager.VVarekost       = Lager.VVarekost
                  tt_webLager.LagAnt          = ArtLag.LagAnt
                  tt_webLager.Kode            = cKode
                  tt_webLager.Pris            = IF AVAILABLE ArtPris THEN ArtPris.Pris[1] ELSE 0
                  .
              IF tt_webLager.LagAnt < 0 OR tt_webLager.LagAnt = ? THEN 
                  tt_webLager.LagAnt = 0. 
            END.
            /* Hvis ikke nettbutikken har eget felt på artikkelen for å styre publisering  */
            /* fra butikksystemet, kan dette flagges ved å legge -1000 i antallsfeltet på  */
            /* artikkelens lagerposter.                                                    */
            IF ArtBas.PubliserINettbutikk = FALSE AND bNegLager THEN
            DO:
                ASSIGN
                    tt_webLager.LagAnt = -1000.
            END.
        END.
    END. /* BYGG */
    END.
    
    /* Legger ut sum lagerantall for alle butikker, på nettbutikk */        
    ELSE IF iAkkumLager = 2 THEN
    BYGG_AKKUM_LAGER: 
    DO:
        /* Logger at aukkumulering er kjørt for artikkelen. */
        IF NOT CAN-FIND(FIRST tt2_ELogg WHERE  
            tt2_ELogg.EksterntSystem = tt_Elogg.EksterntSystem AND 
            tt2_ELogg.TabellNavn     = tt_Elogg.TabellNavn AND
            tt2_ELogg.Verdier        = ENTRY(1,tt_Elogg.Verdier,CHR(1)) AND 
            tt2_Elogg.EndringsType   = 1) THEN 
        DO:
            CREATE tt2_Elogg.
            ASSIGN 
                tt2_ELogg.EksterntSystem = tt_Elogg.EksterntSystem 
                tt2_ELogg.TabellNavn     = tt_Elogg.TabellNavn 
                tt2_ELogg.Verdier        = ENTRY(1,tt_Elogg.Verdier,CHR(1))
                tt2_Elogg.EndringsType   = 1. 
        END.
        
        FOR EACH Lager WHERE
            Lager.ArtikkelNr = ArtBas.ArtikkelNr AND  
            CAN-DO(cLagerLst,STRING(Lager.Butik)) NO-LOCK:
            FOR EACH ArtLag NO-LOCK WHERE
                ArtLag.ArtikkelNr = Lager.ArtikkelNr AND
                ArtLag.Butik      = Lager.butik:

                cKode = ''.
                
                /* Henter strekkode for størrelsen */
                STREK1:
                FOR EACH Strekkode NO-LOCK WHERE
                    Strekkode.ArtikkelNr   = ArtLag.ArtikkelNr AND
                    Strekkode.StrKode      = ArtLag.StrKode AND
                    LENGTH(Strekkode.Kode) = 13 AND               
                    SUBSTRING(Strekkode.Kode,1,2) <> '02'
                    BREAK BY Strekkode.ArtikkelNr
                    BY Strekkode.EDato DESCENDING
                    :
                    cKode = Strekkode.Kode.
                    IF cKode <> '' THEN LEAVE STREK1.
                END.      
                /* Da tar vi interngenerert. */
                IF cKode = '' THEN 
                    STREK2:
                    FOR EACH Strekkode NO-LOCK WHERE
                        Strekkode.ArtikkelNr   = ArtLag.ArtikkelNr AND
                        Strekkode.StrKode      = ArtLag.StrKode AND
                        LENGTH(Strekkode.Kode) = 13:
                        cKode = Strekkode.Kode.
                        IF cKode <> '' THEN LEAVE STREK2.
                    END.      
                /* Da tar vi plunr. */
                IF cKode = '' THEN 
                    PLU:
                    FOR EACH Strekkode NO-LOCK WHERE
                        Strekkode.ArtikkelNr   = ArtLag.ArtikkelNr AND
                        Strekkode.StrKode      = ArtLag.StrKode:
                        cKode = Strekkode.Kode.
                        IF cKode <> '' THEN LEAVE PLU.
                    END.      
                /* Legger bare ut lager der hvor det finnes strekkode. */                
                IF cKode <> '' THEN 
                DO:
                    FIND FIRST tt_webLager WHERE                    
                        tt_webLager.ArtikkelNr = dec(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                        tt_webLager.Butik      = wbButiker.butik AND 
                        tt_webLager.StrKode    = ArtLag.StrKode NO-ERROR.
                    IF NOT AVAILABLE tt_webLager THEN 
                    DO:
                        CREATE tt_webLager.
                        ASSIGN
                            tt_webLager.iRecType   = 1
                            tt_webLager.ArtikkelNr = dec(ENTRY(1,TT_Elogg.Verdier,CHR(1)))
                            tt_webLager.Butik      = wbButiker.butik
                            tt_webLager.StrKode    = ArtLag.StrKode.
                    END.
                    ASSIGN       
                        tt_webLager.Kode       = cKode
                        tt_webLager.LevKod     = ArtBas.LevKod
                        tt_webLager.Beskr      = ArtBas.Beskr
                        tt_webLager.LevFargKod = ArtBas.LevFargKod
                        tt_webLager.StrTypeId  = ArtBas.StrTypeId
                        tt_webLager.Storl      = ArtLag.Storl
                        tt_webLager.VVarekost  = Lager.VVarekost
                        tt_webLager.LagAnt     = tt_webLager.LagAnt + ArtLag.LagAnt
                        tt_webLager.Pris       = IF AVAILABLE ArtPris THEN ArtPris.Pris[1] ELSE 0
                        .
                    IF tt_webLager.LagAnt < 0 OR tt_webLager.LagAnt = ? THEN 
                        tt_webLager.LagAnt = 0. 
                END. 
                /* Hvis ikke nettbutikken har eget felt på artikkelen for å styre publisering  */
                /* fra butikksystemet, kan dette flagges ved å legge -1000 i antallsfeltet på  */
                /* artikkelens lagerposter.                                                    */
                IF ArtBas.PubliserINettbutikk = FALSE AND bNegLager THEN
                DO:
                    ASSIGN
                        tt_webLager.LagAnt = -1000.
                END.
            END.
        END. /* BYGG */
    END. /* BYGG_AKKUM_LAGER */        
    
    ELSE IF iAkkumLager = 3 THEN
    AKKUM_OG_SPES_PR_BUTIKK:
    DO:
        /* Legger ut lager pr. butikk. */
        BYGG:
        FOR EACH Lager WHERE
            Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
            Lager.Butik      = INT(ENTRY(2,TT_Elogg.Verdier,CHR(1))) AND  
            CAN-DO(cLagerLst,ENTRY(2,TT_Elogg.Verdier,CHR(1))) NO-LOCK:
          
            ENKELTBUTIKKER:
            FOR EACH ArtLag NO-LOCK WHERE
                ArtLag.ArtikkelNr = Lager.ArtikkelNr AND
                ArtLag.Butik      = Lager.butik:

                cKode = ''.
                /* Henter strekkode for størrelsen */
                STREK1:
                FOR EACH Strekkode NO-LOCK WHERE
                    Strekkode.ArtikkelNr   = ArtLag.ArtikkelNr AND
                    Strekkode.StrKode      = ArtLag.StrKode AND
                    LENGTH(Strekkode.Kode) = 13 AND               
                    SUBSTRING(Strekkode.Kode,1,2) <> '02'
                    BREAK BY Strekkode.ArtikkelNr
                    BY Strekkode.EDato DESCENDING:
                    cKode = Strekkode.Kode.
                    IF cKode <> '' THEN LEAVE STREK1.
                END.     
            
                /* Da tar vi interngenerert. */
                IF cKode = '' THEN 
                    STREK2:
                    FOR EACH Strekkode NO-LOCK WHERE
                        Strekkode.ArtikkelNr   = ArtLag.ArtikkelNr AND
                        Strekkode.StrKode      = ArtLag.StrKode AND
                        LENGTH(Strekkode.Kode) = 13:
                        cKode = Strekkode.Kode.
                        IF cKode <> '' THEN LEAVE STREK2.
                    END.      
                /* Da tar vi plunr. */
                IF cKode = '' THEN 
                    PLU:
                    FOR EACH Strekkode NO-LOCK WHERE
                        Strekkode.ArtikkelNr   = ArtLag.ArtikkelNr AND
                        Strekkode.StrKode      = ArtLag.StrKode:
                        cKode = Strekkode.Kode.
                        IF cKode <> '' THEN LEAVE PLU.
                    END.      
                /* Legger bare ut lager der hvor det finnes strekkode */
                IF cKode <> '' THEN 
                DO:
                    CREATE tt_webLager.
                    ASSIGN
                        tt_webLager.iRecType   = 1
                        tt_webLager.ArtikkelNr = dec(ENTRY(1,TT_Elogg.Verdier,CHR(1)))
                        tt_webLager.Butik      = Lager.butik
                        tt_webLager.LevKod     = ArtBas.LevKod
                        tt_webLager.Beskr      = ArtBas.Beskr
                        tt_webLager.LevFargKod = ArtBas.LevFargKod
                        tt_webLager.StrTypeId  = ArtBas.StrTypeId
                        tt_webLager.StrKode    = ArtLag.StrKode
                        tt_webLager.Storl      = ArtLag.Storl
                        tt_webLager.VVarekost  = Lager.VVarekost
                        tt_webLager.LagAnt     = ArtLag.LagAnt
                        tt_webLager.Kode       = cKode
                        tt_webLager.Pris       = IF AVAILABLE ArtPris THEN ArtPris.Pris[1] ELSE 0
                        .
                    IF tt_webLager.LagAnt < 0 OR tt_webLager.LagAnt = ? THEN 
                        tt_webLager.LagAnt = 0. 
                END.
                /* Hvis ikke nettbutikken har eget felt på artikkelen for å styre publisering  */
                /* fra butikksystemet, kan dette flagges ved å legge -1000 i antallsfeltet på  */
                /* artikkelens lagerposter.                                                    */
                IF ArtBas.PubliserINettbutikk = FALSE AND bNegLager THEN
                DO:
                    ASSIGN
                        tt_webLager.LagAnt = -1000.
                END.
            END. /* ENKELTBUTIKKER*/
        END. /* BYGG */

        /* Legger ut lager pr. butikk. */
        IF iSumButikk > 0 AND ENTRY(1,TT_Elogg.Verdier,CHR(1)) <> cOldArtikkelnr THEN
        DO:
            cOldArtikkelnr = ENTRY(1,TT_Elogg.Verdier,CHR(1)).
            /* Nettbutikkens lager skal være med */
            cTekst = cLagerLst.
            IF NOT CAN-DO(cTekst,cNettButLager) THEN 
                cTekst = cNettButLager + ',' + cTekst. 
        BYGG_SUM:
        FOR EACH Lager WHERE
            Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
            CAN-DO(cTekst,STRING(Lager.Butik)) NO-LOCK:
          
            ENKELTBUTIKKER:
            FOR EACH ArtLag NO-LOCK WHERE
                ArtLag.ArtikkelNr = Lager.ArtikkelNr AND
                ArtLag.Butik      = Lager.butik:

                cKode = ''.
                /* Henter strekkode for størrelsen */
                STREK1:
                FOR EACH Strekkode NO-LOCK WHERE
                    Strekkode.ArtikkelNr   = ArtLag.ArtikkelNr AND
                    Strekkode.StrKode      = ArtLag.StrKode AND
                    LENGTH(Strekkode.Kode) = 13 AND               
                    SUBSTRING(Strekkode.Kode,1,2) <> '02'
                    BREAK BY Strekkode.ArtikkelNr
                    BY Strekkode.EDato DESCENDING:
                    cKode = Strekkode.Kode.
                    IF cKode <> '' THEN LEAVE STREK1.
                END.     
            
                /* Da tar vi interngenerert. */
                IF cKode = '' THEN 
                    STREK2:
                    FOR EACH Strekkode NO-LOCK WHERE
                        Strekkode.ArtikkelNr   = ArtLag.ArtikkelNr AND
                        Strekkode.StrKode      = ArtLag.StrKode AND
                        LENGTH(Strekkode.Kode) = 13:
                        cKode = Strekkode.Kode.
                        IF cKode <> '' THEN LEAVE STREK2.
                    END.      
                /* Da tar vi plunr. */
                IF cKode = '' THEN 
                    PLU:
                    FOR EACH Strekkode NO-LOCK WHERE
                        Strekkode.ArtikkelNr   = ArtLag.ArtikkelNr AND
                        Strekkode.StrKode      = ArtLag.StrKode:
                        cKode = Strekkode.Kode.
                        IF cKode <> '' THEN LEAVE PLU.
                    END.      
                /* Legger bare ut sumlager. */
                IF cKode <> '' THEN 
                DO:
                    FIND tt_webLAger NO-LOCK WHERE 
                        tt_webLager.ArtikkelNr = DECIMAL(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND 
                        tt_webLager.Butik      = iSumbutikk AND
                        tt_webLager.Kode       = cKode NO-ERROR.
                    IF NOT AVAILABLE tt_webLager THEN 
                    DO:
                        CREATE tt_webLager.
                        ASSIGN 
                            tt_webLager.ArtikkelNr = DECIMAL(ENTRY(1,TT_Elogg.Verdier,CHR(1)))  
                            tt_webLager.Butik      = iSumbutikk 
                            tt_webLager.Kode       = cKode 
                            tt_webLager.LevKod     = ArtBas.LevKod
                            tt_webLager.Beskr      = ArtBas.Beskr
                            tt_webLager.LevFargKod = ArtBas.LevFargKod
                            tt_webLager.StrTypeId  = ArtBas.StrTypeId
                            tt_webLager.StrKode    = ArtLag.StrKode
                            tt_webLager.Storl      = ArtLag.Storl
                            tt_webLager.VVarekost  = Lager.VVarekost
                            tt_webLager.Pris       = IF AVAILABLE ArtPris THEN ArtPris.Pris[1] ELSE 0
                            .
                    END.
                    tt_webLager.LagAnt = tt_webLager.LagAnt + 
                        (IF ArtLag.LagAnt <> ? THEN ArtLag.LagAnt ELSE 0). 
                END.
            END. /* ENKELTBUTIKKER*/
        END. /* BYGG_SUM */
        END.
    END. /* AKKUM_OG_SPES_PR_BUTIKK*/
END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-byggTmpTabellLast-Sko) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE byggTmpTabellLast-Sko Procedure 
PROCEDURE byggTmpTabellLast-Sko :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webLast-Sko:
    DELETE tt_webLast-Sko.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "Last-Sko": 
    FIND Last-Sko NO-LOCK WHERE
        Last-Sko.Last-Id = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE Last-Sko THEN
    DO:
        CREATE tt_webLast-Sko.
        ASSIGN
            tt_webLast-Sko.iRecType  = tt_ELogg.EndringsType
            tt_webLast-Sko.Last-Id   = Last-Sko.Last-Id
            tt_webLast-Sko.LastBeskr = Last-Sko.LastBeskr
            .
    END.
END. /* WEBBUT */   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellLevBas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellLevBas Procedure 
PROCEDURE ByggTmpTabellLevBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webLevBas:
    DELETE tt_webLevBas.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "LevBas": 
    FIND LevBas NO-LOCK WHERE
        LevBas.LevNr = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE LevBas THEN
    DO:
        CREATE tt_webLevBas.
        ASSIGN
            tt_webLevBas.iRecType    = tt_ELogg.EndringsType
            tt_webLevBas.LevNr       = LevBas.LevNr
            tt_webLevBas.LevNamn     = LevBas.LevNamn
            .
    END.
END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellMaterial) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellMaterial Procedure 
PROCEDURE ByggTmpTabellMaterial :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webMaterial:
    DELETE tt_webMaterial.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "Material": 
    FIND Material NO-LOCK WHERE
        Material.MatKod = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE Material THEN
    DO:
        CREATE tt_webMaterial.
        ASSIGN
            tt_webMaterial.iRecType    = tt_ELogg.EndringsType
            tt_webMaterial.MatKod      = Material.MatKod
            tt_webMaterial.MatBeskr    = Material.MatBeskr
            .
    END.
END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellMedlem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellMedlem Procedure 
PROCEDURE ByggTmpTabellMedlem :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      
    ------------------------------------------------------------------------------*/

    DEF VAR iRecType AS INT NO-UNDO.

    FOR EACH tt_webMedlem:
        DELETE tt_webMedlem.
    END.

    WEBBUT:
    FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "Medlem": 
        FIND Medlem NO-LOCK WHERE
            Medlem.MedlemsNr  = dec(TT_Elogg.Verdier) NO-ERROR.
        IF AVAILABLE Medlem THEN
        DO:
            CREATE tt_webMedlem.
            ASSIGN
                tt_webMedlem.iRecType       = tt_ELogg.EndringsType
                tt_webMedlem.MedlemsNr         = Medlem.MedlemsNr
                tt_webMedlem.ForNavn           = Medlem.ForNavn
                tt_webMedlem.EDato             = Medlem.EDato
                tt_webMedlem.ETid              = Medlem.ETid
                tt_webMedlem.BrukerID          = Medlem.BrukerID
                tt_webMedlem.RegistrertDato    = Medlem.RegistrertDato
                tt_webMedlem.RegistrertTid     = Medlem.RegistrertTid
                tt_webMedlem.RegistrertAv      = Medlem.RegistrertAv
                tt_webMedlem.MedType           = Medlem.MedType
                tt_webMedlem.MedGruppe         = Medlem.MedGruppe
                tt_webMedlem.Adresse1          = Medlem.Adresse1
                tt_webMedlem.Adresse2          = Medlem.Adresse2
                tt_webMedlem.PostNr            = Medlem.PostNr
                tt_webMedlem.Telefon           = Medlem.Telefon
                tt_webMedlem.Telefaks          = Medlem.Telefaks
                tt_webMedlem.MobilTlf          = Medlem.MobilTlf
                tt_webMedlem.Land              = Medlem.Land
                tt_webMedlem.Opphort           = Medlem.Opphort
                tt_webMedlem.ButikkNr          = Medlem.ButikkNr
                tt_webMedlem.BydelsNr          = Medlem.BydelsNr
                tt_webMedlem.ePostAdresse      = Medlem.ePostAdresse
                tt_webMedlem.EtterNavn         = Medlem.EtterNavn
                tt_webMedlem.HovedMedlemFlagg  = Medlem.HovedMedlemFlagg
                tt_webMedlem.HovedMedlemsNr    = Medlem.HovedMedlemsNr
                tt_webMedlem.FodselsDato       = Medlem.FodselsDato
                tt_webMedlem.FodtAr            = Medlem.FodtAr
                tt_webMedlem.Kjonn             = Medlem.Kjonn
                tt_webMedlem.RegKode           = Medlem.RegKode
                tt_webMedlem.KundeNr           = Medlem.KundeNr
                tt_webMedlem.Aktiv             = Medlem.Aktiv
                tt_webMedlem.AktivertFraWeb    = Medlem.AktivertFraWeb
                tt_webMedlem.WebBrukerId       = Medlem.WebBrukerId
                tt_webMedlem.WebPassord        = Medlem.WebPassord
                tt_webMedlem.Kilde             = Medlem.Kilde
                tt_webMedlem.TilgKilde         = Medlem.TilgKilde
                tt_webMedlem.Rabatt            = Medlem.Rabatt
                tt_webMedlem.EksterntMedlemsNr = Medlem.EksterntMedlemsNr
                tt_webMedlem.Personnr          = Medlem.Medleminfo.
                .
        END.
    END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellMedlemSaldo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellMedlemSaldo Procedure 
PROCEDURE ByggTmpTabellMedlemSaldo :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      
    ------------------------------------------------------------------------------*/

    DEF VAR iRecType AS INT NO-UNDO.

    FOR EACH tt_webMedlemSaldo:
        DELETE tt_webMedlemSaldo.
    END.

    WEBBUT:
    FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "MedlemSaldo": 
        FIND MedlemSaldo NO-LOCK WHERE
            MedlemSaldo.MedlemsNr  = dec(ENTRY(1,TT_Elogg.Verdier,'|')) AND
            MedlemSaldo.ButikkNr   = int(ENTRY(2,TT_Elogg.Verdier,'|')) 
            NO-ERROR.
        IF AVAILABLE MedlemSaldo THEN
        DO:
            CREATE tt_webMedlemSaldo.
            ASSIGN
                tt_webMedlemSaldo.iRecType        = tt_ELogg.EndringsType
                tt_webMedlemSaldo.MedlemsNr      = MedlemSaldo.MedlemsNr
                tt_webMedlemSaldo.EDato          = MedlemSaldo.EDato
                tt_webMedlemSaldo.ETid           = MedlemSaldo.ETid
                tt_webMedlemSaldo.BrukerID       = MedlemSaldo.BrukerID
                tt_webMedlemSaldo.RegistrertDato = MedlemSaldo.RegistrertDato
                tt_webMedlemSaldo.RegistrertTid  = MedlemSaldo.RegistrertTid
                tt_webMedlemSaldo.RegistrertAv   = MedlemSaldo.RegistrertAv
                tt_webMedlemSaldo.ButikkNr       = MedlemSaldo.ButikkNr
                tt_webMedlemSaldo.ForsteDato     = MedlemSaldo.ForsteDato
                tt_webMedlemSaldo.DatoSiste      = MedlemSaldo.DatoSiste
                tt_webMedlemSaldo.ForsteTid      = MedlemSaldo.ForsteTid
                tt_webMedlemSaldo.SisteTid       = MedlemSaldo.SisteTid
                tt_webMedlemSaldo.Saldo          = MedlemSaldo.Saldo
                tt_webMedlemSaldo.TotaltKjop     = MedlemSaldo.TotaltKjop.
            .
        END.
    END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellMedlemsGruppe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellMedlemsGruppe Procedure 
PROCEDURE ByggTmpTabellMedlemsGruppe :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      
    ------------------------------------------------------------------------------*/

    DEF VAR iRecType AS INT NO-UNDO.

    FOR EACH tt_webMedlemsGruppe:
        DELETE tt_webMedlemsGruppe.
    END.

    WEBBUT:
    FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "MedlemsGruppe": 
        FIND MedlemsGruppe NO-LOCK WHERE
             MedlemsGruppe.MedGruppe = int(TT_Elogg.Verdier) NO-ERROR.
        IF AVAILABLE MedlemsGruppe THEN
        DO:
            CREATE tt_webMedlemsGruppe.
            ASSIGN
                tt_webMedlemsGruppe.iRecType       = tt_ELogg.EndringsType
                tt_webMedlemsGruppe.MedGruppe      = MedlemsGruppe.MedGruppe
                tt_webMedlemsGruppe.Beskrivelse    = MedlemsGruppe.Beskrivelse
                tt_webMedlemsGruppe.Notat          = MedlemsGruppe.Notat
                tt_webMedlemsGruppe.EDato          = MedlemsGruppe.EDato
                tt_webMedlemsGruppe.ETid           = MedlemsGruppe.ETid
                tt_webMedlemsGruppe.BrukerID       = MedlemsGruppe.BrukerID
                tt_webMedlemsGruppe.RegistrertDato = MedlemsGruppe.RegistrertDato
                tt_webMedlemsGruppe.RegistrertTid  = MedlemsGruppe.RegistrertTid
                tt_webMedlemsGruppe.RegistrertAv   = MedlemsGruppe.RegistrertAv.
            .
        END.
    END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellMedlemsKort) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellMedlemsKort Procedure 
PROCEDURE ByggTmpTabellMedlemsKort :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      
    ------------------------------------------------------------------------------*/

    DEF VAR iRecType AS INT NO-UNDO.

    FOR EACH tt_webMedlemsKort:
        DELETE tt_webMedlemsKort.
    END.

    WEBBUT:
    FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "MedlemsKort": 
        FIND MedlemsKort NO-LOCK WHERE
                                 MedlemsKort.KortNr  = (TT_Elogg.Verdier) NO-ERROR.
        IF AVAILABLE MedlemsKort THEN
        DO:
            CREATE tt_webMedlemsKort.
            ASSIGN
                tt_webMedlemsKort.iRecType       = tt_ELogg.EndringsType
                tt_webMedlemsKort.MedlemsNr       = MedlemsKort.MedlemsNr
                tt_webMedlemsKort.EDato           = MedlemsKort.EDato
                tt_webMedlemsKort.ETid            = MedlemsKort.ETid
                tt_webMedlemsKort.BrukerID        = MedlemsKort.BrukerID
                tt_webMedlemsKort.RegistrertDato  = MedlemsKort.RegistrertDato
                tt_webMedlemsKort.RegistrertTid   = MedlemsKort.RegistrertTid
                tt_webMedlemsKort.RegistrertAv    = MedlemsKort.RegistrertAv
                tt_webMedlemsKort.KortNr          = MedlemsKort.KortNr
                tt_webMedlemsKort.Merknad         = MedlemsKort.Merknad
                tt_webMedlemsKort.AktivertDato    = MedlemsKort.AktivertDato
                tt_webMedlemsKort.UtgarDato       = MedlemsKort.UtgarDato
                tt_webMedlemsKort.Sperret         = MedlemsKort.Sperret
                tt_webMedlemsKort.Innehaver       = MedlemsKort.Innehaver
                tt_webMedlemsKort.KundeRabattKort = MedlemsKort.KundeRabattKort
                tt_webMedlemsKort.KundeKortNr     = MedlemsKort.KundeKortNr
                tt_webMedlemsKort.KortType        = MedlemsKort.KortType
                tt_webMedlemsKort.InterntKKortId  = MedlemsKort.InterntKKortId.
                .
        END.
    END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellMoms) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellMoms Procedure 
PROCEDURE ByggTmpTabellMoms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webMoms:
    DELETE tt_webMoms.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "Moms": 
    FIND Moms NO-LOCK WHERE
        Moms.MomsKod = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE Moms THEN
    DO:
        CREATE tt_webMoms.
        ASSIGN
            tt_webMoms.iRecType    = tt_ELogg.EndringsType
            tt_webMoms.MomsKod     = Moms.MomsKod
            tt_webMoms.MomsProc    = Moms.MomsProc
            tt_webMoms.Beskrivelse = Moms.Beskrivelse
            .
    END.
END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-byggTmpTabellOvandel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE byggTmpTabellOvandel Procedure 
PROCEDURE byggTmpTabellOvandel :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webOvandel:
    DELETE tt_webOvandel.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "Ovandel": 
    FIND Ovandel NO-LOCK WHERE
        Ovandel.Ov-Id = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE Ovandel THEN
    DO:
        CREATE tt_webOvandel.
        ASSIGN
            tt_webOvandel.iRecType    = tt_ELogg.EndringsType
            tt_webOvandel.Ov-Id       = Ovandel.Ov-Id
            tt_webOvandel.OvBeskr     = Ovandel.OvBeskr
            .
    END.
END. /* WEBBUT */   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellProdusent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellProdusent Procedure 
PROCEDURE ByggTmpTabellProdusent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webProdusent:
    DELETE tt_webProdusent.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "Produsent": 
    FIND Produsent NO-LOCK WHERE
        Produsent.ProdNr = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE Produsent THEN
    DO:
        CREATE tt_webProdusent.
        ASSIGN
            tt_webProdusent.iRecType    = tt_ELogg.EndringsType
            tt_webProdusent.ProdNr      = Produsent.ProdNr
            tt_webProdusent.Beskrivelse = Produsent.Beskrivelse
            .
    END.
END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-byggTmpTabellRegnskapsavdeling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE byggTmpTabellRegnskapsavdeling Procedure 
PROCEDURE byggTmpTabellRegnskapsavdeling :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webRegnskapsavdeling:
    DELETE tt_webRegnskapsavdeling.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "Regnskapsavdeling": 
    FIND Regnskapsavdeling NO-LOCK WHERE
        Regnskapsavdeling.RAvdNr = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE Regnskapsavdeling THEN
    DO:
        CREATE tt_webRegnskapsavdeling.
        ASSIGN
            tt_webRegnskapsavdeling.iRecType        = tt_ELogg.EndringsType
            tt_webRegnskapsavdeling.RAvdNr          = Regnskapsavdeling.RAvdNr
            tt_webRegnskapsavdeling.RAvdBeskrivelse = Regnskapsavdeling.RAvdBeskrivelse
            .
    END.
END. /* WEBBUT */   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellSasong) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellSasong Procedure 
PROCEDURE ByggTmpTabellSasong :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webSasong:
    DELETE tt_webSasong.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "Sasong": 
    FIND Sasong NO-LOCK WHERE
        Sasong.Sasong = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE Sasong THEN
    DO:
        CREATE tt_webSasong.
        ASSIGN
            tt_webSasong.iRecType    = tt_ELogg.EndringsType
            tt_webSasong.Sasong      = Sasong.Sasong
            tt_webSasong.SasBeskr    = Sasong.SasBeskr
            .
    END.
END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-byggTmpTabellSlitsula) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE byggTmpTabellSlitsula Procedure 
PROCEDURE byggTmpTabellSlitsula :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webSlitSula:
    DELETE tt_webSlitSula.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "SlitSula": 
    FIND SlitSula NO-LOCK WHERE
        SlitSula.Slit-Id = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE SlitSula THEN
    DO:
        CREATE tt_webSlitSula.
        ASSIGN
            tt_webSlitSula.iRecType    = tt_ELogg.EndringsType
            tt_webSlitSula.Slit-Id     = Slitsula.Slit-Id
            tt_webSlitSula.SlitBeskr   = Slitsula.SlitBeskr
            .
    END.
END. /* WEBBUT */   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellStrKonv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellStrKonv Procedure 
PROCEDURE ByggTmpTabellStrKonv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webStrKonv:
    DELETE tt_webStrKonv.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "StrKonv": 
    FIND StrKonv NO-LOCK WHERE
        StrKonv.StrKode = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE StrKonv THEN
    DO:
        CREATE tt_webStrKonv.
        ASSIGN
            tt_webStrKonv.iRecType    = tt_ELogg.EndringsType
            tt_webStrKonv.StrKode     = StrKonv.StrKode
            tt_webStrKonv.Storl       = StrKonv.Storl
            tt_webStrKonv.Merknad     = StrKonv.Merknad
            .
    END.
END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellStrTStr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellStrTStr Procedure 
PROCEDURE ByggTmpTabellStrTStr :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webStrTStr:
    DELETE tt_webStrTStr.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "StrType": 
    FIND StrType NO-LOCK WHERE
        StrType.StrTypeId = INT(ENTRY(1,TT_Elogg.Verdier,'|')) NO-ERROR.
        
    IF AVAILABLE StrType THEN
    DO:
        FOR EACH StrTStr OF StrType
            BREAK BY StrTstr.StrTypeId
                  BY StrTStr.SeqNr:
                      
            FIND FIRST StrKonv NO-LOCK WHERE
                StrKonv.Storl = StrTStr.SoStorl NO-ERROR.
            CREATE tt_webStrTStr.
            ASSIGN
                tt_webStrTStr.StrTypeID     = StrType.StrTypeId
                tt_webStrTStr.Beskrivelse   = StrType.Beskrivelse
                tt_webStrTStr.Intervall     = StrType.Intervall
                tt_webStrTStr.Fordeling     = StrType.Fordeling
                tt_webStrTStr.KortNavn      = StrType.KortNavn
                tt_webStrTStr.AlfaFordeling = StrType.AlfaFordeling
                tt_webStrTStr.Hg            = StrType.Hg
                tt_webStrTStr.AvdelingNr    = StrType.AvdelingNr
                tt_webStrTStr.SoStorl       = StrTStr.SoStorl
                tt_webStrTStr.SeqNr         = StrTStr.SeqNr
                tt_webStrTStr.StrKode       = IF AVAILABLE StrKonv THEN StrKonv.StrKode ELSE 0
                .                     
        END.
    END.
END. /* WEBBUT */   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellUnderkategori) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellUnderkategori Procedure 
PROCEDURE ByggTmpTabellUnderkategori :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webUnderkategori:
    DELETE tt_webUnderkategori.
END.

WEBUKAT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "Underkategori": 
    FIND Underkategori NO-LOCK WHERE
        Underkategori.UnderKatNr = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE Underkategori THEN
    DO:
        CREATE tt_webUnderkategori.
        ASSIGN
            tt_webUnderkategori.iRecType      = tt_ELogg.EndringsType
            tt_webUnderkategori.UnderKatNr    = Underkategori.UnderKatNr
            tt_webUnderkategori.UnderKatTekst = Underkategori.UnderKatTekst
            .
    END.
END. /* WEBUKAT */   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellVaremerke) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellVaremerke Procedure 
PROCEDURE ByggTmpTabellVaremerke :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webVaremerke:
    DELETE tt_webVaremerke.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "Varemerke": 
    FIND Varemerke NO-LOCK WHERE
        Varemerke.VmId = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE Varemerke THEN
    DO:
        CREATE tt_webVaremerke.
        ASSIGN
            tt_webVaremerke.iRecType    = tt_ELogg.EndringsType
            tt_webVaremerke.VmId        = Varemerke.VmId
            tt_webVaremerke.Beskrivelse = Varemerke.Beskrivelse
            tt_webVaremerke.KortNavn    = Varemerke.KortNavn
            .
    END.
END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellVarGr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellVarGr Procedure 
PROCEDURE ByggTmpTabellVarGr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.

FOR EACH tt_webVarGr:
    DELETE tt_webVarGr.
END.

WEBBUT:
FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "VarGr": 
    FIND VarGr NO-LOCK WHERE
        VarGr.Vg = INT(TT_Elogg.Verdier) NO-ERROR.
    IF AVAILABLE VarGr THEN
    DO:
        FIND Moms OF VarGr NO-LOCK NO-ERROR.
        CREATE tt_webVarGr.
        ASSIGN
            tt_webVarGr.iRecType    = tt_ELogg.EndringsType
            tt_webVarGr.Vg          = VarGr.Vg
            tt_webVarGr.VgBeskr     = VarGr.VgBeskr
            tt_webVarGr.Hg          = VarGr.Hg
            tt_webVarGr.MomsKod     = VarGr.MomsKod
            tt_webVarGr.Kost_Proc   = VarGr.Kost_Proc
            tt_webVarGr.Mva_Proc    = IF AVAILABLE Moms THEN Moms.MomsProc ELSE 0
            .
    END.
END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellVgAkt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellVgAkt Procedure 
PROCEDURE ByggTmpTabellVgAkt :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      
    ------------------------------------------------------------------------------*/

    DEF VAR iRecType AS INT NO-UNDO.

    FOR EACH tt_webVgAkt:
        DELETE tt_webVgAkt.
    END.
    WEBBUT:
    FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "VgAkt": 
        FIND VgAkt NO-LOCK WHERE
            VgAkt.Vg    = INTEGER(ENTRY(1,TT_Elogg.Verdier,'|')) AND
            VgAkt.AktNr = INTEGER(ENTRY(2,TT_Elogg.Verdier,'|')) NO-ERROR.
        IF AVAILABLE VgAkt THEN
        DO:
            CREATE tt_webVgAkt.
            ASSIGN
                tt_webVgAkt.iRecType    = tt_ELogg.EndringsType
                tt_webVgAkt.Vg     = VgAkt.Vg
                tt_webVgAkt.AktNr  = VgAkt.AktNr
                .
        END.
    END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellVgKat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellVgKat Procedure 
PROCEDURE ByggTmpTabellVgKat :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      
    ------------------------------------------------------------------------------*/

    DEF VAR iRecType AS INT NO-UNDO.

    FOR EACH tt_webVgKat:
        DELETE tt_webVgKat.
    END.

    WEBBUT:
    FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "VgKat": 
        FIND VgKat NO-LOCK WHERE
            VgKat.Vg    = INTEGER(ENTRY(1,TT_Elogg.Verdier,'|')) AND
            VgKat.VgKat = INTEGER(ENTRY(2,TT_Elogg.Verdier,'|')) NO-ERROR.
            
        IF AVAILABLE VgKat THEN
        DO:
            CREATE tt_webVgKat.
            ASSIGN
                tt_webVgKat.iRecType = tt_ELogg.EndringsType
                tt_webVgKat.Vg       = VgKat.Vg
                tt_webVgKat.VgKat    = VgKat.VgKat
                tt_webVgKat.KatNr    = VgKat.KatNr
                .
        END.
    END. /* WEBBUT */   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabellVgKundeGrpRabatt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabellVgKundeGrpRabatt Procedure 
PROCEDURE ByggTmpTabellVgKundeGrpRabatt :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
      
    ------------------------------------------------------------------------------*/

    DEF VAR iRecType AS INT NO-UNDO.
    FOR EACH tt_webVgKundeGrpRabatt:
        DELETE tt_webVgKundeGrpRabatt.
    END.

    WEBBUT:        
    FOR EACH TT_Elogg WHERE tt_Elogg.TabellNavn = "VgKundeGrpRabatt": 
        FIND VgKundeGrpRabatt NO-LOCK WHERE
            VgKundeGrpRabatt.Vg       = INTEGER(ENTRY(1,TT_Elogg.Verdier,'|')) AND
            VgKundeGrpRabatt.GruppeId = INTEGER(ENTRY(2,TT_Elogg.Verdier,'|')) NO-ERROR.
            
        IF AVAILABLE VgKundeGrpRabatt THEN
        DO:
            CREATE tt_webVgKundeGrpRabatt.
            ASSIGN
                tt_webVgKundeGrpRabatt.iRecType = tt_ELogg.EndringsType
                tt_webVgKundeGrpRabatt.Vg          = VgKundeGrpRabatt.Vg
                tt_webVgKundeGrpRabatt.GruppeId    = VgKundeGrpRabatt.GruppeId
                tt_webVgKundeGrpRabatt.Rabatt_Proc = VgKundeGrpRabatt.Rabatt%
                .
        END.
    END. /* WEBBUT */   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterAktivitetFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterAktivitetFil Procedure 
PROCEDURE EksporterAktivitetFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
    DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webAktivitet
        BREAK BY tt_webAktivitet.AktNr:
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
        /* Antall poster i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webAktivitet:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"Aktivitet",iAntRec,"AktNr",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterAnv-KodFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterAnv-KodFil Procedure 
PROCEDURE EksporterAnv-KodFil :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webAnv-Kod
        BREAK BY tt_webAnv-Kod.Anv-Id:
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
        /* Antall psoter i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webAnv-Kod:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"Anv-Kod",iAntRec,"Anv-Id",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterArtikkelFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterArtikkelFil Procedure 
PROCEDURE EksporterArtikkelFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.
DEFINE VARIABLE piAnt        AS INTEGER NO-UNDO.
DEFINE VARIABLE pcTmpFilNavn AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcFilNavn    AS CHARACTER NO-UNDO.
DEFINE VARIABLE piFilAnt     AS INTEGER    NO-UNDO.

iAntRec = 0.
EKSPORTER:
FOR EACH tt_webArtikkel
    BREAK BY tt_webArtikkel.ArtikkelNr
          BY tt_webArtikkel.StrKode:
    /* Kontrollsiffer på filutlegg. */
    iAntRec = iAntRec + 1.
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
    END. /* EKSPORTER */   

    IF CAN-DO('WEBBUT',cEDBSystem) THEN 
    WEBBUT:
    DO:
        IF iAntRec > 0 THEN
        UtleggXML:
        DO:
            ASSIGN 
                piFilAnt = 0
                .
            EVIGHETEN:
            DO WHILE CAN-FIND(FIRST tt_webArtikkel):
                piAnt = 0.
                piFilAnt = piFilAnt + 1.
                pcTmpFilNavn = cTmpFilNavn.
                pcFilNavn    = cFilNavn.
                FOR EACH exp_webArtikkel: DELETE exp_webArtikkel. END.
                WEBARTIKKEL:
                FOR EACH tt_webArtikkel
                    BREAK BY tt_webArtikkel.ArtikkelNr
                          BY tt_webArtikkel.StrKode:
                    piAnt = piAnt + 1.
                    CREATE exp_webArtikkel.
                    BUFFER-COPY tt_webArtikkel TO exp_webArtikkel.
                    DELETE tt_webArtikkel.
                    IF piAnt > 100 THEN LEAVE WEBARTIKKEL.          
                END. /* WEBARTIKKEL */

                ASSIGN
                    cOmmit = ""
                    hBuf   = BUFFER exp_webArtikkel:HANDLE
                    pcTmpFilNavn = REPLACE(pcTmpFilNavn,'.xml',TRIM(STRING(piFilAnt,">>999")) + '.xml')
                    pcFilNavn    = REPLACE(pcFilNavn,'.xml',TRIM(STRING(piFilAnt,">>999")) + '.xml')
                    .
/*    MESSAGE 'piAnt' piAnt skip              */
/*            'piFilAnt' piFilAnt skip        */
/*            'cFilNavn' cFilNavn skip        */
/*            'pcTmpFilNavn' pcTmpFilNavn SKIP*/
/*            'pcFilNavn' pcFilNavn           */
/*    VIEW-AS ALERT-BOX.                      */
                RUN tabelleksportXml.p (hBuf,"Artbas",piAnt,"Artikkelnr",cOmmit,pcTmpFilNavn).
                piAnt  = 0.
                IF SEARCH(pcTmpFilNavn) <> ? THEN
                DO:
                    OS-COMMAND SILENT VALUE("RENAME " + pcTmpFilNavn + " " + pcFilNavn).
                    RUN loggFilNavn (pcFilNavn, piAnt).
                END.
            END. /* EVIGHETEN */
        END. /* UtleggXML */
    END. /* WEBBUT */

/* TEST TEST Utlegg av eksempelfil til Magento  */
/* TN 13/3-14                                   */
ELSE IF CAN-DO('MAGENTO',cEDBSystem) AND iAntRec > 0 THEN 
MAGENTO:
DO:
  bStreamAapen = FALSE. 

  CREATE SAX-WRITER hSAXWriter.
  hSAXWriter:FORMATTED = TRUE.
  lOK = hSAXWriter:SET-OUTPUT-DESTINATION("file", cTmpFilNavn).
  lOK = hSAXWriter:START-DOCUMENT( ).
  ASSIGN 
  lOK = hSAXWriter:START-ELEMENT("PublishEComArticlePrice")
  lOK = hSAXWriter:INSERT-ATTRIBUTE("xmlns:xsi", 'http://www.w3.org/2001/XMLSchema-instance')
  lOK = hSAXWriter:INSERT-ATTRIBUTE("xsi:noNamespaceSchemaLocation" , 'PublishEComArticlePrice.xsd')
  lOK = hSAXWriter:START-ELEMENT("header")
  lOK = hSAXWriter:WRITE-DATA-ELEMENT("messageID", 'MAG' + STRING(TIME))
  lOK = hSAXWriter:WRITE-DATA-ELEMENT("sendTime", STRING(YEAR(TODAY),"9999") + '-' + STRING(MONTH(TODAY),"99") + '-' + STRING(DAY(TODAY),"99") + 'T' + STRING(TIME,"HH:MM:SS"))
  lOK = hSAXWriter:END-ELEMENT("header").
  
  EKSPORTER:
  FOR EACH tt_webArtikkel
    BREAK BY tt_webArtikkel.ArtikkelNr:
    ASSIGN
    lOK = hSAXWriter:START-ELEMENT("ArticlePriceRow")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("sku", tt_webArtikkel.Kode)
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("LevKod", STRING(tt_webArtikkel.LevKod))
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Beskr", STRING(tt_webArtikkel.Beskr))
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("LevFargKod", STRING(tt_webArtikkel.LevFargKod))
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("OrdInnkjopsPris", STRING(tt_webArtikkel.OrdInnkjopsPris))
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("OrdVarekost", STRING(tt_webArtikkel.OrdVarekost))
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("OrdPris", STRING(tt_webArtikkel.OrdPris))   
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Pris", STRING(tt_webArtikkel.Pris))
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Tilbud", STRING(tt_webArtikkel.Tilbud))
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("AktiveringsDato", STRING(tt_webArtikkel.AktiveringsDato))
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("AktiveringsTid", STRING(tt_webArtikkel.AktiveringsTid))
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("AvsluttDato", STRING(tt_webArtikkel.AvsluttDato))
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("AvsluttTid", STRING(tt_webArtikkel.AvsluttTid))
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("store", 'NO')
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("website", 'norway')
    lOK = hSAXWriter:END-ELEMENT("ArticlePriceRow")
    NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
      DO: 
        NEXT EKSPORTER.
      END. 
  END. /* EKSPORTER */
  lOK = hSAXWriter:END-ELEMENT("PublishEComArticlePrice").
  lOK = hSAXWriter:END-DOCUMENT( ).
  DELETE OBJECT hSAXWriter. 
  
  IF SEARCH(cTmpFilNavn) <> ? THEN 
  DO:
    OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
    IF bCopy THEN 
      RUN stgCopy (REPLACE(cTmpFilNavn,'TMP','')).
  END.
    
END. /* MAGENTO */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterAvdelinglFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterAvdelinglFil Procedure 
PROCEDURE EksporterAvdelinglFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webAvdeling
        BREAK BY tt_webAvdeling.AvdelingNr:
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
        /* Antall psoter i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webAvdeling:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"Avdeling",iAntRec,"AvdelingNr",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterFargFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterFargFil Procedure 
PROCEDURE EksporterFargFil :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webFarg
        BREAK BY tt_webFarg.Farg:
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
        /* Antall poster i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webFarg:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"Farg",iAntRec,"Farg",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterHandteringFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterHandteringFil Procedure 
PROCEDURE EksporterHandteringFil :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webHandtering
        BREAK BY tt_webHandtering.HandKode:
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
        /* Antall psoter i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webHandtering:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"Handtering",iAntRec,"HandKode",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterHovedkategoriFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterHovedkategoriFil Procedure 
PROCEDURE EksporterHovedkategoriFil :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webHovedkategori
        BREAK BY tt_webHovedKategori.HovedKatNr:
        iAntEksport = iAntEksport + 1.
        /* Antall psoter i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webHovedkategori:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"Hovedkategori",iantrec,"HovedKatNr",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterHuvGrFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterHuvGrFil Procedure 
PROCEDURE EksporterHuvGrFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webHuvGr
        BREAK BY tt_webHuvGr.Hg:
        iAntEksport = iAntEksport + 1.
        /* Antall psoter i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webHuvGr:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"HuvGr",iantrec,"Hg",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterInnersulaFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterInnersulaFil Procedure 
PROCEDURE EksporterInnersulaFil :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webInnersula
        BREAK BY tt_webInnersula.Inner-Id:
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
        /* Antall psoter i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webInnersula:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"Innersula",iAntRec,"Inner-Id",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterKategoriFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterKategoriFil Procedure 
PROCEDURE EksporterKategoriFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webKategori
        BREAK BY tt_webKategori.KatNr:
        /* Kontrollsiffer på filutlegg. */
        iAntRec = iAntRec + 1.
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webKategori:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"Kategori",iAntRec,"KatNr",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterKlackFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterKlackFil Procedure 
PROCEDURE EksporterKlackFil :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webKlack
        BREAK BY tt_webKlack.Klack-Id:
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
        /* Antall psoter i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webKlack:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"Klack",iAntRec,"Klack-Id",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterKordreHodeFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterKordreHodeFil Procedure 
PROCEDURE EksporterKordreHodeFil :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.
DEFINE VARIABLE iAntNeg AS INTEGER NO-UNDO.
DEFINE VARIABLE bHeader AS LOG NO-UNDO.
DEFINE VARIABLE ctmp2FileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE c2FileName    AS CHARACTER NO-UNDO.

DEFINE BUFFER bufKOrdreHode FOR KOrdreHode. 

iAntRec = 0.
EKSPORTER:
FOR EACH tt_webKOrdreHode
    BREAK BY tt_webKOrdreHode.KOrdre_Id:
    /* antall utlagte psoter. */
    iAntEksport = iAntEksport + 1.
    /* Antall poster i tabell */
    iAntRec = iAntRec + 1.
  
END. /* EKSPORTER */
    
IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webKOrdreHode:HANDLE.
    
        RUN tabelleksportXml2.p (hBuf,"KOrdreHode",iAntRec,"KOrdre_Id",cOmmit,"KOrdreLinje",cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

ELSE IF CAN-DO('MAGENTO',cEDBSystem) AND iAntRec > 0 THEN 
MAGENTO:
DO:
  bStreamAapen = FALSE. 

  /* Eksport av ordre */
  IF CAN-FIND( FIRST tt_webKOrdreHode WHERE tt_webKOrdreHode.Totalt >= 0) THEN 
  ORDREEKSPORT:
  DO:
    CREATE SAX-WRITER hSAXWriter.
    hSAXWriter:FORMATTED = TRUE.

    lOK = hSAXWriter:SET-OUTPUT-DESTINATION("file", cTmpFilNavn).
    lOK = hSAXWriter:START-DOCUMENT( ).

    ASSIGN 
    lOK = hSAXWriter:START-ELEMENT("PublishEComShippingInfo")
    lOK = hSAXWriter:INSERT-ATTRIBUTE("xmlns:xsi", 'http://www.w3.org/2001/XMLSchema-instance')
    lOK = hSAXWriter:INSERT-ATTRIBUTE("xsi:noNamespaceSchemaLocation" , 'PublishEComShippingInfo.xsd')
    lOK = hSAXWriter:START-ELEMENT("header")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("messageID", 'MAG' + STRING(TIME))
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("sendTime", STRING(YEAR(TODAY),"9999") + '-' + STRING(MONTH(TODAY),"99") + '-' + STRING(DAY(TODAY),"99") + 'T' + STRING(TIME,"HH:MM:SS"))
    lOK = hSAXWriter:END-ELEMENT("header").

    EKSPORTER:
    FOR EACH tt_webKOrdreHode WHERE 
             tt_webKOrdreHode.Totalt >= 0
      BREAK BY tt_webKOrdreHode.KOrdre_Id :
      
      FIND Leveringsform NO-LOCK WHERE
        Leveringsform.LevFNr = tt_webKOrdreHode.LevFNr NO-ERROR.  
        
      /* Stempler kundeordren som sent, slik at den ikke sendes flere ganger. */
      DO FOR bufKOrdreHode TRANSACTION:
          FIND bufKOrdreHode EXCLUSIVE-LOCK WHERE bufKOrdreHode.KOrdre_Id = KOrdreHode.KOrdre_Id NO-ERROR.
          IF AVAILABLE bufKOrdreHode THEN 
          DO:
              ASSIGN 
                  bufKOrdreHode.ProduksjonsDato = TODAY
                  bufKOrdreHode.VerkstedMerknad = (IF bufKOrdreHode.VerkstedMerknad<> '' THEN CHR(13) ELSE '') + 'Sendt Magento ' + STRING(TODAY)
                  .
              RELEASE bufKOrdreHode.              
          END.
      END.  /* TRANSACTION */
        
      ASSIGN 
      lOK = hSAXWriter:START-ELEMENT("shippingInfoRow")
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("orderID",tt_webKOrdreHode.EkstOrdreNr)
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("trackingID",tt_webKOrdreHode.SendingsNr)
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("carrierCode",'custom') /* if AVAILABLE Leveringsform THEN Leveringsform.LevFormMetode ELSE '' */
      lOK = hSAXWriter:END-ELEMENT("shippingInfoRow").
    END. /* EKSPORTER */
    ASSIGN 
    lOK = hSAXWriter:END-ELEMENT("PublishEComShippingInfo")
    lOK = hSAXWriter:END-DOCUMENT( ).
    DELETE OBJECT hSAXWriter.

    IF SEARCH(cTmpFilNavn) <> ? THEN 
      OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
  END. /* ORDREEKSPORT */
  
  /* Retur håndteres som negative ordre. UpdateEComReturnRequest */
  IF CAN-FIND( FIRST tt_webKOrdreHode WHERE tt_webKOrdreHode.Totalt < 0) THEN    
  RETUR_ORDRE:
  DO:
    ctmp2FileName = REPLACE(cTmpFilNavn,'PublishEComShippingInfo','UpdateEComReturnRequest').
    c2FileName    = REPLACE(ctmp2FileName,'TMP','').
    
    CREATE SAX-WRITER hSAXWriter.
    hSAXWriter:FORMATTED = TRUE.

    lOK = hSAXWriter:SET-OUTPUT-DESTINATION("file", ctmp2FileName).
    lOK = hSAXWriter:START-DOCUMENT( ).

    ASSIGN 
    lOK = hSAXWriter:START-ELEMENT("UpdateEComReturnRequest")
    lOK = hSAXWriter:INSERT-ATTRIBUTE("xmlns:xsi", 'http://www.w3.org/2001/XMLSchema-instance')
    lOK = hSAXWriter:INSERT-ATTRIBUTE("xsi:noNamespaceSchemaLocation" , 'UpdateEComReturnRequest.xsd')
    lOK = hSAXWriter:START-ELEMENT("header")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("messageID", 'MAG' + STRING(TIME))
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("sendTime", STRING(YEAR(TODAY),"9999") + '-' + STRING(MONTH(TODAY),"99") + '-' + STRING(DAY(TODAY),"99") + 'T' + STRING(TIME,"HH:MM:SS"))
    lOK = hSAXWriter:END-ELEMENT("header").

    EKSPORTER:
    FOR EACH tt_webKOrdreHode WHERE 
             tt_webKOrdreHode.Totalt < 0 
      BREAK BY tt_webKOrdreHode.KOrdre_Id :
      
      ASSIGN 
      lOK = hSAXWriter:START-ELEMENT("returnRequestRow")
      lOK = hSAXWriter:WRITE-DATA-ELEMENT("returnID",REPLACE(tt_webKOrdreHode.EkstOrdreNr,'RETUR ',''))
      lOK = hSAXWriter:START-ELEMENT("returnedItemRows").
      
      FOR EACH KordreLinje NO-LOCK WHERE 
        KOrdreLinje.KOrdre_Id = tt_webKOrdreHode.KOrdre_Id:
        ASSIGN 
        lOK = hSAXWriter:START-ELEMENT("returnItem").

        FIND LAST Strekkode NO-LOCK WHERE
          Strekkode.ArtikkelNr = DEC(KOrdreLinje.VareNr) AND 
          Strekkode.StrKode    = KOrdreLinje.StrKode NO-ERROR.

        ASSIGN 
        /*lOK = hSAXWriter:WRITE-DATA-ELEMENT("itemID",KOrdreLinje.VareNr)*/
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("sku",IF AVAILABLE Strekkode THEN Strekkode.Kode ELSE '')
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("approved",IF tt_webKOrdreHode.SendingsNr = 'OK' THEN 'TRUE' ELSE 'FALSE')
        lOK = hSAXWriter:WRITE-DATA-ELEMENT("qty",STRING(KOrdreLinje.Antall * 1)).
        
        ASSIGN 
        lOK = hSAXWriter:END-ELEMENT("returnItem").
      END. 
      ASSIGN  
      lOK = hSAXWriter:END-ELEMENT("returnedItemRows").
      lOK = hSAXWriter:END-ELEMENT("returnRequestRow").
    END. /* EKSPORTER */
    ASSIGN 
    lOK = hSAXWriter:END-ELEMENT("UpdateEComReturnRequest")
    lOK = hSAXWriter:END-DOCUMENT( ).
    DELETE OBJECT hSAXWriter.
    
    IF SEARCH(ctmp2FileName) <> ? THEN
    DO: 
      /*OS-COMMAND SILENT VALUE("RENAME " + ctmp2FileName + " " + c2FileName).*/
      OS-COPY VALUE(ctmp2FileName) VALUE(c2FileName).      
      OS-DELETE VALUE(ctmp2FileName).
      IF bCopy THEN 
        RUN stgCopy (REPLACE(ctmp2FileName,'TMP','')).      
    END.  
  END. /* RETUR_ORDRE */
  
END. /* MAGENTO */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterKundeFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterKundeFil Procedure 
PROCEDURE EksporterKundeFil :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG NO-UNDO.
DEF VAR iAntPoster   AS INT NO-UNDO.

iAntRec = 0.
EKSPORTER:
FOR EACH tt_webKunde
    BREAK BY tt_webKunde.KundeNr:
    /* antall utlagte psoter. */
    iAntEksport = iAntEksport + 1.
    /* Antall poster i tabell */
    iAntRec = iAntRec + 1.
END. /* EKSPORTER */

IF CAN-DO('WEBBUT',cEDBSystem) AND iAntRec > 0 THEN 
WEBBUT:
DO:
    ASSIGN
        cOmmit = ""
        hBuf   = BUFFER tt_webKunde:HANDLE.

    RUN tabelleksportXml.p (hBuf,"Kunde",iAntRec,"KundeNr",cOmmit,cTmpFilNavn).
    IF SEARCH(cTmpFilNavn) <> ? THEN
    DO:
        OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
        RUN loggFilNavn (cFilNavn, iAntRec).
    END.
END. /* WEBBUT */

ELSE IF CAN-DO('MAGENTO',cEDBSystem) AND iAntRec > 0 AND 
     tt_webKunde.ePostAdresse <> '' THEN /* Magento skal ikke ha endringer på kunder som ikke har eMail adresse. TN 22/4-15 */ 
MAGENTO:
DO:
  bStreamAapen = FALSE. 

  CREATE SAX-WRITER hSAXWriter.
  hSAXWriter:FORMATTED = TRUE.
  lOK = hSAXWriter:SET-OUTPUT-DESTINATION("file", cTmpFilNavn).
  lOK = hSAXWriter:START-DOCUMENT( ).
  ASSIGN 
  lOK = hSAXWriter:START-ELEMENT("UpdateEComCustomer")
  lOK = hSAXWriter:INSERT-ATTRIBUTE("xmlns:xsi", 'http://www.w3.org/2001/XMLSchema-instance')
  lOK = hSAXWriter:INSERT-ATTRIBUTE("xsi:noNamespaceSchemaLocation" , 'UpdateEComCustomer.xsd')
  lOK = hSAXWriter:START-ELEMENT("header")
  lOK = hSAXWriter:WRITE-DATA-ELEMENT("messageID", 'MAG' + STRING(TIME))
  lOK = hSAXWriter:WRITE-DATA-ELEMENT("sendTime", STRING(YEAR(TODAY),"9999") + '-' + STRING(MONTH(TODAY),"99") + '-' + STRING(DAY(TODAY),"99") + 'T' + STRING(TIME,"HH:MM:SS"))
  lOK = hSAXWriter:END-ELEMENT("header").
  
  EKSPORTER:
  FOR EACH tt_webKunde
    BREAK BY tt_webKunde.KundeNr:
    FIND Post NO-LOCK WHERE Post.PostNr = tt_webKunde.PostNr NO-ERROR.
    ASSIGN
      cForNavn   = ENTRY(1,tt_webKunde.Navn,' ')
      cEtterNavn = REPLACE(tt_webKunde.Navn,ENTRY(1,tt_webKunde.Navn,' '),'')
      cFodtDato  = IF tt_webKunde.FodtDato <> ? 
                     THEN STRING(YEAR(tt_webKunde.FodtDato)) + STRING(MONTH(tt_webKunde.FodtDato),"99") + STRING(DAY(tt_webKunde.FodtDato),"99")
                     ELSE ''
      cPostSted  = IF AVAILABLE Post 
                     THEN Post.Beskrivelse
                     ELSE ''.
    ASSIGN
    lOK = hSAXWriter:START-ELEMENT("customerRow")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("customerID", tt_webKunde.EksterntKundeNr)
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("extcustomerID", STRING(tt_webKunde.KundeNr))
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("title", '')
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("firstname", cForNavn)
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("lastname", cEtterNavn)
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("dob", cFodtDato)
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("email", tt_webKunde.ePostAdresse)
    lOK =   hSAXWriter:START-ELEMENT("shippingaddress")
    lOK =   hSAXWriter:WRITE-DATA-ELEMENT("firstname", cForNavn)
    lOK =   hSAXWriter:WRITE-DATA-ELEMENT("lastname", cEtterNavn)
    lOK =   hSAXWriter:WRITE-DATA-ELEMENT("country", tt_webKunde.Land)
    lOK =   hSAXWriter:WRITE-DATA-ELEMENT("city", cPostSted)
    lOK =   hSAXWriter:WRITE-DATA-ELEMENT("postcode", tt_webKunde.PostNr)
    lOK =   hSAXWriter:WRITE-DATA-ELEMENT("street", tt_webKunde.Adresse1)
    lOK =   hSAXWriter:WRITE-DATA-ELEMENT("telephone", tt_webKunde.MobilTlf)
    lOK =   hSAXWriter:END-ELEMENT("shippingaddress")
    lOK =   hSAXWriter:START-ELEMENT("billingaddress")
    lOK =   hSAXWriter:WRITE-DATA-ELEMENT("firstname", cForNavn)
    lOK =   hSAXWriter:WRITE-DATA-ELEMENT("lastname", cEtterNavn)
    lOK =   hSAXWriter:WRITE-DATA-ELEMENT("country", tt_webKunde.Land)
    lOK =   hSAXWriter:WRITE-DATA-ELEMENT("city", cPostSted)
    lOK =   hSAXWriter:WRITE-DATA-ELEMENT("postcode", tt_webKunde.PostNr)
    lOK =   hSAXWriter:WRITE-DATA-ELEMENT("street", tt_webKunde.Adresse1)
    lOK =   hSAXWriter:WRITE-DATA-ELEMENT("telephone", tt_webKunde.MobilTlf)
    lOK =   hSAXWriter:END-ELEMENT("billingaddress")
    lOK = hSAXWriter:END-ELEMENT("customerRow").
  END. /* EKSPORTER */
  lOK = hSAXWriter:END-ELEMENT("UpdateEComCustomer").
  lOK = hSAXWriter:END-DOCUMENT( ).
  DELETE OBJECT hSAXWriter.
  
  IF SEARCH(cTmpFilNavn) <> ? THEN DO:
    OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
    IF bCopy THEN 
      RUN stgCopy (REPLACE(cTmpFilNavn,'TMP','')).
  END.
  
  /*
  OUTPUT STREAM Ut TO VALUE(cTmpFilNavn) NO-ECHO.
  PUT STREAM Ut UNFORMATTED '<UpdateEComCustomer xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="UpdateEComCustomer.xsd">' SKIP.
  PUT STREAM Ut UNFORMATTED '  <header>' SKIP.
  PUT STREAM Ut UNFORMATTED '      <messageID>MAG' + STRING(TIME) + '</messageID>' SKIP.
  PUT STREAM Ut UNFORMATTED '      <sendTime>' + 
                            STRING(YEAR(TODAY),"9999") + '-' + STRING(MONTH(TODAY),"99") + '-' + STRING(DAY(TODAY),"99") + 'T' + STRING(TIME,"HH:MM:SS") + '</sendTime>' SKIP.
  PUT STREAM Ut UNFORMATTED '  </header>' SKIP.

  PUT STREAM Ut UNFORMATTED '<customerRow>' SKIP.
  EKSPORTER:
  FOR EACH tt_webKunde
      BREAK BY tt_webKunde.KundeNr:
      FIND Post NO-LOCK WHERE Post.PostNr = tt_webKunde.PostNr NO-ERROR.
      ASSIGN
        cForNavn   = ENTRY(1,tt_webKunde.Navn,' ')
        cEtterNavn = REPLACE(tt_webKunde.Navn,ENTRY(1,tt_webKunde.Navn,' '),'')
        cFodtDato  = IF tt_webKunde.FodtDato <> ? 
                       THEN STRING(YEAR(tt_webKunde.FodtDato)) + STRING(MONTH(tt_webKunde.FodtDato),"99") + STRING(DAY(tt_webKunde.FodtDato),"99")
                       ELSE ''
        cPostSted  = IF AVAILABLE Post 
                       THEN Post.Beskrivelse
                       ELSE ''
        .
  
          
      PUT STREAM Ut UNFORMATTED '    <customerID>' + STRING(tt_webKunde.KundeNr) + '</customerID>' SKIP.
      PUT STREAM Ut UNFORMATTED '    <extcustomerID>' + tt_webKunde.EksterntKundeNr + '</extcustomerID>' SKIP.
      PUT STREAM Ut UNFORMATTED '    <title></title>' SKIP.
      PUT STREAM Ut UNFORMATTED '    <firstname>' + cForNavn + '</firstname>' SKIP.
      PUT STREAM Ut UNFORMATTED '    <lastname>' + cEtterNavn + '</lastname>' SKIP.
      PUT STREAM Ut UNFORMATTED '    <dob>' + cFodtDato + '</dob>' SKIP.
      PUT STREAM Ut UNFORMATTED '    <email>' + tt_webKunde.ePostAdresse + '</email>' SKIP.
      PUT STREAM Ut UNFORMATTED '    <shippingaddress>' SKIP.
      PUT STREAM Ut UNFORMATTED '        <firstname>' + cForNavn + '</firstname>' SKIP.
      PUT STREAM Ut UNFORMATTED '        <lastname>' + cEtterNavn + '</lastname>' SKIP.
      PUT STREAM Ut UNFORMATTED '        <country>' + tt_webKunde.Land + '</country>' SKIP.
      PUT STREAM Ut UNFORMATTED '        <city>' + cPostSted + '</city>' SKIP.
      PUT STREAM Ut UNFORMATTED '        <postcode>' + tt_webKunde.PostNr + '</postcode>' SKIP.
      PUT STREAM Ut UNFORMATTED '        <street>' + tt_webKunde.Adresse1 + '</street>' SKIP.
      PUT STREAM Ut UNFORMATTED '        <telephone>' + tt_webKunde.MobilTlf + '</telephone>' SKIP.
      PUT STREAM Ut UNFORMATTED '    </shippingaddress>' SKIP.
      PUT STREAM Ut UNFORMATTED '    <billingaddress>' SKIP.
      PUT STREAM Ut UNFORMATTED '        <firstname>' + cForNavn + '</firstname>' SKIP.
      PUT STREAM Ut UNFORMATTED '        <lastname>' + cEtterNavn + '</lastname>' SKIP.
      PUT STREAM Ut UNFORMATTED '        <country>' + tt_webKunde.Land + '</country>' SKIP.
      PUT STREAM Ut UNFORMATTED '        <city>' + cPostSted + '</city>' SKIP.
      PUT STREAM Ut UNFORMATTED '        <postcode>' + tt_webKunde.PostNr + '</postcode>' SKIP.
      PUT STREAM Ut UNFORMATTED '        <street>' + tt_webKunde.Adresse1 + '</street>' SKIP.
      PUT STREAM Ut UNFORMATTED '        <telephone>' + tt_webKunde.MobilTlf + '</telephone>' SKIP.
      PUT STREAM Ut UNFORMATTED '    </billingaddress>' SKIP.
  END. /* EKSPORTER */
  
  PUT STREAM Ut UNFORMATTED '    </customerRow>' SKIP.
  PUT STREAM Ut UNFORMATTED '</UpdateEComCustomer>' SKIP.

  OUTPUT STREAM Ut CLOSE.
  OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
  */
END. /* MAGENTO */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterKundeGruppeFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterKundeGruppeFil Procedure 
PROCEDURE EksporterKundeGruppeFil :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG NO-UNDO.
DEF VAR iAntPoster   AS INT NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webKundeGruppe
        BREAK BY tt_webKundeGruppe.GruppeId:
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
        /* Antall poster i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */

    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf   = BUFFER tt_webKundeGruppe:HANDLE.

        RUN tabelleksportXml.p (hBuf,"KundeGruppe",iAntRec,"GruppeId",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterKundeKortFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterKundeKortFil Procedure 
PROCEDURE EksporterKundeKortFil :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG NO-UNDO.
DEF VAR iAntPoster   AS INT NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webKundeKort
        BREAK BY tt_webKundeKort.KortNr:
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
        /* Antall poster i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */

    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf   = BUFFER tt_webKundeKort:HANDLE.

        RUN tabelleksportXml.p (hBuf,"KundeKort",iAntRec,"KortNr",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterKundeSaldoFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterKundeSaldoFil Procedure 
PROCEDURE EksporterKundeSaldoFil :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG NO-UNDO.
DEF VAR iAntPoster   AS INT NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webKundeSaldo
        BREAK BY tt_webKundeSaldo.KundeNr
              BY tt_webKundeSaldo.ButikkNr:
        /* antall utlagte poster. */
        iAntEksport = iAntEksport + 1.
        /* Antall poster i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */

    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf   = BUFFER tt_webKundeSaldo:HANDLE.

        RUN tabelleksportXml.p (hBuf,"KundeSaldo",iAntRec,"KundeNr,ButikkNr",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterLagerFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterLagerFil Procedure 
PROCEDURE EksporterLagerFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.
    DEFINE VARIABLE piAnt        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE pcTmpFilNavn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pcFilNavn    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE piFilAnt     AS INTEGER   NO-UNDO.

iAntRec = 0.
EKSPORTER:
FOR EACH tt_webLager
    BREAK BY tt_webLager.ArtikkelNr
          BY tt_webLager.Butik
          BY tt_webLager.Storl:
    /* Kontrollsiffer på filutlegg. */
    iAntRec = iAntRec + 1.
    /* antall utlagte psoter. */
    iAntEksport = iAntEksport + 1.
    
END. /* EKSPORTER */   

    /*IF CAN-DO('WEBBUT',cEDBSystem) THEN                                                   */
    /*WEBBUT:                                                                               */
    /*DO:                                                                                   */
    /*    IF iAntRec > 0 THEN                                                               */
    /*    UtleggXML:                                                                        */
    /*    DO:                                                                               */
    /*        ASSIGN                                                                        */
    /*            cOmmit = ""                                                               */
    /*            hBuf = BUFFER tt_webLager:HANDLE.                                         */
    /*                                                                                      */
    /*        RUN tabelleksportXml.p (hBuf,"Lager",iAntRec,"ArtikkelNr",cOmmit,cTmpFilNavn).*/
    /*        IF SEARCH(cTmpFilNavn) <> ? THEN                                              */
    /*        DO:                                                                           */
    /*            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).        */
    /*            RUN loggFilNavn (cFilNavn, iAntRec).                                      */
    /*        END.                                                                          */
    /*    END. /* UtleggXML */                                                              */
    /*END. /* WEBBUT */                                                                     */

    IF CAN-DO('WEBBUT',cEDBSystem) THEN 
    WEBBUT:
    DO:
        IF iAntRec > 0 THEN
        UtleggXML:
        DO:
            ASSIGN 
                piFilAnt = 0
                .
            EVIGHETEN:
            DO WHILE CAN-FIND(FIRST tt_webLager):
                piAnt = 0.
                piFilAnt = piFilAnt + 1.
                pcTmpFilNavn = cTmpFilNavn.
                pcFilNavn    = cFilNavn.
                FOR EACH exp_webLager: 
                    DELETE exp_webLager. 
                END.
                LAGER:
                FOR EACH tt_webLager
                    BREAK BY tt_webLager.ArtikkelNr
                    BY tt_webLager.Butik 
                    BY tt_webLager.Storl:
                    piAnt = piAnt + 1.
                    CREATE exp_webLager.
                    BUFFER-COPY tt_webLager TO exp_webLager.
                    DELETE tt_webLager.
                    IF piAnt > 100 THEN LEAVE LAGER.          
                END. /* LAGER */

                ASSIGN
                    cOmmit       = ""
                    hBuf         = BUFFER exp_webLager:HANDLE
                    pcTmpFilNavn = REPLACE(pcTmpFilNavn,'.xml',TRIM(STRING(piFilAnt,">>999")) + '.xml')
                    pcFilNavn    = REPLACE(pcFilNavn,'.xml',TRIM(STRING(piFilAnt,">>999")) + '.xml')
                    .
    
                RUN tabelleksportXml.p (hBuf,"Artbas",piAnt,"Artikkelnr",cOmmit,pcTmpFilNavn).
                piAnt = 0.
                IF SEARCH(pcTmpFilNavn) <> ? THEN
                DO:
                    OS-COMMAND SILENT VALUE("RENAME " + pcTmpFilNavn + " " + pcFilNavn).
                    RUN loggFilNavn (pcFilNavn, piAnt).
                END.
            END. /* EVIGHETEN */
        END. /* UtleggXML */
    END. /* WEBBUT */

ELSE IF CAN-DO('MAGENTO',cEDBSystem) AND iAntRec > 0 THEN 
MAGENTO:
DO ON ERROR UNDO, RETRY:
  IF RETRY THEN 
  DO:
        RUN bibl_loggDbFri.p (cUtfilNavn,
            'Retry på MAGENTO BLokk.'
            ).
        LEAVE MAGENTO.          
  END.  
    
  bStreamAapen = FALSE. 

  CREATE SAX-WRITER hSAXWriter.
  hSAXWriter:FORMATTED = TRUE.
  lOK = hSAXWriter:SET-OUTPUT-DESTINATION("file", cTmpFilNavn).
  lOK = hSAXWriter:START-DOCUMENT( ).
  ASSIGN 
  lOK = hSAXWriter:START-ELEMENT("PublishEComStocklevel")
  lOK = hSAXWriter:INSERT-ATTRIBUTE("xmlns:xsi", 'http://www.w3.org/2001/XMLSchema-instance')
  lOK = hSAXWriter:INSERT-ATTRIBUTE("xsi:noNamespaceSchemaLocation" , 'PublishEComStocklevel.xsd')
  lOK = hSAXWriter:START-ELEMENT("header")
  lOK = hSAXWriter:WRITE-DATA-ELEMENT("messageID", 'MAG' + STRING(TIME))
  lOK = hSAXWriter:WRITE-DATA-ELEMENT("sendTime", STRING(YEAR(TODAY),"9999") + '-' + STRING(MONTH(TODAY),"99") + '-' + STRING(DAY(TODAY),"99") + 'T' + STRING(TIME,"HH:MM:SS"))
  lOK = hSAXWriter:END-ELEMENT("header").
  
  DO ON ERROR UNDO, LEAVE:
  EKSPORTER:
  FOR EACH tt_webLager
    BREAK BY tt_webLager.ArtikkelNr:
    ASSIGN
    lOK = hSAXWriter:START-ELEMENT("stocklevelRow")
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("sku", tt_webLager.Kode)
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("qty", STRING(tt_webLager.Lagant))
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("Pris", STRING(tt_webLager.Pris))

    lOK = hSAXWriter:WRITE-DATA-ELEMENT("store", 'NO')
    lOK = hSAXWriter:WRITE-DATA-ELEMENT("website", 'norway')
    lOK = hSAXWriter:END-ELEMENT("stocklevelRow")
    NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
      DO:
          RUN bibl_loggDbFri.p (cUtfilNavn,
              'Eksportfeil på artikkel: ' + STRING(tt_webLager.ArtikkelNr) + ' Butikk: ' + STRING(tt_webLager.Butik)
              ).          
      END.
  END. /* EKSPORTER */
  END.
  IF ERROR-STATUS:ERROR THEN 
    DO:
        RUN bibl_loggDbFri.p (cUtfilNavn,
            'Eksportfeil på blokk.'
            ).          
    END.
  
  lOK = hSAXWriter:END-ELEMENT("PublishEComStocklevel").
  lOK = hSAXWriter:END-DOCUMENT( ).
  DELETE OBJECT hSAXWriter.
  
  IF SEARCH(cTmpFilNavn) <> ? THEN DO:
    OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
    IF bCopy THEN 
      RUN stgCopy (REPLACE(cTmpFilNavn,'TMP','')).
  END.
END. /* MAGENTO */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterLast-SkoFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterLast-SkoFil Procedure 
PROCEDURE EksporterLast-SkoFil :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webLast-Sko
        BREAK BY tt_webLast-Sko.Last-Id:
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
        /* Antall psoter i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webLast-Sko:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"Last-Sko",iAntRec,"Last-Id",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterLevBasFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterLevBasFil Procedure 
PROCEDURE EksporterLevBasFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webLevBas
        BREAK BY tt_webLevBas.LevNr:
        /* Kontrollsiffer på filutlegg. */
        iAntRec = iAntRec + 1.
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webLevbas:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"Levbas",iAntRec,"Levnr",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterMaterialFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterMaterialFil Procedure 
PROCEDURE EksporterMaterialFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webMaterial
        BREAK BY tt_webMaterial.MatKod:
        /* Kontrollsiffer på filutlegg. */
        iAntRec = iAntRec + 1.
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webMaterial:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"Material",iAntRec,"MatKod",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterMedlemFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterMedlemFil Procedure 
PROCEDURE EksporterMedlemFil :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF VAR bStreamAapen AS LOG NO-UNDO.
    DEF VAR iAntPoster   AS INT NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webMedlem
        BREAK BY tt_webMedlem.MedlemsNr:
        /* antall utlagte poster. */
        iAntEksport = iAntEksport + 1.
        /* Antall poster i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */

    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf   = BUFFER tt_webMedlem:HANDLE.

        RUN tabelleksportXml.p (hBuf,"Medlem",iAntRec,"MedlemsNr",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterMedlemSaldoFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterMedlemSaldoFil Procedure 
PROCEDURE EksporterMedlemSaldoFil :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF VAR bStreamAapen AS LOG NO-UNDO.
    DEF VAR iAntPoster   AS INT NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webMedlemSaldo
        BREAK BY tt_webMedlemSaldo.MedlemsNr
              BY tt_webMedlemSaldo.ButikkNr:
        /* antall utlagte poster. */
        iAntEksport = iAntEksport + 1.
        /* Antall poster i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */

    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf   = BUFFER tt_webMedlemSaldo:HANDLE.

        RUN tabelleksportXml.p (hBuf,"MedlemSaldo",iAntRec,"MedlemsNr,ButikkNr",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterMedlemsGruppeFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterMedlemsGruppeFil Procedure 
PROCEDURE EksporterMedlemsGruppeFil :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF VAR bStreamAapen AS LOG NO-UNDO.
    DEF VAR iAntPoster   AS INT NO-UNDO.


IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webMedlemsGruppe
        BREAK BY tt_webMedlemsGruppe.Medgruppe:
        /* antall utlagte poster. */
        iAntEksport = iAntEksport + 1.
        /* Antall poster i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */

    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf   = BUFFER tt_webMedlemsGruppe:HANDLE.

        RUN tabelleksportXml.p (hBuf,"MedlemsGruppe",iAntRec,"MedGruppe",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterMedlemsKortFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterMedlemsKortFil Procedure 
PROCEDURE EksporterMedlemsKortFil :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF VAR bStreamAapen AS LOG NO-UNDO.
    DEF VAR iAntPoster   AS INT NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webMedlemsKort
        BREAK BY tt_webMedlemsKort.KortNr:
        /* antall utlagte poster. */
        iAntEksport = iAntEksport + 1.
        /* Antall poster i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */

    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf   = BUFFER tt_webMedlemsKort:HANDLE.

        RUN tabelleksportXml.p (hBuf,"MedlemsKort",iAntRec,"KortNr",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterMomsFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterMomsFil Procedure 
PROCEDURE EksporterMomsFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webMoms
        BREAK BY tt_webMoms.MomsKod:
        /* Kontrollsiffer på filutlegg. */
        iAntRec = iAntRec + 1.
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webMoms:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"Moms",iAntRec,"MomsKod",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterOvandelFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterOvandelFil Procedure 
PROCEDURE EksporterOvandelFil :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webOvandel
        BREAK BY tt_webOvandel.Ov-Id:
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
        /* Antall psoter i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webOvandel:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"Ovandel",iAntRec,"Ov-Id",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterProdusentFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterProdusentFil Procedure 
PROCEDURE EksporterProdusentFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webProdusent
        BREAK BY tt_webProdusent.ProdNr:
        /* Kontrollsiffer på filutlegg. */
        iAntRec = iAntRec + 1.
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webProdusent:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"Produsent",iAntRec,"Prodnr",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */
IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterRegnskapsavdelingFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterRegnskapsavdelingFil Procedure 
PROCEDURE EksporterRegnskapsavdelingFil :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webRegnskapsavdeling
        BREAK BY tt_webRegnskapsavdeling.RAvdNr:
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
        /* Antall psoter i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webRegnskapsavdeling:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"Regnskapsavdeling",iAntRec,"RAvdNr",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterSasongFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterSasongFil Procedure 
PROCEDURE EksporterSasongFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webSasong
        BREAK BY tt_webSasong.Sasong:
        /* Kontrollsiffer på filutlegg. */
        iAntRec = iAntRec + 1.
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webSasong:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"Sasong",iAntRec,"Sasong",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterSlitSulaFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterSlitSulaFil Procedure 
PROCEDURE EksporterSlitSulaFil :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webSlitSula
        BREAK BY tt_webSlitSula.Slit-Id:
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
        /* Antall psoter i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webSlitSula:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"SlitSula",iAntRec,"Slit-Id",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterStrKonvFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterStrKonvFil Procedure 
PROCEDURE EksporterStrKonvFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webStrKonv
        BREAK BY tt_webStrKonv.StrKode:
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
        /* Antall psoter i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webStrKonv:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"StrKonv",iAntRec,"StrKode",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterStrTStrFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterStrTStrFil Procedure 
PROCEDURE EksporterStrTStrFil :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webStrTStr
        BREAK BY tt_webStrTStr.StrTypeId:
        IF LAST-OF(tt_webStrTStr.StrTypeId) THEN
          DO:
            /* antall utlagte poster. */
            iAntEksport = iAntEksport + 1.
            /* Antall poster i tabell */
            iAntRec = iAntRec + 1.
          END.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webStrTStr:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"StrTStr",iAntRec,"StrTypeID",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterUnderkategoriFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterUnderkategoriFil Procedure 
PROCEDURE EksporterUnderkategoriFil :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webUnderkategori
        BREAK BY tt_webUnderKategori.UnderKatNr:
        iAntEksport = iAntEksport + 1.
        /* Antall psoter i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webUnderkategori:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"Ubnderkategori",iantrec,"UnderKatNr",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterVaremerkeFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterVaremerkeFil Procedure 
PROCEDURE EksporterVaremerkeFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webVaremerke
        BREAK BY tt_webVaremerke.VmId:
        /* Kontrollsiffer på filutlegg. */
        iAntRec = iAntRec + 1.
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webVaremerke:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"Varemerke",iAntRec,"VmId",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterVarGrFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterVarGrFil Procedure 
PROCEDURE EksporterVarGrFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.
DEF VAR iAntPoster   AS INT  NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webVarGr
        BREAK BY tt_webVarGr.Vg:
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
        /* Antall poster i tabell */
        iAntRec = iAntRec + 1.
    END. /* EKSPORTER */
    
    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf = BUFFER tt_webVarGr:HANDLE.
    
        RUN tabelleksportXml.p (hBuf,"VarGr",iantrec,"Vg",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterVgAktFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterVgAktFil Procedure 
PROCEDURE EksporterVgAktFil :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF VAR bStreamAapen AS LOG NO-UNDO.
    DEF VAR iAntPoster   AS INT NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webVgAkt
        BREAK BY tt_webVgAkt.Vg
              BY tt_webVgAkt.AktNr:
        /* Kontrollsiffer på filutlegg. */
        iAntRec = iAntRec + 1.
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
    END. /* EKSPORTER */

    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf   = BUFFER tt_webVgAkt:HANDLE.

        RUN tabelleksportXml.p (hBuf,"VgAkt",iAntRec,"Vg,AktNr",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterVgKatFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterVgKatFil Procedure 
PROCEDURE EksporterVgKatFil :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF VAR bStreamAapen AS LOG NO-UNDO.
    DEF VAR iAntPoster   AS INT NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webVgKat
        BREAK BY tt_webVgKat.Vg
              BY tt_webVgKat.VgKat:
        /* Kontrollsiffer på filutlegg. */
        iAntRec = iAntRec + 1.
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
    END. /* EKSPORTER */

    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf   = BUFFER tt_webVgKat:HANDLE.

        RUN tabelleksportXml.p (hBuf,"VgKat",iAntRec,"Vg,VgKat",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterVgKundeGrpRabattFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterVgKundeGrpRabattFil Procedure 
PROCEDURE EksporterVgKundeGrpRabattFil :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF VAR bStreamAapen AS LOG NO-UNDO.
    DEF VAR iAntPoster   AS INT NO-UNDO.

IF CAN-DO('WEBBUT',cEDBSystem) THEN 
WEBBUT:
DO:
    iAntRec = 0.
    EKSPORTER:
    FOR EACH tt_webVgKundeGrpRabatt
        BREAK BY tt_webVgKundeGrpRabatt.Vg
        BY tt_webVgKundeGrpRabatt.GruppeId:
        /* Kontrollsiffer på filutlegg. */
        iAntRec = iAntRec + 1.
        /* antall utlagte psoter. */
        iAntEksport = iAntEksport + 1.
    END. /* EKSPORTER */

    IF iAntRec > 0 THEN
    UtleggXML:
    DO:
        ASSIGN
            cOmmit = ""
            hBuf   = BUFFER tt_webVgKundeGrpRabatt:HANDLE.

        RUN tabelleksportXml.p (hBuf,"VgKundeGrpRabatt",iAntRec,"Vg,GruppeId",cOmmit,cTmpFilNavn).
        IF SEARCH(cTmpFilNavn) <> ? THEN
        DO:
            OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).
            RUN loggFilNavn (cFilNavn, iAntRec).
        END.
    END. /* UtleggXML */
END. /* WEBBUT */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.

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
  DEF VAR cFilnavn AS CHAR NO-UNDO.

  ASSIGN
      cFilnavn = OS-GETENV('TMP') + '\' + 'WebError' + STRING(TIME) + '.log'.
  
  OUTPUT TO VALUE(cFilnavn).
    PUT UNFORMATTED
      "Eksport til Web butik " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      "Feil ved eksport." SKIP(1)
      .
    FOR EACH tt_Error:
      PUT UNFORMATTED tt_Error.Tekst SKIP.
    END.
  OUTPUT CLOSE.
  IF SEARCH(cFilnavn) <> ? THEN
  DO:
    DEF VAR hInstance AS INT.

    RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH(cFilnavn),
                                  "",
                                  1,
                                  OUTPUT hInstance).

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KopierElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierElogg Procedure 
PROCEDURE KopierElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cFeltLst AS CHAR NO-UNDO.
    DEF VAR piLoop   AS INT  NO-UNDO.
    DEF VAR iTst     AS INT  NO-UNDO.

    ASSIGN
        cFeltLst = 'ArtBas,Lager,Avdeling,Aktivitet,HuvGr,Kategori,LevBas,Material,Sasong,Produsent,Varemerke,VarGr,StrKonv,Moms,VgKat,VgAkt,VgKundeGrpRabatt' +
                   ',Kunde,KundeKort,KundeSaldo,KundeGruppe,Medlem,MedlemsKort,MedlemSaldo,MedlemsGruppe,StrType,Farg,KOrdreHode,Hovedkategori,Underkategori,' +
                   'Klack,Handtering,Innersula,Ovandel,Anv-Kod,Regnskapsavdeling,Last-Sko,Slitsula'.

    DEFINE BUFFER bElogg   FOR Elogg.
    DEFINE BUFFER erpELogg FOR Elogg.

    DO piLoop = 1 TO NUM-ENTRIES(cFeltLst):

        /* NB: Her skal ALLTID stå WebBut, da det er det db triggerne bruker */
        FOR EACH ELogg WHERE ELogg.TabellNavn     = entry(piLoop,cFeltLst) AND
                             ELogg.EksterntSystem = "WEBBUT" /*AND 
                             Elogg.EndringsType   = 1      */ NO-LOCK:
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF NOT AVAIL bElogg THEN
                NEXT.
            BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-loggFilNavn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loggFilNavn Procedure 
PROCEDURE loggFilNavn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pcFilNavn AS CHAR NO-UNDO.                                            
DEF INPUT PARAMETER piantRec  AS INT NO-UNDO.

DEF VAR cFilNavn  AS CHAR NO-UNDO.
DEF VAR c2FilNavn AS CHAR NO-UNDO.
DEF VAR iEntry    AS INT  NO-UNDO.

ASSIGN
    iEntry    = NUM-ENTRIES(cTmpFilNavn,'\')
    cFilNavn  = cTmpFilNavn
    c2FilNavn = cTmpFilNavn
    .

ENTRY(iEntry,cFilNavn,'\')  = pcFilNavn.
ENTRY(iEntry,c2FilNavn,'\') = cLogFilNavn.

OUTPUT STREAM FilLogg TO VALUE(c2FilNavn) NO-ECHO APPEND.

FILE-INFO:FILE-NAME = cFilNavn.  

EXPORT STREAM FilLogg DELIMITER ';'
    STRING(TODAY)
    STRING(TIME,"HH:MM:SS")
    pcFilNavn
    piAntRec
    FILE-INFO:FILE-SIZE.
OUTPUT STREAM FilLogg CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-stgCopy) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE stgCopy Procedure 
PROCEDURE stgCopy :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER icFilNavn AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE iInt        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cstgFilNavn AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cKatalog    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cNyKat      AS CHARACTER NO-UNDO.

  ASSIGN 
    iInt = NUM-ENTRIES(icFilNavn,'\')
    iInt = iInt - 1
    cNyKat = '\stg'.
  
  IF SEARCH(icFilNavn) <> ? AND iInt > 0 THEN 
  DO:
      ASSIGN 
          cstgFilNavn = icFilNavn  
          cKatalog    = icFilNavn    
          ENTRY(iInt,cstgFilNavn,'\') = ENTRY(iInt,cstgFilNavn,'\') + cNyKat 
          ENTRY(iInt + 1,cKatalog,'\') = '' 
          cKatalog    = RIGHT-TRIM(cKatalog,'\') + cNyKat   
          .
      OS-COMMAND SILENT mkdir VALUE('"' + cKatalog + '"') NO-ERROR.    
      OS-COPY VALUE(icFilNavn) VALUE(cstgFilNavn).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-webbutikkeksporterr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE webbutikkeksporterr Procedure 
PROCEDURE webbutikkeksporterr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcErrTekst  AS CHAR NO-UNDO.
  DEF INPUT PARAMETER piGradering AS INT NO-UNDO.

  DEF VAR piLinjeNr AS INT NO-UNDO.
  FIND LAST tt_error NO-LOCK NO-ERROR.
  IF AVAILABLE tt_error THEN
      piLinjeNr = tt_error.LinjeNr + 1.
  ELSE
      piLinjeNr = 1.

    DO:
        CREATE tt_Error.
        ASSIGN
            pcErrTekst         = "* " + STRING(TODAY) + " " + STRING(TIME,"HH:MM:SS") + " " + pcErrTekst
            tt_Error.LinjeNr   = piLinjeNr
            tt_Error.Tekst     = pcErrTekst
            tt_error.Gradering = piGradering
            .
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fixChkEAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fixChkEAN Procedure 
FUNCTION fixChkEAN RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  Räknar ut checksiffra för ean EAN-kod - parameter utan chksiffra
              i.e 12 lång
      Notes:  
  ------------------------------------------------------------------------------*/
      
  cKode = cKode + '0'.
  RUN bibl_chkean.p(INPUT-OUTPUT cKode).
  RETURN cKode.

  /*
      DEF VAR iCount1 AS INTE NO-UNDO.
      DEF VAR iMulti  AS INTE INIT 1 NO-UNDO.
      DEF VAR iSum AS INTE NO-UNDO.
      DO iCount1 = LENGTH(cKode) TO 1 BY -1:  
          ASSIGN iMulti = IF iMulti = 1 THEN 3 ELSE 1
                 iSum = iSum + INT(SUBSTR(cKode,iCount1,1)) * iMulti.
      END.
      RETURN cKode + string((10 - iSum MODULO 10) MODULO 10).
  */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getBildefil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBildefil Procedure 
FUNCTION getBildefil RETURNS CHARACTER
  ( INPUT ipBildNr AS INTEGER, 
    INPUT iType    AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ocBildeFil AS CHARACTER  NO-UNDO.
  FIND BildeRegister NO-LOCK WHERE
    BildeRegister.BildNr = ipBildNr NO-ERROR.
  IF AVAIL BildeRegister AND TRIM(BildeRegister.FilNavn) <> "" THEN DO:
    IF VALID-HANDLE(wLibHandle) THEN
      RUN HentBildePeker IN wLibHandle (INPUT ipBildNr, iType, BildeRegister.FilNavn, OUTPUT ocBildeFil).
  END.
  /* cBlanktBilde */
  RETURN ocBildeFil.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

