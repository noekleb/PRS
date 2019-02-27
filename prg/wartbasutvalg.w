&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEF VAR hParent       AS HANDLE NO-UNDO.
  DEF VAR h_QueryObject AS HANDLE NO-UNDO.
/*   DEF VAR gcWhere AS CHAR NO-UNDO. */
&ELSE
  DEF INPUT PARAM hParent       AS HANDLE NO-UNDO.
  DEF INPUT PARAM h_QueryObject AS HANDLE NO-UNDO.
/*   DEF OUTPUT PARAMETER gcWhere AS CHAR NO-UNDO. */
&ENDIF

/* Local Variable Definitions ---                                       */
DEF VAR gcFields    AS CHAR NO-UNDO.  
DEF VAR gcValues    AS CHAR NO-UNDO.
DEF VAR gcSort      AS CHAR NO-UNDO.
DEF VAR gcOperator  AS CHAR NO-UNDO.
DEF VAR gcFeltListe AS CHAR NO-UNDO.
DEF VAR gbOk        AS CHAR INITIAL "AVBRYT" NO-UNDO.

DEF VAR gcWhere     AS CHAR NO-UNDO.

DEF VAR bOk                       AS LOG    NO-UNDO.    
DEF VAR ix                        AS INT    NO-UNDO.
DEF VAR hFartSokFrame             AS HANDLE NO-UNDO.

DEF VAR cInitDefaults             AS CHAR NO-UNDO.
DEF VAR cViewDefaults             AS CHAR NO-UNDO.
                                  
/* Rowids (+ eksta lister for FK, Avdeling og HuvGr: */
DEF VAR cAvdelingRowIdList        AS CHAR   NO-UNDO.
DEF VAR cHuvGrRowIdList           AS CHAR   NO-UNDO.
DEF VAR cHuvGrAvdelingList        AS CHAR   NO-UNDO.
DEF VAR cVarGrRowIdList           AS CHAR   NO-UNDO.
DEF VAR cVarGrHuvGrList           AS CHAR   NO-UNDO.
DEF VAR cBehandlingskodeRowIdList AS CHAR   NO-UNDO.
DEF VAR cStrTypeRowIdList         AS CHAR   NO-UNDO.
DEF VAR cArtSlagRowIdList         AS CHAR   NO-UNDO.
DEF VAR cIndTypeRowIdList         AS CHAR   NO-UNDO.
DEF VAR cKategoriRowIdList        AS CHAR   NO-UNDO.
DEF VAR cRAvdNr-RowIdList         AS CHAR   NO-UNDO.
DEF VAR cAlfaKode2-RowIdList         AS CHAR   NO-UNDO.

DEF VAR cOnLineLevNr-RowIdList AS CHAR NO-UNDO.
DEF VAR cOnLineLevNr-IdList    AS CHAR NO-UNDO.

DEF VAR cLevBasRowIdList          AS CHAR   NO-UNDO.
DEF VAR cProdusentRowIdList       AS CHAR   NO-UNDO.
DEF VAR cVaremerkeRowIdList       AS CHAR   NO-UNDO.
DEF VAR cSaSongRowIdList          AS CHAR   NO-UNDO.
DEF VAR cFargRowIdList            AS CHAR   NO-UNDO.
DEF VAR cGarantiRowIdList         AS CHAR   NO-UNDO.
DEF VAR cValutaRowIdList          AS CHAR   NO-UNDO.
                                  
DEF VAR cMaterialRowIdList        AS CHAR   NO-UNDO.
DEF VAR cKlackRowIdList           AS CHAR   NO-UNDO.
DEF VAR cInnerSulaRowIdList       AS CHAR   NO-UNDO.
DEF VAR cOvandelRowIdList         AS CHAR   NO-UNDO.
DEF VAR cSlitSulaRowIdList        AS CHAR   NO-UNDO.
DEF VAR cLast-SkoRowIdList        AS CHAR   NO-UNDO.
DEF VAR cAnv-KodRowIdList         AS CHAR   NO-UNDO.
                                  
DEF VAR cButikerRowIdList         AS CHAR   NO-UNDO.
DEF VAR cButikerRowIdList-2       AS CHAR   NO-UNDO.
DEF VAR cButikerRowIdList-3       AS CHAR   NO-UNDO.
DEF VAR cPrisprofilRowIdList      AS CHAR   NO-UNDO.
DEF VAR cBestStatRowIdList        AS CHAR   NO-UNDO.

DEF VAR cArtButikerIdList         AS CHAR   NO-UNDO.
DEF VAR cArtButikerRowIdList      AS CHAR   NO-UNDO.
                
DEF VAR cAktivitetRowIdList       AS CHAR   NO-UNDO.
DEF VAR cVarebokRowIdList         AS CHAR   NO-UNDO.
DEF VAR cMesseRowIdList           AS CHAR   NO-UNDO.
DEF VAR cKampanjeRowIdList        AS CHAR   NO-UNDO.
DEF VAR cKombKampanjeRowIdList    AS CHAR   NO-UNDO.

/* Primærnøkkellister - til bruk i utvalg: */
DEF VAR cAvdelingIdList           AS CHAR   NO-UNDO.
DEF VAR cHuvGrIdList              AS CHAR   NO-UNDO.
DEF VAR cVarGrIdList              AS CHAR   NO-UNDO.
DEF VAR cBehandlingskodeIdList    AS CHAR   NO-UNDO.
DEF VAR cStrTypeIdList            AS CHAR   NO-UNDO.
DEF VAR cArtSlagIdList            AS CHAR   NO-UNDO.
DEF VAR cIndTypeIdList            AS CHAR   NO-UNDO.
DEF VAR cKategoriIdList           AS CHAR   NO-UNDO.
                                  
DEF VAR cLevBasIdList             AS CHAR   NO-UNDO.
DEF VAR cProdusentIdList          AS CHAR   NO-UNDO.
DEF VAR cVaremerkeIdList          AS CHAR   NO-UNDO.
DEF VAR cSaSongIdList             AS CHAR   NO-UNDO.
DEF VAR cFargIdList               AS CHAR   NO-UNDO.
DEF VAR cGarantiIdList            AS CHAR   NO-UNDO.
DEF VAR cValutaIdList             AS CHAR   NO-UNDO.
                                     
DEF VAR cMaterialIdList           AS CHAR   NO-UNDO.
DEF VAR cKlackIdList              AS CHAR   NO-UNDO.
DEF VAR cInnerSulaIdList          AS CHAR   NO-UNDO.
DEF VAR cOvandelIdList            AS CHAR   NO-UNDO.
DEF VAR cSlitSulaIdList           AS CHAR   NO-UNDO.
DEF VAR cLast-SkoIdList           AS CHAR   NO-UNDO.
DEF VAR cAnv-KodIdList            AS CHAR   NO-UNDO.
DEF VAR cRAvdNr-IdList            AS CHAR   NO-UNDO.
DEF VAR cAlfaKode2-IdList         AS CHAR   NO-UNDO.

DEF VAR cButikerIdList            AS CHAR   NO-UNDO.
DEF VAR cButikerIdList-2         AS CHAR   NO-UNDO.
DEF VAR cButikerIdList-3         AS CHAR   NO-UNDO.
DEF VAR cPrisprofilIdList         AS CHAR   NO-UNDO.
DEF VAR cBestStatIdList           AS CHAR   NO-UNDO.

DEF VAR cAktivitetIdList          AS CHAR   NO-UNDO.
DEF VAR cVarebokIdList            AS CHAR   NO-UNDO.
DEF VAR cMesseIdList              AS CHAR   NO-UNDO.
DEF VAR cKampanjeIdList           AS CHAR   NO-UNDO.
DEF VAR cKombKampanjeIdList       AS CHAR   NO-UNDO.

DEF VAR cComboDefault             AS CHAR   NO-UNDO.
DEF VAR cDelimDefault             AS CHAR   NO-UNDO.
                                  
DEF VAR iSelectorSourcCount       AS INT    NO-UNDO.
DEF VAR cCurrSelectBuffer         AS CHAR   NO-UNDO.

DEFINE VARIABLE lArtBut AS LOGICAL     NO-UNDO. /* bara om man har flera */

{tmp2artbasdef.i}

    {runlib.i} /* Starter procedurebibloteket. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-1 RECT-3 btnUtvalgAvdeling ~
btnUtvalgLevBas btnUtvalgMesse btnUtvalgMaterial btnUtvalgKampanje ~
fi-cListKampanje fi-cListAvdeling fi-cListLevBas fi-cListMesse ~
fi-cListMaterial btnUtvalgHuvGr btnUtvalgProdusent btnUtvalgVarebok ~
btnUtvalgAnv-Kod btnKombKampanje fi-cListKombKampanje fi-cListHuvGr ~
fi-cListProdusent fi-cListVarebok fi-cListAnv-Kod btnUtvalgVarGr ~
btnUtvalgVaremerke btnUtvalgKlack btnArtButiker fi-cListArtButiker ~
fi-cListVarGr fi-cListVaremerke fi-cListKlack btnUtvalgStrType ~
fi-cListStrType btnUtvalgSaSong fi-cListSaSong btnUtvalgRAvdNr ~
fi-cList-ravdnr btnUtvalgInnerSula fi-cListInnerSula btnUtvalgArtSlag ~
fi-cListArtSlag btnUtvalgFarg fi-cListFarg btnUtvalgAlfaKode2 ~
fi-cList-AlfaKode2 btnUtvalgOvandel fi-cListOvandel btnUtvalgIndType ~
btnUtvalgGaranti btnUtvalgOnLineLevNr btnUtvalgSlitSula fi-cListIndType ~
fi-cListGaranti fi-cList-OnLineLevNr fi-cListSlitSula btnUtvalgKategori ~
btnUtvalgValuta fi-cListKategori fi-cListValuta btnUtvalgLast-Sko ~
fi-cListLast-sko btnUtvalgAktivitet btnUtvalgBehandlingskode ~
fi-cListAktivitet fi-cListBehandlingskode FI-Strekkode FI-Bestillingsnummer ~
FI-ERPNr btnUtvalgButiker-2 cmbTTId cmbVaremottak fi-cListButiker-2 ~
fi-dFraVaremottak fi-dTilVaremottak btnUtvalgButiker cmbLager ~
fi-cListButiker btnUtvalgPrisprofil cmbTilbud fi-cListPrisprofil ~
btnUtvalgButiker-3 fi-cListButiker-3 cmbKombKampanje btnUtvalgBestStat ~
cmbBekreftet cmbBest fi-dFraBest fi-dTilBest fi-cListBestStat cmbBestType ~
Btn_Nullstill Btn_OK Btn_Cancel tbVisQuery 
&Scoped-Define DISPLAYED-OBJECTS fi-cListKampanje fi-cListAvdeling ~
fi-cListLevBas fi-cListMesse fi-cListMaterial fi-cListKombKampanje ~
fi-cListHuvGr fi-cListProdusent fi-cListVarebok fi-cListAnv-Kod ~
fi-cListArtButiker fi-cListVarGr fi-cListVaremerke fi-cListKlack ~
fi-cListStrType fi-cListSaSong fi-cList-ravdnr fi-cListInnerSula ~
fi-cListArtSlag fi-cListFarg fi-cList-AlfaKode2 fi-cListOvandel ~
fi-cListIndType fi-cListGaranti fi-cList-OnLineLevNr fi-cListSlitSula ~
fi-cListKategori fi-cListValuta fi-cListLast-sko fi-cListAktivitet ~
fi-cListBehandlingskode FI-Strekkode FI-Bestillingsnummer FI-ERPNr cmbTTId ~
cmbVaremottak fi-cListButiker-2 fi-dFraVaremottak fi-dTilVaremottak ~
cmbLager fi-cListButiker cmbTilbud fi-cListPrisprofil fi-cListButiker-3 ~
cmbKombKampanje cmbBekreftet cmbBest fi-dFraBest fi-dTilBest ~
fi-cListBestStat cmbBestType tbVisQuery 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddCharToQuery wWin 
FUNCTION AddCharToQuery RETURNS CHARACTER
  ( INPUT icCriteria AS CHAR,
    INPUT icList     AS CHAR,
    INPUT icField    AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddIntToQuery wWin 
FUNCTION AddIntToQuery RETURNS CHARACTER
  ( INPUT icCriteria AS CHAR,
    INPUT icList     AS CHAR,
    INPUT icField    AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD applyOKbutton wWin 
FUNCTION applyOKbutton RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MaxValue wWin 
FUNCTION MaxValue RETURNS CHARACTER
  ( INPUT icList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD MinValue wWin 
FUNCTION MinValue RETURNS CHARACTER
  ( INPUT icList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSelectionFillIn wWin 
FUNCTION setSelectionFillIn RETURNS LOGICAL
  ( INPUT ihFillIn    AS HANDLE,
    INPUT icRowidList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dartbas AS HANDLE NO-UNDO.
DEFINE VARIABLE h_fartsokfilter AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnArtButiker 
     LABEL "Webshop" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnKombKampanje 
     LABEL "Komb.kamp." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgAktivitet 
     LABEL "Aktivitet..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgAlfaKode2 
     LABEL "Oppr. land..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgAnv-Kod 
     LABEL "Brukskode..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgArtSlag 
     LABEL "Varetype..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgAvdeling 
     LABEL "Avdeling..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgBehandlingskode 
     LABEL "Beh.kode..." 
     SIZE 15 BY 1.14 TOOLTIP "Behandlingskode".

DEFINE BUTTON btnUtvalgBestStat 
     LABEL "Best.status..." 
     SIZE 13.4 BY 1.14 TOOLTIP "Hvis butikk(liste) ikke angis sjekkes kun lagertall".

DEFINE BUTTON btnUtvalgButiker 
     LABEL "i Butikk..." 
     SIZE 22 BY 1.14 TOOLTIP "Hvis butikk(liste) ikke angis sjekkes kun lagertall".

DEFINE BUTTON btnUtvalgButiker-2 
     LABEL "i Butikk..." 
     SIZE 22 BY 1.14 TOOLTIP "Hvis butikk(liste) ikke angis sjekkes kun lagertall".

DEFINE BUTTON btnUtvalgButiker-3 
     LABEL "i Butikk..." 
     SIZE 22 BY 1.14 TOOLTIP "Hvis butikk(liste) ikke angis sjekkes kun lagertall".

DEFINE BUTTON btnUtvalgFarg 
     LABEL "Farger..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgGaranti 
     LABEL "Garanti..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgHuvGr 
     LABEL "Hovedgr..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgIndType 
     LABEL "Individtype..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgInnerSula 
     LABEL "Innersåle..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgKampanje 
     LABEL "Tilbud/prisendr" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgKategori 
     LABEL "Kategori..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgKlack 
     LABEL "Hæl..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgLast-Sko 
     LABEL "Læst..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgLevBas 
     LABEL "Leverandør..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgMaterial 
     LABEL "Material..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgMesse 
     LABEL "Messe..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgOnLineLevNr 
     LABEL "Online levnr" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgOvandel 
     LABEL "Innerfor..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgPrisprofil 
     LABEL "for Profil..." 
     SIZE 22 BY 1.14 TOOLTIP "Hvis butikk(liste) ikke angis sjekkes kun lagertall".

DEFINE BUTTON btnUtvalgProdusent 
     LABEL "Produsent..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgRAvdNr 
     LABEL "Vareområde..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgSaSong 
     LABEL "Sesong..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgSlitSula 
     LABEL "Slitesåle..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgStrType 
     LABEL "Str.type..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgValuta 
     LABEL "Valuta..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgVarebok 
     LABEL "Varebok..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgVaremerke 
     LABEL "Varemerke..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnUtvalgVarGr 
     LABEL "Varegr..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Nullstill 
     LABEL "Nullstill kriterier" 
     SIZE 26 BY 1.14.

DEFINE BUTTON Btn_OK 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cmbBekreftet AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Bekreftet" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "0","1"
     DROP-DOWN-LIST
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE cmbBest AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bestilling" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 29 BY 1 TOOLTIP "Minst en dato må fylles ut ved sjekk mot bestilling!" NO-UNDO.

DEFINE VARIABLE cmbBestType AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "0","1"
     DROP-DOWN-LIST
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE cmbKombKampanje AS CHARACTER FORMAT "X(256)":U 
     LABEL "Komb.kampanje" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 28.8 BY 1 TOOLTIP "Lager" NO-UNDO.

DEFINE VARIABLE cmbLager AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lager" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 28.8 BY 1 TOOLTIP "Lager" NO-UNDO.

DEFINE VARIABLE cmbTilbud AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tilbud" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 28.8 BY 1 TOOLTIP "Flagg for automatisk bestillingsforslag" NO-UNDO.

DEFINE VARIABLE cmbTTId AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 1 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1",1
     DROP-DOWN-LIST
     SIZE 18.2 BY 1 TOOLTIP "Minst en dato må fylles ut ved sjekk mot bestilling!" NO-UNDO.

DEFINE VARIABLE cmbVaremottak AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 29 BY 1 TOOLTIP "Minst en dato må fylles ut ved sjekk mot bestilling!" NO-UNDO.

DEFINE VARIABLE FI-Bestillingsnummer AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bestillingsnr" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 TOOLTIP "Alle artikler som har bestillingsnr som begynner med ..." NO-UNDO.

DEFINE VARIABLE fi-cList-AlfaKode2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cList-OnLineLevNr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cList-ravdnr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListAktivitet AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListAnv-Kod AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListArtButiker AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListArtSlag AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListAvdeling AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListBehandlingskode AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListBestStat AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 13.8 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListButiker AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListButiker-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListButiker-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListFarg AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListGaranti AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListHuvGr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListIndType AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListInnerSula AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListKampanje AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListKategori AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListKlack AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListKombKampanje AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListLast-sko AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListLevBas AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListMaterial AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListMesse AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListOvandel AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListPrisprofil AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListProdusent AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListSaSong AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListSlitSula AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListStrType AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListValuta AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListVarebok AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListVaremerke AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cListVarGr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fi-dFraBest AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-dFraVaremottak AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-dTilBest AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-dTilVaremottak AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ERPNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "ERPNr" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 TOOLTIP "Alle artikler som har ERPNr som begynner med ..." NO-UNDO.

DEFINE VARIABLE FI-Strekkode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 TOOLTIP "Alle artikler som har strekkoder som begynner med ..." NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 161 BY 6.67.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 161 BY 10.24.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 161 BY 1.91.

DEFINE VARIABLE tbVisQuery AS LOGICAL INITIAL no 
     LABEL "Vis spørring" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     btnUtvalgAvdeling AT ROW 11.05 COL 2.6
     btnUtvalgLevBas AT ROW 11.05 COL 35
     btnUtvalgMesse AT ROW 11.05 COL 66.8
     btnUtvalgMaterial AT ROW 11.05 COL 98.6
     btnUtvalgKampanje AT ROW 11.05 COL 132
     fi-cListKampanje AT ROW 11.05 COL 145 COLON-ALIGNED NO-LABEL
     fi-cListAvdeling AT ROW 11.1 COL 15.8 COLON-ALIGNED NO-LABEL
     fi-cListLevBas AT ROW 11.1 COL 48.2 COLON-ALIGNED NO-LABEL
     fi-cListMesse AT ROW 11.1 COL 80.2 COLON-ALIGNED NO-LABEL
     fi-cListMaterial AT ROW 11.1 COL 112 COLON-ALIGNED NO-LABEL
     btnUtvalgHuvGr AT ROW 12.29 COL 2.6
     btnUtvalgProdusent AT ROW 12.29 COL 35
     btnUtvalgVarebok AT ROW 12.29 COL 66.8
     btnUtvalgAnv-Kod AT ROW 12.29 COL 98.6
     btnKombKampanje AT ROW 12.29 COL 132 WIDGET-ID 22
     fi-cListKombKampanje AT ROW 12.29 COL 145 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     fi-cListHuvGr AT ROW 12.33 COL 16 COLON-ALIGNED NO-LABEL
     fi-cListProdusent AT ROW 12.33 COL 48.2 COLON-ALIGNED NO-LABEL
     fi-cListVarebok AT ROW 12.33 COL 80.2 COLON-ALIGNED NO-LABEL
     fi-cListAnv-Kod AT ROW 12.33 COL 112 COLON-ALIGNED NO-LABEL
     btnUtvalgVarGr AT ROW 13.52 COL 2.6
     btnUtvalgVaremerke AT ROW 13.52 COL 35
     btnUtvalgKlack AT ROW 13.52 COL 98.6
     btnArtButiker AT ROW 13.52 COL 132
     fi-cListArtButiker AT ROW 13.52 COL 145 COLON-ALIGNED NO-LABEL
     fi-cListVarGr AT ROW 13.57 COL 16 COLON-ALIGNED NO-LABEL
     fi-cListVaremerke AT ROW 13.57 COL 48.2 COLON-ALIGNED NO-LABEL
     fi-cListKlack AT ROW 13.57 COL 112 COLON-ALIGNED NO-LABEL
     btnUtvalgStrType AT ROW 14.76 COL 2.4
     fi-cListStrType AT ROW 14.81 COL 16 COLON-ALIGNED NO-LABEL
     btnUtvalgSaSong AT ROW 14.81 COL 35
     fi-cListSaSong AT ROW 14.81 COL 48.2 COLON-ALIGNED NO-LABEL
     btnUtvalgRAvdNr AT ROW 14.81 COL 66.6
     fi-cList-ravdnr AT ROW 14.81 COL 80.2 COLON-ALIGNED NO-LABEL
     btnUtvalgInnerSula AT ROW 14.81 COL 98.6
     fi-cListInnerSula AT ROW 14.81 COL 112 COLON-ALIGNED NO-LABEL
     btnUtvalgArtSlag AT ROW 16 COL 2.4
     fi-cListArtSlag AT ROW 16.05 COL 16 COLON-ALIGNED NO-LABEL
     btnUtvalgFarg AT ROW 16.05 COL 35
     fi-cListFarg AT ROW 16.05 COL 48.2 COLON-ALIGNED NO-LABEL
     btnUtvalgAlfaKode2 AT ROW 16.05 COL 66.6 WIDGET-ID 14
     fi-cList-AlfaKode2 AT ROW 16.05 COL 80.2 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     btnUtvalgOvandel AT ROW 16.05 COL 98.6
     fi-cListOvandel AT ROW 16.05 COL 112 COLON-ALIGNED NO-LABEL
     btnUtvalgIndType AT ROW 17.24 COL 2.4
     btnUtvalgGaranti AT ROW 17.24 COL 35
     btnUtvalgOnLineLevNr AT ROW 17.24 COL 66.6 WIDGET-ID 18
     btnUtvalgSlitSula AT ROW 17.24 COL 98.6
     fi-cListIndType AT ROW 17.29 COL 16 COLON-ALIGNED NO-LABEL
     fi-cListGaranti AT ROW 17.29 COL 48.2 COLON-ALIGNED NO-LABEL
     fi-cList-OnLineLevNr AT ROW 17.29 COL 80.2 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     fi-cListSlitSula AT ROW 17.29 COL 112 COLON-ALIGNED NO-LABEL
     btnUtvalgKategori AT ROW 18.48 COL 2.4
     btnUtvalgValuta AT ROW 18.48 COL 35
     fi-cListKategori AT ROW 18.52 COL 16 COLON-ALIGNED NO-LABEL
     fi-cListValuta AT ROW 18.52 COL 48.2 COLON-ALIGNED NO-LABEL
     btnUtvalgLast-Sko AT ROW 18.52 COL 98.6
     fi-cListLast-sko AT ROW 18.52 COL 112 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 162.6 BY 30.14.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     btnUtvalgAktivitet AT ROW 19.71 COL 2.4
     btnUtvalgBehandlingskode AT ROW 19.71 COL 35
     fi-cListAktivitet AT ROW 19.76 COL 16 COLON-ALIGNED NO-LABEL
     fi-cListBehandlingskode AT ROW 19.76 COL 48.2 COLON-ALIGNED NO-LABEL
     FI-Strekkode AT ROW 21.48 COL 15.8 COLON-ALIGNED
     FI-Bestillingsnummer AT ROW 21.48 COL 72.8 COLON-ALIGNED
     FI-ERPNr AT ROW 21.48 COL 103.8 COLON-ALIGNED
     btnUtvalgButiker-2 AT ROW 23.14 COL 50.8 WIDGET-ID 8
     cmbTTId AT ROW 23.19 COL 3 NO-LABEL WIDGET-ID 12
     cmbVaremottak AT ROW 23.19 COL 21.2 NO-LABEL WIDGET-ID 2
     fi-cListButiker-2 AT ROW 23.19 COL 71.4 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     fi-dFraVaremottak AT ROW 23.19 COL 91.4 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fi-dTilVaremottak AT ROW 23.19 COL 106 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     btnUtvalgButiker AT ROW 24.24 COL 50.6
     cmbLager AT ROW 24.29 COL 14.6
     fi-cListButiker AT ROW 24.29 COL 71.2 COLON-ALIGNED NO-LABEL
     btnUtvalgPrisprofil AT ROW 25.43 COL 50.6
     cmbTilbud AT ROW 25.48 COL 14.2
     fi-cListPrisprofil AT ROW 25.48 COL 71.2 COLON-ALIGNED NO-LABEL
     btnUtvalgButiker-3 AT ROW 26.52 COL 50.6 WIDGET-ID 26
     fi-cListButiker-3 AT ROW 26.57 COL 71.2 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     cmbKombKampanje AT ROW 26.62 COL 4.6 WIDGET-ID 28
     btnUtvalgBestStat AT ROW 27.67 COL 79.6
     cmbBekreftet AT ROW 27.67 COL 146 COLON-ALIGNED
     cmbBest AT ROW 27.71 COL 11.6
     fi-dFraBest AT ROW 27.71 COL 48.6 COLON-ALIGNED NO-LABEL
     fi-dTilBest AT ROW 27.71 COL 63.2 COLON-ALIGNED NO-LABEL
     fi-cListBestStat AT ROW 27.71 COL 91.2 COLON-ALIGNED NO-LABEL
     cmbBestType AT ROW 27.71 COL 111 COLON-ALIGNED
     Btn_Nullstill AT ROW 29.76 COL 2
     Btn_OK AT ROW 29.76 COL 132
     Btn_Cancel AT ROW 29.76 COL 147.6
     tbVisQuery AT ROW 30 COL 29.6
     RECT-2 AT ROW 10.71 COL 2
     RECT-1 AT ROW 22.91 COL 2
     RECT-3 AT ROW 21 COL 1.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 162.6 BY 30.14
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Utvalg artikler..."
         HEIGHT             = 30.14
         WIDTH              = 162.6
         MAX-HEIGHT         = 30.14
         MAX-WIDTH          = 162.6
         VIRTUAL-HEIGHT     = 30.14
         VIRTUAL-WIDTH      = 162.6
         TOP-ONLY           = yes
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{incl/DevMode.i}
{incl/CustDevMode.i}
{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
ASSIGN 
       btnUtvalgBestStat:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       btnUtvalgButiker:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       btnUtvalgButiker-2:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       btnUtvalgButiker-3:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       btnUtvalgPrisprofil:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR COMBO-BOX cmbBest IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX cmbKombKampanje IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX cmbLager IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX cmbTilbud IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX cmbTTId IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX cmbVaremottak IN FRAME fMain
   ALIGN-L                                                              */
ASSIGN 
       fi-cList-AlfaKode2:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cList-OnLineLevNr:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cList-ravdnr:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListAktivitet:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListAnv-Kod:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListArtButiker:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListArtSlag:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListAvdeling:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListBehandlingskode:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListBestStat:HIDDEN IN FRAME fMain           = TRUE
       fi-cListBestStat:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListButiker:HIDDEN IN FRAME fMain           = TRUE
       fi-cListButiker:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListButiker-2:HIDDEN IN FRAME fMain           = TRUE
       fi-cListButiker-2:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListButiker-3:HIDDEN IN FRAME fMain           = TRUE
       fi-cListButiker-3:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListFarg:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListGaranti:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListHuvGr:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListIndType:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListInnerSula:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListKampanje:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListKategori:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListKlack:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListKombKampanje:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListLast-sko:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListLevBas:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListMaterial:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListMesse:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListOvandel:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListPrisprofil:HIDDEN IN FRAME fMain           = TRUE
       fi-cListPrisprofil:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListProdusent:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListSaSong:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListSlitSula:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListStrType:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListValuta:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListVarebok:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListVaremerke:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-cListVarGr:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       fi-dFraBest:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       fi-dFraVaremottak:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       fi-dTilBest:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       fi-dTilVaremottak:HIDDEN IN FRAME fMain           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Utvalg artikler... */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Utvalg artikler... */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = TRUE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnArtButiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnArtButiker wWin
ON CHOOSE OF btnArtButiker IN FRAME fMain /* Webshop */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Butiker;Butik;ButNamn",
                      "where Butiker.webbutik = true",
                      INPUT-OUTPUT cArtButikerRowIdList,
                      "Butik",
                      INPUT-OUTPUT cArtButikerIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListArtButiker:HANDLE,cArtButikerRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKombKampanje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKombKampanje wWin
ON CHOOSE OF btnKombKampanje IN FRAME fMain /* Komb.kamp. */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "kampanjemixmatch;KampId;KampKlar|Aktiv;KampNavn;KampStartDato;KampSluttDato",
                      "where true",
                      INPUT-OUTPUT cKombKampanjeRowIdList,
                      "KampId",
                      INPUT-OUTPUT cKombKampanjeIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListKombKampanje:HANDLE,cKombKampanjeRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgAktivitet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgAktivitet wWin
ON CHOOSE OF btnUtvalgAktivitet IN FRAME fMain /* Aktivitet... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Aktivitet;AktNr;Beskrivelse",
                      "WHERE true",
                      INPUT-OUTPUT cAktivitetRowIdList,
                      "AktNr",
                      INPUT-OUTPUT cAktivitetIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListAktivitet:HANDLE,cAktivitetRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgAlfaKode2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgAlfaKode2 wWin
ON CHOOSE OF btnUtvalgAlfaKode2 IN FRAME fMain /* Oppr. land... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "AlfaLandKode;AlfaKode2,NumLandKode;Land",
                      "where true, FIRST NumLandKode OF AlfaLandKode NO-LOCK",
                      INPUT-OUTPUT cAlfaKode2-RowIdList,
                      "AlfaKode2",
                      INPUT-OUTPUT cAlfaKode2-IdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cList-AlfaKode2:HANDLE,cAlfaKode2-RowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgAnv-Kod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgAnv-Kod wWin
ON CHOOSE OF btnUtvalgAnv-Kod IN FRAME fMain /* Brukskode... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Anv-Kod;Anv-Id;AnvBeskr|Beskrivelse",
                      "where true",
                      INPUT-OUTPUT cAnv-KodRowIdList,
                      "Anv-Id",
                      INPUT-OUTPUT cAnv-KodIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListAnv-Kod:HANDLE,cAnv-KodRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgArtSlag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgArtSlag wWin
ON CHOOSE OF btnUtvalgArtSlag IN FRAME fMain /* Varetype... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "SysPara;Parameter1|Vareslag|xx;Beskrivelse;!SysHid;!SysGr",
                      "WHERE SysHId = 2 AND SysGr = 7 and ParaNr <= 6",
                      INPUT-OUTPUT cArtSlagRowIdList,
                      "Parameter1",
                      INPUT-OUTPUT cArtSlagIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListArtSlag:HANDLE,cArtSlagRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgAvdeling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgAvdeling wWin
ON CHOOSE OF btnUtvalgAvdeling IN FRAME fMain /* Avdeling... */
DO:

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Avdeling;AvdelingNr;AvdelingNavn",
                      "where true",
                      INPUT-OUTPUT cAvdelingRowIdList,
                      "AvdelingNr",
                      INPUT-OUTPUT cAvdelingIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListAvdeling:HANDLE,cAvdelingRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgBehandlingskode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgBehandlingskode wWin
ON CHOOSE OF btnUtvalgBehandlingskode IN FRAME fMain /* Beh.kode... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Behandlingskode;BehKode;Beskrivelse",
                      "where true",
                      INPUT-OUTPUT cBehandlingskodeRowIdList,
                      "BehKode",
                      INPUT-OUTPUT cBehandlingskodeIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListBehandlingskode:HANDLE,cBehandlingskodeRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgBestStat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgBestStat wWin
ON CHOOSE OF btnUtvalgBestStat IN FRAME fMain /* Best.status... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "SysPara;Paranr|Best.status;Beskrivelse;!SysHid;!SysGr",
                      "WHERE SysHId = 5 AND SysGr = 2",
                      INPUT-OUTPUT cBestStatRowIdList,
                      "Paranr",
                      INPUT-OUTPUT cBestStatIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListBestStat:HANDLE,cBestStatRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgButiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgButiker wWin
ON CHOOSE OF btnUtvalgButiker IN FRAME fMain /* i Butikk... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Butiker;Butik;ButNamn",
                      "where true",
                      INPUT-OUTPUT cButikerRowIdList,
                      "Butik",
                      INPUT-OUTPUT cButikerIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListButiker:HANDLE,cButikerRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgButiker-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgButiker-2 wWin
ON CHOOSE OF btnUtvalgButiker-2 IN FRAME fMain /* i Butikk... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Butiker;Butik;ButNamn",
                      "where true",
                      INPUT-OUTPUT cButikerRowIdList-2,
                      "Butik",
                      INPUT-OUTPUT cButikerIdList-2,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListButiker-2:HANDLE,cButikerRowIdList-2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgButiker-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgButiker-3 wWin
ON CHOOSE OF btnUtvalgButiker-3 IN FRAME fMain /* i Butikk... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Butiker;Butik;ButNamn",
                      "where true",
                      INPUT-OUTPUT cButikerRowIdList-3,
                      "Butik",
                      INPUT-OUTPUT cButikerIdList-3,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListButiker-3:HANDLE,cButikerRowIdList-3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgFarg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgFarg wWin
ON CHOOSE OF btnUtvalgFarg IN FRAME fMain /* Farger... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Farg;Farg;FarBeskr",
                      "where true",
                      INPUT-OUTPUT cFargRowIdList,
                      "Farg",
                      INPUT-OUTPUT cFargIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListFarg:HANDLE,cFargRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgGaranti
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgGaranti wWin
ON CHOOSE OF btnUtvalgGaranti IN FRAME fMain /* Garanti... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Garanti;garantikl;garantitekst",
                      "where true",
                      INPUT-OUTPUT cGarantiRowIdList,
                      "garantikl",
                      INPUT-OUTPUT cGarantiIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListGaranti:HANDLE,cGarantiRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgHuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgHuvGr wWin
ON CHOOSE OF btnUtvalgHuvGr IN FRAME fMain /* Hovedgr... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "HuvGr;Hg;HgBeskr,Avdeling;AvdelingNr;AvdelingNavn",
                      "where true, first Avdeling OF HuvGr NO-LOCK " + 
                         (IF cAvdelingRowIdList NE "" THEN
                            "WHERE CAN-DO('" + cAvdelingRowIdList + "',STRING(ROWID(Avdeling)))"
                          ELSE ""),
                      INPUT-OUTPUT cHuvGrRowIdList,
                      "Hg",
                      INPUT-OUTPUT cHuvGrIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListHuvGr:HANDLE,cHuvGrRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgIndType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgIndType wWin
ON CHOOSE OF btnUtvalgIndType IN FRAME fMain /* Individtype... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "IndType;IndividType;IndividBeskr",
                      "WHERE true",
                      INPUT-OUTPUT cIndTypeRowIdList,
                      "IndividType",
                      INPUT-OUTPUT cIndTypeIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListIndType:HANDLE,cIndTypeRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgInnerSula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgInnerSula wWin
ON CHOOSE OF btnUtvalgInnerSula IN FRAME fMain /* Innersåle... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "InnerSula;Inner-Id|Innersålenr;InnerBeskr|Beskrivelse",
                      "where true",
                      INPUT-OUTPUT cInnerSulaRowIdList,
                      "Inner-Id",
                      INPUT-OUTPUT cInnerSulaIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  IF bOK THEN setSelectionFillIn(fi-cListInnerSula:HANDLE,cInnerSulaRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgKampanje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgKampanje wWin
ON CHOOSE OF btnUtvalgKampanje IN FRAME fMain /* Tilbud/prisendr */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "KampanjeHode;KampanjeId;Beskrivelse",
                      "where true",
                      INPUT-OUTPUT cKampanjeRowIdList,
                      "KampanjeId",
                      INPUT-OUTPUT cKampanjeIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListKampanje:HANDLE,cKampanjeRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgKategori
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgKategori wWin
ON CHOOSE OF btnUtvalgKategori IN FRAME fMain /* Kategori... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Kategori;KatNr;Beskrivelse",
                      "WHERE true",
                      INPUT-OUTPUT cKategoriRowIdList,
                      "KatNr",
                      INPUT-OUTPUT cKategoriIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListKategori:HANDLE,cKategoriRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgKlack
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgKlack wWin
ON CHOOSE OF btnUtvalgKlack IN FRAME fMain /* Hæl... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Klack;klack-id|Hælnr;beskrivning|Beskrivelse",
                      "where true",
                      INPUT-OUTPUT cKlackRowIdList,
                      "klack-id",
                      INPUT-OUTPUT cKlackIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListKlack:HANDLE,cKlackRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgLast-Sko
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgLast-Sko wWin
ON CHOOSE OF btnUtvalgLast-Sko IN FRAME fMain /* Læst... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Last-Sko;Last-Id;LastBeskr|Beskrivelse",
                      "where true",
                      INPUT-OUTPUT cLast-SkoRowIdList,
                      "Last-Id",
                      INPUT-OUTPUT cLast-SkoIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListLast-Sko:HANDLE,cLast-SkoRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgLevBas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgLevBas wWin
ON CHOOSE OF btnUtvalgLevBas IN FRAME fMain /* Leverandør... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "LevBas;levnr;levnamn;KjedeAvtale|Kjedeavtale|J/N",
                      "where true",
                      INPUT-OUTPUT cLevBasRowIdList,
                      "Levnr",
                      INPUT-OUTPUT cLevBasIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN setSelectionFillIn(fi-cListLevBas:HANDLE,cLevBasRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgMaterial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgMaterial wWin
ON CHOOSE OF btnUtvalgMaterial IN FRAME fMain /* Material... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Material;MatKod;MatBeskr",
                      "where true",
                      INPUT-OUTPUT cMaterialRowIdList,
                      "MatKod",
                      INPUT-OUTPUT cMaterialIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListMaterial:HANDLE,cMaterialRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgMesse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgMesse wWin
ON CHOOSE OF btnUtvalgMesse IN FRAME fMain /* Messe... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Messe;MesseNr;MesseBeskrivelse;FraDato;TilDato;MobilTlf;Telefon;Adresse1",
                      "where true",
                      INPUT-OUTPUT cMesseRowIdList,
                      "MesseNr",
                      INPUT-OUTPUT cMesseIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListMesse:HANDLE,cMesseRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgOnLineLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgOnLineLevNr wWin
ON CHOOSE OF btnUtvalgOnLineLevNr IN FRAME fMain /* Online levnr */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.

  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "OnLineLeverandor;OnLineLevNr;OnLineLevBeskr",
                      "where true",
                      INPUT-OUTPUT cOnLineLevNr-RowIdList,
                      "OnLineLevNr",
                      INPUT-OUTPUT cOnLineLevNr-IdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cList-OnLineLevNr:HANDLE,cOnLineLevNr-RowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgOvandel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgOvandel wWin
ON CHOOSE OF btnUtvalgOvandel IN FRAME fMain /* Innerfor... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Ovandel;Ov-Id|Innerfornr;OvBeskr|Beskrivelse",
                      "where true",
                      INPUT-OUTPUT cOvandelRowIdList,
                      "Ov-Id",
                      INPUT-OUTPUT cOvandelIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListOvandel:HANDLE,cOvandelRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgPrisprofil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgPrisprofil wWin
ON CHOOSE OF btnUtvalgPrisprofil IN FRAME fMain /* for Profil... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Prisprofil;ProfilNr;KortNavn;Beskrivelse",
                      "where true",
                      INPUT-OUTPUT cPrisprofilRowIdList,
                      "ProfilNr",
                      INPUT-OUTPUT cPrisprofilIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListPrisprofil:HANDLE,cPrisprofilRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgProdusent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgProdusent wWin
ON CHOOSE OF btnUtvalgProdusent IN FRAME fMain /* Produsent... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Produsent;ProdNr;Beskrivelse;Adresse1;Land",
                      "where true",
                      INPUT-OUTPUT cProdusentRowIdList,
                      "ProdNr",
                      INPUT-OUTPUT cProdusentIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN setSelectionFillIn(fi-cListProdusent:HANDLE,cProdusentRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgRAvdNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgRAvdNr wWin
ON CHOOSE OF btnUtvalgRAvdNr IN FRAME fMain /* Vareområde... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Regnskapsavdeling;RAvdNr;RAvdBeskrivelse|Beskrivelse",
                      "where true",
                      INPUT-OUTPUT cRAvdNr-RowIdList,
                      "RAvdNr",
                      INPUT-OUTPUT cRAvdNr-IdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cList-ravdnr:HANDLE,cRAvdNr-RowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgSaSong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgSaSong wWin
ON CHOOSE OF btnUtvalgSaSong IN FRAME fMain /* Sesong... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "SaSong;Sasong;SasBeskr",
                      "where true",
                      INPUT-OUTPUT cSaSongRowIdList,
                      "Sasong",
                      INPUT-OUTPUT cSaSongIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN setSelectionFillIn(fi-cListSaSong:HANDLE,cSaSongRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgSlitSula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgSlitSula wWin
ON CHOOSE OF btnUtvalgSlitSula IN FRAME fMain /* Slitesåle... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "SlitSula;Slit-Id;SlitBeskr|Beskrivelse",
                      "where true",
                      INPUT-OUTPUT cSlitSulaRowIdList,
                      "Slit-Id",
                      INPUT-OUTPUT cSlitSulaIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListSlitSula:HANDLE,cSlitSulaRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgStrType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgStrType wWin
ON CHOOSE OF btnUtvalgStrType IN FRAME fMain /* Str.type... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "StrType;StrTypeID;Beskrivelse",
                      "where true",
                      INPUT-OUTPUT cStrTypeRowIdList,
                      "StrTypeId",
                      INPUT-OUTPUT cStrTypeIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListStrType:HANDLE,cStrTypeRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgValuta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgValuta wWin
ON CHOOSE OF btnUtvalgValuta IN FRAME fMain /* Valuta... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Valuta;ValKod;ValNavn;ValLand;ValKurs;ValDatum",
                      "where true",
                      INPUT-OUTPUT cValutaRowIdList,
                      "ValKod",
                      INPUT-OUTPUT cValutaIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListValuta:HANDLE,cValutaRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgVarebok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgVarebok wWin
ON CHOOSE OF btnUtvalgVarebok IN FRAME fMain /* Varebok... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "VareBokHode;VareBokNr;VareBokBeskrivelse;MesseNr,Messe;MesseBeskrivelse",
                      "where true,first Messe OF vareBokHode",
                      INPUT-OUTPUT cVareBokRowIdList,
                      "VareBokNr",
                      INPUT-OUTPUT cVareBokIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListVareBok:HANDLE,cVareBokRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgVaremerke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgVaremerke wWin
ON CHOOSE OF btnUtvalgVaremerke IN FRAME fMain /* Varemerke... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Varemerke;VMid;Beskrivelse",
                      "where true",
                      INPUT-OUTPUT cVaremerkeRowIdList,
                      "VMid",
                      INPUT-OUTPUT cVaremerkeIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN setSelectionFillIn(fi-cListVaremerke:HANDLE,cVaremerkeRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgVarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgVarGr wWin
ON CHOOSE OF btnUtvalgVarGr IN FRAME fMain /* Varegr... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "VarGr;Vg;VgBeskr,HuvGr;Hg;HgBeskr",
                      "where true, first HuvGr OF VarGr NO-LOCK " + 
                         (IF cHuvGrRowIdList NE "" THEN
                            "WHERE CAN-DO('" + cHuvGrRowIdList + "',STRING(ROWID(HuvGr)))"
                          ELSE ""),
                      INPUT-OUTPUT cVarGrRowIdList,
                      "Vg",
                      INPUT-OUTPUT cVarGrIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOK THEN setSelectionFillIn(fi-cListVarGr:HANDLE,cVarGrRowIdList).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel wWin
ON CHOOSE OF Btn_Cancel IN FRAME fMain /* Avbryt */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Nullstill
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Nullstill wWin
ON CHOOSE OF Btn_Nullstill IN FRAME fMain /* Nullstill kriterier */
DO:
  RUN NullstillKrit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK wWin
ON CHOOSE OF Btn_OK IN FRAME fMain /* OK */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = TRUE.
  RUN StartSok IN h_fartsokfilter.
  RUN StartUtvalg.
  gbOk = "Ok". 
  RUN ApplyEntryToBrowse IN hParent.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbBest wWin
ON VALUE-CHANGED OF cmbBest IN FRAME fMain /* Bestilling */
DO:
  IF cmbBest:SCREEN-VALUE = cComboDefault THEN
    ASSIGN fi-dFraBest:HIDDEN       = TRUE
           fi-dTilBest:HIDDEN       = TRUE
           btnUtvalgBestStat:HIDDEN = TRUE
           fi-cListBestStat:HIDDEN  = TRUE
           cmbBestType:HIDDEN       = TRUE
           cmbBekreftet:HIDDEN      = TRUE
      .
  ELSE
    ASSIGN fi-dFraBest:HIDDEN       = FALSE
           fi-dTilBest:HIDDEN       = FALSE
           btnUtvalgBestStat:HIDDEN = FALSE
           fi-cListBestStat:HIDDEN  = FALSE
           cmbBestType:HIDDEN       = FALSE
           fi-dFraBest:SCREEN-VALUE = STRING(?)
           fi-dTilBest:SCREEN-VALUE = STRING(?)
           fi-cListBestStat:SCREEN-VALUE = cComboDefault
           cmbBestType:SCREEN-VALUE = "0"
           cmbBekreftet:HIDDEN      = FALSE
           cmbBekreftet:SCREEN-VALUE = "0"
           .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbKombKampanje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbKombKampanje wWin
ON VALUE-CHANGED OF cmbKombKampanje IN FRAME fMain /* Komb.kampanje */
DO:
  IF SELF:SCREEN-VALUE = cComboDefault THEN 
    ASSIGN fi-cListButiker-3:SCREEN-VALUE = ""
           cButikerRowIdList-3            = ""
           cButikerIdList-3               = ""
           fi-cListButiker-3:HIDDEN       = TRUE
           btnUtvalgButiker-3:HIDDEN      = TRUE
           . 
  ELSE
    ASSIGN fi-cListButiker-3:HIDDEN       = FALSE
           btnUtvalgButiker-3:HIDDEN      = FALSE
           . 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbLager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbLager wWin
ON VALUE-CHANGED OF cmbLager IN FRAME fMain /* Lager */
DO:
  IF SELF:SCREEN-VALUE = cComboDefault THEN 
    ASSIGN fi-cListButiker:SCREEN-VALUE = ""
           cButikerRowIdList            = ""
           cButikerIdList               = ""
           fi-cListButiker:HIDDEN       = TRUE
           btnUtvalgButiker:HIDDEN      = TRUE
           . 
  ELSE
    ASSIGN fi-cListButiker:HIDDEN       = FALSE
           btnUtvalgButiker:HIDDEN      = FALSE
           . 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbTilbud
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbTilbud wWin
ON VALUE-CHANGED OF cmbTilbud IN FRAME fMain /* Tilbud */
DO:
  IF SELF:SCREEN-VALUE = cComboDefault THEN 
    ASSIGN fi-cListPrisprofil:SCREEN-VALUE = ""
           cPrisprofilRowIdList            = ""
           cPrisprofilIdList               = ""
           fi-cListPrisprofil:HIDDEN       = TRUE
           btnUtvalgPrisprofil:HIDDEN      = TRUE
           . 
  ELSE
    ASSIGN fi-cListPrisprofil:HIDDEN       = FALSE
           btnUtvalgPrisprofil:HIDDEN      = FALSE
           .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbTTId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbTTId wWin
ON VALUE-CHANGED OF cmbTTId IN FRAME fMain
DO:
  IF cmbVaremottak:SCREEN-VALUE = cComboDefault THEN
    ASSIGN fi-dFraVaremottak:HIDDEN  = TRUE
           fi-dTilVaremottak:HIDDEN  = TRUE
           btnUtvalgButiker-2:HIDDEN = TRUE
           fi-cListButiker-2:HIDDEN  = TRUE
           .
  ELSE
    ASSIGN fi-dFraVaremottak:HIDDEN  = FALSE
           fi-dTilVaremottak:HIDDEN  = FALSE
           btnUtvalgButiker-2:HIDDEN = FALSE
           fi-cListButiker-2:HIDDEN  = FALSE
           fi-dFraVaremottak:SCREEN-VALUE = STRING(?)
           fi-dTilVaremottak:SCREEN-VALUE = STRING(?)
           fi-cListButiker-2:SCREEN-VALUE = cComboDefault
           .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbVaremottak
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbVaremottak wWin
ON VALUE-CHANGED OF cmbVaremottak IN FRAME fMain
DO:
  IF cmbVaremottak:SCREEN-VALUE = cComboDefault THEN
    ASSIGN fi-dFraVaremottak:HIDDEN  = TRUE
           fi-dTilVaremottak:HIDDEN  = TRUE
           btnUtvalgButiker-2:HIDDEN = TRUE
           fi-cListButiker-2:HIDDEN  = TRUE
           .
  ELSE
    ASSIGN fi-dFraVaremottak:HIDDEN  = FALSE
           fi-dTilVaremottak:HIDDEN  = FALSE
           btnUtvalgButiker-2:HIDDEN = FALSE
           fi-cListButiker-2:HIDDEN  = FALSE
           fi-dFraVaremottak:SCREEN-VALUE = STRING(?)
           fi-dTilVaremottak:SCREEN-VALUE = STRING(?)
           fi-cListButiker-2:SCREEN-VALUE = cComboDefault
           .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbVisQuery
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbVisQuery wWin
ON VALUE-CHANGED OF tbVisQuery IN FRAME fMain /* Vis spørring */
DO:
  ASSIGN tbVisQuery.
  DYNAMIC-FUNCTION("setVisQuery" IN h_QueryObject,tbVisQuery).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}
/* {lng.i &SDO = "SDO"} */
    {lng.i}
ON 'return' OF FRAME {&FRAME-NAME} ANYWHERE
  APPLY "choose" TO Btn_OK.

RETURN gbOk.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'sdo/dartbas.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedartbasOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dartbas ).
       RUN repositionObject IN h_dartbas ( 24.19 , 122.00 ) NO-ERROR.
       /* Size in AB:  ( 1.67 , 11.00 ) */

       RUN constructObject (
             INPUT  'prg/fartutvalgfilter.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_fartsokfilter ).
       RUN repositionObject IN h_fartsokfilter ( 1.00 , 1.60 ) NO-ERROR.
       /* Size in AB:  ( 9.52 , 162.00 ) */

       /* Links to SmartFrame h_fartsokfilter. */
       RUN addLink ( h_dartbas , 'Data':U , h_fartsokfilter ).
       RUN addLink ( h_fartsokfilter , 'Update':U , h_dartbas ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_fartsokfilter ,
             btnUtvalgAvdeling:HANDLE IN FRAME fMain , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fi-cListKampanje fi-cListAvdeling fi-cListLevBas fi-cListMesse 
          fi-cListMaterial fi-cListKombKampanje fi-cListHuvGr fi-cListProdusent 
          fi-cListVarebok fi-cListAnv-Kod fi-cListArtButiker fi-cListVarGr 
          fi-cListVaremerke fi-cListKlack fi-cListStrType fi-cListSaSong 
          fi-cList-ravdnr fi-cListInnerSula fi-cListArtSlag fi-cListFarg 
          fi-cList-AlfaKode2 fi-cListOvandel fi-cListIndType fi-cListGaranti 
          fi-cList-OnLineLevNr fi-cListSlitSula fi-cListKategori fi-cListValuta 
          fi-cListLast-sko fi-cListAktivitet fi-cListBehandlingskode 
          FI-Strekkode FI-Bestillingsnummer FI-ERPNr cmbTTId cmbVaremottak 
          fi-cListButiker-2 fi-dFraVaremottak fi-dTilVaremottak cmbLager 
          fi-cListButiker cmbTilbud fi-cListPrisprofil fi-cListButiker-3 
          cmbKombKampanje cmbBekreftet cmbBest fi-dFraBest fi-dTilBest 
          fi-cListBestStat cmbBestType tbVisQuery 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-2 RECT-1 RECT-3 btnUtvalgAvdeling btnUtvalgLevBas btnUtvalgMesse 
         btnUtvalgMaterial btnUtvalgKampanje fi-cListKampanje fi-cListAvdeling 
         fi-cListLevBas fi-cListMesse fi-cListMaterial btnUtvalgHuvGr 
         btnUtvalgProdusent btnUtvalgVarebok btnUtvalgAnv-Kod btnKombKampanje 
         fi-cListKombKampanje fi-cListHuvGr fi-cListProdusent fi-cListVarebok 
         fi-cListAnv-Kod btnUtvalgVarGr btnUtvalgVaremerke btnUtvalgKlack 
         btnArtButiker fi-cListArtButiker fi-cListVarGr fi-cListVaremerke 
         fi-cListKlack btnUtvalgStrType fi-cListStrType btnUtvalgSaSong 
         fi-cListSaSong btnUtvalgRAvdNr fi-cList-ravdnr btnUtvalgInnerSula 
         fi-cListInnerSula btnUtvalgArtSlag fi-cListArtSlag btnUtvalgFarg 
         fi-cListFarg btnUtvalgAlfaKode2 fi-cList-AlfaKode2 btnUtvalgOvandel 
         fi-cListOvandel btnUtvalgIndType btnUtvalgGaranti btnUtvalgOnLineLevNr 
         btnUtvalgSlitSula fi-cListIndType fi-cListGaranti fi-cList-OnLineLevNr 
         fi-cListSlitSula btnUtvalgKategori btnUtvalgValuta fi-cListKategori 
         fi-cListValuta btnUtvalgLast-Sko fi-cListLast-sko btnUtvalgAktivitet 
         btnUtvalgBehandlingskode fi-cListAktivitet fi-cListBehandlingskode 
         FI-Strekkode FI-Bestillingsnummer FI-ERPNr btnUtvalgButiker-2 cmbTTId 
         cmbVaremottak fi-cListButiker-2 fi-dFraVaremottak fi-dTilVaremottak 
         btnUtvalgButiker cmbLager fi-cListButiker btnUtvalgPrisprofil 
         cmbTilbud fi-cListPrisprofil btnUtvalgButiker-3 fi-cListButiker-3 
         cmbKombKampanje btnUtvalgBestStat cmbBekreftet cmbBest fi-dFraBest 
         fi-dTilBest fi-cListBestStat cmbBestType Btn_Nullstill Btn_OK 
         Btn_Cancel tbVisQuery 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSelectorAttributes wWin 
PROCEDURE getSelectorAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM  ihSelectorSource AS HANDLE NO-UNDO.
DEF INPUT PARAM  ihSelectorTarget AS HANDLE NO-UNDO.
DEF INPUT PARAM  icDeSelRowidList AS CHAR NO-UNDO.
DEF OUTPUT PARAM oiReturn         AS INT NO-UNDO.

DEF VAR cTmpHuvGrList    AS CHAR NO-UNDO.
DEF VAR cTmpVarGrList    AS CHAR NO-UNDO.

cCurrSelectBuffer = ihSelectorSource:QUERY:GET-BUFFER-HANDLE(1):NAME.

IF DYNAMIC-FUNCTION("getAttribute",ihSelectorSource,"lastrowid") NE "" THEN
  iSelectorSourcCount = INT(DYNAMIC-FUNCTION("getAttribute",ihSelectorSource,"totalcount")).
ELSE 
  iSelectorSourcCount = ?.

/* Håndtering av avhengighet Avdeling/HuvGr: */
IF cCurrSelectBuffer = "HuvGr" THEN DO:
  cHuvGrAvdelingList = "".
  ihSelectorTarget:QUERY:GET-FIRST().
  REPEAT WHILE NOT ihSelectorTarget:QUERY:QUERY-OFF-END:
    cHuvGrAvdelingList = cHuvGrAvdelingList + STRING(ihSelectorTarget:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("AvdelingNr"):BUFFER-VALUE) + ",".
    ihSelectorTarget:QUERY:GET-NEXT().
  END.  
  cHuvGrAvdelingList = TRIM(cHuvGrAvdelingList,",").

  IF cVarGrHuvGrList NE "" THEN DO:
    DO ix = 1 TO NUM-ENTRIES(cVarGrRowIdList):
      bOK = ihSelectorTarget:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE Hg = " + ENTRY(ix,cVarGrHuvGrList)) NO-ERROR.
      IF bOk THEN
        cTmpVarGrList = cTmpVarGrList + ENTRY(ix,cVarGrRowIdList) + ",".
    END.
    IF NUM-ENTRIES(TRIM(cTmpVarGrList,",")) NE NUM-ENTRIES(cVarGrRowIdList) THEN 
      oiReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Fjerne valgte vargrupper som ikke lenger passer med utvalg på hovedgrupper?","Valg","").
    IF oiReturn = 1 THEN DO:
      cVarGrRowIdList = TRIM(cTmpVarGrList,",").  
      setSelectionFillIn(fi-cListVarGr:HANDLE IN FRAME {&FRAME-NAME},cVarGrRowIdList).
    END.
  END.
END.
ELSE IF cCurrSelectBuffer = "Avdeling" AND cHuvGrAvdelingList NE "" THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cHuvGrRowIdList):
    bOK = ihSelectorTarget:QUERY:GET-BUFFER-HANDLE(1):FIND-FIRST("WHERE AvdelingNr = " + ENTRY(ix,cHuvGrAvdelingList)) NO-ERROR.
    IF bOk THEN
      cTmpHuvGrList = cTmpHuvGrList + ENTRY(ix,cHuvGrRowIdList) + ",".
  END.
  IF NUM-ENTRIES(TRIM(cTmpHuvGrList,",")) NE NUM-ENTRIES(cHuvGrRowIdList) THEN 
    oiReturn = DYNAMIC-FUNCTION("DoMessage",0,1,"Fjerne valgte hovedgrupper som ikke lenger passer med utvalg på avdeling?","Valg","").
  IF oiReturn = 1 THEN DO:
    cHuvGrRowIdList = TRIM(cTmpHuvGrList,",").  
    setSelectionFillIn(fi-cListHuvGr:HANDLE IN FRAME {&FRAME-NAME},cHuvGrRowIdList).
    /* Nullstiller valg av varegrupper */
    cVarGrRowIdList = "".  
    fi-cListVarGr:SCREEN-VALUE = cComboDefault.
  END.
END.

/* Håndtering av avhengighet HuvGr/VarGr: */
ELSE IF cCurrSelectBuffer = "VarGr" THEN DO:
  cVarGrHuvGrList = "".
  ihSelectorTarget:QUERY:GET-FIRST().
  REPEAT WHILE NOT ihSelectorTarget:QUERY:QUERY-OFF-END:
    cVarGrHuvGrList = cVarGrHuvGrList + STRING(ihSelectorTarget:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Hg"):BUFFER-VALUE) + ",".
    ihSelectorTarget:QUERY:GET-NEXT().
  END.  
  cVarGrHuvGrList = TRIM(cVarGrHuvGrList,",").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
/* Code placed here will execute PRIOR to standard behavior. */

ASSIGN THIS-PROCEDURE:CURRENT-WINDOW:X = hParent:CURRENT-WINDOW:X + 50
       THIS-PROCEDURE:CURRENT-WINDOW:Y = hParent:CURRENT-WINDOW:Y + 50
       .

RUN SUPER.
DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

/* Code placed here will execute AFTER standard behavior.    */
 SUBSCRIBE TO 'SokSdo' IN h_fartsokfilter.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cInitDefaults = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                        "WHERE SysHId = 1 and SysGr = 100 and ParaNr = 1","Parameter1,Parameter2")
         cViewDefaults = DYNAMIC-FUNCTION("getFieldList","SysPara;Parameter1",
                        "WHERE SysHId = 2 and SysGr = 5 BY ParaNr")
         cComboDefault                  = ENTRY(1,cInitDefaults,"|")
         cDelimDefault                  = IF ENTRY(2,cInitDefaults,"|") NE "" THEN ENTRY(2,cInitDefaults,"|") ELSE "av"
         cmbLager:LIST-ITEMS            = cComboDefault + ",Har lager,Har ikke lager,Har negativt lager"
         cmbKombKampanje:LIST-ITEMS     = cComboDefault + ",Aktiv på komb.kampanje,Ikke aktiv på komb.kampanje"
         cmbTilbud:LIST-ITEMS           = cComboDefault + ",Aktiv på tilbud,Ikke aktiv på tilbud"
         /*cmbSalg:LIST-ITEMS             = cComboDefault + ",Salg i periode:,IKKE salg i periode:"*/
         cmbTTId:LIST-ITEM-PAIRS        = "Varesalg,1,Varemottak,5,Brekkasje,2,Kundereklamasjon,3,Lagerreklamasjon,4,Overføring,6,Lagerjustering,7,Nedskrivning,8,Svinn,9,Retur,10,Internt forbruk,11"
         cmbVaremottak:LIST-ITEMS       = cComboDefault + ",Transaksjoner i periode:,IKKE transaksjoner i periode:"
         cmbBest:LIST-ITEMS             = cComboDefault + ",Planlagt lev. i periode:,IKKE planlagt lev. i periode:" + 
                                                          ",Varemottak i periode:,IKKE varemottak i periode:" +
                                                          ",Bestilling i periode:,IKKE bestilling i periode:"
         cmbBestType:DELIMITER          = "|"
         cmbBestType:LIST-ITEM-PAIRS    = cComboDefault + "|0|" + DYNAMIC-FUNCTION("getFieldList","Syspara;Beskrivelse;parameter1","WHERE SysHId = 5 AND Sysgr = 5")
         cmbBekreftet:DELIMITER         = "|"
         cmbBekreftet:LIST-ITEM-PAIRS   = cComboDefault + "|0|" + (IF wCurrLng = "SE" THEN "Ja|1|Nej|2" ELSE "Ja|1|Nei|2")
         /*
         fi-dFraSalg:HIDDEN             = TRUE
         fi-dTilSalg:HIDDEN             = TRUE
         */
         btnArtButiker:SENSITIVE = CAN-FIND(FIRST artbut)
         fi-dFraBest:HIDDEN             = TRUE
         fi-dTilBest:HIDDEN             = TRUE
         fi-dFraVaremottak:HIDDEN       = TRUE
         fi-dTilVaremottak:HIDDEN       = TRUE
         .
  RUN NullstillKrit.

  DO ix = 1 TO NUM-ENTRIES(cViewDefaults,"|"):
    IF ENTRY(ix,cViewDefaults,"|") = "0" THEN
      CASE ix:
        WHEN 1 THEN 
          ASSIGN fi-cListKlack:HIDDEN           = TRUE
                 btnUtvalgKlack:HIDDEN          = TRUE.
        WHEN 2 THEN 
          ASSIGN fi-cListInnerSula:HIDDEN       = TRUE
                 btnUtvalgInnerSula:HIDDEN      = TRUE.
        WHEN 3 THEN 
          ASSIGN fi-cListOvandel:HIDDEN         = TRUE
                 btnUtvalgOvandel:HIDDEN        = TRUE.
        WHEN 4 THEN 
          ASSIGN fi-cListSlitSula:HIDDEN        = TRUE
                 btnUtvalgSlitSula:HIDDEN       = TRUE.
        WHEN 5 THEN 
          ASSIGN fi-cListLast-Sko:HIDDEN        = TRUE
                 btnUtvalgLast-Sko:HIDDEN       = TRUE.
        WHEN 6 THEN 
          ASSIGN fi-cListAnv-Kod:HIDDEN         = TRUE
                 btnUtvalgAnv-Kod:HIDDEN        = TRUE.
/*         WHEN 7 THEN                                     */
/*           ASSIGN fi-cListProv:HIDDEN            = TRUE  */
/*                  btnUtvalgProv:HIDDEN           = TRUE. */
/*         WHEN 8 THEN                                     */
/*           ASSIGN fi-cListRabatt:HIDDEN          = TRUE  */
/*                  btnUtvalgRabatt:HIDDEN         = TRUE. */
        WHEN 9 THEN 
          ASSIGN fi-cListBehandlingskode:HIDDEN = TRUE
                 btnUtvalgBehandlingskode:HIDDEN = TRUE.
      END CASE.
  END.
  
END.
/*  RUN SwitchLng. */
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop wWin 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

DYNAMIC-FUNCTION("ChooseBeskr" IN h_fartsokfilter).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NullstillKrit wWin 
PROCEDURE NullstillKrit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
                      INPUT-OUTPUT cRAvdNr-RowIdList,
                      "RAvdNr",
                      INPUT-OUTPUT cRAvdNr-IdList,
  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN 
         fi-cListAvdeling:SCREEN-VALUE   = cComboDefault
         fi-cListHuvGr:SCREEN-VALUE      = cComboDefault
         fi-cListVarGr:SCREEN-VALUE      = cComboDefault
         fi-cListBehandlingskode:SCREEN-VALUE = cComboDefault
         fi-cListStrType:SCREEN-VALUE    = cComboDefault
         fi-cListArtSlag:SCREEN-VALUE    = cComboDefault
         fi-cListIndType:SCREEN-VALUE    = cComboDefault
         fi-cListKategori:SCREEN-VALUE   = cComboDefault

         fi-cListLevBas:SCREEN-VALUE     = cComboDefault
         fi-cListProdusent:SCREEN-VALUE  = cComboDefault
         fi-cListVaremerke:SCREEN-VALUE  = cComboDefault
         fi-cListSaSong:SCREEN-VALUE     = cComboDefault
         fi-cListFarg:SCREEN-VALUE       = cComboDefault
         fi-cListGaranti:SCREEN-VALUE    = cComboDefault
         fi-cListValuta:SCREEN-VALUE     = cComboDefault

         fi-cListMaterial:SCREEN-VALUE   = cComboDefault
         fi-cListKlack:SCREEN-VALUE      = cComboDefault
         fi-cListInnerSula:SCREEN-VALUE  = cComboDefault
         fi-cListOvandel:SCREEN-VALUE    = cComboDefault
         fi-cListSlitSula:SCREEN-VALUE   = cComboDefault
         fi-cListLast-Sko:SCREEN-VALUE   = cComboDefault
         fi-cListAnv-Kod:SCREEN-VALUE    = cComboDefault

         fi-cListButiker:SCREEN-VALUE    = "" /* cComboDefault */
         fi-cListPrisprofil:SCREEN-VALUE = "" /* cComboDefault */
         fi-cListBestStat:SCREEN-VALUE   = cComboDefault 

         fi-cListAktivitet:SCREEN-VALUE  = cComboDefault 
         fi-cListVarebok:SCREEN-VALUE    = cComboDefault 
         fi-cListMesse:SCREEN-VALUE      = cComboDefault 
         fi-cListKampanje:SCREEN-VALUE   = cComboDefault 
         fi-cList-ravdnr:SCREEN-VALUE    = cComboDefault 
         fi-cList-AlfaKode2:SCREEN-VALUE = cComboDefault 
         fi-cList-OnLineLevNr:SCREEN-VALUE = cComboDefault 
         fi-cListKombKampanje:SCREEN-VALUE = cComboDefault

         fi-cList-ravdnr                 = ""
         fi-cList-AlfaKode2              = ""
         fi-cList-OnLineLevNr            = ""
         cAvdelingRowIdList              = ""
         cHuvGrRowIdList                 = ""
         cHuvGrAvdelingList              = ""
         cVarGrRowIdList                 = ""
         cVarGrHuvGrList                 = ""
         cBehandlingskodeRowIdList       = ""
         cStrTypeRowIdList               = ""
         cArtSlagRowIdList               = ""
         cIndTypeRowIdList               = ""
         cKategoriRowIdList              = ""
         cRAvdNr-RowIdList               = ""
         cAlfaKode2-RowIdList            = ""
         cOnLineLevNr-RowIdList          = ""
                                  
         cLevBasRowIdList                = ""
         cProdusentRowIdList             = ""
         cVaremerkeRowIdList             = ""
         cSaSongRowIdList                = ""
         cFargRowIdList                  = ""
         cGarantiRowIdList               = ""
         cValutaRowIdList                = ""
                                        
         cMaterialRowIdList              = ""
         cKlackRowIdList                 = ""
         cInnerSulaRowIdList             = ""
         cOvandelRowIdList               = ""
         cSlitSulaRowIdList              = ""
         cLast-SkoRowIdList              = ""
         cAnv-KodRowIdList               = ""
                                         
         cButikerRowIdList               = ""
         cPrisprofilRowIdList            = ""
         cBestStatRowIdList              = ""

         cAktivitetRowIdList             = ""
         cVarebokRowIdList               = ""
         cMesseRowIdList                 = ""
         cKampanjeRowIdList              = ""
         cKombKampanjeRowIdList          = ""

         cAvdelingIdList                 = ""
         cHuvGrIdList                    = ""
         cVarGrIdList                    = ""
         cBehandlingskodeIdList          = ""
         cStrTypeIdList                  = ""
         cArtSlagIdList                  = ""
         cIndTypeIdList                  = ""
         cKategoriIdList                 = ""
                                        
         cLevBasIdList                   = ""
         cProdusentIdList                = ""
         cVaremerkeIdList                = ""
         cSaSongIdList                   = ""
         cFargIdList                     = ""
         cGarantiIdList                  = ""
         cValutaIdList                   = ""
                                         
         cMaterialIdList                 = ""
         cKlackIdList                    = ""
         cInnerSulaIdList                = ""
         cOvandelIdList                  = ""
         cSlitSulaIdList                 = ""
         cLast-SkoIdList                 = ""
         cRAvdNr-IdList                  = ""
         cAlfaKode2-IdList               = ""
         cOnLineLevNr-IdList             = ""
         cAnv-KodIdList                  = ""

         cButikerIdList                  = ""
         cPrisprofilIdList               = ""
         cBestStatIdList                 = ""

         cAktivitetIdList                = ""
         cVarebokIdList                  = ""
         cMesseIdList                    = ""
         cKampanjeIdList                 = ""
         cKombKampanjeIdList                 = ""
    
         gcFields                        = ""
         gcValues                        = ""
         gcSort                          = ""
         gcOperator                      = ""
         gcFeltListe                     = ""
  
         cmbLager:SCREEN-VALUE           = cComboDefault
         cmbTilbud:SCREEN-VALUE          = cComboDefault
         /*cmbSalg:SCREEN-VALUE            = cComboDefault*/
         cmbBest:SCREEN-VALUE            = cComboDefault
         cmbBestType:SCREEN-VALUE        = "0"
         cmbBekreftet:SCREEN-VALUE       = "0"
         cmbVaremottak:SCREEN-VALUE      = cComboDefault
         cmbTTId:SCREEN-VALUE            = '1'
         cmbKombKampanje:SCREEN-VALUE    = cComboDefault
         /*
         fi-dFraSalg:SCREEN-VALUE        = ""
         fi-dTilSalg:SCREEN-VALUE        = ""
         */
         fi-dFraBest:SCREEN-VALUE        = ""
         fi-dTilBest:SCREEN-VALUE        = ""
         /*
         fi-dFraSalg                     = ?
         fi-dTilSalg                     = ?
         */
         fi-dFraBest                     = ?
         fi-dTilBest                     = ?
         .                              

  /*APPLY "value-changed" TO cmbSalg.*/
  APPLY "value-changed" TO cmbLager.
  APPLY "value-changed" TO cmbTilbud.
  APPLY "value-changed" TO cmbBest.
  APPLY "value-changed" TO cmbBestType.
  APPLY "value-changed" TO cmbBekreftet.
  APPLY "value-changed" TO cmbVaremottak.
  APPLY "value-changed" TO cmbTTId.
  APPLY "value-changed" TO cmbKombKampanje.

  RUN NullstillKrit IN h_fartsokfilter.
END.
DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OKbutton wWin 
PROCEDURE OKbutton :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
APPLY "CHOOSE" TO Btn_OK IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSelectorAttributes wWin 
PROCEDURE setSelectorAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrwSource AS HANDLE NO-UNDO.
DEF INPUT PARAM ihBrwTarget AS HANDLE NO-UNDO.

ihBrwSource:WINDOW:TOP-ONLY = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokSdo wWin 
PROCEDURE SokSdo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER lcFields    AS CHAR NO-UNDO.  
  DEF INPUT PARAMETER lcValues    AS CHAR NO-UNDO.
  DEF INPUT PARAMETER lcSort      AS CHAR NO-UNDO.
  DEF INPUT PARAMETER lcOperator  AS CHAR NO-UNDO.
  DEF INPUT PARAMETER lcFeltListe AS CHAR NO-UNDO.

  ASSIGN
      gcFields    = ""
      gcValues    = ""
      gcSort      = ""
      gcOperator  = ""
      gcFeltListe = ""

      gcFields    = lcFields
      gcValues    = lcValues
      gcSort      = lcSort 
      gcOperator  = lcOperator
      gcFeltListe = lcFeltListe
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartUtvalg wWin 
PROCEDURE StartUtvalg :
/*------------------------------------------------------------------------------
  Purpose:     Benytter en "død" sdo til å omforme feltlisten til en 
               query. Denne benyttes så i en dynamisk query for å kjøre
               utvalget og bygge tmplisten.
               
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR liLoop1       AS INT  NO-UNDO.
DEF VAR lcRemoveFelt  AS CHAR NO-UNDO.
DEF VAR lcRemoveOper  AS CHAR NO-UNDO.

DEF VAR cForEach      AS CHAR NO-UNDO.
DEF VAR cCriteria     AS CHAR NO-UNDO.
DEF VAR cNoLock       AS CHAR NO-UNDO.
DEF VAR cMinValue     AS CHAR NO-UNDO.
DEF VAR cVarGrAvdList AS CHAR NO-UNDO.
DEF VAR cVarGrHgList  AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  /* For å speede opp (den unødvendige) openQuery under: */
  ASSIGN gcFields   = "ArtikkelNr," + gcFields
         gcValues   = "-99999999" + CHR(1) + gcValues
         gcOperator = "=," + gcOperator
         .
  
  /* Fjerner gamle og legger på nye kriterier */
  DYNAMIC-FUNCTION('setQueryWhere':U IN h_dartbas,"").

  DYNAMIC-FUNCTION('assignQuerySelection':U IN h_dartbas,
                   INPUT gcFields   /* CHARACTER */,
                   INPUT gcValues   /* CHARACTER */,
                   INPUT gcOperator /* CHARACTER */).
  
  /* Legger inn valgt sortering */
  IF gcSort <> "" THEN
    DYNAMIC-FUNCTION('setQuerySort':U IN h_dartbas,
       INPUT gcSort /* CHARACTER */).

  /* Åpner query med ny filterverdi. */
  DYNAMIC-FUNCTION('openQuery':U IN h_dartbas).
/*   MESSAGE "hej"                          */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  /* Tar vare på Where satsen som sendes tilbake til kallende program */
  ASSIGN
      gcWhere            = DYNAMIC-FUNCTION('getQueryWhere':U IN h_dartbas)
      .

  ASSIGN gcWhere    = REPLACE(gcWhere,"ArtBas.ArtikkelNr = '-99999999' AND ","")
         gcWhere    = REPLACE(gcWhere,"ArtikkelNr = '-99999999' AND ","")
         gcWhere    = REPLACE(gcWhere,"ArtBas.ArtikkelNr = '-99999999'","true")
         gcWhere    = REPLACE(gcWhere,"ArtikkelNr = '-99999999'","true")
         gcWhere    = REPLACE(gcWhere,"ArtikkelNr = ?","true")
         gcWhere    = REPLACE(gcWhere,"SalgsStopp = ?","true")
         cForEach   = SUBSTR(gcWhere,1,INDEX(gcWhere,"("))
         cCriteria  = SUBSTR(gcWhere,INDEX(gcWhere,"(") + 1,R-INDEX(gcWhere,")") - INDEX(gcWhere,"(") - 1)
         cNoLock    = SUBSTR(gcWhere,R-INDEX(gcWhere,")"))

         cmbBest fi-dFraBest  
         fi-dTilBest cmbTilbud cmbLager cmbKombKampanje 
         cmbBestType cmbBekreftet cmbVaremottak fi-dFraVaremottak fi-dTilVaremottak cmbTTId
         .
  IF cAvdelingIdList NE "" THEN DO:
    DO ix = 1 TO NUM-ENTRIES(cAvdelingIdList,"|"):
      FOR EACH HuvGr NO-LOCK 
          WHERE HuvGr.AvdelingNr = INT(ENTRY(ix,cAvdelingIdList,"|")),
          EACH VarGr NO-LOCK OF HuvGr:
        cVarGrAvdList = cVarGrAvdList + STRING(VarGr.Vg) + ",".
      END.
    END.
    cVarGrAvdList = TRIM(cVarGrAvdList,",").
  END.
  IF cHuvGrIdList NE "" THEN DO:
    DO ix = 1 TO NUM-ENTRIES(cHuvGrIdList,"|"):
      FOR EACH VarGr NO-LOCK 
          WHERE VarGr.HG = INT(ENTRY(ix,cHuvGrIdList,"|")):
        cVarGrHgList = cVarGrHgList + STRING(VarGr.Vg) + ",".
      END.
    END.
    cVarGrHgList = TRIM(cVarGrHgList,",").
  END.

  ASSIGN cCriteria = AddIntToQuery(cCriteria,cVarGrAvdList,"Vg")
         cCriteria = AddIntToQuery(cCriteria,cVarGrHgList,"Vg")
         cCriteria = AddIntToQuery(cCriteria,cVarGrIdList,"Vg")
         cCriteria = AddIntToQuery(cCriteria,cBehandlingskodeIdList,"BehKode")
         cCriteria = AddIntToQuery(cCriteria,cStrTypeIdList,"StrTypeId")
         cCriteria = AddIntToQuery(cCriteria,cArtSlagIdList,"ArtSlag")
         cCriteria = AddIntToQuery(cCriteria,cIndTypeIdList,"IndividType")
         cCriteria = AddIntToQuery(cCriteria,cLevBasIdList,"LevNr")
         cCriteria = AddIntToQuery(cCriteria,cProdusentIdList,"ProdNr")
         cCriteria = AddIntToQuery(cCriteria,cVaremerkeIdList,"VMId")
         cCriteria = AddIntToQuery(cCriteria,cSaSongIdList,"SaSong")
         cCriteria = AddIntToQuery(cCriteria,cFargIdList,"Farg")
         cCriteria = AddIntToQuery(cCriteria,cGarantiIdList,"GarantiKl")
         cCriteria = AddCharToQuery(cCriteria,cValutaIdList,"valkod")
         cCriteria = AddIntToQuery(cCriteria,cMaterialIdList,"MatKod")
         cCriteria = AddIntToQuery(cCriteria,cKlackIdList,"Klack")
         cCriteria = AddIntToQuery(cCriteria,cInnerSulaIdList,"inner-id")
         cCriteria = AddIntToQuery(cCriteria,cOvandelIdList,"ov-id")
         cCriteria = AddIntToQuery(cCriteria,cSlitSulaIdList,"slit-id")
         cCriteria = AddIntToQuery(cCriteria,cLast-SkoIdList,"last-id")
         cCriteria = AddIntToQuery(cCriteria,cAnv-KodIdList,"anv-id")
         cCriteria = AddIntToQuery(cCriteria,cRAvdNr-IdList,"RAvdNr")
         cCriteria = AddCharToQuery(cCriteria,cAlfaKode2-IdList,"AlfaKode2")
         cCriteria = AddCharToQuery(cCriteria,cOnLineLevNr-IdList,"OnLineLevNr")
         .

  DYNAMIC-FUNCTION("setSelectorFilter" IN h_QueryObject,
                   LOOKUP(cmbLager,cmbLager:LIST-ITEMS), /* 2: Aktiv på tilbud, 3: Ikke aktiv */
                   cButikerIdList,
                   LOOKUP(cmbTilbud,cmbTilbud:LIST-ITEMS), /* 2: Pos.lager, 3: Ikke lager, 4: Neg.lager */
                   cPrisprofilIdList, FALSE
                   /*
                   (IF LOOKUP(cmbSalg,cmbSalg:LIST-ITEMS) = 2 THEN TRUE
                     ELSE IF LOOKUP(cmbSalg,cmbSalg:LIST-ITEMS) = 3 THEN FALSE
                     ELSE ?)*/ ,
                   ?,?,
                   LOOKUP(cmbBest,cmbBest:LIST-ITEMS), /* 2: Plan.lev, 3: IKKE plan.lev, 4: Varemott, 5: IKKE varemott, 6: Bestilling, 7: IKKE bestilling */
                   fi-dFraBest,fi-dTilBest, /* Best. eller lev dato */
                   REPLACE(cBestStatIdList,"|",","),
                   INT(cmbBestType),
                   INT(cmbBekreftet),
                   cKategoriIdList,
                   cAktivitetIdList,
                   cMesseIdList,
                   cVarebokIdList,
                   cKampanjeIdList,
                   FI-Strekkode:SCREEN-VALUE,
                   FI-Bestillingsnummer:SCREEN-VALUE,
                   FI-ERPNr:SCREEN-VALUE,
                   (IF LOOKUP(cmbVaremottak,cmbVaremottak:LIST-ITEMS) = 2 THEN TRUE
                     ELSE IF LOOKUP(cmbVaremottak,cmbVaremottak:LIST-ITEMS) = 3 THEN FALSE
                     ELSE ?),
                   fi-dFraVaremottak,fi-dTilVaremottak,cButikerIdList-2,cmbTTId,cKombKampanjeIdList,
                   LOOKUP(cmbKombKampanje,cmbKombKampanje:LIST-ITEMS), /* 2: Aktiv på komb.kampanje, 3: Ikke aktiv */
                   cButikerIdList-3,
                   cArtButikerIdList /* lista av butiker som skall avgränsa artbut artiklar vid webartiklar */
                   ).
  
  gcWhere = cForEach + cCriteria + cNoLock.                  
  
  RUN StartUtvalg IN hParent (gcWhere).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddCharToQuery wWin 
FUNCTION AddCharToQuery RETURNS CHARACTER
  ( INPUT icCriteria AS CHAR,
    INPUT icList     AS CHAR,
    INPUT icField    AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icList = "" THEN RETURN icCriteria.

DEF VAR cCriteria AS CHAR NO-UNDO.

ASSIGN icList = REPLACE(icList,"|",",")
       cCriteria = icCriteria + " AND " + 
                  (IF NUM-ENTRIES(icList) > 1 THEN 
                     "CAN-DO('" + icList + "',ArtBas." + icField + ")" 
                   ELSE "ArtBas." + icField + " = '" + icList + "'").

RETURN cCriteria. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddIntToQuery wWin 
FUNCTION AddIntToQuery RETURNS CHARACTER
  ( INPUT icCriteria AS CHAR,
    INPUT icList     AS CHAR,
    INPUT icField    AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icList = "" THEN RETURN icCriteria.

DEF VAR cCriteria AS CHAR NO-UNDO.

ASSIGN icList = REPLACE(icList,"|",",")
       cCriteria = icCriteria + " AND " + 
                  (IF NUM-ENTRIES(icList) > 1 THEN 
                     " ArtBas." + icField + " GE " + MinValue(icList) +
                     " AND ArtBas." + icField + " LE " + MaxValue(icList) +
                     " AND CAN-DO('" + icList + "',STRING(ArtBas." + icField + "))" 
                   ELSE "ArtBas." + icField + " = " + icList).

RETURN cCriteria. 


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION applyOKbutton wWin 
FUNCTION applyOKbutton RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
APPLY "choose" TO Btn_Ok IN FRAME {&FRAME-NAME}.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MaxValue wWin 
FUNCTION MaxValue RETURNS CHARACTER
  ( INPUT icList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iMaxValue AS INT NO-UNDO.

DO ix = 1 TO NUM-ENTRIES(icList):
  IF INT(ENTRY(ix,icList)) > iMaxValue THEN
    iMaxValue = INT(ENTRY(ix,icList)).
END.

RETURN STRING(iMaxValue). 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION MinValue wWin 
FUNCTION MinValue RETURNS CHARACTER
  ( INPUT icList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iMinValue AS INT NO-UNDO INIT 99999999.

DO ix = 1 TO NUM-ENTRIES(icList):
  IF INT(ENTRY(ix,icList)) < iMinValue THEN
    iMinValue = INT(ENTRY(ix,icList)).
END.

RETURN STRING(iMinValue). 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSelectionFillIn wWin 
FUNCTION setSelectionFillIn RETURNS LOGICAL
  ( INPUT ihFillIn    AS HANDLE,
    INPUT icRowidList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icRowidList NE "" THEN DO:
  IF iSelectorSourcCount NE ? THEN
    ihFillIn:SCREEN-VALUE = STRING(NUM-ENTRIES(icRowidList)) + " " + cDelimDefault + " " +
                                  STRING(iSelectorSourcCount).
  ELSE 
    ihFillIn:SCREEN-VALUE = STRING(NUM-ENTRIES(icRowidList)) + " " + cDelimDefault + " " +
                                  DYNAMIC-FUNCTION("getRecordCount",cCurrSelectBuffer,"").
END.
ELSE ihFillIn:SCREEN-VALUE = cComboDefault.
  
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

