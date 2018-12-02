&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS fFrameWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrfrm.w - ADM2 SmartFrame Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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

DEFINE VARIABLE cFelter    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLabels    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDecimaler AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRightCols AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cHodeFelter    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cHodeLabels    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cHodeDecimaler AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cHodeRightCols AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cLinjeFelter    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLinjeLabels    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLinjeDecimaler AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLinjeRightCols AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cTidFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFilename  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAlle      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFieldDefs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSummerFelter AS CHARACTER     NO-UNDO.
DEFINE VARIABLE cTransFelter   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE cGetTransVerdier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cHodeTransVerdier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLinjeTransVerdier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cUserDefaultBut  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgButikker AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgKasserer AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgKasser   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgSelger   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE h_Window       AS HANDLE     NO-UNDO.
DEFINE VARIABLE pcFeltListe    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcVerdier      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcHodeVerdier  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcAndQuery     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcBongLinje    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cBLFieldDefs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lBokfDagFinns AS LOGICAL    NO-UNDO.
/* ASSIGN cFelter = "Butik,KassaNr,ProfilNr,TTId,ArtikkelNr,Kode,Storl,Antall,Pris,RabKr,Mva,KalkylePris,VVarekost,ForsNr,SelgerNr,KortNr,KortType,KundNr,LevNr,Vg,LopNr,MedlemsNr,Dato,Tid,LinjeRab,PersonalRab,SubtotalRab,BatchNr,TransNr,BongId,BongLinjeNr,Postert,PostertDato,PostertTid"                         */
/*        cLabels = "Butikk,Kasse,Profil,TrTypeId,Artikkel,Strekkode,Størrelse,Antall,Pris,Rabatt,Mva,Kalkylepris,Vektet varekost,Kasserer,Selger,Kortnr,KortType,Kundenr,Leverandør,VgNr,LpNr,Medlemsnr,Transdato,Tid,Linjerab,Personalrab,Subtotalrab,BatchNr,TransNr,BongID,LinjeNr,Postert,PostertDato,Postert tid" */
/*        cTidFelter = "PostertTid,Tid,". */
/*        cDecimaler = ",,,,,,2,2,2,,,,2,2,2,,,,,,,,,2,2,2,,,,,,," /* för hantering av decimaler */                                                                                                                                                                                                                     */
/*        cRightCols = "1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1". /* 1=right-align */                                                                                                                                                                                                       */
/* 
ASSIGN cFelter = "Butik,KassaNr,ProfilNr,ArtikkelNr,Kode,Storl,Antall,Pris,RabKr,LinjeRab,PersonalRab,SubtotalRab,Mva,KalkylePris,VVarekost,ForsNr,SelgerNr,BongId,BongLinjeNr,KortNr,KortType,KundNr,LevNr,Vg,LopNr,MedlemsNr,Dato,Tid,BatchNr,TransNr,TTId,Postert,PostertDato,PostertTid"
       cLabels = "Butikknummer,KasseNr,Prisprofil,Artikkelnummer,Strekkode,Størrelse,Antall,Pris,Rabatt,Linjerabatt,Personalrabatt,Subtotalrabatt,Mva,Kalkylepris,Vektet varekost,Kasserernummer,Selgernummer,BongID,LinjeNr,Kortnummer,KortType,Kundenummer,Leverandør,VgNr,LpNr,Medlemsnummer,Transaksjonsdato,Tid,BatchNummer,TransaksjonsNr,TransTypeId,Postert,PostertDato,Postert tid"
 ASSIGN cFelter = "Butik,KassaNr,ProfilNr,ArtikkelNr,Kode,Storl,Antall,Pris,RabKr,LinjeRab,PersonalRab,SubtotalRab,Mva,KalkylePris,VVarekost,SattVVareKost,ForsNr,SelgerNr,BatchNr,BongId,BongLinjeNr,Dato,KortNr,KortType,KundNr,LevNr,Vg,LopNr,MedlemsNr,Postert,PostertDato,PostertTid,TBId,Tid,TransNr,TTId"
       cLabels = "Butikknummer,KasseNr,Prisprofil,Artikkelnummer,Strekkode,Størrelse,Antall,Pris,Rabatt,Linjerabatt,Personalrabatt,Subtotalrabatt,Mva,Kalkylepris,Vektet varekost,SattVVareKost,Kasserernummer,Selgernummer,BatchNummer,BongID,LinjeNr,Transaksjonsdato,Kortnummer,KortType,Kundenummer,Leverandør,VgNr,LpNr,Medlemsnummer,Postert,PostertDato,Postert tid,Transaksjonstype beskrivelse,Tid,TransaksjonsNr,TransTypeId"
 */
IF  CAN-FIND(FIRST Bokforingsdag) THEN
    ASSIGN lBokfDagFinns = TRUE.
/* Fältnamn;Labels;Antal decimaler(blank = 0);Rightcols(1 = right) */
ASSIGN cTransFelter = "ButikkNr,KasseNr,Dato,BongNr"
/*        cSummerFelter = "Antall,SumNetto,SumVk,SumDBKr" */

cFieldDefs = "ButikkNr;Butikk;;1," + 
             "KasseNr;Kasse;;1," + 
             "Belop;Beløp;2;1," + 
             "BongNr;BongNr;;1," + 
             "Dato;Dato-YMD;;1," + (IF lBokfDagFinns = TRUE THEN "Skiftnr;Skiftnr;;1," ELSE "") +
             "flBankkort;Bankkort;;1," + 
             "flBetalingskort;Betalingskort;;1," + 
             "flGavekort;Gavekort;;1," + 
             "flKreditkort;Kreditkort;;1," + 
             "flKupong1;Kupong1;;1," + 
             "flRabatt;Rabatt;;1," + 
             "flRekvisisasjon;Rekvisisjon;;1," + 
             "flSjekk;Sjekk;;1," + 
             "flSystemkort;Systemkort;;1," + 
             "Systemkort;Systemkort;;1," +
             "KassererNr;KassererNr;;1," + 
             "KassererNavn;KassererNavn;;1," + 
             "SelgerNavn;SelgerNavn;;1," + 
             "SelgerNr;SelgerNr;;1," + 
             "Konvertert;Konvertert;;1," + 
             "KortType;KortType;;1," + 
             "KundeKort;KundeKort;;1," + 
             "KundeNavn;KundeNavn;;1," + 
             "KundeNr;KundeNr;;1," + 
             "EkstOrdreNr;EkstOrdreNr;;1," +
             "MedlemNavn;MedlemNavn;;1," + 
             "MedlemsKort;MedlemsKort;;1," + 
             "MedlemsNr;MedlemsNr;;1," + 
             "OpdKvit;OpdKvit;;1," + 
             "OpdUtskKopi;OpdUtskKopi;;1," + 
             "OverforingsNr;OverforingsNr;;1," + 
             "BongStatus;BongStatus;;1," + 
             "DataSettId;DataSettId;;1," +
             "b_id;BongId;;1".

cBLFieldDefs =
    "ButikkNr;Butikk;;1," +
    "KasseNr;Kasse;;1," +
    "ArtikkelNr;ArtikkelNr;;1," +
    "BongTekst;BongTekst;;1," +
    "Storrelse;Størrelse;;1," +
    "Strekkode;Strekkode;;1," +
               "Antall;Antall;3;1," +
    "LinjeSum;LinjeSum;2;1," +
    "LinjeRab;LinjeRab;2;1," +
    "FeilKode;FK;;1,"      +
    "FeilKodeTekst;Beskr;;1," +
    "Nettokr;Nettokr;2;1," +
    "DBKr;DBKr;2;1," +
    "DB%;DB%;2;1," +
    "Mva%;Mva%;2;1," +
    "MvaKr;MvaKr;2;1," +
    "SubtotalRab;SubtotalRab;2;1," +
    "VVarekost;Varekost;2;1," +
    "BongNr;BongNr;;1," +
    "Dato;Dato;;1," +
               "Kunderabatt;Kunderabatt;2;1," +
               "Makulert;Makulert;;1," +
               "MButikkNr;MButikkNr;;1," +
               "Medlemsrabatt;Medlemsrabatt;2;1," +
               "Personalrabatt;Personalrabatt;2;1," +
               "RefNr;RefNr;;1," +
               "RefTekst;RefTekst;;1," +
               "ReturButikk;ReturButikk;;1," +
               "ReturKasserer;ReturKasserer;;1," +
               "ReturKassererNavn;ReturKassererNavn;;1," +
               "Type;Type;;1," +
    "HovedGr;HovedGr;;1," +
    "HovedGrBeskrivelse;HovedGrBeskrivelse;;1," +
               "VareGr;VareGr;;1," +
               "VareGruppeNavn;VareGruppeNavn;;1," +
    "SelgerNavn;SelgerNavn;;1," + 
    "SelgerNr;SelgerNr;;1," + 
    "LopeNr;LopeNr;;1," +
    "TransDato;TransDato;;1," +
    "TransNr;TransNr;;1," +
    "TransTid;TransTid;;1," +
    "TTId;TTId;;1," +
    "TBId;TBId;;1,"  +
    "b_id;Bongid;;1," +
    "KundeNavn;KundeNavn;;1," + 
    "KundeNr;KundeNr;;1," + 
    "KampId;Kampanje;;," +
    "KampTilbId;Kamptilbud;;".             


/*
               "AaaaMmDd;AaaaMmDd;;1," +
               "b_id;b_id;;1," +
               "BongPris;BongPris;;1," +
               "FeilKode;FeilKode;;1," +
               "FeilKodeTekst;FeilKodeTekst;;1," +
               "ForKonvertering;ForKonvertering;;1," +
               "GenerellRabatt;GenerellRabatt;2;1," +
               "GruppeNr;GruppeNr;;1," +
               "LevNavn;LevNavn;;1," +
               "LevNr;LevNr;;1," +
               "MvaGr;MvaGr;;1," +
               "MvaGruppeNavn;MvaGruppeNavn;;1," +
*/
             
DEFINE TEMP-TABLE TT_TillgButikker NO-UNDO
    FIELD Butik LIKE Butiker.Butik
    INDEX Butik Butik.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 FI-Dato FI-DatoTil CB-TTId ~
TG-Bankkort TG-Rabatt TG-Betalingskort TG-Rekvisisjon B-KasseNr CB-Butik ~
FI-LBelopFra FI-LBelopTil TG-Gavekort TG-Sjekk B-KasseNrBlank ~
TG-VisBonglinje TG-SokNetto TG-Kredittkort TG-Systemkort TG-MakulerteBL ~
TG-Kupong1 B-ForsNrBlank TG-Makulerte B-Artikkelkort B-Aktiver B-VisTrans ~
B-Nullstill B-SelgerNrBlank CB-Belop FI-Kortnr B-KortNrBlank FI-BelopFra ~
FI-BelopTil BUTTON-SokDatoTil B-SelgerNr B-ForsNr BUTTON-SokDato ~
FI-BhodeTxt FI-BhodeTxt-2 
&Scoped-Define DISPLAYED-OBJECTS FI-Dato FI-DatoTil CB-TTId TG-Bankkort ~
TG-Rabatt TG-Betalingskort TG-Rekvisisjon CB-Butik FI-LBelopFra ~
FI-LBelopTil TG-Gavekort TG-Sjekk FI-KasseNr TG-VisBonglinje TG-SokNetto ~
TG-Kredittkort TG-Systemkort TG-MakulerteBL TG-Kupong1 FI-ForsNr ~
TG-Makulerte FI-SelgerNr CB-Belop FI-Kortnr FI-BelopFra FI-BelopTil ~
FI-BhodeTxt FI-BhodeTxt-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLockvindu fFrameWin 
FUNCTION fLockvindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSumFelter fFrameWin 
FUNCTION getSumFelter RETURNS CHARACTER
  ( INPUT cFeltnavnListe AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dbong AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Aktiver 
     LABEL "&Aktiver" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Artikkelkort 
     LABEL "Arti&kkelkort" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-ForsNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-ForsNrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-KasseNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-KasseNrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-KassSelgStat 
     LABEL "Kasserer/selgerstat" 
     SIZE 23.6 BY 1.14.

DEFINE BUTTON B-KortNrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-Nullstill 
     LABEL "N&ullstill" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-SelgerNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SelgerNrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-VisTrans 
     IMAGE-UP FILE "icon/e-detail":U
     LABEL "&Vis transaksjon" 
     SIZE 4.4 BY 1.14.

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDatoTil 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-Belop AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beløp" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Butik AS CHARACTER FORMAT "X(256)" INITIAL "0" 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEM-PAIRS "","1"
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE CB-TTId AS CHARACTER FORMAT "X(256)" INITIAL "0" 
     LABEL "Transtype" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","0"
     DROP-DOWN-LIST
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE FI-BelopFra AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Beløp fra/til" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-BelopTil AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-BhodeTxt AS CHARACTER FORMAT "X(256)":U INITIAL " Bonghode" 
      VIEW-AS TEXT 
     SIZE 12.2 BY .62 NO-UNDO.

DEFINE VARIABLE FI-BhodeTxt-2 AS CHARACTER FORMAT "X(256)":U INITIAL " Bonglinje" 
      VIEW-AS TEXT 
     SIZE 11.2 BY .62 NO-UNDO.

DEFINE VARIABLE FI-Dato AS DATE FORMAT "99/99/99" 
     LABEL "Dato fra/til" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI-DatoTil AS DATE FORMAT "99/99/99" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI-ForsNr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Kasserer" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KasseNr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Kassenr" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Kortnr AS DECIMAL FORMAT ">>>>>>>>>>>9":U INITIAL 0 
     LABEL "Kundekort" 
     VIEW-AS FILL-IN 
     SIZE 18.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LBelopFra AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     LABEL "Beløp fra/til" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LBelopTil AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SelgerNr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Selger" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 103 BY 6.76.

DEFINE VARIABLE TG-Bankkort AS LOGICAL INITIAL no 
     LABEL "Bankkort" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Betalingskort AS LOGICAL INITIAL no 
     LABEL "Betalingskort" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Gavekort AS LOGICAL INITIAL no 
     LABEL "Gavekort" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Kredittkort AS LOGICAL INITIAL no 
     LABEL "Kredittkort" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Kupong1 AS LOGICAL INITIAL no 
     LABEL "Kupong1" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Makulerte AS LOGICAL INITIAL no 
     LABEL "Makulerte" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-MakulerteBL AS LOGICAL INITIAL no 
     LABEL "Makulerte" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Rabatt AS LOGICAL INITIAL no 
     LABEL "Rabatt" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Rekvisisjon AS LOGICAL INITIAL no 
     LABEL "Rekvisisjon" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Sjekk AS LOGICAL INITIAL no 
     LABEL "Sjekk" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SokNetto AS LOGICAL INITIAL no 
     LABEL "Nettobeløp" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Systemkort AS LOGICAL INITIAL no 
     LABEL "Systemkort" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-VisBonglinje AS LOGICAL INITIAL no 
     LABEL "Vis bonglinjer" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FI-Dato AT ROW 1.81 COL 14 COLON-ALIGNED HELP
          "Transaksjonsdato"
     FI-DatoTil AT ROW 1.81 COL 32.2 COLON-ALIGNED HELP
          "Transaksjonsdato" NO-LABEL
     CB-TTId AT ROW 1.81 COL 118.2 COLON-ALIGNED HELP
          "TransaksjonstypensID"
     TG-Bankkort AT ROW 1.86 COL 53.8
     TG-Rabatt AT ROW 1.86 COL 72.4
     TG-Betalingskort AT ROW 2.57 COL 53.8
     TG-Rekvisisjon AT ROW 2.57 COL 72.4
     B-KasseNr AT ROW 3.81 COL 29.4 NO-TAB-STOP 
     CB-Butik AT ROW 2.81 COL 14 COLON-ALIGNED HELP
          "Butikknummer"
     FI-LBelopFra AT ROW 2.81 COL 118.2 COLON-ALIGNED
     FI-LBelopTil AT ROW 2.81 COL 135.6 COLON-ALIGNED NO-LABEL
     TG-Gavekort AT ROW 3.33 COL 53.8
     TG-Sjekk AT ROW 3.33 COL 72.4
     FI-KasseNr AT ROW 3.81 COL 14 COLON-ALIGNED
     B-KasseNrBlank AT ROW 3.81 COL 35.4
     TG-VisBonglinje AT ROW 3.95 COL 120
     TG-SokNetto AT ROW 3.95 COL 138
     TG-Kredittkort AT ROW 4 COL 53.8
     TG-Systemkort AT ROW 4 COL 72.4
     TG-MakulerteBL AT ROW 4 COL 153
     TG-Kupong1 AT ROW 4.67 COL 53.8
     FI-ForsNr AT ROW 4.81 COL 14 COLON-ALIGNED
     B-ForsNrBlank AT ROW 4.81 COL 35.4
     TG-Makulerte AT ROW 5.43 COL 53.8
     B-Artikkelkort AT ROW 5.43 COL 143
     B-Aktiver AT ROW 5.48 COL 107.4
     B-VisTrans AT ROW 5.48 COL 122.8
     B-Nullstill AT ROW 5.48 COL 127.4
     FI-SelgerNr AT ROW 5.81 COL 14 COLON-ALIGNED
     B-SelgerNrBlank AT ROW 5.81 COL 35.4
     CB-Belop AT ROW 5.81 COL 86.4 COLON-ALIGNED
     FI-Kortnr AT ROW 6.81 COL 14 COLON-ALIGNED
     B-KortNrBlank AT ROW 6.81 COL 35.4
     B-KassSelgStat AT ROW 6.86 COL 107.4
     FI-BelopFra AT ROW 6.91 COL 68.8 COLON-ALIGNED
     FI-BelopTil AT ROW 6.91 COL 86.2 COLON-ALIGNED NO-LABEL
     BUTTON-SokDatoTil AT ROW 1.81 COL 47.4
     B-SelgerNr AT ROW 5.81 COL 29.4 NO-TAB-STOP 
     B-ForsNr AT ROW 4.81 COL 29.4 NO-TAB-STOP 
     BUTTON-SokDato AT ROW 1.81 COL 29.4
     FI-BhodeTxt AT ROW 1.05 COL 1.8 COLON-ALIGNED NO-LABEL
     FI-BhodeTxt-2 AT ROW 1.05 COL 106.2 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1.38 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 170.8 BY 7.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW fFrameWin ASSIGN
         HEIGHT             = 7.33
         WIDTH              = 170.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB fFrameWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}
{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW fFrameWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME fMain:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON B-KassSelgStat IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       B-KassSelgStat:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR FILL-IN FI-ForsNr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KasseNr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-SelgerNr IN FRAME fMain
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Options          = "NO-LOCK INDEXED-REPOSITION KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME B-Aktiver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Aktiver fFrameWin
ON CHOOSE OF B-Aktiver IN FRAME fMain /* Aktiver */
DO:
    DEFINE VARIABLE TTH AS HANDLE     NO-UNDO.
    DEFINE VARIABLE qh  AS HANDLE     NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
/*     Dessa är flyttade till definitionsblocket för att kunna hanteras om vi har valt avancerat */
/*     eller kommer från artikkelutvalg */
/*     DEFINE VARIABLE pcFeltListe AS CHARACTER  NO-UNDO. */
/*     DEFINE VARIABLE pcVerdier   AS CHARACTER  NO-UNDO. */
    DEFINE VARIABLE cSumCols    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cSumString  AS CHARACTER  NO-UNDO.
    

    IF INPUT FI-Dato > INPUT FI-DatoTil THEN DO:
        MESSAGE "Feil dato, fra dato > til dato"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO FI-DatoTIl.
        RETURN NO-APPLY.
    END.
    IF INPUT FI-BelopTil <> 0 AND INPUT FI-BelopFra > INPUT FI-BelopTil THEN DO:
        MESSAGE "Feil beløp, fra beløp > til beløp"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO FI-BelopFra.
        RETURN NO-APPLY.
    END.
    IF INPUT FI-LBelopTil <> 0 AND INPUT FI-LBelopFra > INPUT FI-LBelopTil THEN DO:
        MESSAGE "Feil beløp, fra beløp > til beløp"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO FI-LBelopFra.
        RETURN NO-APPLY.
    END.

    RUN SetFilterParam.
    PUBLISH "VisTxtBox" ("Søker data......").
    RUN StartSok.
/*     MESSAGE pcBongLinje                    */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*     RETURN.                                */

    RUN BongToTT IN h_dbong
      ( OUTPUT TTH,pcHodeVerdier,pcAndQuery,pcBongLinje,?).
    CREATE QUERY qh.
    qh:SET-BUFFERS(TTH).
    qh:QUERY-PREPARE(IF TG-VisBongLinje:CHECKED THEN "for each TT_Bonglinje" ELSE "for each TT_Bonghode").
    qh:QUERY-OPEN().

/*   RUN rappgenqry.p ("Translogg",DYNAMIC-FUNCTION('getQueryWhere':U IN h_dbonghode),cFileName,cLabels,cFelter,cDecimaler,cTidFelter,?). */
/*   RUN rappgenqry.p ("Translogg","",cFileName,cLabels,cFelter,cDecimaler,cTidFelter,DYNAMIC-FUNCTION('getQueryHandle':U IN h_dbonghode)). */
  PUBLISH "VisTxtBox" ("Leser ut data......").
  RUN rappgenqry.p ("TT_BongHode","",cFileName,cLabels,cFelter,cDecimaler,cTidFelter,qh).

  DO:
      PUBLISH "VisTxtBox" ("Leser inn og bearbeider data......").
      PUBLISH "LoadGrid" (cFileName,4).  /* 3 = antall frozen cols  */
      PUBLISH "AlignCol" (LOOKUP("Storrelse",cFelter),8). /* högerjustert */
      IF INT(CB-TTId:SCREEN-VALUE) > 0 THEN DO:
          ASSIGN cSumCols   = getSumFelter(cSummerFelter)
                 cSumString = getSumFelter("Bongtekst") + ",SUM" .
          PUBLISH "Summer" (cSumCols,cSumString).
      END.

  END.
  qh:QUERY-CLOSE().
  DELETE OBJECT qh.
  TTH:EMPTY-TEMP-TABLE().
  PUBLISH "VisTxtBox" ("").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Artikkelkort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Artikkelkort fFrameWin
ON CHOOSE OF B-Artikkelkort IN FRAME fMain /* Artikkelkort */
DO:
  RUN ArtikkelKort.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ForsNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ForsNr fFrameWin
ON CHOOSE OF B-ForsNr IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE cForsaljRowIdList  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cForsaljIdList     AS CHARACTER  NO-UNDO.
    IF FI-ForsNr:PRIVATE-DATA <> "" AND FI-ForsNr:PRIVATE-DATA <> ? THEN
        ASSIGN cForsaljRowIdList = FI-ForsNr:PRIVATE-DATA.
     RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                         "Forsalj;ForsNr;FoNamn",
                         "where CAN-DO('" + cTillgKasserer + "',STRING(Forsalj.ForsNr))",
                         INPUT-OUTPUT cForsaljRowIdList,
                         "ForsNr",
                         INPUT-OUTPUT cForsaljIdList,
                         "","",
                         OUTPUT bOK).
  IF bOK = FALSE THEN
        RETURN NO-APPLY.
  assign
    FI-ForsNr:SCREEN-VALUE = IF cForsaljIdList = ""
                      then cAlle
                    else "( " + STRING(NUM-ENTRIES(cForsaljIdList,"|")) + " )"
    FI-ForsNr     = if cForsaljIdList = ""
                      then "*"
                      else REPLACE(cForsaljIdList,"|",",")
    FI-ForsNr:TOOLTIP = IF FI-ForsNr = "*" THEN "" ELSE FI-ForsNr
    FI-ForsNr:PRIVATE-DATA = cForsaljRowIdList
    FI-ForsNr:BGCOLOR  = IF FI-ForsNr = "*" THEN ? ELSE 11.
/*   def var IO-Liste as char no-undo.                                  */
/*                                                                      */
/* /*   if wVareGrupper = cAlle then */                                 */
/* /*     RUN InitVaregrupper.       */                                 */
/*                                                                      */
/*   assign                                                             */
/*     IO-Liste = if FI-ForsNr:SCREEN-VALUE = cAlle                     */
/*                  then ""                                             */
/*                  else FI-ForsNr.                                     */
/*                                                                      */
/*   run d-tagforsalj.w (input-output IO-Liste).                        */
/*   IF RETURN-VALUE = "Avbryt" THEN                                    */
/*         RETURN NO-APPLY.                                             */
/*   assign                                                             */
/*     FI-ForsNr:SCREEN-VALUE = if IO-Liste = ""                        */
/*                       then cAlle                                     */
/*                     else "( " + STRING(NUM-ENTRIES(IO-Liste)) + " )" */
/*     FI-ForsNr     = if IO-Liste = ""                                 */
/*                       then "*"                                       */
/*                       else IO-Liste                                  */
/*     FI-ForsNr:TOOLTIP = IF FI-ForsNr = "*" THEN "" ELSE FI-ForsNr.   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ForsNrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ForsNrBlank fFrameWin
ON CHOOSE OF B-ForsNrBlank IN FRAME fMain /* Blank */
DO:
    IF FI-ForsNr:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-ForsNr:SCREEN-VALUE = cAlle
               FI-ForsNr              = "*"
               FI-ForsNr:TOOLTIP      = ""
               FI-ForsNr:BGCOLOR      = ?
               FI-ForsNr:PRIVATE-DATA = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KasseNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KasseNr fFrameWin
ON CHOOSE OF B-KasseNr IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

/*     IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN          */
/*         ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))  */
/*                cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)). */
    IF FI-KasseNr:PRIVATE-DATA <> "" AND FI-KasseNr:PRIVATE-DATA <> ? THEN
        ASSIGN cRowIdList = FI-KasseNr:PRIVATE-DATA.
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "Kasse;KasseNr;Navn;Aktiv;!ButikkNr",
                        "WHERE ButikkNr = " + CB-Butik:SCREEN-VALUE,
                        INPUT-OUTPUT cRowIdList,
                        "KasseNr",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-KasseNr:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-KasseNr     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-KasseNr:TOOLTIP = IF FI-KasseNr = "*" THEN "" ELSE FI-KasseNr.
        IF FI-KasseNr <> "*" THEN
/*             ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList */
            ASSIGN FI-KasseNr:PRIVATE-DATA = cRowIdList
                   FI-KasseNr:BGCOLOR  = 11.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
                   FI-KasseNr:BGCOLOR  = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KasseNrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KasseNrBlank fFrameWin
ON CHOOSE OF B-KasseNrBlank IN FRAME fMain /* Blank */
DO:
    IF FI-KasseNr:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-KasseNr:SCREEN-VALUE = cAlle
               FI-KasseNr              = "*"
               FI-KasseNr:TOOLTIP      = ""
               FI-KasseNr:BGCOLOR      = ?
               FI-KasseNr:PRIVATE-DATA = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KassSelgStat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KassSelgStat fFrameWin
ON CHOOSE OF B-KassSelgStat IN FRAME fMain /* Kasserer/selgerstat */
DO:
    DEFINE VARIABLE iRappType AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cValgteBut AS CHARACTER  NO-UNDO.
    ASSIGN cValgteBut = CB-Butik:SCREEN-VALUE.
    RUN d-VelgStattype.w (INPUT INPUT FI-Dato,
                          INPUT INPUT FI-DatoTil,
                          CB-Butik:LIST-ITEM-PAIRS,
                          INPUT-OUTPUT cValgteBut,
                          FI-ForsNr,
                          FI-SelgerNr).
    IF CAN-DO("1,2",RETURN-VALUE) THEN DO:
        RUN SelgerStat (cValgteBut,RETURN-VALUE).
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KortNrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KortNrBlank fFrameWin
ON CHOOSE OF B-KortNrBlank IN FRAME fMain /* Blank */
DO:
    ASSIGN FI-Kortnr:SCREEN-VALUE = "0".
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Nullstill
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Nullstill fFrameWin
ON CHOOSE OF B-Nullstill IN FRAME fMain /* Nullstill */
DO:
    RUN Nullstill.
    APPLY "ENTRY" TO FI-Dato.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SelgerNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SelgerNr fFrameWin
ON CHOOSE OF B-SelgerNr IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE cSelgerRowIdList  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cSelgerIdList     AS CHARACTER  NO-UNDO.
    IF FI-SelgerNr:PRIVATE-DATA <> "" AND FI-SelgerNr:PRIVATE-DATA <> ? THEN
        ASSIGN cSelgerRowIdList = FI-SelgerNr:PRIVATE-DATA.
     RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                         "Selger;SelgerNr;Navn",
                         "where CAN-DO('" + cTillgSelger + "',STRING(Selger.SelgerNr))",
                         INPUT-OUTPUT cSelgerRowIdList,
                         "SelgerNr",
                         INPUT-OUTPUT cSelgerIdList,
                         "","",
                         OUTPUT bOK).
  IF bOK = FALSE THEN
        RETURN NO-APPLY.
  assign
    FI-SelgerNr:SCREEN-VALUE = IF cSelgerIdList = ""
                      then cAlle
                    else "( " + STRING(NUM-ENTRIES(cSelgerIdList,"|")) + " )"
    FI-SelgerNr     = if cSelgerIdList = ""
                      then "*"
                      else REPLACE(cSelgerIdList,"|",",")
    FI-SelgerNr:TOOLTIP = IF FI-SelgerNr = "*" THEN "" ELSE FI-SelgerNr
    FI-SelgerNr:PRIVATE-DATA = cSelgerRowIdList
    FI-SelgerNr:BGCOLOR  = IF FI-SelgerNr = "*" THEN ? ELSE 11.
    
    
/*   def var IO-Liste as char no-undo.                                      */
/*                                                                          */
/* /*   if wVareGrupper = cAlle then */                                     */
/* /*     RUN InitVaregrupper.       */                                     */
/*                                                                          */
/*   assign                                                                 */
/*     IO-Liste = if FI-SelgerNr:SCREEN-VALUE = cAlle                       */
/*                  then ""                                                 */
/*                  else FI-SelgerNr.                                       */
/*                                                                          */
/*   run d-tagselger.w (input-output IO-Liste).                             */
/*   IF RETURN-VALUE = "Avbryt" THEN                                        */
/*         RETURN NO-APPLY.                                                 */
/*   assign                                                                 */
/*     FI-SelgerNr:SCREEN-VALUE = if IO-Liste = ""                          */
/*                       then cAlle                                         */
/*                     else "( " + STRING(NUM-ENTRIES(IO-Liste)) + " )"     */
/*     FI-SelgerNr     = if IO-Liste = ""                                   */
/*                       then "*"                                           */
/*                       else IO-Liste                                      */
/*     FI-SelgerNr:TOOLTIP = IF FI-SelgerNr = "*" THEN "" ELSE FI-SelgerNr. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SelgerNrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SelgerNrBlank fFrameWin
ON CHOOSE OF B-SelgerNrBlank IN FRAME fMain /* Blank */
DO:
    IF FI-SelgerNr:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-SelgerNr:SCREEN-VALUE = cAlle
               FI-SelgerNr              = "*"
               FI-SelgerNr:TOOLTIP      = ""
               FI-SelgerNr:BGCOLOR      = ?
               FI-SelgerNr:PRIVATE-DATA = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VisTrans
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VisTrans fFrameWin
ON CHOOSE OF B-VisTrans IN FRAME fMain /* Vis transaksjon */
DO:
    DEFINE VARIABLE cVerdier     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cYMD         AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDato        AS CHARACTER  NO-UNDO.

    PUBLISH "FeltVerdier" (OUTPUT cVerdier,cGetTransVerdier,"SAME"). 
    IF cVerdier = "" THEN
        RETURN.
    ASSIGN cYMD = ENTRY(3,cVerdier).
    IF SESSION:DATE-FORMAT = "ymd" THEN
        ASSIGN cDato = cYMD.
    ELSE IF SESSION:DATE-FORMAT = "mdy" THEN
        ASSIGN cDato = ENTRY(2,cYMD,"/") + "/" + ENTRY(3,cYMD,"/") + "/" + ENTRY(1,cYMD,"/").
    ELSE IF SESSION:DATE-FORMAT = "dmy" THEN
        ASSIGN cDato = ENTRY(3,cYMD,"/") + "/" + ENTRY(2,cYMD,"/") + "/" + ENTRY(1,cYMD,"/").
    RUN gviskvittokopi2.w (INT(ENTRY(1,cVerdier)),1,INT(ENTRY(2,cVerdier)),DATE(cDato),INT(ENTRY(4,cVerdier)),THIS-PROCEDURE).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato fFrameWin
ON CHOOSE OF BUTTON-SokDato IN FRAME fMain /* ... */
or F10 of FI-Dato
DO:
  /* Start søkeprogram */
    DEFINE VARIABLE cTittel AS CHARACTER INIT "Transdato" NO-UNDO.
    DEFINE VARIABLE dDato   AS DATE       NO-UNDO.
    ASSIGN dDato = INPUT FI-Dato.
    RUN kalender.w (INPUT-OUTPUT dDato,cTittel).
    IF RETURN-VALUE <> "<avbryt>" THEN DO:
        IF dDato > INPUT FI-DatoTil THEN DO:
            MESSAGE "Feil dato, > Til dato"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        ELSE
            ASSIGN FI-Dato:SCREEN-VALUE = STRING(dDato).
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDatoTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDatoTil fFrameWin
ON CHOOSE OF BUTTON-SokDatoTil IN FRAME fMain /* ... */
or F10 of FI-DatoTil
DO:
  /* Start søkeprogram */
    DEFINE VARIABLE cTittel AS CHARACTER INIT "Transdato" NO-UNDO.
    DEFINE VARIABLE dDato   AS DATE       NO-UNDO.
    ASSIGN dDato = IF INPUT FI-DatoTil = ? THEN INPUT FI-Dato ELSE INPUT FI-DatoTil.
    RUN kalender.w (INPUT-OUTPUT dDato,cTittel).
    IF RETURN-VALUE <> "<avbryt>" THEN DO:
        IF dDato < INPUT FI-Dato THEN DO:
            MESSAGE "Feil dato, < Fra dato"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        ELSE
            ASSIGN FI-DatoTil:SCREEN-VALUE = STRING(dDato).
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Belop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Belop fFrameWin
ON VALUE-CHANGED OF CB-Belop IN FRAME fMain /* Beløp */
DO:
    ASSIGN FI-BelopFra:SENSITIVE = SELF:SCREEN-VALUE = ""
           FI-BelopTil:SENSITIVE = FI-BelopFra:SENSITIVE
           FI-BelopFra:SCREEN-VALUE = "0"
           FI-BelopTil:SCREEN-VALUE = "0".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Butik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Butik fFrameWin
ON VALUE-CHANGED OF CB-Butik IN FRAME fMain /* Butikk */
DO:
    RUN SetTillgKassSelger.
    APPLY "CHOOSE" TO B-SelgerNrBlank.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-TTId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-TTId fFrameWin
ON VALUE-CHANGED OF CB-TTId IN FRAME fMain /* Transtype */
DO:
    ASSIGN FI-LBelopFra:SENSITIVE    = NOT TRIM(SELF:SCREEN-VALUE) = ""
           FI-LBelopTil:SENSITIVE    = FI-LBelopFra:SENSITIVE
           FI-LBelopFra:SCREEN-VALUE = "0"
           FI-LBelopTil:SCREEN-VALUE = "0"
           B-ArtikkelKort:SENSITIVE  = TG-VisBongLinje:CHECKED
        .
           TG-VisBonglinje:SENSITIVE = FI-LBelopFra:SENSITIVE.
           TG-SokNetto:SENSITIVE     = TG-VisBonglinje:SENSITIVE.
           TG-MakulerteBL:SENSITIVE  = TRIM(SELF:SCREEN-VALUE) = "1".
    IF NOT TG-VisBonglinje:SENSITIVE THEN DO:
        ASSIGN TG-VisBonglinje:CHECKED = FALSE.
/*                TG-VisBonglinje:CHECKED = FALSE. */
        APPLY "VALUE-CHANGED" TO TG-VisBonglinje.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Bankkort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Bankkort fFrameWin
ON VALUE-CHANGED OF TG-Bankkort IN FRAME fMain /* Bankkort */
DO:
    TG-Makulerte:CHECKED = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Betalingskort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Betalingskort fFrameWin
ON VALUE-CHANGED OF TG-Betalingskort IN FRAME fMain /* Betalingskort */
DO:
    TG-Makulerte:CHECKED = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Gavekort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Gavekort fFrameWin
ON VALUE-CHANGED OF TG-Gavekort IN FRAME fMain /* Gavekort */
DO:
    TG-Makulerte:CHECKED = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Kredittkort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Kredittkort fFrameWin
ON VALUE-CHANGED OF TG-Kredittkort IN FRAME fMain /* Kredittkort */
DO:
    TG-Makulerte:CHECKED = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Kupong1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Kupong1 fFrameWin
ON VALUE-CHANGED OF TG-Kupong1 IN FRAME fMain /* Kupong1 */
DO:
    TG-Makulerte:CHECKED = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Makulerte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Makulerte fFrameWin
ON VALUE-CHANGED OF TG-Makulerte IN FRAME fMain /* Makulerte */
DO:
    IF SELF:CHECKED THEN DO WITH FRAME {&FRAME-NAME}:
        TG-Bankkort:CHECKED = FALSE.
        TG-Betalingskort:CHECKED = FALSE.
        TG-Gavekort:CHECKED = FALSE.
        TG-Kredittkort:CHECKED = FALSE.
        TG-Kupong1:CHECKED = FALSE.
        TG-Rabatt:CHECKED = FALSE.
        TG-Rekvisisjon:CHECKED = FALSE.
        TG-Sjekk:CHECKED = FALSE.
/*         TG-SokNetto:CHECKED = FALSE. */
        TG-Systemkort:CHECKED = FALSE.
/*         TG-VisBonglinje:CHECKED = FALSE. */
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-MakulerteBL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-MakulerteBL fFrameWin
ON VALUE-CHANGED OF TG-MakulerteBL IN FRAME fMain /* Makulerte */
DO:
    IF SELF:CHECKED THEN DO WITH FRAME {&FRAME-NAME}:
        TG-Bankkort:CHECKED = FALSE.
        TG-Betalingskort:CHECKED = FALSE.
        TG-Gavekort:CHECKED = FALSE.
        TG-Kredittkort:CHECKED = FALSE.
        TG-Kupong1:CHECKED = FALSE.
        TG-Rabatt:CHECKED = FALSE.
        TG-Rekvisisjon:CHECKED = FALSE.
        TG-Sjekk:CHECKED = FALSE.
/*         TG-SokNetto:CHECKED = FALSE. */
        TG-Systemkort:CHECKED = FALSE.
/*         TG-VisBonglinje:CHECKED = FALSE. */
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Rabatt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Rabatt fFrameWin
ON VALUE-CHANGED OF TG-Rabatt IN FRAME fMain /* Rabatt */
DO:
    TG-Makulerte:CHECKED = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Rekvisisjon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Rekvisisjon fFrameWin
ON VALUE-CHANGED OF TG-Rekvisisjon IN FRAME fMain /* Rekvisisjon */
DO:
    TG-Makulerte:CHECKED = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Sjekk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Sjekk fFrameWin
ON VALUE-CHANGED OF TG-Sjekk IN FRAME fMain /* Sjekk */
DO:
    TG-Makulerte:CHECKED = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Systemkort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Systemkort fFrameWin
ON VALUE-CHANGED OF TG-Systemkort IN FRAME fMain /* Systemkort */
DO:
    TG-Makulerte:CHECKED = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-VisBonglinje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-VisBonglinje fFrameWin
ON VALUE-CHANGED OF TG-VisBonglinje IN FRAME fMain /* Vis bonglinjer */
DO:
    IF SELF:CHECKED THEN
        ASSIGN cFelter          = cLinjeFelter   
               cLabels          = cLinjeLabels   
               cDecimaler       = cLinjeDecimaler
               cRightCols       = cLinjeRightCols
               cGetTransVerdier = cLinjeTransVerdier
        .
    ELSE
        ASSIGN cFelter          = cHodeFelter   
               cLabels          = cHodeLabels   
               cDecimaler       = cHodeDecimaler
               cRightCols       = cHodeRightCols
               cGetTransVerdier = cHodeTransVerdier.
    ASSIGN B-ArtikkelKort:SENSITIVE = SELF:CHECKED.
    PUBLISH "ClearGrid" (cLabels).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK fFrameWin 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN initializeObject.
&ENDIF
   {lng.i &SDO = "SDO"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects fFrameWin  _ADM-CREATE-OBJECTS
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
             INPUT  'sdo/dbong.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedbongOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dbong ).
       RUN repositionObject IN h_dbong ( 1.62 , 83.60 ) NO-ERROR.
       /* Size in AB:  ( 1.86 , 10.80 ) */

       /* Adjust the tab order of the smart objects. */
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Aktiver fFrameWin 
PROCEDURE Aktiver :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iButikkNr AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER dTransDat AS DATE    NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-DO(cTillgButikker,STRING(iButikkNr)) THEN DO:
            FIND Butiker WHERE Butiker.butik = iButikkNr NO-LOCK NO-ERROR.
            IF AVAIL Butiker THEN DO:
                CB-Butik:ADD-LAST(Butiker.Butnamn,STRING(iButikkNr)).
            END.
        END.
        ASSIGN CB-Butik:SCREEN-VALUE = STRING(iButikkNr)
               FI-Dato = dTransDat
               FI-Dato:SCREEN-VALUE = STRING(FI-Dato)
               CB-Butik:SENSITIVE   = FALSE
               FI-Dato:SENSITIVE    = FALSE
               FI-DatoTil:SENSITIVE = FALSE
               BUTTON-SokDato:SENSITIVE    = FALSE
               BUTTON-SokDatoTil:SENSITIVE = FALSE.
    END.
    PROCESS EVENTS.
    APPLY "VALUE-CHANGED" TO CB-Butik.
    APPLY "CHOOSE" TO B-Aktiver.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Artikkelkort fFrameWin 
PROCEDURE Artikkelkort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cArtikkelNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cGetVerdier  AS CHARACTER  NO-UNDO.
  
  ASSIGN cGetVerdier = STRING(LOOKUP("Artikkelnr",cFelter)).

  PUBLISH "FeltVerdier" (OUTPUT cArtikkelNr,cGetVerdier,"SAME").                         
  IF cArtikkelNr = "" THEN
    RETURN.
  FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(cArtikkelNr) NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN 
      RETURN.
  fLockvindu(TRUE).
  run w-vartkor (input recid(ArtBas), "ENDRE," + STRING(THIS-PROCEDURE)).
  fLockvindu(FALSE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Avancerat fFrameWin 
PROCEDURE Avancerat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     RUN tmpUtvalg IN h_Window (THIS-PROCEDURE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI fFrameWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME fMain.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI fFrameWin  _DEFAULT-ENABLE
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
  DISPLAY FI-Dato FI-DatoTil CB-TTId TG-Bankkort TG-Rabatt TG-Betalingskort 
          TG-Rekvisisjon CB-Butik FI-LBelopFra FI-LBelopTil TG-Gavekort TG-Sjekk 
          FI-KasseNr TG-VisBonglinje TG-SokNetto TG-Kredittkort TG-Systemkort 
          TG-MakulerteBL TG-Kupong1 FI-ForsNr TG-Makulerte FI-SelgerNr CB-Belop 
          FI-Kortnr FI-BelopFra FI-BelopTil FI-BhodeTxt FI-BhodeTxt-2 
      WITH FRAME fMain.
  ENABLE RECT-1 FI-Dato FI-DatoTil CB-TTId TG-Bankkort TG-Rabatt 
         TG-Betalingskort TG-Rekvisisjon B-KasseNr CB-Butik FI-LBelopFra 
         FI-LBelopTil TG-Gavekort TG-Sjekk B-KasseNrBlank TG-VisBonglinje 
         TG-SokNetto TG-Kredittkort TG-Systemkort TG-MakulerteBL TG-Kupong1 
         B-ForsNrBlank TG-Makulerte B-Artikkelkort B-Aktiver B-VisTrans 
         B-Nullstill B-SelgerNrBlank CB-Belop FI-Kortnr B-KortNrBlank 
         FI-BelopFra FI-BelopTil BUTTON-SokDatoTil B-SelgerNr B-ForsNr 
         BUTTON-SokDato FI-BhodeTxt FI-BhodeTxt-2 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixStrings fFrameWin 
PROCEDURE FixStrings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:
    /* Fältnamn;Labels;Antal decimaler(blank = 0);Rightcols(1 = right) */
    ASSIGN cHodeFelter    = FILL(",",NUM-ENTRIES(cFieldDefs) - 1)
           cHodeLabels    = cHodeFelter
           cHodeDecimaler = cHodeFelter
           cHodeRightCols = cHodeFelter.
    DO iCount = 1 TO NUM-ENTRIES(cFieldDefs):
        ASSIGN ENTRY(iCount,cHodeFelter)    = ENTRY(1,ENTRY(iCount,cFieldDefs),";")
               ENTRY(iCount,cHodeLabels)    = ENTRY(2,ENTRY(iCount,cFieldDefs),";")
               ENTRY(iCount,cHodeDecimaler) = ENTRY(3,ENTRY(iCount,cFieldDefs),";")
               ENTRY(iCount,cHodeRightCols) = ENTRY(4,ENTRY(iCount,cFieldDefs),";").
    END.
    ASSIGN cLinjeFelter    = FILL(",",NUM-ENTRIES(cBLFieldDefs) - 1)
           cLinjeLabels    = cLinjeFelter
           cLinjeDecimaler = cLinjeFelter
           cLinjeRightCols = cLinjeFelter.
    DO iCount = 1 TO NUM-ENTRIES(cBLFieldDefs):
        ASSIGN ENTRY(iCount,cLinjeFelter)    = ENTRY(1,ENTRY(iCount,cBLFieldDefs),";")
               ENTRY(iCount,cLinjeLabels)    = ENTRY(2,ENTRY(iCount,cBLFieldDefs),";")
               ENTRY(iCount,cLinjeDecimaler) = ENTRY(3,ENTRY(iCount,cBLFieldDefs),";")
               ENTRY(iCount,cLinjeRightCols) = ENTRY(4,ENTRY(iCount,cBLFieldDefs),";").
    END.
END.

/* default felter ær hodefelter */
ASSIGN cFelter    = cHodeFelter   
       cLabels    = cHodeLabels   
       cDecimaler = cHodeDecimaler
       cRightCols = cHodeRightCols.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetRecord fFrameWin 
PROCEDURE GetRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cRettning AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER iButikkNr LIKE BongHode.ButikkNr NO-UNDO.
    DEFINE OUTPUT PARAMETER iGruppeNr LIKE BongHode.GruppeNr NO-UNDO.
    DEFINE OUTPUT PARAMETER iKasseNr  LIKE BongHode.KasseNr  NO-UNDO.
    DEFINE OUTPUT PARAMETER dDato     LIKE BongHode.Dato     NO-UNDO.
    DEFINE OUTPUT PARAMETER iBongNr   LIKE BongHode.BongNr   NO-UNDO.
    DEFINE VARIABLE cYMD         AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDato        AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cVerdier     AS CHARACTER  NO-UNDO.
    IF CAN-DO("Prev,Next",cRettning) THEN DO:
        PUBLISH "FeltVerdier" (OUTPUT cVerdier,cGetTransVerdier,cRettning). 
        IF cVerdier = "" THEN
            RETURN.
        ASSIGN cYMD = ENTRY(3,cVerdier).
        IF SESSION:DATE-FORMAT = "ymd" THEN
            ASSIGN cDato = cYMD.
        ELSE IF SESSION:DATE-FORMAT = "mdy" THEN
            ASSIGN cDato = ENTRY(2,cYMD,"/") + "/" + ENTRY(3,cYMD,"/") + "/" + ENTRY(1,cYMD,"/").
        ELSE IF SESSION:DATE-FORMAT = "dmy" THEN
            ASSIGN cDato = ENTRY(3,cYMD,"/") + "/" + ENTRY(2,cYMD,"/") + "/" + ENTRY(1,cYMD,"/").
        ASSIGN iButikkNr = INT(ENTRY(1,cVerdier))
               iGruppeNr = 1
               iKasseNr  = INT(ENTRY(2,cVerdier))
               dDato     = DATE(cDato)
               iBongNr   = INT(ENTRY(4,cVerdier)).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCombos fFrameWin 
PROCEDURE InitCombos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        CB-Belop:LIST-ITEM-PAIRS = " " + cAlle + "," +
                                   ", < 0,< 0, = 0,= 0, > 0,> 0".
        CB-Belop:SCREEN-VALUE = "".
        /* Transtype start */

        CB-TTId:LIST-ITEM-PAIRS = " " + cAlle + ", ".
/*         FOR EACH Transtype WHERE (Transtype.TTId > 0 AND Transtype.TTId < 12) OR        */
/*                                  (Transtype.TTId > 49 AND Transtype.TTId < 70) NO-LOCK: */
        FOR EACH Transtype NO-LOCK WHERE Transtype.TTId > 0 AND TransType.Aktiv = TRUE 
            BY TransType.TTId:
          CB-TTId:ADD-LAST(STRING(Transtype.TTId,"zz9") + "   " +
                           REPLACE(TransType.Beskrivelse,","," "),string(Transtype.TTId)).
        END.
        CB-TTId:SCREEN-VALUE = " ".
        /* Transtype end */

        /* Butiker start */
        FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK.
        ASSIGN cListItemPairs  = "".
        /* Leta upp unika förekomster av butiker genom brukers team */
        FOR EACH ButikkTeam NO-LOCK WHERE ButikkTeam.BrGrpNr = Bruker.BrGrpNr AND
                                  ButikkTeam.TeamTypeId = 2 BY ButikkTeam.Beskrivelse.
            FOR EACH ButikkKobling OF ButikkTeam.
                FIND TT_TillgButikker WHERE TT_TillgButikker.Butik = ButikkKobling.butik NO-ERROR.
                IF NOT AVAIL TT_TillgButikker THEN DO:
                        CREATE TT_TillgButikker.
                        ASSIGN TT_TillgButikker.Butik = ButikkKobling.butik.
                END.
            END.
        END.
        FOR EACH TT_TillgButikker:
            FIND Butiker OF TT_TillgButikker NO-LOCK NO-ERROR.
            IF NOT AVAIL Butiker THEN
                NEXT.
            ASSIGN cTillgButikker = cTillgButikker + (IF cTillgButikker <> "" THEN "," ELSE "") + STRING(Butiker.Butik)
                   cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") +
                                    TRIM(STRING(Butiker.Butik,">>>999")) + "-" + Butiker.butnamn + "," + STRING(Butiker.Butik).
                   cUserDefaultBut = IF Butiker.Butik = Bruker.ButikkNr THEN STRING(Butiker.Butik) ELSE cUserDefaultBut.
        END.
        ASSIGN CB-Butik:LIST-ITEM-PAIRS = cListItemPairs
               CB-Butik:SCREEN-VALUE = IF cUserDefaultBut <> "" THEN cUserDefaultBut ELSE ENTRY(2,CB-Butik:LIST-ITEM-PAIRS).
/*         ASSIGN cUserDefaultBut = IF TRIM(cUserDefaultBut) = "" THEN TRIM(ENTRY(2,cListItemPairs)) ELSE cUserDefaultBut */
/*                CB-Butik:LIST-ITEM-PAIRS = cListItemPairs                                                               */
/*                CB-Butik:SCREEN-VALUE = cUserDefaultBut.                                                                */
        /* Butiker end */

/* gammal butikshantering */
/*       FOR EACH Butiker /* WHERE CAN-FIND(FIRST Kasse WHERE Kasse.Butik = Butiker.Butik) */ NO-LOCK:                                           */
/*           ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") + Butiker.ButNamn + "," + STRING(Butiker.Butik). */
/*       END.                                                                                                                                    */
/*       IF NUM-ENTRIES(cListItemPairs) > 2 THEN                                                                                                 */
/*           ASSIGN cListItemPairs = cAlle + ",," + cListItemPairs.                                                                              */
/*       ASSIGN CB-Butik:LIST-ITEM-PAIRS = cListItemPairs                                                                                        */
/*              CB-Butik:SCREEN-VALUE = IF NUM-ENTRIES(cListItemPairs) > 2 THEN " " ELSE ENTRY(2,cListItemPairs).                                */
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject fFrameWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */
  RUN FixStrings.
  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  PUBLISH "GetWindowH" (OUTPUT h_Window ).
  {syspara.i 1 100 1 cAlle}
  IF cAlle = "" THEN
      ASSIGN cAlle = "[Alle]".
  RUN initCombos.
  APPLY "VALUE-CHANGED" TO CB-TTId IN FRAME {&FRAME-NAME}.
  RUN InitVerdier.
  CB-Butik:SCREEN-VALUE = ENTRY(2,CB-Butik:LIST-ITEM-PAIRS).
  ASSIGN FI-Dato:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).
  ASSIGN cFilename = SESSION:TEMP-DIRECTORY + "gridbong.txt"
         FI-KasseNr  = "*"
         FI-KasseNr:SCREEN-VALUE   = cAlle
         FI-ForsNr  = "*"
         FI-ForsNr:SCREEN-VALUE   = cAlle
         FI-SelgerNr  = "*"
         FI-SelgerNr:SCREEN-VALUE = cAlle.
  APPLY "VALUE-CHANGED" TO CB-Butik.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitVerdier fFrameWin 
PROCEDURE InitVerdier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    DO iCount = 1 TO NUM-ENTRIES(cTransFelter):
        ASSIGN cHodeTransVerdier = cHodeTransVerdier + (IF cHodeTransVerdier = "" THEN "" ELSE ",") + STRING(LOOKUP(ENTRY(iCount,cTransFelter),cHodeFelter)).
    END.
    DO iCount = 1 TO NUM-ENTRIES(cTransFelter):
        ASSIGN cLinjeTransVerdier = cLinjeTransVerdier + (IF cLinjeTransVerdier = "" THEN "" ELSE ",") + STRING(LOOKUP(ENTRY(iCount,cTransFelter),cLinjeFelter)).
    END.
    /* default */
    ASSIGN cGetTransVerdier = cHodeTransVerdier.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Nullstill fFrameWin 
PROCEDURE Nullstill :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN FI-Dato:SCREEN-VALUE = STRING(TODAY)
               FI-DatoTil:SCREEN-VALUE = ?
               TG-Bankkort:CHECKED = FALSE
               TG-Betalingskort:CHECKED = FALSE
               TG-Gavekort:CHECKED = FALSE
               TG-Kredittkort:CHECKED = FALSE
               TG-Kupong1:CHECKED = FALSE
               TG-Rabatt:CHECKED = FALSE
               TG-Rekvisisjon:CHECKED = FALSE
               TG-Sjekk:CHECKED = FALSE
               TG-Systemkort:CHECKED = FALSE
               CB-Belop:SCREEN-VALUE = " "
               CB-TTId:SCREEN-VALUE = " ".
        APPLY "CHOOSE" TO B-ForsNrBlank.
        APPLY "CHOOSE" TO B-KasseNrBlank.
        APPLY "CHOOSE" TO B-SelgerNrBlank.
        APPLY "VALUE-CHANGED" TO CB-Belop.
        APPLY "VALUE-CHANGED" TO CB-TTId.
        APPLY "VALUE-CHANGED" TO TG-VisBonglinje.

    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevNext fFrameWin 
PROCEDURE PrevNext :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cRettning AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE pcState AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cArtikkelNr AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cGetVerdier  AS CHARACTER  NO-UNDO.
    ASSIGN cGetVerdier = STRING(LOOKUP("Artikkelnr",cFelter)).

    IF CAN-DO("Prev,Next",cRettning) THEN DO:
        PUBLISH "FeltVerdier" (OUTPUT cArtikkelNr,cGetVerdier,cRettning).
        IF cArtikkelNr = "" THEN
          RETURN.
        PUBLISH "ByttArtikkel" (DECI(cArtikkelNr)).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelgerStat fFrameWin 
PROCEDURE SelgerStat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cButiker  AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cRappType AS CHARACTER  NO-UNDO.
    
/*                                                     */
/*     RUN d-VelgStattype.w (INPUT INPUT FI-Dato,      */
/*                           INPUT INPUT FI-DatoTil,   */
/*                           CB-Butik:LIST-ITEM-PAIRS, */
/*                           INPUT-OUTPUT cValgteBut,  */
/*                           FI-ForsNr,                */
/*                           FI-SelgerNr).             */
/*                                                     */
/*     RUN                                             */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendFilterValues fFrameWin 
PROCEDURE SendFilterValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER cFilterVerdier AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cColAlign      AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cFstPeriode    AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cButikker      AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cPeriodeTmp    AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cPeriode       AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cFraAar        AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cTilAar        AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cFraPerLinNr   AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cTilPerLinNr   AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cTTId          AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  iCount         AS INTEGER    NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      FIND Butiker WHERE Butiker.Butik = INT(CB-Butik:SCREEN-VALUE) NO-LOCK NO-ERROR.
      IF CB-TTId:SCREEN-VALUE = "" THEN
          ASSIGN cTTId = TRIM(TRIM(cAlle,"["),"]").
      ELSE DO iCount = 1 TO NUM-ENTRIES(CB-TTId:SCREEN-VALUE):
          FIND TransType WHERE TransType.TTId = INT(ENTRY(iCount,CB-TTId:SCREEN-VALUE)) NO-LOCK NO-ERROR.
          IF AVAIL TransType THEN
              ASSIGN cTTId = cTTId + (IF cTTId = "" THEN "" ELSE ",") + TransType.Beskrivelse.
      END.
      ASSIGN cFilterVerdier = "Butikk: " + (IF AVAIL Butiker THEN Butiker.Butnamn ELSE "") + CHR(10) +
             "Dato: " + FI-Dato:SCREEN-VALUE + CHR(10) +
             "TransType: " + cTTId
             cColAlign = cRightCols.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetFilterParam fFrameWin 
PROCEDURE SetFilterParam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN pcFeltListe = "KasseNr,ForsNr,SelgerNr"
             pcVerdier   = FILL(CHR(1),NUM-ENTRIES(pcFeltListe) - 1).
      DO iCount = 1 TO NUM-ENTRIES(pcFeltListe):
        CASE ENTRY(iCount,pcFeltliste):
            WHEN "KasseNr" THEN
                ASSIGN ENTRY(iCount,pcVerdier,CHR(1)) = FI-KasseNr.
            WHEN "ForsNr" THEN
                ASSIGN ENTRY(iCount,pcVerdier,CHR(1)) = FI-ForsNr.
            WHEN "SelgerNr" THEN
                ASSIGN ENTRY(iCount,pcVerdier,CHR(1)) = FI-SelgerNr.
        END CASE.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetTillgKassSelger fFrameWin 
PROCEDURE SetTillgKassSelger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* cTillgKasserer */
    /* Tillgängliga selgere för den butik som är vald   */
    ASSIGN cTillgKasser   = ""
           cTillgSelger   = ""
           cTillgKasserer = "".
    DO WITH FRAME {&FRAME-NAME}:
        FOR EACH Kasse WHERE Kasse.butikknr = INT(CB-Butik:SCREEN-VALUE) AND Kasse.Aktiv = TRUE NO-LOCK:
            ASSIGN cTillgKasser = cTillgKasser + (IF cTillgKasser <> "" THEN "," ELSE "")
                                               + STRING(Kasse.KasseNr).
        END.
        FOR EACH ButikkSelger WHERE ButikkSelger.butikknr = INT(CB-Butik:SCREEN-VALUE) NO-LOCK:
            ASSIGN cTillgSelger = cTillgSelger + (IF cTillgSelger <> "" THEN "," ELSE "")
                                               + STRING(ButikkSelger.SelgerNr).
        END.
        FOR EACH ButikkSelger WHERE ButikkSelger.butikknr = INT(CB-Butik:SCREEN-VALUE) NO-LOCK:
            ASSIGN cTillgSelger = cTillgSelger + (IF cTillgSelger <> "" THEN "," ELSE "")
                                               + STRING(ButikkSelger.SelgerNr).
        END.
        FOR EACH butikkforsalj WHERE butikkforsalj.butik = INT(CB-Butik:SCREEN-VALUE) NO-LOCK:
            ASSIGN cTillgKasserer = cTillgKasserer + (IF cTillgKasserer <> "" THEN "," ELSE "")
                                               + STRING(butikkforsalj.ForsNr).
        END.
    END.
    APPLY "CHOOSE" TO B-ForsNrBlank.
    APPLY "CHOOSE" TO B-SelgerNrBlank.
    APPLY "CHOOSE" TO B-KasseNrBlank.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSok fFrameWin 
PROCEDURE StartSok :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcAndFeltListe AS CHAR NO-UNDO.
  DEF VAR iCount        AS INTE NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN pcBongLinje    = ""
           pcAndFeltListe = "KassererNr,SelgerNr,flBankkort,flBetalingskort,flGavekort,flKreditkort,flKupong1,flRabatt,flRekvisisasjon,flSjekk,flSystemkort,Kundekort,flMakulert"
           pcHodeVerdier = CB-Butik:SCREEN-VALUE
           pcHodeVerdier = pcHodeVerdier + CHR(1) + (IF FI-KasseNr = "*" THEN cTillgKasser ELSE FI-KasseNr)
           pcHodeVerdier = pcHodeVerdier + CHR(1) + FI-Dato:SCREEN-VALUE + "," + (IF INPUT FI-DatoTil = ? THEN FI-Dato:SCREEN-VALUE ELSE FI-DatoTil:SCREEN-VALUE).
    END.
    ASSIGN pcAndQuery =  (/* IF TRIM(CB-TTId:SCREEN-VALUE) <> "" THEN "" ELSE */ IF CB-Belop:SCREEN-VALUE = "" AND INPUT FI-BelopFra = 0 AND INPUT FI-BelopTil = 0 THEN "" ELSE 
                          IF CB-Belop:SCREEN-VALUE = "" AND INPUT FI-BelopTil = 0 THEN " AND BongHode.Belop = '" + FI-BelopFra:SCREEN-VALUE + "'" ELSE
                          IF CB-Belop:SCREEN-VALUE = "" THEN " AND BongHode.Belop >= '" + FI-BelopFra:SCREEN-VALUE + "' AND BongHode.Belop <= '" + FI-BelopTil:SCREEN-VALUE + "'"
                              ELSE IF CB-Belop:SCREEN-VALUE <> "" THEN " AND BongHode.Belop " + CB-Belop:SCREEN-VALUE ELSE "") +
                         (IF FI-ForsNr = "*" THEN "" ELSE " AND CAN-DO('" + FI-ForsNr + "',STRING(BongHode.KassererNr))") +
                         (IF FI-SelgerNr = "*" THEN "" ELSE " AND CAN-DO('" + FI-SelgerNr + "',STRING(BongHode.SelgerNr))").
    
    DO iCount = 1 TO NUM-ENTRIES(pcAndFeltListe):
        CASE ENTRY(iCount,pcAndFeltListe):
            WHEN "flBankkort" THEN IF TG-Bankkort:CHECKED THEN
                ASSIGN pcAndQuery = pcAndQuery + " AND BongHode.flBankkort = TRUE".
            WHEN "flBetalingskort" THEN IF TG-Betalingskort:CHECKED THEN
                ASSIGN pcAndQuery = pcAndQuery + " AND BongHode.flBetalingskort = TRUE".
            WHEN "flGavekort" THEN IF TG-Gavekort:CHECKED THEN
                ASSIGN pcAndQuery = pcAndQuery + " AND BongHode.flGavekort = TRUE".
            WHEN "flKreditkort" THEN IF TG-Kredittkort:CHECKED THEN
                ASSIGN pcAndQuery = pcAndQuery + " AND BongHode.flKreditkort = TRUE".
            WHEN "flKupong1" THEN IF TG-Kupong1:CHECKED THEN
                ASSIGN pcAndQuery = pcAndQuery + " AND BongHode.flKupong1 = TRUE".
            WHEN "flRabatt" THEN IF TG-Rabatt:CHECKED THEN
                ASSIGN pcAndQuery = pcAndQuery + " AND BongHode.flRabatt = TRUE".
            WHEN "flRekvisisasjon" THEN IF TG-Rekvisisjon:CHECKED THEN
                ASSIGN pcAndQuery = pcAndQuery + " AND BongHode.flRekvisisasjon = TRUE".
            WHEN "flSjekk" THEN IF TG-Sjekk:CHECKED THEN
                ASSIGN pcAndQuery = pcAndQuery + " AND BongHode.flSjekk = TRUE".
            WHEN "flSystemkort" THEN IF TG-Systemkort:CHECKED THEN
                ASSIGN pcAndQuery = pcAndQuery + " AND BongHode.flSystemkort = TRUE".
            WHEN "flMakulert" THEN IF TG-Makulerte:CHECKED THEN
                ASSIGN pcAndQuery = pcAndQuery + " AND BongHode.Makulert = 2".
            WHEN "Kundekort" THEN IF FI-Kortnr:SCREEN-VALUE <> "0" THEN
                ASSIGN pcAndQuery = pcAndQuery + " AND BongHode.Kundekort = '" + FI-Kortnr:SCREEN-VALUE + "'".
        END CASE.
    END.
    IF TRIM(CB-TTId:SCREEN-VALUE) <> "" THEN DO:
        ASSIGN pcBongLinje = CB-TTId:SCREEN-VALUE + CHR(1).

        IF INPUT FI-LBelopFra <> 0 OR INPUT FI-LBelopTil <> 0 THEN DO:
            ASSIGN pcBongLinje = pcBongLinje + STRING(INPUT FI-LBelopFra) + ";" +
                           IF INPUT FI-LBelopTil = 0 THEN STRING(INPUT FI-LBelopFra) ELSE STRING(INPUT FI-LBelopTil).
        END.
        ASSIGN pcBongLinje = pcBongLinje + CHR(1) + STRING(TG-VisBongLinje:CHECKED,"J/N") + CHR(1) + STRING(TG-SokNetto:CHECKED,"J/N") + CHR(1) + STRING(TG-MakulerteBL:CHECKED,"J/N").
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viewObject fFrameWin 
PROCEDURE viewObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  PUBLISH "ClearGrid" (cLabels).
  APPLY "ENTRY" TO FI-Dato IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLockvindu fFrameWin 
FUNCTION fLockvindu RETURNS CHARACTER
  ( INPUT lLock AS LOGICAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hDetteVindu AS HANDLE     NO-UNDO.
  ASSIGN hDetteVindu = THIS-PROCEDURE:CURRENT-WINDOW
         hDetteVindu:SENSITIVE = NOT lLock.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSumFelter fFrameWin 
FUNCTION getSumFelter RETURNS CHARACTER
  ( INPUT cFeltnavnListe AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cFeltNumListe AS CHARACTER  NO-UNDO.
  ASSIGN cFeltNumListe = FILL(",",NUM-ENTRIES(cFeltnavnListe) - 1).
  DO iCount = 1 TO NUM-ENTRIES(cFeltnavnListe):
      ASSIGN ENTRY(iCount,cFeltNumListe) = STRING(LOOKUP(ENTRY(iCount,cFeltnavnListe),cFelter)).
  END.
  RETURN cFeltNumListe.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

