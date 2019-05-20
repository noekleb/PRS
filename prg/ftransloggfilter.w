&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE tmpLevBas NO-UNDO LIKE LevBas.



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

DEFINE VARIABLE cLabels    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFelter    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTidFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFilename  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAlle      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDecimaler AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRightCols AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFieldDefs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSummerFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTransFelter  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cGetTransVerdier AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cUserDefaultBut  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgButikker AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgKasserer AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgSelger   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE h_Window     AS HANDLE     NO-UNDO.
DEFINE VARIABLE pcFeltListe AS CHARACTER  NO-UNDO.
DEFINE VARIABLE pcVerdier   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikerRowIdList AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikerIdList    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lButikkBruker  AS LOGICAL     NO-UNDO.

/* ASSIGN cFelter = "Butik,KassaNr,ProfilNr,TTId,ArtikkelNr,Kode,Storl,Antall,Pris,RabKr,Mva,KalkylePris,VVarekost,ForsNr,SelgerNr,KortNr,KortType,KundNr,LevNr,Vg,LopNr,MedlemsNr,Dato,Tid,LinjeRab,PersonalRab,SubtotalRab,BatchNr,TransNr,BongId,BongLinjeNr,Postert,PostertDato,PostertTid"                         */
/*        cLabels = "Butikk,Kasse,Profil,TrTypeId,Artikkel,Strekkode,Størrelse,Antall,Pris,Rabatt,Mva,Kalkylepris,Vektet varekost,Kasserer,Selger,Kortnr,KortType,Kundenr,Leverandør,VgNr,LpNr,Medlemsnr,Transdato,Tid,Linjerab,Personalrab,Subtotalrab,BatchNr,TransNr,BongID,LinjeNr,Postert,PostertDato,Postert tid" */
       cTidFelter = "PostertTid,Tid,".
/*        cDecimaler = ",,,,,,2,2,2,,,,2,2,2,,,,,,,,,2,2,2,,,,,,," /* för hantering av decimaler */                                                                                                                                                                                                                     */
/*        cRightCols = "1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1". /* 1=right-align */                                                                                                                                                                                                       */
/* 
ASSIGN cFelter = "Butik,KassaNr,ProfilNr,ArtikkelNr,Kode,Storl,Antall,Pris,RabKr,LinjeRab,PersonalRab,SubtotalRab,Mva,KalkylePris,VVarekost,ForsNr,SelgerNr,BongId,BongLinjeNr,KortNr,KortType,KundNr,LevNr,Vg,LopNr,MedlemsNr,Dato,Tid,BatchNr,TransNr,TTId,Postert,PostertDato,PostertTid"
       cLabels = "Butikknummer,KasseNr,Prisprofil,Artikkelnummer,Strekkode,Størrelse,Antall,Pris,Rabatt,Linjerabatt,Personalrabatt,Subtotalrabatt,Mva,Kalkylepris,Vektet varekost,Kasserernummer,Selgernummer,BongID,LinjeNr,Kortnummer,KortType,Kundenummer,Leverandør,VgNr,LpNr,Medlemsnummer,Transaksjonsdato,Tid,BatchNummer,TransaksjonsNr,TransTypeId,Postert,PostertDato,Postert tid"
 ASSIGN cFelter = "Butik,KassaNr,ProfilNr,ArtikkelNr,Kode,Storl,Antall,Pris,RabKr,LinjeRab,PersonalRab,SubtotalRab,Mva,KalkylePris,VVarekost,SattVVareKost,ForsNr,SelgerNr,BatchNr,BongId,BongLinjeNr,Dato,KortNr,KortType,KundNr,LevNr,Vg,LopNr,MedlemsNr,Postert,PostertDato,PostertTid,TBId,Tid,TransNr,TTId"
       cLabels = "Butikknummer,KasseNr,Prisprofil,Artikkelnummer,Strekkode,Størrelse,Antall,Pris,Rabatt,Linjerabatt,Personalrabatt,Subtotalrabatt,Mva,Kalkylepris,Vektet varekost,SattVVareKost,Kasserernummer,Selgernummer,BatchNummer,BongID,LinjeNr,Transaksjonsdato,Kortnummer,KortType,Kundenummer,Leverandør,VgNr,LpNr,Medlemsnummer,Postert,PostertDato,Postert tid,Transaksjonstype beskrivelse,Tid,TransaksjonsNr,TransTypeId"
 */

/* Fältnamn;Labels;Antal decimaler(blank = 0);Rightcols(1 = right) */
ASSIGN cTransFelter = "Butik,KassaNr,Dato,BongId"
       cSummerFelter = "Antall,SumNetto,Mva,SumVk,SumDBKr"

cFieldDefs = "Butik;Butikk;;1," +
             "KassaNr;Kasse;;1," +
             "ArtikkelNr;Artikkel;;1," +
/*              "Beskr;Beskrivelse;;," + */
             "Bongtekst;Bongtekst;;," +
             "Levkod;Levkod;;," +
             "LevFargKod;LevFarg;;," +
             "TTId;TTId;;1," +
             "Kode;Strekkode;;1," +
             "Storl;Størrelse;;1," +
             "Lagervara;Lagervare;;1," +
             "Antall;Antall;2;1," +
             "Pris;Veil. pris;2;1," +
             "NettoPris;Pris inkl. rab;2;1," +
             "SumNetto;Sum netto;2;1," +
    "Dbkr;DB kr;2;1," +
    "SumDBKr;Sum DB kr;2;1," +
    "Db%;DB%;1;1," +
             "RabKr;Rabatt;2;1," +
             "Rab%;Rab%;1;1," +
/*              "Feilkode;FK;;1," + */
             "Mva;Mva;2;1," +
             "Mva%;Mva%;2;1," +
/*              "KalkylePris;Kalkylepris;2;1," + */
             "VVarekost;Vektet varekost;2;1," +
             "SumVk;Sum Vektet Vk;2;1," +
    "Varekost;Varekost;2;1," +
    "SolgtNegativt;NegLager;;1," +
    "Sesongtxt;Sesong;;," +
    "ForsNr;Kasserer;;1," +

        "Kasserernavn;Kasserernavn;;," +
             "SelgerNr;Selger;;1," +
    "Selgernavn;Selgernavn;;1," +
             "KortNr;Kortnr;;1," +
             "KortType;KortType;;1," +
             "KundNr;Kundenr;;1," +
    "Kundenavn;Kundenavn;;," +
             "Vg;VgNr;;1," +
    "VgBeskr;Vgnavn;;," +
             "LevNr;Leverandør;;1," +
    "Levnamn;Levnavn;;," +
             "LopNr;LpNr;;1," +
             "MedlemsNr;Medlemsnr;;1," +
    "Medlemnavn;Medlemsnavn;;," +
             "Dato;Transdato-YMD;;1," +
             "Tid;Transtid;;1," +
             "LinjeRab;Linjerab;2;1," +
             "PersonalRab;Personalrab;2;1," +
             "SubtotalRab;Subtotalrab;2;1," +
    "OvButik;Ov.butikk;;1," +
    "TilStorl;Til strl;;1," +
             "BatchNr;BatchNr;;1," +
             "TransNr;TransNr;;1," +
             "BongId;BongID;;1," +
             "BongLinjeNr;LinjeNr;;1," +
    "RefNr;RefNr;;1," +
    "RefTekst;RefTekst;;1," +
             "Plukket;Plukket;;," +
             "Postert;Postert;;1," +
             "PostertDato;PostertDato;;1," +
             "PostertTid;Postert tid;;," +
             "KampId;Kampanje;;," +
             "KampTilbId;Kamptilbud;;".             
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
&Scoped-Define ENABLED-OBJECTS FI-Dato FI-DatoTil B-AvdelingBlank ~
B-KasseNrBlank B-Aktiver B-HgBlank B-ForsNrBlank CB-Butik B-VgBlank ~
B-SelgerNrBlank B-VisTrans CB-TTId B-LevNrBlank B-KundeNrBlank ~
B-Artikkelkort B-TranstyperBlank FI-Kortnr B-KortNrBlank TG-AvFilter ~
TG-Neglager B-KundeNr B-Transtyper BUTTON-SokBut B-Avdeling B-KasseNr ~
B-LevNr B-HuvGr B-VarGr BUTTON-SokDatoTil B-SelgerNr B-ForsNr ~
BUTTON-SokDato 
&Scoped-Define DISPLAYED-OBJECTS FI-Dato FI-DatoTil FI-Avdeling FI-KasseNr ~
FI-Butikker FI-HuvGr FI-ForsNr CB-Butik FI-VarGr FI-SelgerNr CB-TTId ~
FI-LevNr FI-KundeNr FI-Transtyper FI-Kortnr TG-AvFilter TG-Neglager 

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
DEFINE VARIABLE h_dtranslogg AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Aktiver 
     LABEL "&Aktiver" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Artikkelkort 
     LABEL "Arti&kkelkort" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Avdeling  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-AvdelingBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-ForsNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-ForsNrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-HgBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-HuvGr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-KasseNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-KasseNrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-KortNrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-KundeNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-KundeNrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-LevNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-LevNrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SelgerNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SelgerNrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-Transtyper  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-TranstyperBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-VarGr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-VgBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-VisTrans 
     IMAGE-UP FILE "icon/e-detail":U
     LABEL "&Vis transaksjon" 
     SIZE 4.4 BY 1.14.

DEFINE BUTTON BUTTON-SokBut 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDatoTil 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-Butik AS CHARACTER FORMAT "X(256)" INITIAL "0" 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEM-PAIRS "","1"
     DROP-DOWN-LIST
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE CB-TTId AS CHARACTER FORMAT "X(256)" INITIAL "0" 
     LABEL "TransTypeId" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "","0"
     DROP-DOWN-LIST
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Avdeling AS CHARACTER FORMAT "X(10)":U 
     LABEL "Avdeling" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Butikker AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

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
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-HuvGr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Hovedgruppe" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KasseNr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Kassenr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Kortnr AS DECIMAL FORMAT ">>>>>>>>>>>9":U INITIAL 0 
     LABEL "Kundekort" 
     VIEW-AS FILL-IN 
     SIZE 18.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KundeNr AS CHARACTER FORMAT "X(256)" 
     LABEL "Kundenummer" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Levnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SelgerNr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Selger" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Transtyper AS CHARACTER FORMAT "X(10)":U 
     LABEL "Transtyper" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VarGr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE TG-AvFilter AS LOGICAL INITIAL no 
     LABEL "Velg artikkler" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Neglager AS LOGICAL INITIAL no 
     LABEL "Neg. lager" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.6 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FI-Dato AT ROW 1.19 COL 14 COLON-ALIGNED HELP
          "Transaksjonsdato"
     FI-DatoTil AT ROW 1.19 COL 32 COLON-ALIGNED HELP
          "Transaksjonsdato" NO-LABEL
     FI-Avdeling AT ROW 1.19 COL 75.6 COLON-ALIGNED
     B-AvdelingBlank AT ROW 1.19 COL 97.2
     FI-KasseNr AT ROW 1.19 COL 121.6 COLON-ALIGNED
     B-KasseNrBlank AT ROW 1.19 COL 143.2
     FI-Butikker AT ROW 2.19 COL 14 COLON-ALIGNED
     B-Aktiver AT ROW 2.19 COL 47
     FI-HuvGr AT ROW 2.19 COL 75.6 COLON-ALIGNED
     B-HgBlank AT ROW 2.19 COL 97.2
     FI-ForsNr AT ROW 2.19 COL 121.6 COLON-ALIGNED
     B-ForsNrBlank AT ROW 2.19 COL 143.2
     CB-Butik AT ROW 3.19 COL 14 COLON-ALIGNED HELP
          "Butikknummer"
     FI-VarGr AT ROW 3.19 COL 75.6 COLON-ALIGNED
     B-VgBlank AT ROW 3.19 COL 97.2
     FI-SelgerNr AT ROW 3.19 COL 121.6 COLON-ALIGNED
     B-SelgerNrBlank AT ROW 3.19 COL 143.2
     B-VisTrans AT ROW 3.33 COL 47
     CB-TTId AT ROW 4.19 COL 14 COLON-ALIGNED HELP
          "TransaksjonstypensID"
     FI-LevNr AT ROW 4.19 COL 75.6 COLON-ALIGNED
     B-LevNrBlank AT ROW 4.19 COL 97.2
     FI-KundeNr AT ROW 4.19 COL 121.6 COLON-ALIGNED HELP
          "Kundenummer"
     B-KundeNrBlank AT ROW 4.19 COL 143.2
     B-Artikkelkort AT ROW 4.48 COL 47
     FI-Transtyper AT ROW 5.19 COL 14 COLON-ALIGNED
     B-TranstyperBlank AT ROW 5.19 COL 36.6
     FI-Kortnr AT ROW 5.19 COL 121.6 COLON-ALIGNED
     B-KortNrBlank AT ROW 5.19 COL 143.2
     TG-AvFilter AT ROW 5.33 COL 77.6
     TG-Neglager AT ROW 6.19 COL 77.6
     B-KundeNr AT ROW 4.19 COL 138.2 NO-TAB-STOP 
     B-Transtyper AT ROW 5.19 COL 31.4 NO-TAB-STOP 
     BUTTON-SokBut AT ROW 2.19 COL 29.2 NO-TAB-STOP 
     B-Avdeling AT ROW 1.19 COL 92.2 NO-TAB-STOP 
     B-KasseNr AT ROW 1.19 COL 138.2 NO-TAB-STOP 
     B-LevNr AT ROW 4.19 COL 92.2 NO-TAB-STOP 
     B-HuvGr AT ROW 2.19 COL 92.2 NO-TAB-STOP 
     B-VarGr AT ROW 3.19 COL 92.2 NO-TAB-STOP 
     BUTTON-SokDatoTil AT ROW 1.19 COL 47.6
     B-SelgerNr AT ROW 3.19 COL 138.2 NO-TAB-STOP 
     B-ForsNr AT ROW 2.19 COL 138.2 NO-TAB-STOP 
     BUTTON-SokDato AT ROW 1.19 COL 29.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 155 BY 6.29.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: tmpLevBas T "NEW SHARED" NO-UNDO skotex LevBas
   END-TABLES.
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
         HEIGHT             = 6.33
         WIDTH              = 155.
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

/* SETTINGS FOR FILL-IN FI-Avdeling IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Butikker IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ForsNr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-HuvGr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KasseNr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KundeNr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LevNr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-SelgerNr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Transtyper IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VarGr IN FRAME fMain
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
    DEFINE VARIABLE iTime1 AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iTime2 AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iTime3 AS INTEGER    NO-UNDO.
    DEFINE VARIABLE ocButiker AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE ocTTId    AS CHARACTER  NO-UNDO.
    
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
    ETIME(TRUE).
    RUN StartSok.
/*     iTime2 = ETIME. */
    IF TG-AvFilter:CHECKED THEN DO:
        /* Vi kör startsok för att få queryn initierad och använder den i sdo'n från startsokdyn */
      APPLY "VALUE-CHANGED" TO TG-AvFilter.
      RUN Avancerat.
      RETURN.
    END.
    RUN SetFilterParam (OUTPUT ocButiker,OUTPUT ocTTId).
/*     iTime3 = ETIME.                           */
/*     MESSAGE "Polygon test, skal fjernes" SKIP */
/*     iTime1 SKIP iTime2 SKIP iTime3            */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.    */
    PUBLISH "VisTxtBox" ("Søker data......").

/* för test */
/*     MESSAGE "NY TEST Ja/Nej"                                                                                 */
/*         VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE lsvar AS logi.                                          */
/*     IF lsvar = TRUE THEN DO:                                                                                 */
/*         iTime1 = ETIME.                                                                                      */
/*         RUN TransLoggToTTNY IN h_dtranslogg                                                                  */
/*           ( OUTPUT TTH,pcFeltListe,pcVerdier,?,ocButiker,ocTTId,INPUT INPUT FI-Dato,INPUT INPUT FI-DatoTil). */
/*     END.                                                                                                     */
/*     ELSE DO:                                                                                                 */
/*         iTime1 = ETIME.                                                                                      */
/*         RUN TransLoggToTT IN h_dtranslogg                                                                    */
/*           ( OUTPUT TTH,pcFeltListe,pcVerdier,?).                                                             */
/*     END.                                                                                                     */
/*     MESSAGE ETIME                                                                                            */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                   */
/* end test */
    RUN TransLoggToTTNY IN h_dtranslogg
      ( OUTPUT TTH,pcFeltListe,pcVerdier,?,ocButiker,ocTTId,INPUT INPUT FI-Dato,INPUT INPUT FI-DatoTil).

    CREATE QUERY qh.
    qh:SET-BUFFERS(TTH).
    qh:QUERY-PREPARE("for each TT_TransLogg").
    qh:QUERY-OPEN().

/*   RUN rappgenqry.p ("Translogg",DYNAMIC-FUNCTION('getQueryWhere':U IN h_dtranslogg),cFileName,cLabels,cFelter,cDecimaler,cTidFelter,?). */
/*   RUN rappgenqry.p ("Translogg","",cFileName,cLabels,cFelter,cDecimaler,cTidFelter,DYNAMIC-FUNCTION('getQueryHandle':U IN h_dtranslogg)). */
  PUBLISH "VisTxtBox" ("Leser ut data......").
  RUN rappgenqry.p ("TT_Translogg","",cFileName,cLabels,cFelter,cDecimaler,cTidFelter,qh).

  DO:
      PUBLISH "VisTxtBox" ("Leser inn og bearbeider data......").
      PUBLISH "LoadGrid" (cFileName,6).  /* 3 = antall frozen cols  */
      PUBLISH "AlignCol" (LOOKUP("Storl",cFelter),8). /* högerjustert */
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


&Scoped-define SELF-NAME B-Avdeling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Avdeling fFrameWin
ON CHOOSE OF B-Avdeling IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "Avdeling;AvdelingNr;AvdelingNavn",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "AvdelingNr",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-Avdeling:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-Avdeling     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-Avdeling:TOOLTIP = IF FI-Avdeling = "*" THEN "" ELSE FI-Avdeling.
        IF FI-Avdeling <> "*" THEN DO:
            APPLY "CHOOSE" TO B-HgBlank.
            APPLY "CHOOSE" TO B-VgBlank.
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
                   FI-Avdeling:BGCOLOR = 11.
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
                   FI-Avdeling:BGCOLOR = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-AvdelingBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AvdelingBlank fFrameWin
ON CHOOSE OF B-AvdelingBlank IN FRAME fMain /* Blank */
DO:
    IF FI-Avdeling:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-Avdeling:SCREEN-VALUE = cAlle
               FI-Avdeling              = "*"
               FI-Avdeling:TOOLTIP      = ""
               FI-Avdeling:BGCOLOR      = ?
               B-Avdeling:PRIVATE-DATA  = "".
    END.
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
    FI-ForsNr:BGCOLOR = IF FI-ForsNr = "*" THEN ? ELSE 11.
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


&Scoped-define SELF-NAME B-HgBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-HgBlank fFrameWin
ON CHOOSE OF B-HgBlank IN FRAME fMain /* Blank */
DO:
    IF FI-HuvGr:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-HuvGr:SCREEN-VALUE = cAlle
               FI-HuvGr              = "*"
               FI-HuvGr:TOOLTIP      = ""
               FI-HuvGr:BGCOLOR      = ?
               B-HuvGr:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-HuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-HuvGr fFrameWin
ON CHOOSE OF B-HuvGr IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "HuvGr;Hg;HgBeskr",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "Hg",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-HuvGr:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-HuvGr     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-HuvGr:TOOLTIP = IF FI-HuvGr = "*" THEN "" ELSE FI-HuvGr.
        IF FI-HuvGr <> "*" THEN DO:
            APPLY "CHOOSE" TO B-AvdelingBlank.
            APPLY "CHOOSE" TO B-VgBlank.
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
                   FI-HuvGr:BGCOLOR = 11.
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
                   FI-HuvGr:BGCOLOR = ?.
    END.
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

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
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
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
                   FI-KasseNr:BGCOLOR = 11.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
                   FI-KasseNr:BGCOLOR = ?.
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


&Scoped-define SELF-NAME B-KortNrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KortNrBlank fFrameWin
ON CHOOSE OF B-KortNrBlank IN FRAME fMain /* Blank */
DO:
    ASSIGN FI-Kortnr:SCREEN-VALUE = "0".
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KundeNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KundeNr fFrameWin
ON CHOOSE OF B-KundeNr IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.
    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    this-procedure:current-window:sensitive = no.
    RUN JBoxSelector.w (THIS-PROCEDURE,0,
                        "Kunde;KundeNr;Navn;Kilde;TilgKilde",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "KundeNr",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    this-procedure:current-window:sensitive = yes.
    IF bOK THEN DO:
        assign
          FI-KundeNr:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-KundeNr     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-KundeNr:TOOLTIP = IF FI-KundeNr = "*" THEN "" ELSE FI-KundeNr.
        IF FI-KundeNr <> "*" THEN
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
                   FI-KundeNr:BGCOLOR = 11.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
                   FI-KundeNr:BGCOLOR = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KundeNrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KundeNrBlank fFrameWin
ON CHOOSE OF B-KundeNrBlank IN FRAME fMain /* Blank */
DO:
    IF FI-KundeNr:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-KundeNr:SCREEN-VALUE = cAlle
               FI-KundeNr              = "*"
               FI-KundeNr:TOOLTIP      = ""
               FI-KundeNr:BGCOLOR      = ?
               B-KundeNr:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevNr fFrameWin
ON CHOOSE OF B-LevNr IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "LevBas;LevNr;Levnamn",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "LevNr",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-Levnr:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-Levnr     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-Levnr:TOOLTIP = IF FI-Levnr = "*" THEN "" ELSE FI-Levnr.
        IF FI-Levnr <> "*" THEN
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
                   FI-LevNr:BGCOLOR = 11.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
                   FI-LevNr:BGCOLOR = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevNrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevNrBlank fFrameWin
ON CHOOSE OF B-LevNrBlank IN FRAME fMain /* Blank */
DO:
    IF FI-LevNr:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-LevNr:SCREEN-VALUE = cAlle
               FI-LevNr              = "*"
               FI-LevNr:TOOLTIP      = ""
               FI-LevNr:BGCOLOR      = ?
               B-LevNr:PRIVATE-DATA  = "".
    END.
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
    FI-SelgerNr:BGCOLOR = IF FI-SelgerNr = "*" THEN ? ELSE 11.
    
    
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


&Scoped-define SELF-NAME B-Transtyper
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Transtyper fFrameWin
ON CHOOSE OF B-Transtyper IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "Transtype;TTId;Beskrivelse",
                        "WHERE TTId < 12",
                        INPUT-OUTPUT cRowIdList,
                        "TTId",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-Transtyper:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-Transtyper     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-Transtyper:TOOLTIP = IF FI-Transtyper = "*" THEN "" ELSE FI-Transtyper.
        IF FI-Transtyper <> "*" THEN DO:
            ASSIGN SELF:PRIVATE-DATA     = cRowIdList + CHR(1) + cIdList
                   FI-Transtyper:BGCOLOR = 11
                   CB-TTId:SCREEN-VALUE = " ".
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
                   FI-Transtyper:BGCOLOR = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-TranstyperBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-TranstyperBlank fFrameWin
ON CHOOSE OF B-TranstyperBlank IN FRAME fMain /* Blank */
DO:
    IF FI-Transtyper:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-Transtyper:SCREEN-VALUE = cAlle
               FI-Transtyper            = "*"
               FI-Transtyper:TOOLTIP      = ""
               FI-Transtyper:BGCOLOR      = ?
               B-Transtyper:PRIVATE-DATA  = "".
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VarGr fFrameWin
ON CHOOSE OF B-VarGr IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "VarGr;Vg;VgBeskr",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "Vg",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-VarGr:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-VarGr     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-VarGr:TOOLTIP = IF FI-VarGr = "*" THEN "" ELSE FI-VarGr.
        IF FI-VarGr <> "*" THEN DO:
            APPLY "CHOOSE" TO B-AvdelingBlank.
            APPLY "CHOOSE" TO B-HgBlank.
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
                   FI-VarGr:BGCOLOR = 11.
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
                   FI-VarGr:BGCOLOR = ?.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VgBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VgBlank fFrameWin
ON CHOOSE OF B-VgBlank IN FRAME fMain /* Blank */
DO:
    IF FI-VarGr:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-VarGr:SCREEN-VALUE = cAlle
               FI-VarGr              = "*"
               FI-VarGr:TOOLTIP      = ""
               FI-VarGr:BGCOLOR      = ?
               B-VarGr:PRIVATE-DATA  = "".
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
    RUN gviskvittokopi2.w (INT(ENTRY(1,cVerdier)),
                           1,
                           INT(ENTRY(2,cVerdier)),
                           DATE(cDato),
/*                            DATE(ENTRY(3,cVerdier)), */
                           INT(ENTRY(4,cVerdier)),
                           THIS-PROCEDURE).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokBut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokBut fFrameWin
ON CHOOSE OF BUTTON-SokBut IN FRAME fMain /* ... */
or F10 of BUTTON-SokBut
DO:
/*    DEFINE VARIABLE cButikerRowIdList AS CHARACTER  NO-UNDO. */
/*    DEFINE VARIABLE cButikerIdList    AS CHARACTER  NO-UNDO. */
   DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "Butiker;Butik;ButNamn",
                        "where CAN-DO('" + cTillgButikker + "',STRING(Butiker.Butik))",
                        INPUT-OUTPUT cButikerRowIdList,
                        "Butik",
                        INPUT-OUTPUT cButikerIdList,
                        "","",
                        OUTPUT bOK).
/*     "where can-do('" + cTillgButikker + "',string(butik))" , */
    IF bOK THEN DO:
        RUN FixButikVis.
    END.
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
ON VALUE-CHANGED OF CB-TTId IN FRAME fMain /* TransTypeId */
DO:
  IF INT(CB-TTId:SCREEN-VALUE) > 0 THEN
      APPLY "CHOOSE" TO B-TranstyperBlank.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-AvFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-AvFilter fFrameWin
ON VALUE-CHANGED OF TG-AvFilter IN FRAME fMain /* Velg artikkler */
DO:
  APPLY "CHOOSE" TO B-AvdelingBlank.
  APPLY "CHOOSE" TO B-HgBlank.
  APPLY "CHOOSE" TO B-LevNrBlank.
  APPLY "CHOOSE" TO B-VgBlank.
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
             INPUT  'sdo/dtranslogg.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedtransloggOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dtranslogg ).
       RUN repositionObject IN h_dtranslogg ( 5.33 , 98.00 ) NO-ERROR.
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
  IF lButikkBruker = TRUE THEN
      RUN ArtBasVisTime.w (THIS-PROCEDURE,artbas.artikkelnr).
  ELSE
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
  DISPLAY FI-Dato FI-DatoTil FI-Avdeling FI-KasseNr FI-Butikker FI-HuvGr 
          FI-ForsNr CB-Butik FI-VarGr FI-SelgerNr CB-TTId FI-LevNr FI-KundeNr 
          FI-Transtyper FI-Kortnr TG-AvFilter TG-Neglager 
      WITH FRAME fMain.
  ENABLE FI-Dato FI-DatoTil B-AvdelingBlank B-KasseNrBlank B-Aktiver B-HgBlank 
         B-ForsNrBlank CB-Butik B-VgBlank B-SelgerNrBlank B-VisTrans CB-TTId 
         B-LevNrBlank B-KundeNrBlank B-Artikkelkort B-TranstyperBlank FI-Kortnr 
         B-KortNrBlank TG-AvFilter TG-Neglager B-KundeNr B-Transtyper 
         BUTTON-SokBut B-Avdeling B-KasseNr B-LevNr B-HuvGr B-VarGr 
         BUTTON-SokDatoTil B-SelgerNr B-ForsNr BUTTON-SokDato 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixButikVis fFrameWin 
PROCEDURE FixButikVis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      IF cButikerIdList <> "" THEN
          ASSIGN CB-Butik:LIST-ITEM-PAIRS = ",INGEN"
                 FI-Butikker = cButikerIdList
                 FI-Butikker:BGCOLOR = 15
                 FI-Butikker:SCREEN-VALUE = "(" + STRING(NUM-ENTRIES(cButikerIdList,"|")) + ")"
                 FI-Butikker:TOOLTIP = REPLACE(cButikerIdList,"|",",")
                 CB-Butik:SCREEN-VALUE = "INGEN"
                 CB-Butik:SENSITIVE    = FALSE.
      ELSE
          ASSIGN FI-Butikker:BGCOLOR = ?
                 FI-Butikker = ""
                 FI-Butikker:SCREEN-VALUE = ""
                 FI-Butikker:TOOLTIP = ""
                 CB-Butik:LIST-ITEM-PAIRS = cListItemPairs
                 CB-Butik:SCREEN-VALUE    = IF cUserDefaultBut <> "" AND CAN-DO(cTillgButikker,cUserDefaultBut) THEN 
                                                    cUserDefaultBut ELSE ENTRY(2,cListItemPairs)
                 CB-Butik:SENSITIVE    = TRUE.
                 .
     APPLY "VALUE-CHANGED" TO CB-Butik.
  END.
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
    ASSIGN cFelter = FILL(",",NUM-ENTRIES(cFieldDefs) - 1)
           cLabels = cFelter
           cDecimaler = cFelter
           cRightCols = cFelter.
    DO iCount = 1 TO NUM-ENTRIES(cFieldDefs):
        ASSIGN ENTRY(iCount,cFelter) = ENTRY(1,ENTRY(iCount,cFieldDefs),";")
               ENTRY(iCount,cLabels) = ENTRY(2,ENTRY(iCount,cFieldDefs),";")
               ENTRY(iCount,cDecimaler) = ENTRY(3,ENTRY(iCount,cFieldDefs),";")
               ENTRY(iCount,cRightCols) = ENTRY(4,ENTRY(iCount,cFieldDefs),";").
    END.
END.

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
/*     DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO. */
    DO WITH FRAME {&FRAME-NAME}:
        /* Transtype start */
        CB-TTId:LIST-ITEM-PAIRS = " " + cAlle + ",".
        FOR EACH Transtype NO-LOCK:
            IF CAN-DO('1,2,3,4,5,6,7,8,9,10,11,109',STRING(TransType.TTId)) THEN 
                CB-TTId:ADD-LAST(STRING(Transtype.TTId,"zz9") + "   " + 
                           REPLACE(TransType.Beskrivelse,","," "),string(Transtype.TTId)).
        END.
        CB-TTId:SCREEN-VALUE = "1".
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
                                    Butiker.butnamn + "," + STRING(Butiker.Butik).
                   cUserDefaultBut = IF Butiker.Butik = Bruker.ButikkNr THEN STRING(Butiker.Butik) ELSE cUserDefaultBut.
        END.
        ASSIGN cUserDefaultBut = IF TRIM(cUserDefaultBut) = "" THEN TRIM(ENTRY(2,cListItemPairs)) ELSE cUserDefaultBut
               CB-Butik:LIST-ITEM-PAIRS = cListItemPairs
               CB-Butik:SCREEN-VALUE = cUserDefaultBut.
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
  ASSIGN h_Window = SOURCE-PROCEDURE.
  FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
  IF AVAIL bruker AND Bruker.BrukerType = 1 THEN
      lButikkBruker = FALSE.
  ELSE
      lButikkBruker = TRUE.
  RUN FixStrings.
  RUN InitLeverandor.
  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
/*   PUBLISH "GetWindowH" (OUTPUT h_Window ). */
  {syspara.i 1 100 1 cAlle}
  IF cAlle = "" THEN
      ASSIGN cAlle = "[Alle]".
  RUN initCombos.
  RUN InitVerdier.
  ASSIGN FI-Dato:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).
  ASSIGN cFilename = SESSION:TEMP-DIRECTORY + "gridtrans.txt"
         FI-TransTyper = "*"
         FI-TransTyper:SCREEN-VALUE = cAlle
         FI-Avdeling  = "*"
         FI-Avdeling:SCREEN-VALUE = cAlle
         FI-HuvGr  = "*"
         FI-HuvGr:SCREEN-VALUE = cAlle
         FI-VarGr  = "*"
         FI-VarGr:SCREEN-VALUE    = cAlle
         FI-LevNr  = "*"
         FI-LevNr:SCREEN-VALUE    = cAlle
         FI-KasseNr  = "*"
         FI-KasseNr:SCREEN-VALUE   = cAlle
         FI-ForsNr  = "*"
         FI-ForsNr:SCREEN-VALUE   = cAlle
         FI-KundeNr  = "*"
         FI-KundeNr:SCREEN-VALUE   = cAlle
         FI-SelgerNr  = "*"
         FI-SelgerNr:SCREEN-VALUE = cAlle.
  RUN viewObject.
  APPLY "VALUE-CHANGED" TO CB-Butik.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitLeverandor fFrameWin 
PROCEDURE InitLeverandor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE wLeverandorer AS CHARACTER  NO-UNDO.
  wLeverandorer = "".

  /* Leverandører - Leses fra Bestillingene */
/*   for each tmpLevBas: delete tmpLevBas. end. */
  for each LevBas no-lock where LevBas.LevNr > 0:
    assign
      wLeverandorer = wLeverandorer + 
                      (if wLeverandorer = ""
                         then ""
                         else ",") + string(LevBas.LevNr).
    create tmpLevBas.
    assign
      tmpLevBas.LevNr   = LevBas.LevNr
      tmpLevBas.LevNamn = LevBas.LeVNamn.          
  end.

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
        ASSIGN cGetTransVerdier = cGetTransVerdier + (IF cGetTransVerdier = "" THEN "" ELSE ",") + STRING(LOOKUP(ENTRY(iCount,cTransFelter),cFelter)).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Move2top fFrameWin 
PROCEDURE Move2top :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FRAME fMain:MOVE-TO-TOP().

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
  DEFINE OUTPUT PARAMETER ocButiker AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER ocTTId    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN ocButiker = IF fi-Butikker <> "" THEN fi-Butikker ELSE CB-Butik:SCREEN-VALUE
             ocTTId    = IF FI-Transtyper = "*" AND TRIM(CB-TTId:SCREEN-VALUE) = "" THEN "1,2,3,4,5,6,7,8,9,10,11,109" ELSE
                         IF TRIM(CB-TTId:SCREEN-VALUE) <> "" THEN CB-TTId:SCREEN-VALUE ELSE FI-Transtyper.
      IF FI-Avdeling <> "*" THEN
          ASSIGN pcFeltListe = "Avdeling".
      ELSE IF FI-HuvGr <> "*" THEN
          ASSIGN pcFeltListe = "Hg".
      ELSE
          ASSIGN pcFeltListe = "Vg".
      ASSIGN pcFeltListe = "Neglager," + pcFeltListe + ",LevNr,ForsNr,SelgerNr,KasseNr,KundNr,KortNr" + 
                           (IF fi-Butikker <> "" THEN "," + "Butik" ELSE "") +
                           (IF FI-Transtyper <> "" THEN "," + "TTId" ELSE "")
             pcVerdier   = FILL(CHR(1),NUM-ENTRIES(pcFeltListe) - 1).
      DO iCount = 1 TO NUM-ENTRIES(pcFeltListe):
        CASE ENTRY(iCount,pcFeltliste):
            WHEN "Avdeling" THEN
                ASSIGN ENTRY(iCount,pcVerdier,CHR(1)) = IF TG-AvFilter:CHECKED THEN "*" ELSE FI-Avdeling.
            WHEN "Hg" THEN
                ASSIGN ENTRY(iCount,pcVerdier,CHR(1)) = IF TG-AvFilter:CHECKED THEN "*" ELSE FI-HuvGr.
            WHEN "Vg" THEN
                ASSIGN ENTRY(iCount,pcVerdier,CHR(1)) = IF TG-AvFilter:CHECKED THEN "*" ELSE FI-Vargr.
            WHEN "LevNr" THEN
                ASSIGN ENTRY(iCount,pcVerdier,CHR(1)) = IF TG-AvFilter:CHECKED THEN "*" ELSE FI-LevNr.
            WHEN "KasseNr" THEN
                ASSIGN ENTRY(iCount,pcVerdier,CHR(1)) = FI-KasseNr.
            WHEN "ForsNr" THEN
                ASSIGN ENTRY(iCount,pcVerdier,CHR(1)) = FI-ForsNr.
            WHEN "SelgerNr" THEN
                ASSIGN ENTRY(iCount,pcVerdier,CHR(1)) = FI-SelgerNr.
            WHEN "KortNr" THEN
                ASSIGN ENTRY(iCount,pcVerdier,CHR(1)) = IF FI-KortNr:SCREEN-VALUE <> "0" THEN FI-KortNr:SCREEN-VALUE ELSE "*".
            WHEN "KundNr" THEN
                ASSIGN ENTRY(iCount,pcVerdier,CHR(1)) = FI-KundeNr.
            WHEN "Neglager" THEN
                ASSIGN ENTRY(iCount,pcVerdier,CHR(1)) = STRING(TG-Neglager:CHECKED,"1/*").
            WHEN "Butik" THEN
                ASSIGN ENTRY(iCount,pcVerdier,CHR(1)) = REPLACE(FI-Butikker,"|",",").
            WHEN "TTId" THEN
                ASSIGN ENTRY(iCount,pcVerdier,CHR(1)) = REPLACE(FI-TransTyper,"|",",").
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
    ASSIGN cTillgSelger   = ""
           cTillgKasserer = "".
    DO WITH FRAME {&FRAME-NAME}:
        FOR EACH ButikkSelger WHERE ButikkSelger.butikknr = INT(CB-Butik:SCREEN-VALUE) NO-LOCK:
            ASSIGN cTillgSelger = cTillgSelger + (IF cTillgSelger <> "" THEN "," ELSE "")
                                               + STRING(ButikkSelger.SelgerNr).
        END.
    END.
    DO WITH FRAME {&FRAME-NAME}:
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
  DEF VAR pcFields   AS CHAR NO-UNDO.
  DEF VAR pcValues   AS CHAR NO-UNDO.
  DEF VAR pcSort     AS CHAR NO-UNDO.
  DEF VAR pcOperator AS CHAR NO-UNDO.
  DEF VAR pcFeltListe AS CHAR NO-UNDO.
  DEF VAR iCount      AS INTE NO-UNDO.
  DEF VAR iTst        AS INTE NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
/*     ASSIGN pcFeltListe = "Dato,Butik,LevNr,TTId". */
    ASSIGN pcFeltListe = "Dato,Butik,TTId".

    DO iCount = 1 TO NUM-ENTRIES(pcFeltListe):
        CASE ENTRY(iCount,pcFeltliste):
            WHEN "Dato" THEN DO:
                IF INPUT FI-DatoTil = ? OR INPUT FI-DatoTil = INPUT FI-Dato THEN
                  ASSIGN
                  pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Dato"
                  pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                             FI-Dato:SCREEN-VALUE
                  pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
                ELSE DO:
                    ASSIGN
                        pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Dato"
                        pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                                   FI-Dato:SCREEN-VALUE
                        pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">=".
                    ASSIGN
                        pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Dato"
                        pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                                  FI-DatoTil:SCREEN-VALUE
                        pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<=".
                END.
            END.
            WHEN "Butik" THEN DO:
/*                 IF CB-Butik:SCREEN-VALUE <> "" THEN */
                IF FI-Butikker = "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Butik"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           CB-Butik:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "TTId" THEN DO:
                IF CB-TTId:SCREEN-VALUE <> "" THEN
                    ASSIGN pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "TTId"
                           pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                                   CB-TTId:SCREEN-VALUE
                           pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
                ELSE IF CB-TTId:SCREEN-VALUE = "" AND FI-Transtyper <> "*" AND NUM-ENTRIES(FI-Transtyper) = 1 THEN
                    ASSIGN pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "TTId"
                           pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                                   FI-Transtyper
                           pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
        END CASE.
    END.
  END.
  RUN SokSdo IN h_dtranslogg (pcFields,
                              pcValues,
                              pcSort,
                              pcOperator,
                              pcFeltListe).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSokArtDyn fFrameWin 
PROCEDURE StartSokArtDyn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER qhArtSok        AS HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER lLocal     AS LOGICAL    NO-UNDO.
  DEFINE INPUT  PARAMETER cButiker   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE qh AS HANDLE     NO-UNDO.
  DEFINE VARIABLE TTH            AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cKriterier     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE ipKriterierTot AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry2     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraAar   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPerId    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilAar   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraPerLinNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilPerLinNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraAarPer   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilAarPer   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFraAarPer   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iButik       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iTilAarPer   AS INTEGER    NO-UNDO.
/*   DEFINE VARIABLE pcFeltListe  AS CHARACTER  NO-UNDO. */
  DEFINE VARIABLE iPeriode     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cDataobjekt  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lEkstern     AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE datofra      AS DATE       NO-UNDO.
  DEFINE VARIABLE datotil      AS DATE       NO-UNDO.
  DEFINE VARIABLE cButik       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cttid        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKassenr   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cForsnr   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSelger   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lneglager AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE ocButiker AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE ocTTId    AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      IF NOT TG-AvFilter:CHECKED THEN DO:
          /* Här kommer vi från artikkelutvalgsbrowsern */
          RUN gvelgtranskrit.w (INPUT CB-Butik:LIST-ITEM-PAIRS + CHR(1) + CB-Butik:SCREEN-VALUE,INPUT CB-TTId:LIST-ITEM-PAIRS + CHR(1) + CB-TTId:SCREEN-VALUE,
                                INPUT cAlle,INPUT cTillgButikker,INPUT cTillgKasserer,INPUT cTillgSelger,
                                OUTPUT cbutik,OUTPUT cttid,
                                OUTPUT datofra,OUTPUT datotil,OUTPUT cKassenr,OUTPUT cForsnr,OUTPUT cSelger,OUTPUT lneglager).
          IF RETURN-VALUE = "AVBRYT" THEN DO:
              APPLY "CLOSE" TO THIS-PROCEDURE.
              RETURN.
          END.
          IF cttid = "" THEN
              ASSIGN cttid = " ".
          ASSIGN FI-Dato:SCREEN-VALUE = STRING(datofra) 
                 FI-DatoTil:SCREEN-VALUE = STRING(datotil) 
                 FI-Butikker = IF NUM-ENTRIES(cButik) > 1 THEN cButik ELSE ""
                 CB-Butik:SCREEN-VALUE = IF NUM-ENTRIES(cButik) > 1 THEN CB-Butik:SCREEN-VALUE ELSE STRING(cButik)  
                 CB-TTId:SCREEN-VALUE = IF NUM-ENTRIES(cttid) > 1 THEN " " ELSE STRING(cttid)
                 FI-Transtyper = IF NUM-ENTRIES(cttid) > 1 THEN cttid ELSE FI-Transtyper
                 FI-KasseNr = cKasseNr
                 FI-ForsNr  = cForsNr
                 FI-SelgerNr = cSelger
                 TG-Neglager:CHECKED = lneglager.
          RUN Startsok.
/*           RUN SetFilterParam (OUTPUT ocButiker,OUTPUT ocTTId). */
/*           RUN SetFilterParam. */
      END.
      RUN SetFilterParam (OUTPUT ocButiker,OUTPUT ocTTId).
      PUBLISH "VisTxtBox" ("Søker data......").
/* för test */
/*       MESSAGE "NY TEST Ja/Nej"                                                                                        */
/*           VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE lsvar AS logi.                                                 */
/*       IF lsvar = TRUE THEN DO:                                                                                        */
/*           ETIME(TRUE).                                                                                                */
/*           RUN TransLoggToTTNY IN h_dtranslogg                                                                         */
/*             ( OUTPUT TTH,pcFeltListe,pcVerdier,qhArtSok,ocButiker,ocTTId,INPUT INPUT FI-Dato,INPUT INPUT FI-DatoTil). */
/*       END.                                                                                                            */
/*       ELSE DO:                                                                                                        */
/*           ETIME(TRUE).                                                                                                */
/*           RUN TransLoggToTT IN h_dtranslogg                                                                           */
/*             ( OUTPUT TTH,pcFeltListe,pcVerdier,qhArtSok).                                                             */
/*       END.                                                                                                            */
/*       MESSAGE ETIME                                                                                                   */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                          */
/* end test */

      RUN TransLoggToTTNY IN h_dtranslogg
        ( OUTPUT TTH,pcFeltListe,pcVerdier,qhArtSok,ocButiker,ocTTId,INPUT INPUT FI-Dato,INPUT INPUT FI-DatoTil).

      CREATE QUERY qh.
      qh:SET-BUFFERS(TTH).
      qh:QUERY-PREPARE("for each TT_TransLogg").
      qh:QUERY-OPEN().

  /*   RUN rappgenqry.p ("Translogg",DYNAMIC-FUNCTION('getQueryWhere':U IN h_dtranslogg),cFileName,cLabels,cFelter,cDecimaler,cTidFelter,?). */
  /*   RUN rappgenqry.p ("Translogg","",cFileName,cLabels,cFelter,cDecimaler,cTidFelter,DYNAMIC-FUNCTION('getQueryHandle':U IN h_dtranslogg)). */
    PUBLISH "VisTxtBox" ("Leser ut data......").
    RUN rappgenqry.p ("TT_Translogg","",cFileName,cLabels,cFelter,cDecimaler,cTidFelter,qh).

    PUBLISH "VisTxtBox" ("Leser inn og bearbeider data......").
    PUBLISH "LoadGrid" (cFileName,5).  /* 3 = antall frozen cols  */
    PUBLISH "AlignCol" (LOOKUP("Storl",cFelter),8). /* högerjustert */
    IF INT(CB-TTId:SCREEN-VALUE) > 0 THEN DO:
        PUBLISH "Summer" (getSumFelter(cSummerFelter),getSumFelter("Bongtekst") + ",SUM").
    END.

  END.
  qh:QUERY-CLOSE().
  DELETE OBJECT qh.
  DELETE OBJECT TTH.
  PUBLISH "VisTxtBox" ("").
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

