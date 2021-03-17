&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_Lager NO-UNDO LIKE Lager
       FIELD UtSolgt% AS DECI
       FIELD Vg LIKE VarGr.Vg
       FIELD VgBeskr LIKE VarGr.VgBeskr
       FIELD HgBeskr LIKE HuvGr.HgBeskr
       FIELD AvdelingNr LIKE Avdeling.AvdelingNr
       FIELD AvdelingNavn LIKE Avdeling.AvdelingNavn
       FIELD LevNr LIKE LevBas.LevNr
       FIELD LevNamn LIKE LevBas.levnamn
       FIElD ForsNr LIKE Forsalj.ForsNr
       FIELD FoNamn LIKE Forsalj.FoNamn
       FIELD SelgerNr LIKE Selger.SelgerNr
       FIELD SelgerNavn LIKE Selger.Navn
       FIELD Beskrivelse AS CHARACTER
       FIELD CharButik AS CHARACTER
       FIELD Sasong LIKE ArtBas.Sasong
       FIELD SasBeskr LIKE Sasong.SasBeskr
       FIELD Farg LIKE ArtBas.Farg
       FIELD FarBeskr LIKE Farg.FarBeskr
       FIELD DbKr LIKE StLinje.DbKr
       FIELD Db% LIKE StLinje.Db%
       FIELD LagerVerdi LIKE Lager.VVarekost
       FIELD Hg LIKE HuvGr.Hg
       FIELD VgLopNr AS CHARACTER
       FIELD T_db% AS DECIMAL FORMAT "->>,>>9.99"
       FIELD Pris  AS DECIMAL FORMAT "->>,>>9.99"
       FIELD T_LagerVerdi  AS DECIMAL FORMAT "->>>,>>>,>>9.99"
       FIELD MatKod LIKE Material.MatKod
       FIELD MatBeskr LIKE Material.MatBeskr
       FIELD VMId LIKE Varemerke.VMId
       FIELD VMBeskr LIKE Varemerke.Beskrivelse
       FIELD Butnamn LIKE Butiker.Butnamn
       FIELD HarNeg AS CHARACTER
       FIELD LevKod AS CHARACTER
       FIELD Solgt% AS DECI
       FIELD DBandel% AS DECI
       FIELD Rabandel% AS DECI
       FIELD Kjopandel% AS DECI
       FIELD LevFargKod like Artbas.Levfargkod
       FIELD NegLagAnt LIKE Lager.Lagant
       FIELD NegLagerVerdi LIKE Lager.VVarekost
       FIELD Storl AS CHAR
       FIELD KjedeInnkPris AS DECI
       .



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

DEFINE VARIABLE cFieldDefs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cArtStrFieldDefs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLabels    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFelter    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDecimaler AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRightCols    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cArtFieldDefs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cArtLabels    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cArtFelter    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cArtDecimaler AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cArtRightCols AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTidFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFilename  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAlle      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cStTypeId  AS CHARACTER  NO-UNDO.
def var wTittel    as char no-undo.
DEFINE VARIABLE h_Window      AS HANDLE     NO-UNDO.
DEFINE VARIABLE h_dstlager    AS HANDLE     NO-UNDO.
DEFINE VARIABLE h_frapportgrid AS HANDLE     NO-UNDO.
DEFINE VARIABLE cSummerFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE hUtskriftProg AS HANDLE     NO-UNDO.
DEFINE VARIABLE cButikerRowIdList AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikerIdList    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cListItemPairs    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cUserDefaultBut   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgButikker    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTmpFieldDefs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE  dSolgtTot      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  dDBTot         AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  dRabTot        AS DECIMAL    NO-UNDO.
DEFINE VARIABLE  dKjopTot       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cVisFelterTxt AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVisFelterNr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSkomodus    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cVgLop       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lButikkBruker AS LOGICAL     NO-UNDO.

DEFINE TEMP-TABLE TT_LagerTMP NO-UNDO LIKE TT_Lager.
DEFINE TEMP-TABLE TT_LagerStr NO-UNDO LIKE TT_Lager
    USE-INDEX butik AS PRIMARY.
DEFINE TEMP-TABLE TT_LagerTMPStr NO-UNDO LIKE TT_LagerStr.
DEFINE TEMP-TABLE TT_TillgButikker NO-UNDO
    FIELD Butik LIKE Butiker.Butik
    INDEX Butik Butik.

DEFINE TEMP-TABLE TT_BigListItem NO-UNDO
    FIELD Butiker AS CHARACTER.

DEF VAR hTTArt  AS HANDLE NO-UNDO.

DEFINE BUFFER bufArtbas FOR Artbas.
/* DEFINE TEMP-TABLE TT_Art NO-UNDO                   */
/*     FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr        */
/*     INDEX ArtikkelNr IS PRIMARY UNIQUE ArtikkelNr. */

/* Fältnamn;Labels;Antal decimaler(blank = 0);Rightcols(1 = right) */
ASSIGN cSummerFelter =
    "AntSolgt,VerdiSolgt,DbKr,AntRab,VerdiRabatt,LagAnt,LagerVerdi," + 
    "NegLagAnt,NegLagerVerdi," + 
    "ReklAnt,ReklVerdi,ReklLAnt,ReklLVerdi,RetLAnt,SvinnAnt,SvinnVerdi," +
    "GjenkjopAnt,GjenkjopVerdi,KjopAnt,KjopVerdi,BrekkAnt,BrekkVerdi," +
    "IntAnt,IntVerdi,JustAnt,JustVerdi,OvAnt,OvVerdi".

ASSIGN cVgLop = "VgLopNr;Vg/Løpenr;;1,".
ASSIGN cFieldDefs = 
  /*  1 */ "DataObjekt;DataobjektTXT;;1," +
  /*  2 */ "Beskrivelse;Beskrivelse;;,"  +
  /*  3 */ "CharButik;Butikk;;,"         +
  /*  4 */ "LagAnt;Pos.lager;;1,"            +
  /*  5 */ "LagerVerdi;Lagerverdi;2;1,"  +
  /*  6 */ "NegLagAnt;Neg.lager;;1,"            +
  /*  7 */ "NegLagerVerdi;Neg.lagerverdi;2;1,"  +
  /*  8 */ "AntSolgt;Solgt;;1,"          +
  /*  8 */ "VerdiSolgt;Solgt netto;2;1," +
  /*  9 */ "Solgt%;Solgt%;2;1,"          +
  /*  9 */ "UtSolgt%;Utsolgt%;2;1,"      +
  /* 10 */ "DbKr;DbKr;2;1,"               +
  /* 11 */ "DbAndel%;DbAndel%;2;1,"      +
  /* 12 */ "Db%;Db%;2;1,"                +
  /* 10 */ "AntRab;Rabatter;;1,"         +
  /* 13 */ "VerdiRabatt;Rabattverdi;2;1," +
  /* 14 */ "Rabandel%;Rabandel%;2;1,"    +
  /* 15 */ "VVarekost;VVarekost;2;1,"    + /* Vid stlager tas detta bort i strängen, tänk på det om vi byter lablar */
  /* 16 */ "ReklAnt;Kunderekl;;1,"       +
  /* 17 */ "ReklVerdi;Kunderekl kr;2;1,"  +
  /* 18 */ "ReklLAnt;Levrekl;;1,"        +
  /* 19 */ "ReklLVerdi;Levrekl kr;2;1,"   +
  /* 20 */ "RetLAnt;Retur lev;;1,"       +
  /* 21 */ "SvinnAnt;Svinn;;1,"          +
  /* 22 */ "SvinnVerdi;Svinn kr;2;1,"     +
  /* 23 */ "GjenkjopAnt;Returer kunde;;1," +
  /* 24 */ "GjenkjopVerdi;Returer kr;2;1,"  +
  /* 10 */ "KjopAnt;Innkjopt;;1,"           +
  /* 25 */ "KjopVerdi;Innkjopt kr;2;1,"    +
  /* 26 */ "Kjopandel%;Kjopandel%;2;1,"   +
  /* 27 */ "BrekkAnt;Brekkasje;;1,"       +
  /* 28 */ "BrekkVerdi;Brekkasje kr;;1,"  +
  /* 29 */ "IntAnt;Internt forbruk;;1,"   +
  /* 30 */ "IntVerdi;Internt forbruk kr;;1," +
  /* 31 */ "JustAnt;Justert;;1,"             +
  /* 32 */ "JustVerdi;Justert kr;;1,"        +
/* NedAnt och NedVerdi används för sålda varors varekost */
/*   /* 33 */ "NedAnt;Nedskrevet;;1,"         + */
/*   /* 34 */ "NedVerdi;Nedskrevet kr;;1,"   +  */
  /* 35 */ "OvAnt;Overført;;1,"            +
  /* 36 */ "OvVerdi;Overførte kr;;1".
/*   /* 35 */ "SistInnlevert;Sist innlevert;;1". */
/* Extra fält för lager avseende artikkel */
ASSIGN cArtFieldDefs = 
  /* xx */ "T_db%;T.db%;2;1," +
  /* xx */ "Pris;Pris;2;1," +
  /* xx */ "KjedeInnkPris;KjedePris;2;1," +
  /* xx */ "T_Lagerverdi;T.Lagerverdi;2;1," +
  /* xx */ "AvdelingNr;Avdnr;;1,"  +
  /* xx */ "AvdelingNavn;Avdeling;;,"  +
  /* xx */ "Hg;Hg;;1,"  +
  /* xx */ "HgBeskr;Hovedgruppe;;,"  +
  /* xx */ "Vg;Vg;;1,"  +
  /* xx */ "VgBeskr;Varegruppe;;,"  +
  /* xx */ (IF cSkomodus = "1" THEN "" ELSE cVgLop)  +
  /* xx */ "LevNr;Levnr;;1,"  +
  /* xx */ "LevNamn;Leverandør;;,"  +
  /* 37 */ "Sasong;Sesong;;1," +
  /* 38 */ "SasBeskr;SesBeskr;;," +
  /* xx */ "Vmbeskr;Varemerke;;,"  +
  /* xx */ "FarBeskr;Farge;;," +
  /* xx */ "Levfargkod;Levfarge;;".

ASSIGN cArtStrFieldDefs = 
  /*  1 */ "DataObjekt;DataobjektTXT;;1," +
  /*  2 */ "Beskrivelse;Beskrivelse;;,"  +
  /*  3 */ "CharButik;Butikk;;,"         +
  /*  3b */ "Storl;Størrelse;;,"         +
  /*  4 */ "LagAnt;Pos.lager;;1,"            +
  /*  5 */ "LagerVerdi;Lagerverdi;2;1,"  +
  /*  6 */ "NegLagAnt;Neg.lager;;1,"            +
  /*  7 */ "NegLagerVerdi;Neg.lagerverdi;2;1,"  +
  /*  8 */ "AntSolgt;Solgt;;1,"          +
  /*  8 */ "VerdiSolgt;Solgt netto;2;1," +
/*   /*  9 */ "Solgt%;Solgt%;2;1,"          + */
/*   /*  9 */ "UtSolgt%;Utsolgt%;2;1,"      + */
  /* 10 */ "DbKr;DbKr;2;1,"               +
/*   /* 11 */ "DbAndel%;DbAndel%;2;1,"      + */
  /* 12 */ "Db%;Db%;2;1,"                +
  /* 10 */ "AntRab;Rabatter;;1,"         +
  /* 13 */ "VerdiRabatt;Rabattverdi;2;1," +
/*   /* 14 */ "Rabandel%;Rabandel%;2;1,"    + */
  /* 15 */ "VVarekost;VVarekost;2;1,"    + /* Vid stlager tas detta bort i strängen, tänk på det om vi byter lablar */
  /* xx */ "KjedeInnkPris;KjedePris;2;1," +
  /* 10 */ "KjopAnt;Innkjopt;;1,"           +
  /* 25 */ "KjopVerdi;Innkjopt kr;2;1,"    +
  /* 16 */ "ReklAnt;Kunderekl;;1,"       +
/*   /* 17 */ "ReklVerdi;Kunderekl kr;2;1,"  + */
  /* 18 */ "ReklLAnt;Levrekl;;1,"        +
/*   /* 19 */ "ReklLVerdi;Levrekl kr;2;1,"   + */
  /* 20 */ "RetLAnt;Retur lev;;1,"       +
  /* 23 */ "GjenkjopAnt;Returer kunde;;1," +
/*   /* 24 */ "GjenkjopVerdi;Returer kr;2;1,"  + */
/*   /* 26 */ "Kjopandel%;Kjopandel%;2;1,"   + */
  /* 27 */ "BrekkAnt;Brekkasje;;1,"       +
/*   /* 28 */ "BrekkVerdi;Brekkasje kr;;1,"  + */
  /* 29 */ "IntAnt;Internt forbruk;;1,"   +
/*   /* 30 */ "IntVerdi;Internt forbruk kr;;1," + */
/*   /* 31 */ "JustAnt;Justert;;1,"             + */
/*   /* 32 */ "JustVerdi;Justert kr;;1,"        + */
/* NedAnt och NedVerdi används för sålda varors varekost */
/*   /* 33 */ "NedAnt;Nedskrevet;;1,"         + */
/*   /* 34 */ "NedVerdi;Nedskrevet kr;;1,"   +  */
  /* 35 */ "OvAnt;Overført;;1,"            +
  /* 36 */ "OvVerdi;Overførte kr;;1," +
  /* xx */ "T_db%;T.db%;2;1," +
  /* xx */ "Pris;Pris;2;1," +
  /* xx */ "T_Lagerverdi;T.Lagerverdi;2;1," +
  /* xx */ "AvdelingNr;Avdnr;;1,"  +
/*   /* xx */ "AvdelingNavn;Avdeling;;,"  + */
  /* xx */ "Hg;Hg;;1,"  +
/*   /* xx */ "HgBeskr;Hovedgruppe;;,"  + */
  /* xx */ "Vg;Vg;;1,"  +
/*   /* xx */ "VgBeskr;Varegruppe;;,"  + */
  /* xx */ "LevNr;Levnr;;1,"  +
  /* xx */ "LevNamn;Leverandør;;,"  +
  /* 37 */ "Sasong;Sesong;;1," +
/*   /* 38 */ "SasBeskr;SesBeskr;;," + */
  /* xx */ "Vmbeskr;Varemerke;;,"  +
  /* xx */ "FarBeskr;Farge;;," +
  /* xx */ "Levfargkod;Levfarge;;".
/*   /* 35 */ "SistInnlevert;Sist innlevert;;1". */


/* ASSIGN cArtStrFieldDefsOLD =                                                                                           */
/*   /*  1 */ "DataObjekt;DataobjektTXT;;1," +                                                                            */
/*   /*  2 */ "Beskrivelse;Bexxxskrivelse;;,"  +                                                                          */
/*   /*  3 */ "CharButik;Butikk;;,"         +                                                                             */
/*   /*  3b */ "Storl;Størrelse;;,"         +                                                                             */
/*   /*  4 */ "LagAnt;Pos.lager;;1,"            +                                                                         */
/*   /*  5 */ "LagerVerdi;Lagerverdi;2;1,"  +                                                                             */
/*   /*  6 */ "NegLagAnt;Neg.lager;;1,"            +                                                                      */
/*   /*  7 */ "NegLagerVerdi;Neg.lagerverdi;2;1,"  +                                                                      */
/*   /* 10 */ "DbKr;DbKr;2;1,"               +                                                                            */
/*   /* 12 */ "Db%;Db%;2;1,"                +                                                                             */
/*   /* 15 */ "VVarekost;VVarekost;2;1,"    + /* Vid stlager tas detta bort i strängen, tänk på det om vi byter lablar */ */
/* /*   /* 35 */ "SistInnlevert;Sist innlevert;;1". */                                                                    */
/*   /* xx */ "T_db%;T.db%;2;1," +                                                                                        */
/*   /* xx */ "Pris;Pris;2;1," +                                                                                          */
/*   /* xx */ "T_Lagerverdi;T.Lagerverdi;2;1," +                                                                          */
/*   /* xx */ "AvdelingNr;Avdnr;;1,"  +                                                                                   */
/*   /* xx */ "AvdelingNavn;Avdeling;;,"  +                                                                               */
/*   /* xx */ "Hg;Hg;;1,"  +                                                                                              */
/*   /* xx */ "HgBeskr;Hovedgruppe;;,"  +                                                                                 */
/*   /* xx */ "Vg;Vg;;1,"  +                                                                                              */
/*   /* xx */ "VgBeskr;Varegruppe;;,"  +                                                                                  */
/*   /* xx */ (IF cSkomodus = "1" THEN "" ELSE cVgLop)  +                                                                 */
/*   /* xx */ "LevNr;Levnr;;1,"  +                                                                                        */
/*   /* xx */ "LevNamn;Leverandør;;,"  +                                                                                  */
/*   /* 37 */ "Sasong;Sesong;;1," +                                                                                       */
/*   /* 38 */ "SasBeskr;SesBeskr;;," +                                                                                    */
/*   /* xx */ "Vmbeskr;Varemerke;;,"  +                                                                                   */
/*   /* xx */ "FarBeskr;Farge;;," +                                                                                       */
/*   /* xx */ "Levfargkod;Levfarge;;".                                                                                    */

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
&Scoped-Define ENABLED-OBJECTS RECT-1 B-Avdeling B-AvdelingBlank ~
B-LevNrBlank RS-Lagertype CB-ButikkTeam B-Aktiver B-HgBlank B-SesongBlank ~
B-VgBlank B-FargBlank B-Artikkelkort CB-Antall Tg-VisButikker TG-Alarm ~
TG-AvFilter B-LevNr B-HuvGr B-VarGr B-Farg B-Sesong BUTTON-SokBut ~
FI-LagerListeTxT 
&Scoped-Define DISPLAYED-OBJECTS FI-Butikker FI-Avdeling FI-LevNr ~
RS-Lagertype CB-ButikkTeam FI-HuvGr FI-Sesong FI-VarGr FI-Farg CB-Antall ~
RS-DataUrval Tg-VisButikker TG-Alarm TG-AvFilter FI-LagerListeTxT 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBeskr fFrameWin 
FUNCTION getBeskr RETURNS CHARACTER
  ( INPUT cStTypeId AS CHARACTER, INPUT cObjekt AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKriterier fFrameWin 
FUNCTION getKriterier RETURNS LOGICAL
  ( OUTPUT cKriterier AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLabel fFrameWin 
FUNCTION getLabel RETURNS CHARACTER
  ( INPUT cStTypeId AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSumFelter fFrameWin 
FUNCTION getSumFelter RETURNS CHARACTER
  ( INPUT cFeltnavnListe AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


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

DEFINE BUTTON B-Farg  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-FargBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-HgBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-HuvGr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-LagerAntBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-Lagerliste 
     IMAGE-UP FILE "icon/e-print.bmp":U NO-FOCUS
     LABEL "Print" 
     SIZE 4.6 BY 1.05 TOOLTIP "Omsettningsrapport".

DEFINE BUTTON B-LevNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-LevNrBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-Sesong  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SesongBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-VarGr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-VgBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON BUTTON-SokBut 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-Antall AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lagerantall" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE CB-ButikkTeam AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikkteam" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 20.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Avdeling AS CHARACTER FORMAT "X(10)":U 
     LABEL "Avdeling" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Butikker AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Farg AS CHARACTER FORMAT "X(10)":U 
     LABEL "Farge" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-HuvGr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Hovedgruppe" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LagerListeTxT AS CHARACTER FORMAT "X(256)":U INITIAL " Lagerliste" 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE FI-LevNr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Levnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Sesong AS CHARACTER FORMAT "X(10)":U 
     LABEL "Sesong" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VarGr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE RS-DataUrval AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Aktuell rad", 1,
"Hele rapporten", 2
     SIZE 23.2 BY 1.62 NO-UNDO.

DEFINE VARIABLE RS-Lagertype AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Avdeling", "AVDELING",
"Hovedgruppe", "HOVEDGR",
"Varegruppe", "VAREGR",
"Leverandør", "LEVERAN",
"Butikk", "BUTSTAT",
"Artikkel", "ARTIKKEL",
"Artikkel/Størrelse", "ARTSTR"
     SIZE 23.4 BY 6.19 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 2.62.

DEFINE VARIABLE TG-Alarm AS LOGICAL INITIAL no 
     LABEL "Alarmliste neg lagerteller" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AvFilter AS LOGICAL INITIAL no 
     LABEL "Avansert filter" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.4 BY .81 NO-UNDO.

DEFINE VARIABLE Tg-VisButikker AS LOGICAL INITIAL no 
     LABEL "Vis per butikk" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.4 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     B-Avdeling AT ROW 1.19 COL 92.8 NO-TAB-STOP 
     FI-Butikker AT ROW 1.19 COL 14 COLON-ALIGNED
     FI-Avdeling AT ROW 1.19 COL 75.8 COLON-ALIGNED
     B-AvdelingBlank AT ROW 1.19 COL 97.8
     FI-LevNr AT ROW 1.19 COL 116.6 COLON-ALIGNED
     B-LevNrBlank AT ROW 1.24 COL 138.6
     RS-Lagertype AT ROW 1.24 COL 148.6 NO-LABEL
     CB-ButikkTeam AT ROW 2.19 COL 14 COLON-ALIGNED
     B-Aktiver AT ROW 2.19 COL 42
     FI-HuvGr AT ROW 2.19 COL 75.8 COLON-ALIGNED
     B-HgBlank AT ROW 2.19 COL 97.8
     FI-Sesong AT ROW 2.19 COL 116.6 COLON-ALIGNED
     B-SesongBlank AT ROW 2.24 COL 138.6
     FI-VarGr AT ROW 3.19 COL 75.8 COLON-ALIGNED
     B-VgBlank AT ROW 3.19 COL 97.8
     FI-Farg AT ROW 3.19 COL 116.6 COLON-ALIGNED
     B-FargBlank AT ROW 3.24 COL 138.6
     B-Artikkelkort AT ROW 3.52 COL 42
     B-LagerAntBlank AT ROW 4.19 COL 97.8
     CB-Antall AT ROW 4.24 COL 75.8 COLON-ALIGNED
     RS-DataUrval AT ROW 4.52 COL 8 NO-LABEL
     Tg-VisButikker AT ROW 4.91 COL 42
     TG-Alarm AT ROW 5.33 COL 78
     TG-AvFilter AT ROW 5.91 COL 42
     B-LevNr AT ROW 1.24 COL 133.6 NO-TAB-STOP 
     B-HuvGr AT ROW 2.19 COL 92.8 NO-TAB-STOP 
     B-VarGr AT ROW 3.19 COL 92.8 NO-TAB-STOP 
     B-Farg AT ROW 3.24 COL 133.6 NO-TAB-STOP 
     B-Sesong AT ROW 2.24 COL 133.6 NO-TAB-STOP 
     BUTTON-SokBut AT ROW 1.19 COL 31.6 NO-TAB-STOP 
     B-Lagerliste AT ROW 4.76 COL 32.8
     FI-LagerListeTxT AT ROW 3.67 COL 16 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 4 COL 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 174 BY 6.62.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_Lager T "?" NO-UNDO SkoTex Lager
      ADDITIONAL-FIELDS:
          FIELD UtSolgt% AS DECI
          FIELD Vg LIKE VarGr.Vg
          FIELD VgBeskr LIKE VarGr.VgBeskr
          FIELD HgBeskr LIKE HuvGr.HgBeskr
          FIELD AvdelingNr LIKE Avdeling.AvdelingNr
          FIELD AvdelingNavn LIKE Avdeling.AvdelingNavn
          FIELD LevNr LIKE LevBas.LevNr
          FIELD LevNamn LIKE LevBas.levnamn
          FIElD ForsNr LIKE Forsalj.ForsNr
          FIELD FoNamn LIKE Forsalj.FoNamn
          FIELD SelgerNr LIKE Selger.SelgerNr
          FIELD SelgerNavn LIKE Selger.Navn
          FIELD Beskrivelse AS CHARACTER
          FIELD CharButik AS CHARACTER
          FIELD Sasong LIKE ArtBas.Sasong
          FIELD SasBeskr LIKE Sasong.SasBeskr
          FIELD Farg LIKE ArtBas.Farg
          FIELD FarBeskr LIKE Farg.FarBeskr
          FIELD DbKr LIKE StLinje.DbKr
          FIELD Db% LIKE StLinje.Db%
          FIELD LagerVerdi LIKE Lager.VVarekost
          FIELD Hg LIKE HuvGr.Hg
          FIELD VgLopNr AS CHARACTER
          FIELD T_db% AS DECIMAL FORMAT "->>,>>9.99"
          FIELD Pris  AS DECIMAL FORMAT "->>,>>9.99"
          FIELD T_LagerVerdi  AS DECIMAL FORMAT "->>>,>>>,>>9.99"
          FIELD MatKod LIKE Material.MatKod
          FIELD MatBeskr LIKE Material.MatBeskr
          FIELD VMId LIKE Varemerke.VMId
          FIELD VMBeskr LIKE Varemerke.Beskrivelse
          FIELD Butnamn LIKE Butiker.Butnamn
          FIELD HarNeg AS CHARACTER
          FIELD LevKod AS CHARACTER
          FIELD Solgt% AS DECI
          FIELD DBandel% AS DECI
          FIELD Rabandel% AS DECI
          FIELD Kjopandel% AS DECI
          FIELD LevFargKod like Artbas.Levfargkod
          FIELD NegLagAnt LIKE Lager.Lagant
          FIELD NegLagerVerdi LIKE Lager.VVarekost
          FIELD Storl AS CHAR
          FIELD KjedeInnkPris AS DECI
          
      END-FIELDS.
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
         HEIGHT             = 6.62
         WIDTH              = 174.
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

/* SETTINGS FOR BUTTON B-LagerAntBlank IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       B-LagerAntBlank:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR BUTTON B-Lagerliste IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Avdeling IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Butikker IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Farg IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-HuvGr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LevNr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Sesong IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VarGr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RS-DataUrval IN FRAME fMain
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
  DEFINE VARIABLE cKriterier AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hQry   AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DEFINE VARIABLE TTH         AS HANDLE     NO-UNDO.
  DEFINE VARIABLE qh          AS HANDLE     NO-UNDO.
  DEFINE VARIABLE qhParam     AS HANDLE     NO-UNDO.
  DEFINE VARIABLE pcFeltListe AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcVerdier   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cXParam     AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      IF NOT DYNAMIC-FUNCTION('getKriterier':U,
         OUTPUT cKriterier /* CHARACTER */) THEN
          RETURN.
      ASSIGN B-Artikkelkort:SENSITIVE = (RS-LagerType:SCREEN-VALUE = "ARTIKKEL" OR RS-LagerType:SCREEN-VALUE = "ARTSTR")
             B-Lagerliste:SENSITIVE = NOT RS-LagerType:SCREEN-VALUE = "BUTSTAT" AND
/*                                       NOT RS-LagerType:SCREEN-VALUE = "Artikkel" AND */
                                      NOT Tg-VisButikker:CHECKED
             RS-DataUrval:SENSITIVE = B-Lagerliste:SENSITIVE.
      IF (RS-Lagertype:SCREEN-VALUE = "ARTIKKEL" OR RS-LagerType:SCREEN-VALUE = "ARTSTR") AND TG-AvFilter:CHECKED THEN DO:
          RUN Avancerat.
          RETURN.
      END.
      CASE RS-Lagertype:SCREEN-VALUE:
          WHEN "AVDELING" THEN DO:
              IF FI-Avdeling <> "*" THEN
                  ASSIGN pcFeltListe = "AvdelingNr"
                         pcVerdier   = FI-Avdeling.
          END.
          WHEN "HOVEDGR" THEN DO:
              ASSIGN pcFeltListe = IF FI-Avdeling <> "*" THEN "AvdelingNr" ELSE 
                                   IF FI-HuvGr    <> "*" THEN "HuvGr"      ELSE ""
                     pcVerdier   = IF FI-Avdeling <> "*" THEN FI-Avdeling  ELSE 
                                   IF FI-HuvGr    <> "*" THEN FI-HuvGr     ELSE "".
          END.
          WHEN "VAREGR" THEN DO:
              ASSIGN pcFeltListe = IF FI-Avdeling <> "*" THEN "AvdelingNr" ELSE 
                                   IF FI-HuvGr    <> "*" THEN "HuvGr"      ELSE 
                                   IF FI-VarGr    <> "*" THEN "VarGr"      ELSE ""
                     pcVerdier   = IF FI-Avdeling <> "*" THEN FI-Avdeling  ELSE 
                                   IF FI-HuvGr    <> "*" THEN FI-HuvGr     ELSE 
                                   IF FI-VarGr    <> "*" THEN FI-VarGr     ELSE "".
          END.
          WHEN "LEVERAN" THEN DO:
              IF FI-LevNr <> "*" THEN
                  ASSIGN pcFeltListe = "LevNr"
                         pcVerdier   = FI-LevNr.
          END.
          WHEN "ARTIKKEL" OR WHEN "ARTSTR" THEN DO:
              ASSIGN pcFeltListe = IF FI-Avdeling <> "*" THEN "AvdelingNr" ELSE 
                                   IF FI-HuvGr    <> "*" THEN "HuvGr"      ELSE 
                                   IF FI-VarGr    <> "*" THEN "VarGr"      ELSE ""
                     pcVerdier   = IF FI-Avdeling <> "*" THEN FI-Avdeling  ELSE 
                                   IF FI-HuvGr    <> "*" THEN FI-HuvGr     ELSE 
                                   IF FI-VarGr    <> "*" THEN FI-VarGr     ELSE "".
              DO iCount = 1 TO NUM-ENTRIES("LevNr,Sasong,Farg"):
                  CASE ENTRY(iCount,"LevNr,Sasong,Farg"):
                      WHEN "LevNr" THEN
                          IF FI-LevNr <> "*" THEN
                              ASSIGN pcFeltListe = pcFeltListe + (IF pcFeltListe <> "" THEN CHR(1) ELSE "") + "LevNr" 
                                     pcVerdier   = pcVerdier   + (IF pcVerdier   <> "" THEN CHR(1) ELSE "") + FI-LevNr.
                      WHEN "Sasong" THEN
                          IF FI-Sesong <> "*" THEN
                              ASSIGN pcFeltListe = pcFeltListe + (IF pcFeltListe <> "" THEN CHR(1) ELSE "") + "Sasong" 
                                     pcVerdier   = pcVerdier   + (IF pcVerdier   <> "" THEN CHR(1) ELSE "") + FI-Sesong.
                      WHEN "Farg" THEN
                          IF FI-Farg <> "*" THEN
                              ASSIGN pcFeltListe = pcFeltListe + (IF pcFeltListe <> "" THEN CHR(1) ELSE "") + "Farg" 
                                     pcVerdier   = pcVerdier   + (IF pcVerdier   <> "" THEN CHR(1) ELSE "") + FI-Farg.
                  END CASE.
              END.
          END.
      END CASE.
      PUBLISH "VisTxtBox" ("Søker data......").
      IF Tg-VisButikker:CHECKED AND NOT (RS-Lagertype:SCREEN-VALUE = "ARTIKKEL" OR RS-LagerType:SCREEN-VALUE = "ARTSTR") THEN DO:
          IF RS-Lagertype:SCREEN-VALUE = "ARTIKKEL" THEN DO:
              ASSIGN cTmpFieldDefs       = cFieldDefs
                     ENTRY(3,cFieldDefs) = ENTRY(3,cFieldDefs) + ",Butnamn;Navn;;".
          END.
          ELSE IF RS-Lagertype:SCREEN-VALUE = "ARTSTR" THEN DO:
              ASSIGN cTmpFieldDefs = cFieldDefs
                     cFieldDefs    = cArtStrFieldDefs.
/*                      ENTRY(3,cFieldDefs) = ENTRY(3,cFieldDefs) + ",Butnamn;Navn;;". */
          END.
          ELSE
              ASSIGN cTmpFieldDefs       = cFieldDefs.
          RUN FixStrings.
        ASSIGN cFieldDefs = cTmpFieldDefs.
      END.
      ELSE IF (RS-Lagertype:SCREEN-VALUE <> "ARTIKKEL" AND RS-LagerType:SCREEN-VALUE <> "ARTSTR") THEN DO:
          ASSIGN cTmpFieldDefs = cFieldDefs
                 cFieldDefs    = REPLACE(cFieldDefs,"VVarekost;VVarekost;2;1,","").
          RUN FixStrings.
          ASSIGN cFieldDefs = cTmpFieldDefs.
      END.
      ELSE DO:
          ASSIGN cTmpFieldDefs = cFieldDefs.
/*           IF RS-LagerType:SCREEN-VALUE = "ARTSTR" THEN */
/*               cFieldDefs = cArtStrFieldDefs.           */
          RUN FixStrings.
/*           ASSIGN cFieldDefs = cTmpFieldDefs. */
      END.
/*       IF RS-Lagertype:SCREEN-VALUE = "ARTIKKEL" THEN DO: */
      DO: /* alltid mot artbas */
          /* StartSokArtDyn innehåller allt för att även klara av allt utifrån  */
          IF RS-LagerType:SCREEN-VALUE <> "ARTSTR" THEN
              RUN StartSokArtDyn (qhParam,TRUE,ENTRY(1,cKriterier,CHR(1))). /* Butiker */
          ELSE
              RUN StartSokArtDynStr (qhParam,TRUE,ENTRY(1,cKriterier,CHR(1))). /* Butiker */
          RETURN NO-APPLY.
      END.
/*       ELSE DO:                                                                                                                                                                                               */
/*           RUN StartSok.                                                                                                                                                                                      */
/*           RUN StLagerToTT IN h_dstlager                                                                                                                                                                      */
/*             ( OUTPUT TTH,RS-Lagertype:SCREEN-VALUE,ENTRY(1,cKriterier,CHR(1)),Tg-VisButikker:CHECKED = TRUE,pcFeltListe + ";" + pcVerdier,"").                                                               */
/*           CREATE QUERY qh.                                                                                                                                                                                   */
/*           qh:SET-BUFFERS(TTH).                                                                                                                                                                               */
/*           qh:QUERY-PREPARE("FOR EACH TT_StLager").                                                                                                                                                           */
/*           qh:QUERY-OPEN().                                                                                                                                                                                   */
/*           PUBLISH "VisTxtBox" ("Leser ut data......").                                                                                                                                                       */
/*           RUN rappgenqry.p ("TT_StLager",DYNAMIC-FUNCTION('getQueryWhere':U IN h_dstlager),cFileName,REPLACE(cLabels,"DataobjektTXT",getLabel(RS-Lagertype:SCREEN-VALUE)),cFelter,cDecimaler,cTidFelter,qh). */
/*       END.                                                                                                                                                                                                   */
      RUN LesInnIGrid.
/*       IF NOT RS-Lagertype:SCREEN-VALUE = "ARTIKKEL" THEN DO: */
/*           qh:QUERY-CLOSE().                                  */
/*           TTH:EMPTY-TEMP-TABLE().                            */
/*           TTH:BUFFER-RELEASE.                                */
/*           DELETE OBJECT TTH NO-ERROR.                        */
/*           DELETE OBJECT qh NO-ERROR.                         */
/*           ASSIGN TTH = ?                                     */
/*                  qh  = ?.                                    */
/*       END.                                                   */
/*       IF TG-Alarm:SENSITIVE AND TG-Alarm:CHECKED THEN */

      PUBLISH "VisTxtBox" ("").
  END.
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
               FI-Avdeling:BGCOLOR      = 11.
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-Avdeling:BGCOLOR      = ?.
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


&Scoped-define SELF-NAME B-Farg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Farg fFrameWin
ON CHOOSE OF B-Farg IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "Farg;Farg;FarBeskr",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "Farg",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-Farg:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-Farg     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-Farg:TOOLTIP = IF FI-Farg = "*" THEN "" ELSE FI-Farg.
        IF FI-Farg <> "*" THEN
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
               FI-Farg:BGCOLOR      = 11.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-Farg:BGCOLOR      = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-FargBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-FargBlank fFrameWin
ON CHOOSE OF B-FargBlank IN FRAME fMain /* Blank */
DO:
    IF FI-Farg:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-Farg:SCREEN-VALUE = cAlle
               FI-Farg              = "*"
               FI-Farg:TOOLTIP      = ""
               FI-Farg:BGCOLOR      = ?
               B-Farg:PRIVATE-DATA  = "".
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
               FI-HuvGr:BGCOLOR      = 11.
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-HuvGr:BGCOLOR      = ?.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LagerAntBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LagerAntBlank fFrameWin
ON CHOOSE OF B-LagerAntBlank IN FRAME fMain /* Blank */
DO:
/*     IF CB-Antall:SCREEN-VALUE <> cAlle THEN DO:    */
/*         ASSIGN CB-Antall              = cAlle      */
/*                CB-Antall:SCREEN-VALUE = CB-Antall. */
/*     END.                                           */
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Lagerliste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Lagerliste fFrameWin
ON CHOOSE OF B-Lagerliste IN FRAME fMain /* Print */
DO:
    RUN Lagerliste.
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
               FI-LevNr:BGCOLOR      = 11.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-LevNr:BGCOLOR      = ?.
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


&Scoped-define SELF-NAME B-Sesong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Sesong fFrameWin
ON CHOOSE OF B-Sesong IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIdList    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    IF NUM-ENTRIES(SELF:PRIVATE-DATA,CHR(1)) = 2 THEN
        ASSIGN cRowIdList = ENTRY(1,SELF:PRIVATE-DATA,CHR(1))
               cIdList    = ENTRY(2,SELF:PRIVATE-DATA,CHR(1)).
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "SaSong;Sasong;SasBeskr",
                        "WHERE TRUE",
                        INPUT-OUTPUT cRowIdList,
                        "Sasong",
                        INPUT-OUTPUT cIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        assign
          FI-Sesong:SCREEN-VALUE = if cIdList = ""
                            then cAlle
                          else "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-Sesong     = if cIdList = ""
                            then "*"
                            else REPLACE(cIdList,"|",",")
          FI-Sesong:TOOLTIP = IF FI-Sesong = "*" THEN "" ELSE FI-Sesong.
        IF FI-Sesong <> "*" THEN
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
               FI-Sesong:BGCOLOR      = 11.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-Sesong:BGCOLOR      = ?.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SesongBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SesongBlank fFrameWin
ON CHOOSE OF B-SesongBlank IN FRAME fMain /* Blank */
DO:
    IF FI-Sesong:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-Sesong:SCREEN-VALUE = cAlle
               FI-Sesong              = "*"
               FI-Sesong:TOOLTIP      = ""
               FI-Sesong:BGCOLOR      = ?
               B-Sesong:PRIVATE-DATA  = "".
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
               FI-VarGr:BGCOLOR      = 11.
        END.
        ELSE
            ASSIGN SELF:PRIVATE-DATA = ""
               FI-VarGr:BGCOLOR      = ?.
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


&Scoped-define SELF-NAME CB-Antall
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Antall fFrameWin
ON VALUE-CHANGED OF CB-Antall IN FRAME fMain /* Lagerantall */
DO:
  ASSIGN CB-Antall.
  IF CB-Antall <> cAlle THEN
      TG-Alarm:CHECKED = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-ButikkTeam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-ButikkTeam fFrameWin
ON VALUE-CHANGED OF CB-ButikkTeam IN FRAME fMain /* Butikkteam */
DO:
    ASSIGN SELF:TOOLTIP = IF SELF:SCREEN-VALUE = "INGEN" THEN "" ELSE IF NUM-ENTRIES(SELF:SCREEN-VALUE,";") = 2 THEN
        ENTRY(1,SELF:SCREEN-VALUE,";") ELSE REPLACE(SELF:SCREEN-VALUE,CHR(1),",").
/*     ASSIGN SELF:TOOLTIP = IF SELF:SCREEN-VALUE = "INGEN" THEN "" ELSE REPLACE(SELF:SCREEN-VALUE,CHR(1),","). */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-Lagertype
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Lagertype fFrameWin
ON VALUE-CHANGED OF RS-Lagertype IN FRAME fMain
DO:
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN B-Sesong:SENSITIVE = (SELF:SCREEN-VALUE = "ARTIKKEL" OR SELF:SCREEN-VALUE = "ARTSTR")
               B-SesongBlank:SENSITIVE = (SELF:SCREEN-VALUE = "ARTIKKEL" OR SELF:SCREEN-VALUE = "ARTSTR")
               B-Farg:SENSITIVE = (SELF:SCREEN-VALUE = "ARTIKKEL" OR SELF:SCREEN-VALUE = "ARTSTR")
               B-FargBlank:SENSITIVE = (SELF:SCREEN-VALUE = "ARTIKKEL" OR SELF:SCREEN-VALUE = "ARTSTR")
/*                CB-Antall:SENSITIVE = TRUE /* SELF:SCREEN-VALUE = "ARTIKKEL" */ */
               B-LagerAntBlank:SENSITIVE = (SELF:SCREEN-VALUE = "ARTIKKEL" OR SELF:SCREEN-VALUE = "ARTSTR")
               TG-AvFilter:SENSITIVE     = (SELF:SCREEN-VALUE = "ARTIKKEL" OR SELF:SCREEN-VALUE = "ARTSTR")
/*                Tg-VisButikker:SENSITIVE = NOT SELF:SCREEN-VALUE = "ARTIKKEL" */
               TG-Alarm:SENSITIVE = RS-Lagertype:SCREEN-VALUE = "ARTIKKEL"
            .

        CASE SELF:SCREEN-VALUE:
            WHEN "AVDELING" THEN DO:
                APPLY "CHOOSE" TO B-HgBlank.
                APPLY "CHOOSE" TO B-VgBlank.
                APPLY "CHOOSE" TO B-LevNrBlank.
                APPLY "CHOOSE" TO B-SesongBlank.
                APPLY "CHOOSE" TO B-FargBlank.
/*                 APPLY "CHOOSE" TO B-LagerAntBlank. */
                ASSIGN B-Avdeling:SENSITIVE = TRUE
                       B-AvdelingBlank:SENSITIVE = TRUE
                       B-HuvGr:SENSITIVE = FALSE
                       B-HgBlank:SENSITIVE = FALSE
                       B-VarGr:SENSITIVE = FALSE
                       B-VgBlank:SENSITIVE = FALSE
                       B-LevNr:SENSITIVE = FALSE
                       B-LevNrBlank:SENSITIVE = FALSE.
            END.
            WHEN "HOVEDGR" THEN DO:
                APPLY "CHOOSE" TO B-VgBlank.
                APPLY "CHOOSE" TO B-LevNrBlank.
                APPLY "CHOOSE" TO B-SesongBlank.
                APPLY "CHOOSE" TO B-FargBlank.
/*                 APPLY "CHOOSE" TO B-LagerAntBlank. */
                ASSIGN B-Avdeling:SENSITIVE = TRUE
                       B-AvdelingBlank:SENSITIVE = TRUE
                       B-HuvGr:SENSITIVE = TRUE
                       B-HgBlank:SENSITIVE = TRUE
                       B-VarGr:SENSITIVE = FALSE
                       B-VgBlank:SENSITIVE = FALSE
                       B-LevNr:SENSITIVE = FALSE
                       B-LevNrBlank:SENSITIVE = FALSE.
            END.
            WHEN "VAREGR" THEN DO:
                APPLY "CHOOSE" TO B-LevNrBlank.
                APPLY "CHOOSE" TO B-SesongBlank.
                APPLY "CHOOSE" TO B-FargBlank.
/*                 APPLY "CHOOSE" TO B-LagerAntBlank. */
                ASSIGN B-Avdeling:SENSITIVE = TRUE
                       B-AvdelingBlank:SENSITIVE = TRUE
                       B-HuvGr:SENSITIVE = TRUE
                       B-HgBlank:SENSITIVE = TRUE
                       B-VarGr:SENSITIVE = TRUE
                       B-VgBlank:SENSITIVE = TRUE
                       B-LevNr:SENSITIVE = FALSE
                       B-LevNrBlank:SENSITIVE = FALSE.
            END.
            WHEN "LEVERAN" THEN DO:
                APPLY "CHOOSE" TO B-AvdelingBlank.
                APPLY "CHOOSE" TO B-HgBlank.
                APPLY "CHOOSE" TO B-VgBlank.
                APPLY "CHOOSE" TO B-SesongBlank.
                APPLY "CHOOSE" TO B-FargBlank.
/*                 APPLY "CHOOSE" TO B-LagerAntBlank. */
                ASSIGN B-Avdeling:SENSITIVE = FALSE
                       B-AvdelingBlank:SENSITIVE = FALSE
                       B-HuvGr:SENSITIVE = FALSE
                       B-HgBlank:SENSITIVE = FALSE
                       B-VarGr:SENSITIVE = FALSE
                       B-VgBlank:SENSITIVE = FALSE
                       B-LevNr:SENSITIVE = TRUE
                       B-LevNrBlank:SENSITIVE = TRUE.
            END.
            WHEN "BUTSTAT" THEN DO:
                APPLY "CHOOSE" TO B-AvdelingBlank.
                APPLY "CHOOSE" TO B-HgBlank.
                APPLY "CHOOSE" TO B-VgBlank.
                APPLY "CHOOSE" TO B-LevNrBlank.
                APPLY "CHOOSE" TO B-SesongBlank.
                APPLY "CHOOSE" TO B-FargBlank.
/*                 APPLY "CHOOSE" TO B-LagerAntBlank. */
                ASSIGN B-Avdeling:SENSITIVE = FALSE
                       B-AvdelingBlank:SENSITIVE = FALSE
                       B-HuvGr:SENSITIVE = FALSE
                       B-HgBlank:SENSITIVE = FALSE
                       B-VarGr:SENSITIVE = FALSE
                       B-VgBlank:SENSITIVE = FALSE
                       B-LevNr:SENSITIVE = FALSE
                       B-LevNrBlank:SENSITIVE = FALSE.
            END.
            WHEN "ARTIKKEL" OR WHEN "ARTSTR" THEN DO:
/*                 APPLY "CHOOSE" TO B-AvdelingBlank. */
                ASSIGN B-Avdeling:SENSITIVE = TRUE
                       B-AvdelingBlank:SENSITIVE = TRUE
                       B-HuvGr:SENSITIVE = TRUE
                       B-HgBlank:SENSITIVE = TRUE
                       B-VarGr:SENSITIVE = TRUE
                       B-VgBlank:SENSITIVE = TRUE
                       B-LevNr:SENSITIVE = TRUE
                       B-LevNrBlank:SENSITIVE = TRUE.
/*                        Tg-VisButikker:CHECKED = FALSE. */
            END.
        END CASE.
        ASSIGN B-Artikkelkort:SENSITIVE = FALSE
               B-Lagerliste:SENSITIVE   = FALSE
               RS-DataUrval:SENSITIVE   = FALSE.
        IF NOT TG-AvFilter:SENSITIVE THEN
            ASSIGN TG-AvFilter:CHECKED = FALSE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Alarm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Alarm fFrameWin
ON VALUE-CHANGED OF TG-Alarm IN FRAME fMain /* Alarmliste neg lagerteller */
DO:
  IF SELF:CHECKED THEN
      CB-Antall:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cAlle.
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
  
  ASSIGN cGetVerdier = getSumFelter("DataObjekt").
  PUBLISH "FeltVerdier" (OUTPUT cArtikkelNr,cGetVerdier,"SAME").                         
  IF cArtikkelNr = "" THEN
    RETURN.
  FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(cArtikkelNr) NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN 
      RETURN.
  fLockvindu(TRUE).
  IF lButikkBruker = TRUE THEN
      RUN w-vartkorJFSpec.w (RECID(artbas),"ENDRE").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject fFrameWin 
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  DELETE OBJECT hTTArt.
  ASSIGN hTTArt = ?.
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

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
  DISPLAY FI-Butikker FI-Avdeling FI-LevNr RS-Lagertype CB-ButikkTeam FI-HuvGr 
          FI-Sesong FI-VarGr FI-Farg CB-Antall RS-DataUrval Tg-VisButikker 
          TG-Alarm TG-AvFilter FI-LagerListeTxT 
      WITH FRAME fMain.
  ENABLE RECT-1 B-Avdeling B-AvdelingBlank B-LevNrBlank RS-Lagertype 
         CB-ButikkTeam B-Aktiver B-HgBlank B-SesongBlank B-VgBlank B-FargBlank 
         B-Artikkelkort CB-Antall Tg-VisButikker TG-Alarm TG-AvFilter B-LevNr 
         B-HuvGr B-VarGr B-Farg B-Sesong BUTTON-SokBut FI-LagerListeTxT 
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
          ASSIGN CB-ButikkTeam:LIST-ITEM-PAIRS = ",INGEN"
                 FI-Butikker = cButikerIdList
                 FI-Butikker:BGCOLOR = 15
                 FI-Butikker:SCREEN-VALUE = "(" + STRING(NUM-ENTRIES(cButikerIdList,"|")) + ")"
                 FI-Butikker:TOOLTIP = REPLACE(cButikerIdList,"|",",")
                 CB-ButikkTeam:SCREEN-VALUE = "INGEN"
                 CB-ButikkTeam:SENSITIVE    = FALSE.
      ELSE
          ASSIGN FI-Butikker:BGCOLOR = ?
                 FI-Butikker = ""
                 FI-Butikker:SCREEN-VALUE = ""
                 FI-Butikker:TOOLTIP = ""
                 CB-ButikkTeam:LIST-ITEM-PAIRS = cListItemPairs
                 CB-ButikkTeam:SCREEN-VALUE    = IF cUserDefaultBut <> "" AND CAN-DO(cTillgButikker,cUserDefaultBut) THEN 
                                                    cUserDefaultBut ELSE ENTRY(2,cListItemPairs)
                 CB-ButikkTeam:SENSITIVE    = TRUE.
                 .
     APPLY "VALUE-CHANGED" TO CB-ButikkTeam.
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
/* Tillægg før artstatistiken */
ASSIGN cArtFelter    = FILL(",",NUM-ENTRIES(cArtFieldDefs) - 1)
       cArtLabels    = cArtFelter
       cArtDecimaler = cArtFelter
       cArtRightCols = cArtFelter.
DO iCount = 1 TO NUM-ENTRIES(cArtFieldDefs):
    ASSIGN ENTRY(iCount,cArtFelter) = ENTRY(1,ENTRY(iCount,cArtFieldDefs),";")
           ENTRY(iCount,cArtLabels) = ENTRY(2,ENTRY(iCount,cArtFieldDefs),";")
           ENTRY(iCount,cArtDecimaler) = ENTRY(3,ENTRY(iCount,cArtFieldDefs),";")
           ENTRY(iCount,cArtRightCols) = ENTRY(4,ENTRY(iCount,cArtFieldDefs),";").
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetAlle fFrameWin 
PROCEDURE GetAlle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER cListe AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cType AS CHARACTER  NO-UNDO.
  IF cType = "VARGR" THEN DO:
      FOR EACH VarGr NO-LOCK:
          ASSIGN cListe = cListe + (IF cListe = "" THEN "" ELSE ",") + STRING(VarGr.Vg).
      END.
  END.
  ELSE IF cType = "LEV" THEN DO:
      FOR EACH LevBas NO-LOCK:
          ASSIGN cListe = cListe + (IF cListe = "" THEN "" ELSE ",") + STRING(LevBas.LevNr).
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetVareGrupper fFrameWin 
PROCEDURE GetVareGrupper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER cVareGrupper AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cVerdier     AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cType        AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  iCount       AS INTEGER    NO-UNDO.
    
  IF cType = "AVDELING" THEN DO:
      DO iCount = 1 TO NUM-ENTRIES(cVerdier):
          FIND Avdeling WHERE Avdeling.AvdelingNr = INT(ENTRY(iCount,cVerdier)) NO-LOCK NO-ERROR.
          FOR EACH HuvGr OF Avdeling NO-LOCK:
              FOR EACH VarGr OF HuvGr NO-LOCK.
                  ASSIGN cVareGrupper = cVareGrupper + (IF cVareGrupper = "" THEN "" ELSE ",") + 
                      STRING(VarGr.Vg).
              END.
          END.
      END.
  END.
  ELSE IF cType = "HOVEDGR" THEN DO:
      DO iCount = 1 TO NUM-ENTRIES(cVerdier):
          FIND HuvGr WHERE HuvGr.Hg = INT(ENTRY(iCount,cVerdier)) NO-LOCK NO-ERROR.
          IF AVAIL HuvGr THEN DO:
              FOR EACH VarGr OF HuvGr NO-LOCK.
                  ASSIGN cVareGrupper = cVareGrupper + (IF cVareGrupper = "" THEN "" ELSE ",") + 
                      STRING(VarGr.Vg).
              END.
          END.
      END.
  END.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB fFrameWin 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cStTypeId         AS CHARACTER INIT "AVDELING" NO-UNDO.
/*     DEFINE VARIABLE cListItemPairs    AS CHARACTER  NO-UNDO. */
    DEFINE VARIABLE cButString        AS CHARACTER  NO-UNDO.
    FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK.
    IF AVAIL bruker AND Bruker.BrukerType = 1 THEN
        lButikkBruker = FALSE.
    ELSE
        lButikkBruker = TRUE.
    ASSIGN cUserDefaultBut = STRING(bruker.butikknr)
           cListItemPairs = "".
    FOR EACH ButikkTeam NO-LOCK WHERE ButikkTeam.BrGrpNr = Bruker.BrGrpNr AND
                              ButikkTeam.TeamTypeId = 2 BY ButikkTeam.Beskrivelse.
        ASSIGN cButString = "".
        FOR EACH ButikkKobling OF ButikkTeam NO-LOCK.
            ASSIGN cButString = cButString + (IF cButString = "" THEN "" ELSE CHR(1)) 
                              + STRING(ButikkKobling.butik).
            FIND TT_TillgButikker WHERE TT_TillgButikker.Butik = ButikkKobling.butik NO-ERROR.
            IF NOT AVAIL TT_TillgButikker THEN DO:
                    CREATE TT_TillgButikker.
                    ASSIGN TT_TillgButikker.Butik = ButikkKobling.butik.
            END.
        END.
        IF NUM-ENTRIES(cButString,CHR(1)) > 25 THEN DO:
            CREATE TT_BigListItem.
            ASSIGN TT_BigListItem.Butiker = cButString
                   cButString = "(" + STRING(NUM-ENTRIES(cButString,CHR(1))) + ");" + STRING(ROWID(TT_BigListItem)).
            RELEASE TT_BigListItem.
        END.
        ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") +
                             ButikkTeam.Beskrivelse + "," + cButString.
    END.
    FOR EACH TT_TillgButikker:
        ASSIGN cTillgButikker = cTillgButikker + (IF cTillgButikker <> "" THEN "," ELSE "") + STRING(TT_TillgButikker.Butik).
    END.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN CB-ButikkTeam:LIST-ITEM-PAIRS = cListItemPairs
               CB-ButikkTeam:SCREEN-VALUE    = ENTRY(2,cListItemPairs).
    END.
/*     ASSIGN CB-Antall:LIST-ITEMS   = cAlle + ", < 0, <= 0, = 0, >= 0, > 0, <> 0" */
    ASSIGN CB-Antall:LIST-ITEMS   = cAlle + ", > 0" /* , = 0, < 0" */
           CB-Antall              = cAlle
           CB-Antall:SCREEN-VALUE = cAlle
        .
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
  {syspara.i 1 100 1 cAlle}
  {syspara.i 220 1 2 cVisFelterTxt}
  {syspara.i 1 1 54 cSkomodus}
  IF cAlle = "" THEN
      ASSIGN cAlle = "[Alle]".
  RUN FixStrings.
  RUN SkapaTTArt.
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  RUN InitCB.
  PUBLISH "GetWindowH" (OUTPUT h_Window ).
  IF VALID-HANDLE(h_Window) THEN DO:
      ASSIGN h_dstlager = DYNAMIC-FUNCTION('geth_dstlager':U IN h_Window).
      ASSIGN h_frapportgrid = DYNAMIC-FUNCTION('geth_frapportgrid':U IN h_Window).
  END.
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN cFilename = SESSION:TEMP-DIRECTORY + "gridstlager.txt".
/*              FI-Avdeling  = "*"                */
/*              FI-Avdeling:SCREEN-VALUE = cAlle. */
      APPLY "CHOOSE" TO B-AvdelingBlank.
      APPLY "CHOOSE" TO B-HgBlank.
      APPLY "CHOOSE" TO B-VgBlank.
      APPLY "VALUE-CHANGED" TO RS-Lagertype.
      IF INT(cUserDefaultBut) > 0 AND CAN-DO(CB-ButikkTeam:LIST-ITEM-PAIRS,cUserDefaultBut) THEN
          ASSIGN CB-ButikkTeam:SCREEN-VALUE = cUserDefaultBut NO-ERROR.
      APPLY "VALUE-CHANGED" TO CB-ButikkTeam.
      ASSIGN CB-Antall:SCREEN-VALUE = " > 0".
/*       ASSIGN CB-Antall:SENSITIVE = FALSE. */
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KalkuleraTTLager fFrameWin 
PROCEDURE KalkuleraTTLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iLnNr AS INTEGER    NO-UNDO.
/* OUTPUT TO VALUE(cFileName).                                                                                                                          */
/* PUT UNFORMATTED "|" REPLACE(REPLACE(cLabels + "," + cArtLabels,",","|"),"DataobjektTXT",getLabel(RS-Lagertype:SCREEN-VALUE IN FRAME {&FRAME-NAME})). */
/*                                                                                                                                                      */
/* ASSIGN iLnNr = 0.                                                                                                                                    */
  FOR EACH TT_Lager:
      ASSIGN TT_Lager.UtSolgt% = IF TT_Lager.KjopAnt > 0 THEN ROUND(TT_Lager.AntSolgt / TT_Lager.KjopAnt * 100,1)
                                    ELSE 0
             TT_Lager.Db%      = IF TT_Lager.VerdiSolgt <> 0 THEN ROUND(TT_Lager.DbKr / TT_Lager.VerdiSolgt * 100,1)
                                    ELSE 0
             TT_Lager.Solgt%    = IF dSolgtTot <> 0 THEN ROUND(TT_Lager.VerdiSolgt / dSolgtTot * 100,1)
                                     ELSE 0
             TT_Lager.DBandel%  = IF dDBTot <> 0 THEN ROUND(TT_Lager.DbKr / dDBTot * 100,1)
                                    ELSE 0
             TT_Lager.Rabandel% = IF dRabTot <> 0 THEN ROUND(TT_Lager.VerdiRabatt / dRabTot * 100,1)
                                    ELSE 0
             TT_Lager.Kjopandel% = IF dKjopTot <> 0 THEN ROUND(TT_Lager.KjopVerdi / dKjopTot * 100,1)
                                    ELSE 0.
/*                TT_Lager.VVarekost = IF TT_Lager.LagAnt <> 0 THEN ROUND(TT_Lager.LagerVerdi / TT_Lager.LagAnt,2) ELSE 0. */
      FIND ArtPris WHERE ArtPris.ArtikkelNr = TT_Lager.ArtikkelNr AND
                         ArtPris.Profil     = 1 NO-LOCK NO-ERROR.
      IF AVAIL ArtPris THEN DO:
          ASSIGN TT_Lager.T_db% = ArtPris.db%[IF NOT ArtPris.Tilbud THEN 1 ELSE 2]
                 TT_Lager.Pris  = ArtPris.Pris[IF NOT ArtPris.Tilbud THEN 1 ELSE 2].
                 TT_Lager.T_Lagerverdi = TT_Lager.LagAnt * TT_Lager.Pris.
      END.
/*       iLnNr = iLnNr + 1.                                                                 */
/*       PUT CONTROL CHR(10).                                                               */
/*       PUT UNFORMATTED iLnNr                                 "|"                          */
/*              /*  1 */ TT_Lager.ArtikkelNr                   "|"                          */
/*              /*  2 */ REPLACE(TT_Lager.Beskrivelse,"|","ö") "|"                          */
/*              /*  3 */ TT_Lager.CharButik                    "|"                          */
/*              /*  4 */ ROUND(TT_Lager.AntSolgt,0)       "|"                               */
/*              /*  5 */ ROUND(TT_Lager.VerdiSolgt,0)     "|"                               */
/*              /*  6 1 */ TRIM(STRING(ROUND(TT_Lager.UtSolgt%,1),"->>>>>>>>>>>9.9"))  "|"  */
/*              /*  7 */ ROUND(TT_Lager.DbKr,0)       "|"                                   */
/*              /*  8 1 */ TRIM(STRING(ROUND(TT_Lager.Db%,1),"->>>>>>>>>>>9.9"))       "|"  */
/*              /*  9 */ ROUND(TT_Lager.AntRab,0)      "|"                                  */
/*              /* 10 */ ROUND(TT_Lager.VerdiRabatt,0) "|"                                  */
/*              /* 11 */ ROUND(TT_Lager.LagAnt,0)      "|"                                  */
/*              /* 12 2 */ TRIM(STRING(ROUND(TT_Lager.LagerVerdi,2),"->>>>>>>>>>9.99")) "|" */
/*              /* 13 2 */ TRIM(STRING(ROUND(TT_Lager.VVarekost,2),"->>>>>>>>>>9.99"))  "|" */
/*              /* 14 */ ROUND(TT_Lager.ReklAnt,0)       "|"                                */
/*              /* 15 */ ROUND(TT_Lager.ReklVerdi,0)     "|"                                */
/*              /* 16 */ ROUND(TT_Lager.ReklLAnt,0)      "|"                                */
/*              /* 17 */ ROUND(TT_Lager.ReklLVerdi,0)    "|"                                */
/*              /* 18 */ ROUND(TT_Lager.RetLAnt,0)       "|"                                */
/*              /* 19 */ ROUND(TT_Lager.SvinnAnt,0)      "|"                                */
/*              /* 20 */ ROUND(TT_Lager.SvinnVerdi,0)    "|"                                */
/*              /* 21 */ ROUND(TT_Lager.GjenkjopAnt,0)   "|"                                */
/*              /* 22 */ ROUND(TT_Lager.GjenkjopVerdi,0) "|"                                */
/*              /* 23 */ ROUND(TT_Lager.KjopAnt,0)       "|"                                */
/*              /* 24 */ ROUND(TT_Lager.KjopVerdi,0)     "|"                                */
/*              /* 25 */ ROUND(TT_Lager.BrekkAnt,0)      "|"                                */
/*              /* 26 */ ROUND(TT_Lager.BrekkVerdi,0)    "|"                                */
/*              /* 27 */ ROUND(TT_Lager.IntAnt,0)        "|"                                */
/*              /* 28 */ ROUND(TT_Lager.IntVerdi,0)      "|"                                */
/*              /* 29 */ ROUND(TT_Lager.JustAnt,0)       "|"                                */
/*              /* 30 */ ROUND(TT_Lager.JustVerdi,0)     "|"                                */
/*              /* 31 */ ROUND(TT_Lager.OvAnt,0)         "|"                                */
/*              /* 32 */ ROUND(TT_Lager.OvVerdi,0)       "|"                                */
/*              /* 33 1 */ TRIM(STRING(ROUND(TT_Lager.T_db%,1),"->>>>>>>>>>>9.9")) "|"      */
/*              /* 34 2 */ TRIM(STRING(ROUND(TT_Lager.Pris,2),"->>>>>>>>>>9.99"))  "|"      */
/*              /* 35 */ ROUND(TT_Lager.T_Lagerverdi,0)   "|"                               */
/*              /* 36 */ TT_Lager.AvdelingNr              "|"                               */
/*              /* 37 */ TT_Lager.AvdelingNavn            "|"                               */
/*              /* 38 */ TT_Lager.Hg                      "|"                               */
/*              /* 39 */ TT_Lager.HgBeskr                 "|"                               */
/*              /* 40 */ TT_Lager.Vg                      "|"                               */
/*              /* 41 */ TT_Lager.VgBeskr                 "|"                               */
/*              /* 42 */ TT_Lager.VgLopNr                 "|"                               */
/*              /* 43 */ TT_Lager.LevNr                   "|"                               */
/*              /* 44 */ TT_Lager.LevNamn                 "|"                               */
/*              /* 45 */ TT_Lager.SasBeskr                "|"                               */
/*              /* 46 */ TT_Lager.FarBeskr.                                                 */
  END.
/*   OUTPUT CLOSE. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Lagerliste fFrameWin 
PROCEDURE Lagerliste :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRadNr AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cGetVerdier   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cColVerdier   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cVareGrupper  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLeverandorer AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iListeNr      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE rListerRecid  AS RECID      NO-UNDO.
  DEFINE VARIABLE cAlleVg       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAlleLev      AS CHARACTER  NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      hTTArt:DEFAULT-BUFFER-HANDLE:EMPTY-TEMP-TABLE().
      DO:
/*           PUBLISH  "FillTT" (INPUT BUFFER TT_Art:HANDLE,"ArtikkelNr",getSumFelter("DataObjekt"),"DECI",INT(RS-DataUrval:SCREEN-VALUE)).  */
          PUBLISH  "FillTT" (hTTArt:DEFAULT-BUFFER-HANDLE,"ArtikkelNr",getSumFelter("DataObjekt"),"DECI",INT(RS-DataUrval:SCREEN-VALUE)).
          RUN DynLagerliste.p (INPUT hTTArt,"FOR EACH TTArt","Artikkelnr",IF FI-Butikker = "" THEN REPLACE(CB-ButikkTeam:SCREEN-VALUE,CHR(1),",") ELSE REPLACE(FI-Butikker,"|",",")).
          hTTArt:DEFAULT-BUFFER-HANDLE:EMPTY-TEMP-TABLE().
          RETURN.
      END.
/*       ELSE IF NOT CAN-DO("AVDELING,HOVEDGR,VAREGR,LEVERAN",RS-Lagertype:SCREEN-VALUE) THEN       */
/*           RETURN.                                                                                */
/*       ASSIGN cGetVerdier = getSumFelter("DataObjekt").                                           */
/*       PUBLISH "FeltVerdier" (OUTPUT cRadNr,"0","SAME").                                          */
/*       IF cRadNr = "" THEN                                                                        */
/*         RETURN.                                                                                  */
/*       IF RS-DataUrval:SCREEN-VALUE = "1" THEN                                                    */
/*           PUBLISH "FeltVerdier" (OUTPUT cColVerdier,cGetVerdier,"SAME").                         */
/*       ELSE                                                                                       */
/*           PUBLISH "ColVerdier" (OUTPUT cColVerdier,cGetVerdier) /* CHR(1) sep */.                */
/*       IF cColVerdier = "" THEN                                                                   */
/*           RETURN.                                                                                */
/*       ASSIGN cColVerdier = REPLACE(cColVerdier,CHR(1),",").                                      */
/*       CASE RS-Lagertype:SCREEN-VALUE:                                                            */
/*           WHEN "AVDELING" OR WHEN "HOVEDGR" OR WHEN "VAREGR" THEN DO:                            */
/*               ASSIGN cLeverandorer = cAlle                                                       */
/*                      cAlleLev      = cAlle.                                                      */
/*               IF RS-Lagertype:SCREEN-VALUE = "VAREGR" THEN                                       */
/*                   ASSIGN cVareGrupper = cColVerdier.                                             */
/*               ELSE                                                                               */
/*                   RUN GetVareGrupper(OUTPUT cVaregrupper,cColVerdier,RS-Lagertype:SCREEN-VALUE). */
/*               IF cVareGrupper = "" THEN                                                          */
/*                   RETURN. /* Kanske message */                                                   */
/*               RUN GetAlle (OUTPUT cAlleVg,"VARGR").                                              */
/*           END.                                                                                   */
/*           WHEN "LEVERAN" THEN DO:                                                                */
/*               ASSIGN cVareGrupper  = cAlle                                                       */
/*                      cAlleVg       = cAlle                                                       */
/*                      cLeverandorer = cColVerdier.                                                */
/*               RUN GetAlle (OUTPUT cAlleLev,"LEV").                                               */
/*           END.                                                                                   */
/*           OTHERWISE /* egentligen onödigt */                                                     */
/*               RETURN.                                                                            */
/*       END CASE.                                                                                  */
  END.
/*   FIND FIRST Lister WHERE Lister.Listetype = "LAGERLISTE" NO-LOCK NO-ERROR.                */
/*   IF AVAIL Lister AND Lister.ListeNr < 0 THEN                                              */
/*       ASSIGN iListeNr = Lister.ListeNr - 1.                                                */
/*   ELSE                                                                                     */
/*       ASSIGN iListeNr =  -1.                                                               */
/*   CREATE Lister.                                                                           */
/*   ASSIGN Lister.ListeNr     = iListeNr                                                     */
/*          Lister.ListeType   = "LAGERLISTE"                                                 */
/*          Lister.Beskrivelse = "Fra lagerrapport"                                           */
/*          rListerRecid       = RECID(Lister)                                                */
/*          Kriterier[1]       = cVareGrupper  + "|" + cAlleVg                                */
/*          Kriterier[2]       = cLeverandorer + "|" + cAlleLev                               */
/*          Kriterier[3]       = "3"                                                          */
/*          Kriterier[4]       = cAlle + "|" + cAlle                                          */
/*          Kriterier[5]       = cAlle + "|" + cAlle                                          */
/*          Kriterier[6]       = cAlle + "|" + cAlle                                          */
/*          Kriterier[7]       = cAlle + "|" + cAlle                                          */
/*          Kriterier[8]       = "FALSE"   /* per but */                                      */
/*          Kriterier[9]       = ";"                                                          */
/*          Kriterier[10]      = ";"                                                          */
/*          Kriterier[11]      = "?;?"                                                        */
/*          Kriterier[12]      = "1;99"                                                       */
/*          Kriterier[13]      = "0;9999999999999"                                            */
/*          Kriterier[14]      = "*"                                                          */
/*          Kriterier[15]      = "0;99999999"                                                 */
/*          Kriterier[16]      = "FALSE"                                                      */
/*          Kriterier[17]      = cAlle + "|" + cAlle                                          */
/*          Kriterier[18]      = "?,?,?,?"                                                    */
/*          Kriterier[19]      = "FALSE".                                                     */
/*                                                                                            */
/*   run byggartikkler.p /* persistent set wChild */                                          */
/*                      (rListerRecid,                                                        */
/*                       ?, /* FI-FraDato */                                                  */
/*                       ?, /* FI-TilDato */                                                  */
/*                       ?, /* FI-FraLevDato */                                               */
/*                       ?, /* FI-TilLevDato */                                               */
/*                       ?, /* FI-FraLopNr */                                                 */
/*                       ?, /* FI-TilLopNr */                                                 */
/*                       0, /* FI-FraArtikkelNr */                                            */
/*                       9999999999999, /*FI-TilArtikkelNr */                                 */
/*                       "*", /* FI-LevKod */                                                 */
/*                       3,   /* RS-Aktiv */                                                  */
/*                       1,   /* FI-FraKateg */                                               */
/*                       99,  /* FI-TilKateg */                                               */
/*                       0,   /* FI-FraBildNr */                                              */
/*                       99999999, /* FI-TilBildNr */                                         */
/*                       ?,        /* FI-FraFraTilbud */                                      */
/*                       ?,        /* FI-TilFraTilbud */                                      */
/*                       ?,        /* FI-FraTilTilbud */                                      */
/*                       ?,        /* FI-TilTilTilbud */                                      */
/*                       FALSE,     /* T-Butikk */                                            */
/*                       cVareGrupper,  /* wvVareGrupper */                                   */
/*                       cLeverandorer, /* wvLeverandorer */                                  */
/*                       cAlle, /* wvSesonger */                                              */
/*                       cAlle, /* wvVmId */                                                  */
/*                       cAlle, /* wvFarger */                                                */
/*                       cAlle, /* wvMaterial */                                              */
/*                       REPLACE(CB-ButikkTeam:SCREEN-VALUE,CHR(1),","),      /* wvButiker */ */
/*                       FALSE, /* T-Annonse */                                               */
/*                       FALSE, /* T-AktivTilbud */                                           */
/*                       ?).    /* this-procedure:handle) */                                  */
/*   run w-rutskrkolleksjon.w persistent set hUtskriftProg (Lister.ListeType).                */
/*   if valid-handle(hUtskriftProg) then                                                      */
/*     run InitListe in hUtskriftProg (input rListerRecid).                                   */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnIGrid fFrameWin 
PROCEDURE LesInnIGrid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cSumCols    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKalkCols   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSumString  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cExtraFelt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilleggsFelter AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cVisFelterTxtTmp AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iFrozenCols AS INTEGER     NO-UNDO.
  DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
  cTilleggsFelter = "Butnamn".
  DO WITH FRAME {&FRAME-NAME}:
      cVisFelterTxtTmp = cVisFelterTxt.

      iFrozenCols = IF RS-Lagertype:SCREEN-VALUE <> "ARTSTR" THEN INT(getSumFelter("CharButik")) ELSE INT(getSumFelter("Storl")).

      PUBLISH "VisTxtBox" ("Leser inn og bearbeider data......").
      DO:
          PUBLISH "LoadGrid" (cFileName,iFrozenCols + 1).
          ASSIGN cSumCols = getSumFelter(cSummerFelter)
              /* entry 1=antal decimaler,2=col där resultatet skall stå,3= det som står över vid div, 4 = det som står under */
                 cKalkCols = "1," + getSumFelter("Db%") + "," + getSumFelter("DbKr") + "," + getSumFelter("VerdiSolgt") + ";" +
                             "2," + getSumFelter("Utsolgt%") + "," + getSumFelter("AntSolgt") + "," + getSumFelter("KjopAnt")
                 cSumString = IF RS-Lagertype:SCREEN-VALUE <> "ARTSTR" THEN getSumFelter("CharButik") + ",SUM" ELSE getSumFelter("Storl") + ",SUM" .
          IF RS-Lagertype:SCREEN-VALUE = "ARTIKKEL" THEN
              ASSIGN cSumCols = cSumCols + ",35".
          IF cVisFelterTxt <> "" THEN DO:
              IF cSkomodus = "1" THEN
                  cVisFelterTxt = cVisFelterTxt + "," + ENTRY(1,cVgLop,";").
              cExtrafelt = "".
              DO ii = 1 TO NUM-ENTRIES(cTilleggsFelter):
                  IF CAN-DO(cFelter,ENTRY(ii,cTilleggsFelter)) THEN
                      cExtraFelt = cExtraFelt + "," + ENTRY(ii,cTilleggsFelter).
              END.
              cVisFelterNr = getSumFelter(cVisFelterTxt + cExtraFelt).
          END.
          PUBLISH "Summer" (cSumCols + ";" + cKalkCols,cSumString).
          IF cVisFelterNr <> "" AND RS-LagerType:SCREEN-VALUE <> "ARTSTR" THEN DO:
              PUBLISH "VisKun" (cVisFelterNr,"SKJUL").
              IF TG-Alarm:SENSITIVE AND TG-Alarm:CHECKED THEN
                  PUBLISH "VisKun" (cVisFelterNr,"").
          END.
      END.
  END.
  ASSIGN cVisFelterTxt = cVisFelterTxtTmp.
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

    ASSIGN cGetVerdier = STRING(LOOKUP("DataObjekt",cFelter)).

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
/* DEFINE        VARIABLE  cFstPeriode    AS CHARACTER  NO-UNDO.  */
DEFINE        VARIABLE  cButikker      AS CHARACTER  NO-UNDO.
/* DEFINE        VARIABLE  cPeriodeTmp    AS CHARACTER  NO-UNDO.  */
/* DEFINE        VARIABLE  cPeriode       AS CHARACTER  NO-UNDO.  */
/* DEFINE        VARIABLE  cFraAar         AS CHARACTER  NO-UNDO. */
/* DEFINE        VARIABLE  cTilAar         AS CHARACTER  NO-UNDO. */
/* DEFINE        VARIABLE  cFraPerLinNr    AS CHARACTER  NO-UNDO. */
/* DEFINE        VARIABLE  cTilPerLinNr    AS CHARACTER  NO-UNDO. */
    DO WITH FRAME {&FRAME-NAME}:
        IF NOT TG-Avfilter:CHECKED THEN DO:
            ASSIGN cButikker = (IF FI-Butikker <> "" THEN REPLACE(FI-Butikker,"|",",") ELSE REPLACE(CB-ButikkTeam:SCREEN-VALUE,CHR(1),",")).
            RELEASE Butiker.
            IF NUM-ENTRIES(cButikker) = 1 THEN
                FIND Butiker WHERE Butiker.Butik = INT(cButikker) NO-LOCK NO-ERROR.
            ASSIGN cFilterVerdier = IF AVAIL Butiker THEN "Butikk: " + Butiker.Butnamn ELSE "Butikker: " + cButikker.
        END.
        IF RS-Lagertype:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "ARTIKKEL" THEN DO:
            ASSIGN cColAlign = cRightCols + "," +  cArtRightCols.
            IF cSkomodus = "1" THEN
                ENTRY(2,cColAlign) = ENTRY(4,cVgLop,";") + "," + ENTRY(2,cColAlign).
        END.
        ELSE
            ASSIGN cColAlign = cRightCols.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setAVfilter fFrameWin 
PROCEDURE setAVfilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN TG-AvFilter:CHECKED IN FRAME {&FRAME-NAME} = TRUE
          RS-Lagertype:SCREEN-VALUE = "ARTIKKEL".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTTArt fFrameWin 
PROCEDURE SkapaTTArt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CREATE TEMP-TABLE hTTArt.
    hTTArt:ADD-LIKE-FIELD("ArtikkelNr","ArtBas.ArtikkelNr").
    hTTArt:ADD-NEW-INDEX("Artikkel",YES).
    hTTArt:ADD-INDEX-FIELD("Artikkel","ArtikkelNr").
    hTTArt:TEMP-TABLE-PREPARE("TTArt").

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
  DEFINE VARIABLE cQryString AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLagAntStr AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN INPUT CB-Antall.
    IF CB-Antall <> cAlle THEN
        ASSIGN cLagAntStr = (IF RS-Lagertype:SCREEN-VALUE = "ARTIKKEL" THEN " WHERE " ELSE " AND ") +
            "LagAnt " + REPLACE(CB-Antall,"0", "'0'").
    IF RS-Lagertype:SCREEN-VALUE = "ARTIKKEL" THEN DO:
    END.
    ELSE DO:
        IF CB-Antall <> cAlle THEN
            ASSIGN cLagAntStr =   " AND StLager.LagAnt " + REPLACE(CB-Antall,"0", "'0'").
        ASSIGN cQryString = SUBSTITUTE("FOR EACH StLager WHERE StTypeId = '&1'",RS-Lagertype:SCREEN-VALUE)
                              + cLagAntStr + " AND SUBSTBUTIK NO-LOCK".
/*                                           + cLagAntStr. */
        DYNAMIC-FUNCTION('setQueryString':U IN h_dstlager,
           INPUT cQryString /* CHARACTER */).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSokArt fFrameWin 
PROCEDURE StartSokArt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: 
         
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cButiker  AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  dTotSum   AS DECIMAL    NO-UNDO.
    DEFINE        VARIABLE  iCount       AS INTEGER    NO-UNDO.
    DEFINE        VARIABLE  lUtvidetFilter AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  lIkkeTreff     AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  cLagAnt        AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  lOK            AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  iLagAnt        AS INTEGER     NO-UNDO.
    DEFINE        VARIABLE  iNegLagAnt     AS INTEGER     NO-UNDO.
    DEFINE        VARIABLE  logTMP AS LOGICAL    NO-UNDO.
    EMPTY TEMP-TABLE TT_Lager.
    EMPTY TEMP-TABLE TT_LagerTMP.
    DO WITH FRAME {&FRAME-NAME}:
        CREATE TT_LagerTMP. /* en temporär record för att kunna summera */
        ASSIGN cLagAnt = IF CB-Antall:SCREEN-VALUE = cAlle THEN "*" ELSE REPLACE(REPLACE(CB-Antall:SCREEN-VALUE,"0","")," ","").
/*         FOR EACH ArtBas WHERE ArtBas.Opris = FALSE AND ArtBas.Lager = TRUE NO-LOCK: */
        FOR EACH ArtBas WHERE ArtBas.Opris = FALSE AND ArtBas.Lager = TRUE AND CAN-DO(FI-Levnr,STRING(Artbas.LevNr)) AND
                              CAN-DO(FI-Farg,STRING(Artbas.Farg)) AND CAN-DO(FI-Sesong,STRING(ArtBas.Sasong)) NO-LOCK:

            FIND HuvGr OF ArtBas NO-LOCK NO-ERROR.
            IF FI-Avdeling <> "*" THEN DO:
                IF NOT AVAIL HuvGr OR NOT CAN-DO(FI-Avdeling,STRING(HuvGr.AvdelingNr)) THEN
                    NEXT.
            END.
            DO iCount = 1 TO NUM-ENTRIES(cButiker):
                FIND Lager WHERE Lager.ArtikkelNr = ArtBas.ArtikkelNr AND Lager.Butik = INT(ENTRY(iCount,cButiker)) NO-LOCK NO-ERROR.
                IF NOT AVAIL Lager THEN
                    NEXT.
/*                 IF cLagAnt <> "*" THEN DO:            */
/*                     ASSIGN lOK = FALSE.               */
/*                     CASE cLagAnt:                     */
/*                         WHEN "<" THEN                 */
/*                             IF Lager.Lagant < 0 THEN  */
/*                                 ASSIGN lOK = TRUE.    */
/*                         WHEN "<=" THEN                */
/*                             IF Lager.Lagant <= 0 THEN */
/*                                 ASSIGN lOK = TRUE.    */
/*                         WHEN "=" THEN                 */
/*                             IF Lager.Lagant = 0 THEN  */
/*                                 ASSIGN lOK = TRUE.    */
/*                         WHEN ">=" THEN                */
/*                             IF Lager.Lagant >= 0 THEN */
/*                                 ASSIGN lOK = TRUE.    */
/*                         WHEN ">" THEN                 */
/*                             IF Lager.Lagant > 0 THEN  */
/*                                 ASSIGN lOK = TRUE.    */
/*                         WHEN "<>" THEN                */
/*                             IF Lager.Lagant <> 0 THEN */
/*                                 ASSIGN lOK = TRUE.    */
/*                     END CASE.                         */
/*                     IF lOK = FALSE THEN               */
/*                         NEXT.                         */
/*                 END. */
                ASSIGN iLagAnt    = 0
                       iNegLagAnt = 0.
                FOR EACH artlag WHERE artlag.artikkelnr = artbas.artikkelnr AND artlag.butik = lager.butik NO-LOCK.
                    IF artlag.lagant >= 0 THEN
                        iLagAnt = iLagAnt + artlag.lagant.
                    ELSE
                        iNegLagAnt = iNegLagAnt + artlag.lagant.
                END.
                BUFFER-COPY Lager TO TT_LagerTMP.
                ASSIGN TT_LagerTMP.lagant = iLagAnt
                       TT_LagerTMP.Neglagant = iNegLagAnt.
                FIND CURRENT TT_LagerTMP.
                RELEASE TT_Lager.
                IF NOT Tg-VisButikker:CHECKED THEN DO:
                    ASSIGN TT_LagerTMP.Butik = 0.
                    FIND TT_Lager WHERE TT_Lager.ArtikkelNr = TT_LagerTMP.ArtikkelNr AND TT_Lager.Butik = TT_LagerTMP.Butik NO-ERROR.
                END.
                IF AVAIL TT_Lager THEN
                    RUN Summera.
                ELSE DO:
                    CREATE TT_Lager.
                    BUFFER-COPY TT_LagerTMP TO TT_Lager.
                    ASSIGN TT_Lager.Beskrivelse  = ArtBas.Beskr
                           TT_Lager.AvdelingNr   = IF AVAIL HuvGr THEN HuvGr.AvdelingNr ELSE 0
                           TT_Lager.AvdelingNavn = getBeskr("AVDELING",STRING(TT_Lager.AvdelingNr))
                           TT_Lager.Hg           = ArtBas.Hg
                           TT_Lager.HgBeskr      = getBeskr("HOVEDGR",STRING(TT_Lager.Hg))
                           TT_Lager.Vg           = ArtBas.Vg
                           TT_Lager.VgBeskr      = getBeskr("VAREGR",STRING(TT_Lager.Vg))
        /*                    TT_Lager.VgLopNr       = STRING(ArtBas.Vg) + "/" + REPLACE(STRING(ArtBas.LopNr,"z999"),"0","  ") */
                           TT_Lager.VgLopNr       = STRING(ArtBas.Vg) + "/" + FILL("  ",4 - LENGTH(STRING(ArtBas.LopNr))) + STRING(ArtBas.LopNr)
                           TT_Lager.LevNr       = ArtBas.LevNr
                           TT_Lager.LevNamn     = getBeskr("LEVERAN",STRING(TT_Lager.LevNr))
                           TT_Lager.Sasong      = ArtBas.Sasong
                           TT_Lager.SasBeskr    = getBeskr("SASONG",STRING(ArtBas.Sasong))
                           TT_Lager.Farg        = ArtBas.Farg
                           TT_Lager.FarBeskr    = getBeskr("FARG",STRING(ArtBas.Farg))
                           TT_Lager.CharButik   = IF Tg-VisButikker:CHECKED THEN STRING(TT_Lager.Butik) ELSE ""
                           TT_Lager.DbKr        = TT_Lager.VerdiSolgt - (TT_Lager.AntSolgt * TT_Lager.VVarekost)
                           TT_Lager.VmId        = Artbas.vmid
                           TT_Lager.Levfargkod  = Artbas.Levfargkod
                           TT_Lager.Vmbeskr     = IF Artbas.vmid = 0 THEN "" ELSE getBeskr("VM",STRING(ArtBas.VMId))
                          /* Här sätter vi den totala lagervärdet utifrån LagAnt och VVarekost */
                          /* vid flera butiker summeras den och längre ner delar vi det totala lagervärdet med totala LagAnt */
                          /* som ger vvarekost/st */
                           TT_Lager.LagerVerdi     = TT_Lager.LagAnt * TT_Lager.VVarekost
                           TT_Lager.NegLagerVerdi     = TT_Lager.NegLagAnt * TT_Lager.VVarekost.
                    IF Tg-VisButikker:CHECKED THEN 
                        TT_Lager.Butnamn = getBeskr("GETBUTNAVN",STRING(TT_Lager.Butik)).
                END.
            END.
        END.
    END.

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
    DEFINE INPUT  PARAMETER qh        AS HANDLE     NO-UNDO.
    DEFINE INPUT  PARAMETER lLocal    AS LOGICAL    NO-UNDO.
    DEFINE INPUT  PARAMETER cButiker  AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  cStatus   AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  dTotSum   AS DECIMAL    NO-UNDO.
    DEFINE        VARIABLE  iCount       AS INTEGER    NO-UNDO.
    DEFINE        VARIABLE  lUtvidetFilter AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  lIkkeTreff     AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  cLagAnt        AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  lOK            AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  cQry           AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  dArtikkelNr    AS DECIMAL    NO-UNDO.
    DEFINE        VARIABLE  iHg            LIKE HuvGr.Hg NO-UNDO.
    DEFINE        VARIABLE  lAvancert      AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  iLagAnt        AS INTEGER     NO-UNDO.
    DEFINE        VARIABLE  iNegLagAnt     AS INTEGER     NO-UNDO.
    DEFINE        VARIABLE  cLagertype AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cLabelsTMP AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cDecimalerTMP AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cFelterTmp  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cFelterBku  AS CHARACTER  NO-UNDO.
    EMPTY TEMP-TABLE TT_Lager.
    EMPTY TEMP-TABLE TT_LagerTMP.
    ASSIGN dSolgtTot = 0
           dDBTot    = 0
           dRabTot   = 0
           dKjopTot  = 0.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN cLagertype = RS-Lagertype:SCREEN-VALUE.
        IF cButiker BEGINS "HENTINTERNT" THEN DO:
            ASSIGN cStatus =  ENTRY(2,cButiker,CHR(1))
                   cButiker = ENTRY(1,cButiker,CHR(1)).
      /* vid HENTINTERNT: cStatus innehåller antal poster i tempdb */
            ASSIGN cButiker  = IF FI-Butikker <> "" THEN REPLACE(FI-Butikker,"|",",") ELSE REPLACE(CB-ButikkTeam:SCREEN-VALUE,CHR(1),",")
                   lAvancert = TRUE.
        END.
        CREATE TT_LagerTMP. /* en temporär record för att kunna summera */
        ASSIGN cLagAnt = IF CB-Antall:SCREEN-VALUE = cAlle THEN "*" ELSE REPLACE(REPLACE(CB-Antall:SCREEN-VALUE,"0","")," ","").
/*         FOR EACH ArtBas WHERE ArtBas.Opris = FALSE AND ArtBas.Lager = TRUE NO-LOCK: */
        IF lLocal THEN DO:
            cQry =
/*             "FOR EACH ArtBas WHERE ArtBas.Opris = FALSE AND ArtBas.Lager = TRUE" + */
                "FOR EACH ArtBas WHERE ArtBas.Lager = TRUE AND ArtBas.SanertDato = ?" + 
                 (IF FI-LevNr <> "*"  THEN (IF NUM-ENTRIES(FI-LevNr) = 1  THEN " AND ArtBas.LevNr = " + FI-LevNr   ELSE " AND CAN-DO('" + FI-LevNr + "',STRING(ArtBas.LevNr))") ELSE "") +
                 (IF FI-VarGr <> "*"  THEN (IF NUM-ENTRIES(FI-VarGr) = 1  THEN " AND ArtBas.Vg = " + FI-VarGr   ELSE " AND CAN-DO('" + FI-VarGr + "',STRING(ArtBas.Vg))") ELSE "") +
                 (IF FI-Farg <> "*"   THEN (IF NUM-ENTRIES(FI-Farg) = 1   THEN " AND ArtBas.Farg = " + FI-Farg     ELSE " AND CAN-DO('" + FI-Farg + "',STRING(ArtBas.Farg))") ELSE "") +
                 (IF FI-Sesong <> "*" THEN (IF NUM-ENTRIES(FI-Sesong) = 1 THEN " AND ArtBas.Sasong = " + FI-Sesong ELSE " AND CAN-DO('" + FI-Sesong + "',STRING(ArtBas.SaSong))") ELSE "") + " NO-LOCK".
            CREATE QUERY qh.
            qh:SET-BUFFERS(BUFFER ArtBas:HANDLE).
            qh:QUERY-PREPARE(cQry).
            qh:QUERY-OPEN().
        END.
        ELSE DO:
            IF cButiker = "" AND NUM-ENTRIES(cTillgButikker) = 1 THEN
                ASSIGN cButiker = cTillgButikker.
            ELSE IF cButiker = "" THEN DO:
                RUN TagButiker IN THIS-PROCEDURE (OUTPUT cButiker).
                ASSIGN cButiker = REPLACE(cButiker,"|",",").
                IF cButiker = "" THEN DO:
                    MESSAGE "Ingen butikker valgt."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    APPLY "CLOSE" TO h_Window.
                    RETURN.
                END.
            END.
            IF NUM-ENTRIES(cButiker) > 1 AND NOT Tg-VisButikker:CHECKED THEN DO:
                MESSAGE "Ønsker du resultatet per butikk? "
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lVisPerBut AS LOGICAL.
                ASSIGN Tg-VisButikker:CHECKED = lVisPerBut.
            END.
            PUBLISH "VisTxtBox" ("Søker data......").
            qh:QUERY-OPEN().
        END.
        ASSIGN cTmpFieldDefs       = cFieldDefs.
        IF cLagertype <> "BUTSTAT" AND Tg-VisButikker:CHECKED THEN DO:
            ENTRY(3,cFieldDefs) = ENTRY(3,cFieldDefs) + ",Butnamn;Navn;;".
            ENTRY(4,cFieldDefs) = ENTRY(4,cFieldDefs) + ",HarNeg;Neg;;".
        END.
        ELSE DO:
            ENTRY(3,cFieldDefs) = ENTRY(3,cFieldDefs) + ",HarNeg;Neg;;".
        END.
        /* Lägg in LevKod */
        IF cLagertype = "ARTIKKEL" THEN
            ASSIGN ENTRY(2,cFieldDefs) = ENTRY(2,cFieldDefs) + ",LevKod;Lev.artnr;;".

        IF RS-LagerType:SCREEN-VALUE = "ARTSTR" THEN
          cFieldDefs = cArtStrFieldDefs.
        RUN FixStrings.
        ASSIGN cFieldDefs = cTmpFieldDefs.
        REPEAT: 
            RELEASE VarGr.
            RELEASE HuvGr.
            qh:GET-NEXT().
            IF qh:QUERY-OFF-END THEN
                LEAVE.
/*                  MESSAGE qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE() */
/*                       VIEW-AS ALERT-BOX INFO BUTTONS OK.                                   */
            IF lLocal = FALSE AND NOT (qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("OPris"):BUFFER-VALUE() = FALSE AND
                                       qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Lager"):BUFFER-VALUE() = TRUE)     THEN
                NEXT.
            FIND VarGr WHERE VarGr.Vg = INT(qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Vg"):BUFFER-VALUE()) NO-LOCK NO-ERROR.
            IF NOT AVAIL VarGr THEN
                NEXT.
            IF lLocal = TRUE AND (FI-HuvGr <> "*" OR FI-Avdeling <> "*") THEN DO:
                IF FI-HuvGr <> "*" AND NOT CAN-DO(FI-HuvGr,STRING(VarGr.Hg)) THEN
                    NEXT.
                ELSE IF FI-Avdeling <> "*" THEN DO:
                    FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
                    IF NOT AVAIL HuvGr OR NOT CAN-DO(FI-Avdeling,STRING(HuvGr.AvdelingNr)) THEN
                        NEXT.
                END.
            END.
            IF NOT AVAIL HuvGr THEN
                FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
            DO iCount = 1 TO NUM-ENTRIES(cButiker):
                FIND Lager WHERE Lager.ArtikkelNr = DECI(qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE()) AND 
                                 Lager.Butik = INT(ENTRY(iCount,cButiker)) NO-LOCK NO-ERROR.
                IF NOT AVAIL Lager THEN
                    NEXT.
                IF TG-Alarm:SENSITIVE AND TG-Alarm:CHECKED = TRUE THEN DO:
                    IF NOT CAN-FIND(FIRST artlag WHERE artlag.artikkelnr = lager.artikkelnr AND artlag.butik = lager.butik AND 
                                                       artlag.lagant < 0) THEN
                        NEXT.
                END.
                ELSE DO:
                    IF CB-Antall:SCREEN-VALUE <> cAlle AND NOT lager.lagant > 0 THEN
                        NEXT.
                END.
                ASSIGN iLagAnt    = 0
                       iNegLagAnt = 0.
                FOR EACH artlag WHERE artlag.artikkelnr = lager.artikkelnr AND artlag.butik = lager.butik NO-LOCK.
                    IF artlag.lagant >= 0 THEN
                        iLagAnt = iLagAnt + artlag.lagant.
                    ELSE
                        iNegLagAnt = iNegLagAnt + artlag.lagant.
                END.
                BUFFER-COPY Lager TO TT_LagerTMP.
                FIND CURRENT TT_LagerTMP.
                ASSIGN TT_LagerTMP.lagant = iLagAnt
                       TT_LagerTMP.Neglagant = iNegLagAnt.
                /* här byter vi artikkelbegreppet ot ev avd,huvgr ... */
                IF cLagertype <> "ARTIKKEL" THEN DO:
                    CASE cLagertype:
                        WHEN "AVDELING" THEN TT_LagerTMP.Artikkelnr = IF AVAIL HuvGr THEN HuvGr.Avdelingnr ELSE 999999.
                        WHEN "HOVEDGR"  THEN TT_LagerTMP.Artikkelnr = IF AVAIL HuvGr THEN HuvGr.Hg ELSE 999999.
                        WHEN "VAREGR"   THEN TT_LagerTMP.Artikkelnr = VarGr.Vg.
                        WHEN "LEVERAN"  THEN TT_LagerTMP.Artikkelnr = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("LevNr"):BUFFER-VALUE().
                        WHEN "BUTSTAT"   THEN TT_LagerTMP.Artikkelnr = TT_LagerTMP.butik.
                    END CASE.
                END.
                RELEASE TT_Lager.
/*                 IF NOT Tg-VisButikker:CHECKED THEN DO:                                                                                */
/*                     ASSIGN TT_LagerTMP.Butik = 0.                                                                                     */
/*                     FIND TT_Lager WHERE TT_Lager.ArtikkelNr = TT_LagerTMP.ArtikkelNr AND TT_Lager.Butik = TT_LagerTMP.Butik NO-ERROR. */
/*                 END.                                                                                                                  */
/*                 IF AVAIL TT_Lager AND TT_LagerTMP.Butik = 0 THEN                                                                      */
/*                     RUN Summera.                                                                                                      */
                IF NOT Tg-VisButikker:CHECKED THEN
                    ASSIGN TT_LagerTMP.Butik = 0.
                FIND TT_Lager WHERE TT_Lager.ArtikkelNr = TT_LagerTMP.ArtikkelNr AND TT_Lager.Butik = TT_LagerTMP.Butik NO-ERROR.
                IF AVAIL TT_Lager THEN
                    RUN Summera.
                ELSE DO:
                    FIND bufArtbas WHERE bufArtbas.artikkelnr = TT_LagerTMP.Artikkelnr NO-LOCK NO-ERROR.
                    CREATE TT_Lager.
                    BUFFER-COPY TT_LagerTMP TO TT_Lager.
                    ASSIGN dSolgtTot = dSolgtTot + TT_LagerTMP.VerdiSolgt
                           dDBTot    = dDBTot    + TT_LagerTMP.VerdiSolgt - TT_LagerTMP.Svk
                           dRabTot   = dRabTot   + TT_LagerTMP.VerdiRabatt
                           dKjopTot  = dKjopTot  + TT_LagerTMP.KjopVerdi.
                    ASSIGN TT_Lager.Beskrivelse  = IF cLagerType = "ARTIKKEL" THEN qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Beskr"):BUFFER-VALUE()
                                                   ELSE getBeskr(cLagertype,STRING(TT_Lager.Artikkelnr))
                           TT_Lager.AvdelingNr   = IF AVAIL HuvGr THEN HuvGr.AvdelingNr ELSE 0
                           TT_Lager.AvdelingNavn = getBeskr("AVDELING",STRING(TT_Lager.AvdelingNr))
                           TT_Lager.Hg           = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Hg"):BUFFER-VALUE()
                           TT_Lager.HgBeskr      = getBeskr("HOVEDGR",STRING(TT_Lager.Hg))
                           TT_Lager.Vg           = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Vg"):BUFFER-VALUE()
                           TT_Lager.VgBeskr      = getBeskr("VAREGR",STRING(TT_Lager.Vg))
        /*                    TT_Lager.VgLopNr       = STRING(ArtBas.Vg) + "/" + REPLACE(STRING(ArtBas.LopNr,"z999"),"0","  ") */
                           TT_Lager.VgLopNr       = STRING(TT_Lager.Vg) + "/" + FILL("  ",4 - LENGTH(STRING(qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Lopnr"):BUFFER-VALUE())))
                                                                                         + STRING(qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Lopnr"):BUFFER-VALUE())
                           TT_Lager.LevNr       = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("LevNr"):BUFFER-VALUE()
                           TT_Lager.LevNamn     = getBeskr("LEVERAN",STRING(TT_Lager.LevNr))
                           TT_Lager.Sasong      = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Sasong"):BUFFER-VALUE()
                           TT_Lager.SasBeskr    = getBeskr("SASONG",STRING(TT_Lager.Sasong))
                           TT_Lager.Farg        = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Farg"):BUFFER-VALUE()
                           TT_Lager.FarBeskr    = getBeskr("FARG",STRING(TT_Lager.Farg))
                           TT_Lager.CharButik   = IF Tg-VisButikker:CHECKED AND cLagertype <> "BUTSTAT" THEN STRING(TT_Lager.Butik) ELSE ""
                           TT_Lager.DbKr        = TT_Lager.VerdiSolgt - TT_Lager.SVK /* (TT_Lager.AntSolgt * TT_Lager.VVarekost) */
                          /* Här sätter vi den totala lagervärdet utifrån LagAnt och VVarekost */
                          /* vid flera butiker summeras den och längre ner delar vi det totala lagervärdet med totala LagAnt */
                          /* som ger vvarekost/st */
                           TT_Lager.LagerVerdi     = TT_Lager.LagAnt * TT_Lager.VVarekost
                           TT_Lager.NegLagerVerdi  = TT_Lager.NegLagAnt * TT_Lager.VVarekost
                           TT_Lager.HarNeg = STRING(CAN-FIND(FIRST artlag WHERE artlag.artikkelnr = TT_Lager.artikkelnr AND artlag.lagant < 0),"J/")
                           TT_Lager.LevKod = TRIM(qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("LevKod"):BUFFER-VALUE())
                           TT_Lager.VmId        = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("vmid"):BUFFER-VALUE()
                           TT_Lager.Levfargkod  = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Levfargkod"):BUFFER-VALUE()
                           TT_Lager.Vmbeskr     = IF TT_Lager.vmid = 0 THEN "" ELSE getBeskr("VM",STRING(TT_Lager.vmid))
                           TT_Lager.KjedeInnkPris = IF AVAIL bufArtbas THEN bufArtBas.KjedeInnkPris ELSE 0.
                           
                     IF Tg-VisButikker:CHECKED THEN 
                         ASSIGN TT_Lager.Butnamn = IF Tg-VisButikker:CHECKED THEN getBeskr("GETBUTNAVN",STRING(TT_Lager.Butik)) ELSE "".
                END.
            END.
        END.
    END.
    IF lLocal = TRUE THEN DO:
        qh:QUERY-CLOSE().
        DELETE OBJECT qh NO-ERROR.
        ASSIGN qh = ?.
    END.
    RUN KalkuleraTTLager.
    PUBLISH "VisTxtBox" ("Leser ut data......").
    CREATE QUERY qh.
    qh:SET-BUFFERS(BUFFER TT_Lager:HANDLE).
    qh:QUERY-PREPARE("FOR EACH TT_Lager").
    qh:QUERY-OPEN().
    PUBLISH "VisTxtBox" ("Leser ut data......").
    ASSIGN cFelterBku = cFelter.
    ASSIGN cFelterTmp = REPLACE(cFelter,"Dataobjekt","ArtikkelNr").
    IF cSkomodus = "1" AND cLagertype = "ARTIKKEL" THEN DO:
        ENTRY(2,cFelterTmp) = ENTRY(1,cVgLop,";") + "," + ENTRY(2,cFelter).
        ENTRY(2,cFelter) = ENTRY(1,cVgLop,";") + "," + ENTRY(2,cFelter).
        cLabelsTMP = cLabels.
        ENTRY(2,cLabels) = ENTRY(2,cVgLop,";") + "," + ENTRY(2,cLabelsTmp).
        cDecimalerTMP = cDecimaler.
        ENTRY(2,cDecimaler) = ENTRY(3,cVgLop,";") + "," + ENTRY(2,cDecimalerTmp).
    END.

    RUN rappgenqry.p ("dynttStLager","FOR EACH TT_Lager",
                      cFileName,
                      REPLACE(cLabels,"DataobjektTXT",getLabel(RS-Lagertype:SCREEN-VALUE)) + (IF cLagertype = "ARTIKKEL" THEN "," + cArtLabels ELSE ""),
                      cFelterTmp + (IF cLagertype = "ARTIKKEL" THEN "," + cArtFelter ELSE ""),
                      cDecimaler + (IF cLagertype = "ARTIKKEL" THEN "," + cArtDecimaler ELSE ""),
                      cTidFelter,qh).
    EMPTY TEMP-TABLE TT_Lager.
    EMPTY TEMP-TABLE TT_LagerTMP.
    DELETE OBJECT qh NO-ERROR.
    ASSIGN qh = ?.
    RUN LesInnIGrid.
    RUN AlignCol IN h_frapportgrid (3,1) NO-ERROR.
    PUBLISH "VisTxtBox" ("").
    IF cSkomodus = "1" AND cLagertype = "ARTIKKEL" THEN DO:
        cLabels = cLabelsTMP.
        cDecimaler = cDecimalerTMP.
        cFelter    = cFelterBku.
    END.
/*     ELSE DO:                                         */
/*         PUBLISH "VisTxtBox" ("Leser ut data......"). */
/*         RUN ExporterTTLager.                         */
/*         RUN LesInnIGrid.                             */
/*         PUBLISH "VisTxtBox" ("").                    */
/*     END.                                             */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSokArtDynStr fFrameWin 
PROCEDURE StartSokArtDynStr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: 
         
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER qh        AS HANDLE     NO-UNDO.
    DEFINE INPUT  PARAMETER lLocal    AS LOGICAL    NO-UNDO.
    DEFINE INPUT  PARAMETER cButiker  AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  cStatus   AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  dTotSum   AS DECIMAL    NO-UNDO.
    DEFINE        VARIABLE  iCount       AS INTEGER    NO-UNDO.
    DEFINE        VARIABLE  lUtvidetFilter AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  lIkkeTreff     AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  cLagAnt        AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  lOK            AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  cQry           AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  dArtikkelNr    AS DECIMAL    NO-UNDO.
    DEFINE        VARIABLE  iHg            LIKE HuvGr.Hg NO-UNDO.
    DEFINE        VARIABLE  lAvancert      AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  iLagAnt        AS INTEGER     NO-UNDO.
    DEFINE        VARIABLE  iNegLagAnt     AS INTEGER     NO-UNDO.
    DEFINE        VARIABLE  cLagertype AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lant AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE cLabelsTMP AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cDecimalerTMP AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cFelterTmp  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cFelterBku  AS CHARACTER  NO-UNDO.
    EMPTY TEMP-TABLE TT_LagerStr.
    EMPTY TEMP-TABLE TT_LagerTMPStr.
    ASSIGN dSolgtTot = 0
           dDBTot    = 0
           dRabTot   = 0
           dKjopTot  = 0.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN cLagertype = "ARTIKKEL". /*  RS-Lagertype:SCREEN-VALUE. */
        IF cButiker BEGINS "HENTINTERNT" THEN DO:
            ASSIGN cStatus =  ENTRY(2,cButiker,CHR(1))
                   cButiker = ENTRY(1,cButiker,CHR(1)).
      /* vid HENTINTERNT: cStatus innehåller antal poster i tempdb */
            ASSIGN cButiker  = IF FI-Butikker <> "" THEN REPLACE(FI-Butikker,"|",",") ELSE REPLACE(CB-ButikkTeam:SCREEN-VALUE,CHR(1),",")
                   lAvancert = TRUE.
        END.
        CREATE TT_LagerTMPStr. /* en temporär record för att kunna summera */
        ASSIGN cLagAnt = IF CB-Antall:SCREEN-VALUE = cAlle THEN "*" ELSE REPLACE(REPLACE(CB-Antall:SCREEN-VALUE,"0","")," ","").
/*         FOR EACH ArtBas WHERE ArtBas.Opris = FALSE AND ArtBas.Lager = TRUE NO-LOCK: */
        IF lLocal THEN DO:
            cQry =
/*             "FOR EACH ArtBas WHERE ArtBas.Opris = FALSE AND ArtBas.Lager = TRUE" + */
                "FOR EACH ArtBas WHERE ArtBas.Lager = TRUE AND ArtBas.SanertDato = ?" + 
                 (IF FI-LevNr <> "*"  THEN (IF NUM-ENTRIES(FI-LevNr) = 1  THEN " AND ArtBas.LevNr = " + FI-LevNr   ELSE " AND CAN-DO('" + FI-LevNr + "',STRING(ArtBas.LevNr))") ELSE "") +
                 (IF FI-VarGr <> "*"  THEN (IF NUM-ENTRIES(FI-VarGr) = 1  THEN " AND ArtBas.Vg = " + FI-VarGr   ELSE " AND CAN-DO('" + FI-VarGr + "',STRING(ArtBas.Vg))") ELSE "") +
                 (IF FI-Farg <> "*"   THEN (IF NUM-ENTRIES(FI-Farg) = 1   THEN " AND ArtBas.Farg = " + FI-Farg     ELSE " AND CAN-DO('" + FI-Farg + "',STRING(ArtBas.Farg))") ELSE "") +
                 (IF FI-Sesong <> "*" THEN (IF NUM-ENTRIES(FI-Sesong) = 1 THEN " AND ArtBas.Sasong = " + FI-Sesong ELSE " AND CAN-DO('" + FI-Sesong + "',STRING(ArtBas.SaSong))") ELSE "") + " NO-LOCK".
            CREATE QUERY qh.
            qh:SET-BUFFERS(BUFFER ArtBas:HANDLE).
            qh:QUERY-PREPARE(cQry).
            qh:QUERY-OPEN().
        END.
        ELSE DO:
            IF cButiker = "" AND NUM-ENTRIES(cTillgButikker) = 1 THEN
                ASSIGN cButiker = cTillgButikker.
            ELSE IF cButiker = "" THEN DO:
                RUN TagButiker IN THIS-PROCEDURE (OUTPUT cButiker).
                ASSIGN cButiker = REPLACE(cButiker,"|",",").
                IF cButiker = "" THEN DO:
                    MESSAGE "Ingen butikker valgt."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    APPLY "CLOSE" TO h_Window.
                    RETURN.
                END.
            END.
            IF NUM-ENTRIES(cButiker) > 1 AND NOT Tg-VisButikker:CHECKED THEN DO:
                MESSAGE "Ønsker du resultatet per butikk? "
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lVisPerBut AS LOGICAL.
                ASSIGN Tg-VisButikker:CHECKED = lVisPerBut.
            END.
            PUBLISH "VisTxtBox" ("Søker data......").
            qh:QUERY-OPEN().
        END.
        ASSIGN cTmpFieldDefs       = cFieldDefs.
        ASSIGN cFieldDefs = cArtStrFieldDefs.
        IF Tg-VisButikker:CHECKED THEN DO:
            ENTRY(3,cFieldDefs) = ENTRY(3,cFieldDefs) + ",Butnamn;Navn;;".
            ENTRY(5,cFieldDefs) = ENTRY(5,cFieldDefs) + ",HarNeg;Neg;;".
        END.
        ELSE 
            ENTRY(4,cFieldDefs) = ENTRY(4,cFieldDefs) + ",HarNeg;Neg;;".
        /* Lägg in LevKod */
/*         IF cLagertype = "ARTIKKEL" THEN   !!! ALLTID !!! */
        ASSIGN ENTRY(2,cFieldDefs) = ENTRY(2,cFieldDefs) + ",LevKod;Lev.artnr;;".
        RUN FixStrings.
        ASSIGN cFieldDefs = cTmpFieldDefs.
        REPEAT: 
            RELEASE VarGr.
            RELEASE HuvGr.
            qh:GET-NEXT().
            IF qh:QUERY-OFF-END THEN
                LEAVE.
/*                  MESSAGE qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE() */
/*                       VIEW-AS ALERT-BOX INFO BUTTONS OK.                                   */
            IF lLocal = FALSE AND NOT (qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("OPris"):BUFFER-VALUE() = FALSE AND
                                       qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Lager"):BUFFER-VALUE() = TRUE)     THEN
                NEXT.
            FIND VarGr WHERE VarGr.Vg = INT(qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Vg"):BUFFER-VALUE()) NO-LOCK NO-ERROR.
            IF NOT AVAIL VarGr THEN
                NEXT.
            IF lLocal = TRUE AND (FI-HuvGr <> "*" OR FI-Avdeling <> "*") THEN DO:
                IF FI-HuvGr <> "*" AND NOT CAN-DO(FI-HuvGr,STRING(VarGr.Hg)) THEN
                    NEXT.
                ELSE IF FI-Avdeling <> "*" THEN DO:
                    FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
                    IF NOT AVAIL HuvGr OR NOT CAN-DO(FI-Avdeling,STRING(HuvGr.AvdelingNr)) THEN
                        NEXT.
                END.
            END.
            IF NOT AVAIL HuvGr THEN
                FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
            DO iCount = 1 TO NUM-ENTRIES(cButiker):
                FIND Lager WHERE Lager.ArtikkelNr = DECI(qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE()) AND 
                                 Lager.Butik = INT(ENTRY(iCount,cButiker)) NO-LOCK NO-ERROR.
                IF NOT AVAIL Lager THEN
                    NEXT.
                IF CB-Antall:SCREEN-VALUE <> cAlle AND NOT lager.lagant > 0 THEN
                    NEXT.
                IF NOT CAN-FIND(FIRST artlag WHERE artlag.artikkelnr = lager.artikkelnr AND artlag.butik = lager.butik) THEN
                        NEXT.
              FOR EACH artlag WHERE artlag.artikkelnr = lager.artikkelnr AND artlag.butik = lager.butik /* AND artlag.lagant <> 0 */ NO-LOCK:
                  IF ArtLag.AntSolgt = 0 AND ArtLag.KjopAnt = 0 AND ArtLag.lagant = 0 THEN
                      NEXT.
/*                 IF TG-Alarm:SENSITIVE AND TG-Alarm:CHECKED = TRUE THEN DO:                                                     */
/*                     IF NOT CAN-FIND(FIRST artlag WHERE artlag.artikkelnr = lager.artikkelnr AND artlag.butik = lager.butik AND */
/*                                                        artlag.lagant < 0) THEN                                                 */
/*                         NEXT.                                                                                                  */
/*                 END.                                                                                                           */
/*                 ELSE DO:                                                                                                       */
/*                     IF CB-Antall:SCREEN-VALUE <> cAlle AND NOT lager.lagant > 0 THEN                                           */
/*                         NEXT.                                                                                                  */
/*                 END.                                                                                                           */
/*                 ASSIGN iLagAnt    = 0                                                                                          */
/*                        iNegLagAnt = 0.                                                                                         */
                BUFFER-COPY artLag TO TT_LagerTMPStr.
                FIND CURRENT TT_LagerTMPStr.
                ASSIGN TT_LagerTMPStr.Storl = artlag.storl
                       TT_LagerTMPStr.lagant = IF artlag.lagant > 0 THEN artlag.lagant ELSE 0
                       TT_LagerTMPStr.Neglagant = IF artlag.lagant < 0 THEN artlag.lagant ELSE 0
                       TT_LagerTMPStr.vvarekost = lager.vvarekost.
                RELEASE TT_LagerStr.
                IF NOT Tg-VisButikker:CHECKED THEN
                    ASSIGN TT_LagerTMPStr.Butik = 0.
                FIND TT_LagerStr WHERE TT_LagerStr.ArtikkelNr = TT_LagerTMPStr.ArtikkelNr AND TT_LagerStr.Butik = TT_LagerTMPStr.Butik AND TT_LagerStr.storl = TT_LagerTMPStr.storl NO-ERROR.
                IF AVAIL TT_LagerStr THEN
                    RUN Summera.
                ELSE DO:
                    FIND bufArtbas WHERE bufArtbas.artikkelnr = TT_LagerTMPStr.Artikkelnr NO-LOCK NO-ERROR.
                    CREATE TT_LagerStr NO-ERROR.
                    BUFFER-COPY TT_LagerTMPStr TO TT_LagerStr.
                    ASSIGN dSolgtTot = dSolgtTot + TT_LagerTMPStr.VerdiSolgt
                           dDBTot    = dDBTot    + TT_LagerTMPStr.VerdiSolgt - TT_LagerTMPStr.Svk
                           dRabTot   = dRabTot   + TT_LagerTMPStr.VerdiRabatt
                           dKjopTot  = dKjopTot  + TT_LagerTMPStr.KjopVerdi.
                    ASSIGN TT_LagerStr.Beskrivelse  = IF cLagerType = "ARTIKKEL" THEN qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Beskr"):BUFFER-VALUE()
                                                   ELSE getBeskr(cLagertype,STRING(TT_LagerStr.Artikkelnr))
                           TT_LagerStr.AvdelingNr   = IF AVAIL HuvGr THEN HuvGr.AvdelingNr ELSE 0
                           TT_LagerStr.AvdelingNavn = getBeskr("AVDELING",STRING(TT_LagerStr.AvdelingNr))
                           TT_LagerStr.Hg           = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Hg"):BUFFER-VALUE()
                           TT_LagerStr.HgBeskr      = getBeskr("HOVEDGR",STRING(TT_LagerStr.Hg))
                           TT_LagerStr.Vg           = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Vg"):BUFFER-VALUE()
                           TT_LagerStr.VgBeskr      = getBeskr("VAREGR",STRING(TT_LagerStr.Vg))
        /*                    TT_LagerStr.VgLopNr       = STRING(ArtBas.Vg) + "/" + REPLACE(STRING(ArtBas.LopNr,"z999"),"0","  ") */
                           TT_LagerStr.VgLopNr       = STRING(TT_LagerStr.Vg) + "/" + FILL("  ",4 - LENGTH(STRING(qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Lopnr"):BUFFER-VALUE())))
                                                                                         + STRING(qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Lopnr"):BUFFER-VALUE())
                           TT_LagerStr.LevNr       = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("LevNr"):BUFFER-VALUE()
                           TT_LagerStr.LevNamn     = getBeskr("LEVERAN",STRING(TT_LagerStr.LevNr))
                           TT_LagerStr.Sasong      = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Sasong"):BUFFER-VALUE()
                           TT_LagerStr.SasBeskr    = getBeskr("SASONG",STRING(TT_LagerStr.Sasong))
                           TT_LagerStr.Farg        = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Farg"):BUFFER-VALUE()
                           TT_LagerStr.FarBeskr    = getBeskr("FARG",STRING(TT_LagerStr.Farg))
                           TT_LagerStr.CharButik   = IF Tg-VisButikker:CHECKED AND cLagertype <> "BUTSTAT" THEN STRING(TT_LagerStr.Butik) ELSE ""
                           TT_LagerStr.DbKr        = TT_LagerStr.VerdiSolgt - TT_LagerStr.SVK /* (TT_LagerStr.AntSolgt * TT_LagerStr.VVarekost) */
                          /* Här sätter vi den totala lagervärdet utifrån LagAnt och VVarekost */
                          /* vid flera butiker summeras den och längre ner delar vi det totala lagervärdet med totala LagAnt */
                          /* som ger vvarekost/st */
                           TT_LagerStr.LagerVerdi     = TT_LagerStr.LagAnt * TT_LagerStr.VVarekost
                           TT_LagerStr.NegLagerVerdi  = TT_LagerStr.NegLagAnt * TT_LagerStr.VVarekost
                           TT_LagerStr.HarNeg = STRING(CAN-FIND(FIRST artlag WHERE artlag.artikkelnr = TT_LagerStr.artikkelnr AND artlag.lagant < 0),"J/")
                           TT_LagerStr.LevKod = TRIM(qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("LevKod"):BUFFER-VALUE())
                           TT_LagerStr.VmId        = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("vmid"):BUFFER-VALUE()
                           TT_LagerStr.Levfargkod  = qh:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Levfargkod"):BUFFER-VALUE()
                           TT_LagerStr.Vmbeskr     = IF TT_LagerStr.vmid = 0 THEN "" ELSE getBeskr("VM",STRING(TT_LagerStr.vmid))
                               
                           TT_LagerStr.VerdiSolgt    = if TT_LagerStr.AntSolgt    > 0 then round(TT_LagerStr.AntSolgt    / Lager.AntSolgt    * Lager.VerdiSolgt   ,2) else 0
                           TT_LagerStr.BrekkVerdi    = if TT_LagerStr.BrekkAnt    > 0 then round(TT_LagerStr.BrekkAnt    / Lager.BrekkAnt    * Lager.BrekkVerdi   ,2) else 0
                           TT_LagerStr.IntVerdi      = if TT_LagerStr.IntAnt      > 0 then round(TT_LagerStr.IntAnt      / Lager.IntAnt      * Lager.IntVerdi     ,2) else 0
                           TT_LagerStr.ReklVerdi     = if TT_LagerStr.ReklAnt     > 0 then round(TT_LagerStr.ReklAnt     / Lager.ReklAnt     * Lager.ReklVerdi    ,2) else 0
                           TT_LagerStr.ReklLVerdi    = if TT_LagerStr.ReklLAnt    > 0 then round(TT_LagerStr.ReklLAnt    / Lager.ReklLAnt    * Lager.ReklLVerdi   ,2) else 0
                           TT_LagerStr.GjenkjopVerdi = if TT_LagerStr.GjenkjopAnt > 0 then round(TT_LagerStr.GjenkjopAnt / Lager.GjenkjopAnt * Lager.GjenkjopVerdi,2) else 0
                           TT_LagerStr.KjopVerdi     = if TT_LagerStr.KjopAnt     > 0 then round(TT_LagerStr.KjopAnt     / Lager.KjopAnt     * Lager.KjopVerdi    ,2) else 0
                           TT_LagerStr.OvVerdi       = if TT_LagerStr.OvAnt       > 0 then round(TT_LagerStr.OvAnt       / Lager.OvAnt       * Lager.OvVerdi      ,2) else 0
                           TT_LagerStr.VerdiRabatt   = if TT_LagerStr.AntRab      > 0 then round(TT_LagerStr.AntRab      / Lager.AntRab      * Lager.VerdiRabatt  ,2) else 0
                           TT_LagerStr.KjedeInnkPris = IF AVAIL bufArtbas THEN bufArtbas.KjedeInnkPris ELSE 0.
                           
                     IF Tg-VisButikker:CHECKED THEN 
                         ASSIGN TT_LagerStr.Butnamn = IF Tg-VisButikker:CHECKED THEN getBeskr("GETBUTNAVN",STRING(TT_LagerStr.Butik)) ELSE "".
                END.
              END. /* artlagloop */
            END.
        END.
    END.
    IF lLocal = TRUE THEN DO:
        qh:QUERY-CLOSE().
        DELETE OBJECT qh NO-ERROR.
        ASSIGN qh = ?.
    END.
    RUN KalkuleraTTLager.
    PUBLISH "VisTxtBox" ("Leser ut data......").
    CREATE QUERY qh.
    qh:SET-BUFFERS(BUFFER TT_LagerStr:HANDLE).
    qh:QUERY-PREPARE("FOR EACH TT_LagerStr").
    qh:QUERY-OPEN().
    PUBLISH "VisTxtBox" ("Leser ut data......").
    ASSIGN cFelterBku = cFelter.
    ASSIGN cFelterTmp = REPLACE(cFelter,"Dataobjekt","ArtikkelNr").
    IF cSkomodus = "1" AND cLagertype = "ARTIKKEL" THEN DO:
        ENTRY(2,cFelterTmp) = ENTRY(1,cVgLop,";") + "," + ENTRY(2,cFelter).
        ENTRY(2,cFelter) = ENTRY(1,cVgLop,";") + "," + ENTRY(2,cFelter).
        cLabelsTMP = cLabels.
        ENTRY(2,cLabels) = ENTRY(2,cVgLop,";") + "," + ENTRY(2,cLabelsTmp).
        cDecimalerTMP = cDecimaler.
        ENTRY(2,cDecimaler) = ENTRY(3,cVgLop,";") + "," + ENTRY(2,cDecimalerTmp).
    END.

    RUN rappgenqry.p ("dynttStLager","FOR EACH TT_LagerStr",
                      cFileName,
                      REPLACE(cLabels,"DataobjektTXT",getLabel(RS-Lagertype:SCREEN-VALUE)) + (IF cLagertype = "xARTIKKEL" THEN "," + cArtLabels ELSE ""),
                      cFelterTmp + (IF cLagertype = "xARTIKKEL" THEN "," + cArtFelter ELSE ""),
                      cDecimaler + (IF cLagertype = "xARTIKKEL" THEN "," + cArtDecimaler ELSE ""),
                      cTidFelter,qh).
    EMPTY TEMP-TABLE TT_LagerStr.
    EMPTY TEMP-TABLE TT_LagerTMPStr.
    DELETE OBJECT qh NO-ERROR.
    ASSIGN qh = ?.
    RUN LesInnIGrid.
    RUN AlignCol IN h_frapportgrid (3,1) NO-ERROR.
    PUBLISH "VisTxtBox" ("").
    IF cSkomodus = "1" AND cLagertype = "ARTIKKEL" THEN DO:
        cLabels = cLabelsTMP.
        cDecimaler = cDecimalerTMP.
        cFelter    = cFelterBku.
    END.
/*     ELSE DO:                                         */
/*         PUBLISH "VisTxtBox" ("Leser ut data......"). */
/*         RUN ExporterTTLager.                         */
/*         RUN LesInnIGrid.                             */
/*         PUBLISH "VisTxtBox" ("").                    */
/*     END.                                             */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Summera fFrameWin 
PROCEDURE Summera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN TT_Lager.LagAnt        = TT_Lager.LagAnt        + TT_LagerTMP.LagAnt       
       TT_Lager.NegLagAnt     = TT_Lager.NegLagAnt     + TT_LagerTMP.NegLagAnt       
       TT_Lager.AntSolgt      = TT_Lager.AntSolgt      + TT_LagerTMP.AntSolgt     
       TT_Lager.VerdiSolgt    = TT_Lager.VerdiSolgt    + TT_LagerTMP.VerdiSolgt   
       TT_Lager.AntRab        = TT_Lager.AntRab        + TT_LagerTMP.AntRab       
/**/   TT_Lager.DbKr          = TT_Lager.DbKr          + TT_LagerTMP.VerdiSolgt - TT_LagerTMP.SVK
                                     /*  TT_LagerTMP.VerdiSolgt - (TT_LagerTMP.AntSolgt * TT_LagerTMP.VVarekost) */
       TT_Lager.VerdiRabatt   = TT_Lager.VerdiRabatt   + TT_LagerTMP.VerdiRabatt  
    /* Här summerar vi varekosten för att få totala lagervärdet, */
    /* Innan vi kör visning delas den med LagAnt och ger VVarekost */
/**/   TT_Lager.LagerVerdi    = TT_Lager.LagerVerdi    + (TT_LagerTMP.LagAnt * TT_LagerTMP.VVarekost)    
/**/   TT_Lager.NegLagerVerdi = TT_Lager.NegLagerVerdi + (TT_LagerTMP.NegLagAnt * TT_LagerTMP.VVarekost)    
       TT_Lager.ReklAnt       = TT_Lager.ReklAnt       + TT_LagerTMP.ReklAnt      
       TT_Lager.ReklVerdi     = TT_Lager.ReklVerdi     + TT_LagerTMP.ReklVerdi    
       TT_Lager.ReklLAnt      = TT_Lager.ReklLAnt      + TT_LagerTMP.ReklLAnt     
       TT_Lager.ReklLVerdi    = TT_Lager.ReklLVerdi    + TT_LagerTMP.ReklLVerdi   
       TT_Lager.RetLAnt       = TT_Lager.RetLAnt       + TT_LagerTMP.RetLAnt      
       TT_Lager.SvinnAnt      = TT_Lager.SvinnAnt      + TT_LagerTMP.SvinnAnt     
       TT_Lager.SvinnVerdi    = TT_Lager.SvinnVerdi    + TT_LagerTMP.SvinnVerdi   
       TT_Lager.GjenkjopAnt   = TT_Lager.GjenkjopAnt   + TT_LagerTMP.GjenkjopAnt  
       TT_Lager.GjenkjopVerdi = TT_Lager.GjenkjopVerdi + TT_LagerTMP.GjenkjopVerdi
       TT_Lager.KjopAnt       = TT_Lager.KjopAnt       + TT_LagerTMP.KjopAnt      
       TT_Lager.KjopVerdi     = TT_Lager.KjopVerdi     + TT_LagerTMP.KjopVerdi    
       TT_Lager.BrekkAnt      = TT_Lager.BrekkAnt      + TT_LagerTMP.BrekkAnt     
       TT_Lager.BrekkVerdi    = TT_Lager.BrekkVerdi    + TT_LagerTMP.BrekkVerdi   
       TT_Lager.IntAnt        = TT_Lager.IntAnt        + TT_LagerTMP.IntAnt       
       TT_Lager.IntVerdi      = TT_Lager.IntVerdi      + TT_LagerTMP.IntVerdi     
       TT_Lager.JustAnt       = TT_Lager.JustAnt       + TT_LagerTMP.JustAnt      
       TT_Lager.JustVerdi     = TT_Lager.JustVerdi     + TT_LagerTMP.JustVerdi    
       TT_Lager.NedAnt        = TT_Lager.NedAnt        + TT_LagerTMP.NedAnt       
       TT_Lager.NedVerdi      = TT_Lager.NedVerdi      + TT_LagerTMP.NedVerdi     
       TT_Lager.OvAnt         = TT_Lager.OvAnt         + TT_LagerTMP.OvAnt        
       TT_Lager.OvVerdi       = TT_Lager.OvVerdi       + TT_LagerTMP.OvVerdi.      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TagButiker fFrameWin 
PROCEDURE TagButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER cButiker AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE cButikerRowIdList AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE cButikerIdList    AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                      "Butiker;Butik;ButNamn",
                      "where CAN-DO('" + cTillgButikker + "',STRING(Butiker.Butik))",
                      INPUT-OUTPUT cButikerRowIdList,
                      "Butik",
                      INPUT-OUTPUT cButikerIdList,
                      "","",
                      OUTPUT bOK).
  cButiker = cButikerIdList.
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

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
  PUBLISH "ClearGrid" (REPLACE(cLabels,"DataobjektTXT",getLabel(RS-Lagertype:SCREEN-VALUE IN FRAME {&FRAME-NAME}))).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBeskr fFrameWin 
FUNCTION getBeskr RETURNS CHARACTER
  ( INPUT cStTypeId AS CHARACTER, INPUT cObjekt AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cBeskrivelse AS CHARACTER  NO-UNDO.
  CASE cStTypeId:
      WHEN "GETBUTNAVN" THEN DO:
          FIND Butiker WHERE Butiker.Butik = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL Butiker THEN Butiker.Butnamn ELSE "Ukjent".
      END.
      WHEN "AVDELING" THEN DO:
          FIND Avdeling WHERE Avdeling.AvdelingNr = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL Avdeling THEN Avdeling.AvdelingNavn ELSE "Ukjent".
      END.
      WHEN "HOVEDGR" THEN DO:
          FIND HuvGr WHERE HuvGr.Hg = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL HuvGr THEN HuvGr.HgBeskr ELSE "Ukjent".
      END.
      WHEN "BUTSTAT" THEN DO:
          FIND Butiker WHERE Butiker.Butik = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL Butiker THEN Butiker.ButNamn ELSE "Ukjent".
      END.
      WHEN "VAREGR" THEN DO:
          FIND VarGr WHERE VarGr.Vg = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL VarGr THEN VarGr.VgBeskr ELSE "Ukjent".
      END.
      WHEN "LEVERAN" THEN DO:
          FIND LevBas WHERE LevBas.LevNr = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL Levbas THEN LevBas.levnamn ELSE "Ukjent".
      END.
      WHEN "SASONG" THEN DO:
          FIND Sasong WHERE Sasong.Sasong = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL SaSong THEN SaSong.SasBeskr ELSE "Ukjent".
      END.
      WHEN "FARG" THEN DO:
          FIND Farg WHERE Farg.Farg = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL Farg THEN Farg.FarBeskr ELSE "Ukjent".
      END.
      WHEN "VM" THEN DO:
          FIND Varemerke WHERE Varemerke.Vmid = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL Varemerke THEN Varemerke.Beskrivelse ELSE "".
      END.
/*       WHEN "ARTIKKEL" THEN DO:                                             */
/*           FIND ArtBas WHERE ArtBas.ForsNr = INT(cObjekt) NO-LOCK NO-ERROR. */
/*           RETURN IF AVAIL Forsalj THEN Forsalj.FoNamn ELSE "Ukjent".       */
/*       END.                                                                 */
      OTHERWISE RETURN "".
  END CASE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKriterier fFrameWin 
FUNCTION getKriterier RETURNS LOGICAL
  ( OUTPUT cKriterier AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cButiker AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      IF FI-Butikker = "" THEN DO:
          IF NUM-ENTRIES(CB-ButikkTeam:SCREEN-VALUE,";") = 2 THEN DO:
              FIND TT_BigListItem WHERE ROWID(TT_BigListItem) = TO-ROWID(ENTRY(2,CB-ButikkTeam:SCREEN-VALUE,";")).
              ASSIGN cButiker = REPLACE(TT_BigListItem.Butiker,CHR(1),",").
          END.
          ELSE
              ASSIGN cButiker = REPLACE(CB-ButikkTeam:SCREEN-VALUE,CHR(1),",").
      END.
      ASSIGN cKriterier = (IF FI-Butikker <> "" THEN REPLACE(FI-Butikker,"|",",") ELSE cButiker) + CHR(1) + RS-Lagertype:SCREEN-VALUE.
/*       ASSIGN cKriterier = (IF FI-Butikker <> "" THEN REPLACE(FI-Butikker,"|",",") ELSE REPLACE(CB-ButikkTeam:SCREEN-VALUE,CHR(1),",")) + CHR(1) + RS-Lagertype:SCREEN-VALUE. */
      RETURN TRUE.
  END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLabel fFrameWin 
FUNCTION getLabel RETURNS CHARACTER
  ( INPUT cStTypeId AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cTxt AS CHARACTER  NO-UNDO.
  CASE cStTypeId:
      WHEN "AVDELING" THEN
          ASSIGN cTxt = "Avdeling".
      WHEN "HOVEDGR"  THEN
          ASSIGN cTxt = "Hovedgr".
      WHEN "VAREGR"   THEN
          ASSIGN cTxt = "Varegr".
      WHEN "LEVERAN"  THEN
          ASSIGN cTxt = "Leverandør".
      WHEN "BUTSTAT"  THEN
          ASSIGN cTxt = "Butikk".
      WHEN "ARTIKKEL" THEN
          ASSIGN cTxt = "Artikkel".
      WHEN "ARTSTR" THEN
          ASSIGN cTxt = "Artikkel".
      OTHERWISE
          ASSIGN cTxt = "??".
  END CASE.
  RETURN cTxt.   /* Function return value. */

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

