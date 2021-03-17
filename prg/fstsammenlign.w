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

DEFINE VARIABLE wTittel           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cListItemPairs    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTillgButikker    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cUserDefaultBut   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikerRowIdList AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikerIdList    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAlle      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAnropButiker AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFilename AS CHARACTER  NO-UNDO.
DEFINE VARIABLE h_Window     AS HANDLE     NO-UNDO.
DEFINE VARIABLE h_frapportgrid AS HANDLE     NO-UNDO.
DEFINE VARIABLE h_dstlinje   AS HANDLE     NO-UNDO.
DEFINE VARIABLE cLabels    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFelter    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFieldDefs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTidFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cStTypeId  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDecimaler AS CHARACTER  NO-UNDO.
DEFINE VARIABLE h_fstperiode AS HANDLE     NO-UNDO.
DEFINE VARIABLE cRightCols    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSummerFelter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cXParam     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVisFelterTxt AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVisFelterNr AS CHARACTER  NO-UNDO.


DEFINE TEMP-TABLE TT_TillgButikker NO-UNDO
    FIELD Butik LIKE Butiker.Butik
    INDEX Butik Butik.

DEFINE TEMP-TABLE TT_BigListItem NO-UNDO
    FIELD Butiker AS CHARACTER.

ASSIGN cSummerFelter =
"AntSolgt,BruttoSolgt,VerdiSolgt," +
"AntSolgt2,BruttoSolgt2,VerdiSolgt2,DbKr,DbKr2,DbKrDiff%,Verdirabatt,Verdirabatt2".
ASSIGN cFieldDefs = 
        /*  1 */ "DataObjekt;DataObjekt;;1," +
        /*  2 */ "Beskrivelse;Beskrivelse;;," +
        /*  3 */ "PerLinTxt;Periode;;," +
        /*  4 */ "AntSolgt;Solgt1;;1," +
        /*  4 */ "AntSolgt2;Solgt2;;1," +
        /*  4 */ "AntDiff%;Antdiff%;2;1," +
        /*  4b */ "BruttoSolgt;Solgt brutto1;2;1," +
        /*  4b */ "BruttoSolgt2;Solgt brutto2;2;1," +
        /*  5 */ "VerdiSolgt;Solgt netto1;2;1," +
        /*  5 */ "VerdiSolgt2;Solgt netto2;2;1," +
        /*  5 */ "SolgtDiff%;Solgt diff%;2;1," +
        /*  9 */ "Db%;Db%1;2;1," +
        /*  9 */ "Db%2;Db%2;2;1," +
        /* 12 */ "Rab%;Rab%1;2;1," +
        /* 12 */ "Rab%2;Rab%2;2;1," +
        /*  9 */ "Dbkr;DbKr1;2;1," +
        /*  9 */ "Dbkr2;DbKr2;2;1," +
        /*  9 */ "DbkrDiff%;DbKr Diff%;2;1," +
        /*  9 */ "Verdirabatt;Verdirabatt1;2;1," +
        /*  9 */ "Verdirabatt2;Verdirabatt2;2;1".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CB-ButikkTeam CB-PerId BUTTON-SSokDato ~
FI-FraAar FI-LinjeNr1 FI-Dato1 FI-Dato2 FI-TilAar FI-LinjeNr2 FI-SFraAar ~
FI-SDato FI-SLinjeNr1 RS-Sttype B-LevNrBlank B-Aktiver B-AvdelingBlank ~
B-HgBlank B-VgBlank BUTTON-SokBut B-LevNr B-Avdeling B-HuvGr B-VarGr ~
BUTTON-SokDato1 BUTTON-SokDato2 FI-SammenlTxt Tg-VisPerBut B-SesongBlank ~
B-Sesong 
&Scoped-Define DISPLAYED-OBJECTS FI-Butikker CB-ButikkTeam CB-PerId ~
FI-FraAar FI-LinjeNr1 FI-Dato1 FI-Dato2 FI-TilAar FI-LinjeNr2 FI-SFraAar ~
FI-SDato FI-SLinjeNr1 RS-Sttype FI-LevNr FI-Avdeling FI-HuvGr FI-VarGr ~
FI-SammenlTxt Tg-VisPerBut FI-Sesong 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKriterier fFrameWin 
FUNCTION getKriterier RETURNS LOGICAL
  ( OUTPUT cKriterier AS CHARACTER )  FORWARD.

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

DEFINE BUTTON B-Avdeling  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-AvdelingBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-HgBlank 
     LABEL "Blank" 
     SIZE 8 BY 1.

DEFINE BUTTON B-HuvGr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

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

DEFINE BUTTON BUTTON-SokDato1 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDato2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SSokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-ButikkTeam AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikkteam" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 20.6 BY 1 NO-UNDO.

DEFINE VARIABLE CB-PerId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Periodetype" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 20.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Avdeling AS CHARACTER FORMAT "X(10)":U 
     LABEL "Avdeling" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Butikker AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikker" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato1 AS DATE FORMAT "99/99/99":U 
     LABEL "Fra dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato2 AS DATE FORMAT "99/99/99":U 
     LABEL "Til dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraAar AS INTEGER FORMAT "zzz9":U INITIAL 0 
     LABEL "Fra år/Uke" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-HuvGr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Hovedgruppe" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Levnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LinjeNr1 AS INTEGER FORMAT "zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LinjeNr2 AS INTEGER FORMAT "zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SammenlTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Sammenligningsperiode" 
      VIEW-AS TEXT 
     SIZE 24 BY .62 NO-UNDO.

DEFINE VARIABLE FI-SDato AS DATE FORMAT "99/99/99":U 
     LABEL "Fra dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Sesong AS CHARACTER FORMAT "X(10)":U 
     LABEL "Sesong" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SFraAar AS INTEGER FORMAT "zzz9":U INITIAL 0 
     LABEL "Fra år/Uke" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SLinjeNr1 AS INTEGER FORMAT "zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilAar AS INTEGER FORMAT "zzz9":U INITIAL 0 
     LABEL "Til år/Uke" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VarGr AS CHARACTER FORMAT "X(10)":U 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE RS-Sttype AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Avdeling", 1,
"Hovedgruppe", 2,
"Varegruppe", 3,
"Leverandør", 4,
"Leverandør/Avd", 5,
"Leverandør/Hg", 6,
"Leverandør/Vg", 7,
"Leverandør/Sesong", 8
     SIZE 25 BY 6.81 NO-UNDO.

DEFINE VARIABLE Tg-VisPerBut AS LOGICAL INITIAL no 
     LABEL "Vis per butikk" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FI-Butikker AT ROW 1.19 COL 14 COLON-ALIGNED
     CB-ButikkTeam AT ROW 2.19 COL 14 COLON-ALIGNED
     CB-PerId AT ROW 3.19 COL 14 COLON-ALIGNED
     BUTTON-SSokDato AT ROW 5.19 COL 64.8 NO-TAB-STOP 
     FI-FraAar AT ROW 4.19 COL 14 COLON-ALIGNED
     FI-LinjeNr1 AT ROW 4.19 COL 23 COLON-ALIGNED NO-LABEL
     FI-Dato1 AT ROW 4.19 COL 14 COLON-ALIGNED
     FI-Dato2 AT ROW 5.19 COL 14 COLON-ALIGNED
     FI-TilAar AT ROW 5.19 COL 14 COLON-ALIGNED
     FI-LinjeNr2 AT ROW 5.19 COL 23 COLON-ALIGNED NO-LABEL
     FI-SFraAar AT ROW 5.19 COL 47.6 COLON-ALIGNED
     FI-SDato AT ROW 5.19 COL 47.6 COLON-ALIGNED
     FI-SLinjeNr1 AT ROW 5.19 COL 56.6 COLON-ALIGNED NO-LABEL
     RS-Sttype AT ROW 1.1 COL 115 NO-LABEL
     FI-LevNr AT ROW 1.19 COL 82.6 COLON-ALIGNED
     B-LevNrBlank AT ROW 1.19 COL 104.6 NO-TAB-STOP 
     B-Aktiver AT ROW 2.19 COL 42
     FI-Avdeling AT ROW 2.19 COL 82.6 COLON-ALIGNED
     FI-HuvGr AT ROW 3.19 COL 82.6 COLON-ALIGNED
     FI-VarGr AT ROW 4.19 COL 82.6 COLON-ALIGNED
     B-AvdelingBlank AT ROW 2.19 COL 104.6 NO-TAB-STOP 
     B-HgBlank AT ROW 3.19 COL 104.6 NO-TAB-STOP 
     B-VgBlank AT ROW 4.19 COL 104.6 NO-TAB-STOP 
     BUTTON-SokBut AT ROW 1.19 COL 31.8 NO-TAB-STOP 
     B-LevNr AT ROW 1.19 COL 99.6 NO-TAB-STOP 
     B-Avdeling AT ROW 2.19 COL 99.6 NO-TAB-STOP 
     B-HuvGr AT ROW 3.19 COL 99.6 NO-TAB-STOP 
     B-VarGr AT ROW 4.19 COL 99.6 NO-TAB-STOP 
     BUTTON-SokDato1 AT ROW 4.19 COL 31.8 NO-TAB-STOP 
     BUTTON-SokDato2 AT ROW 5.19 COL 31.8 NO-TAB-STOP 
     FI-SammenlTxt AT ROW 4.33 COL 39.6 COLON-ALIGNED NO-LABEL
     Tg-VisPerBut AT ROW 6.43 COL 16
     FI-Sesong AT ROW 5.29 COL 82.6 COLON-ALIGNED
     B-SesongBlank AT ROW 5.29 COL 104.6
     B-Sesong AT ROW 5.29 COL 99.6 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 139.8 BY 7.24.


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
         HEIGHT             = 7.24
         WIDTH              = 139.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB fFrameWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW fFrameWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME fMain:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-Avdeling IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Butikker IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-HuvGr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LevNr IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Sesong IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VarGr IN FRAME fMain
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME B-Aktiver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Aktiver fFrameWin
ON CHOOSE OF B-Aktiver IN FRAME fMain /* Aktiver */
DO:
  DEFINE VARIABLE cKriterier  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE TTH         AS HANDLE     NO-UNDO.
  DEFINE VARIABLE qh          AS HANDLE     NO-UNDO.
  DEFINE VARIABLE cSumCols    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cKalkCols   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSumString  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcFeltListe AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE pcVerdier   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry1       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQry2       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTmpFieldDefs AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cExtraFelt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilleggsFelter AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
  IF NOT DYNAMIC-FUNCTION('getKriterier':U,
     OUTPUT cKriterier /* CHARACTER */) THEN
      RETURN.
  IF RS-Sttype:SCREEN-VALUE = "8" THEN
      ASSIGN pcFeltListe = IF FI-Sesong <> "*" THEN "Sasong" ELSE ""
             pcVerdier   = IF FI-Sesong <> "*" THEN FI-Sesong ELSE "".
  ELSE
      ASSIGN pcFeltListe = IF FI-Avdeling <> "*" THEN "AvdelingNr" ELSE IF FI-HuvGr <> "*" THEN "Hg" ELSE IF FI-VarGr <> "*" THEN "Vg" ELSE ""
             pcVerdier   = IF FI-Avdeling <> "*" THEN FI-Avdeling ELSE IF FI-HuvGr <> "*" THEN FI-HuvGr ELSE IF FI-VarGr <> "*" THEN FI-VarGr ELSE "".

cTilleggsFelter = "Butik,Butnamn,AvdelingNr,AvdelingNavn,LevNr,levnamn,Hg,HgBeskr,Vg,VgBeskr,Sasong,SasBeskr".

/*   IF RS-Sttype:SCREEN-VALUE = "4" THEN DO: /* Lev */                       */
/*       ASSIGN cXParam = IF pcFeltListe = "" THEN "ENTRY1" ELSE pcFeltListe. */
/*   END.                                                                     */
  IF INT(RS-Sttype:SCREEN-VALUE) >= 4 THEN DO:
      ASSIGN pcFeltListe = "LevNr"  + (IF pcFeltListe <> "" THEN "," ELSE "") + pcFeltListe
             pcVerdier   = FI-LevNr + (IF pcVerdier <> "" THEN CHR(1) ELSE "") + pcVerdier.
  END.
  APPLY "VALUE-CHANGED" TO RS-Sttype.
  RUN VisTxtBox IN h_frapportgrid ("Søker data......").
  RUN StartSok (ENTRY(2,cKriterier,CHR(1)),OUTPUT cQry1,OUTPUT cQry2).
  RUN StLinjeJmfToTT IN h_dstlinje
      ( OUTPUT TTH,cQry1,cQry2, cStTypeId,ENTRY(1,cKriterier,CHR(1)),pcFeltListe + ";" + pcVerdier,cXParam,FALSE).
/*     ( OUTPUT TTH,cStTypeId,ENTRY(1,cKriterier,CHR(1)),pcFeltListe + ";" + pcVerdier,"",Tg-VisPeriode:CHECKED). */
  CREATE QUERY qh.
  qh:SET-BUFFERS(TTH).
  qh:QUERY-PREPARE("for each TT_StLinjeJmf by dataobjekt").
  qh:QUERY-OPEN().
  RUN VisTxtBox IN h_frapportgrid ("Leser ut data......").
  RUN rappgenqry.p ("TT_StLinjeJmf","",cFileName,cLabels,cFelter,cDecimaler,cTidFelter,qh).
  RUN VisTxtBox IN h_frapportgrid ("Leser inn og bearbeider data......").
  RUN LoadGrid IN h_frapportgrid (cFileName,(IF INT(RS-Sttype:SCREEN-VALUE) < 5 THEN 3 else 5) + (IF TG-VisPerBut:CHECKED THEN 1 ELSE 0)).  /* 3 = antall frozen cols  */
  ASSIGN cSumCols = getSumFelter(cSummerFelter)
         /* entry 1=antal decimaler,2=col där resultatet skall stå,3= det som står över vid div, 4 = det som står under */
         cKalkCols = "1," + getSumFelter("Db%") + "," + getSumFelter("DbKr") + "," + getSumFelter("VerdiSolgt") + ";"
                   + "1," + getSumFelter("Rab%") + "," + getSumFelter("VerdiRabatt") + "," + getSumFelter("VerdiSolgt") + "|+" + getSumFelter("VerdiRabatt") + ";"
              + "1," + getSumFelter("Db%2") + "," + getSumFelter("DbKr2") + "," + getSumFelter("VerdiSolgt2") + ";"
              + "1," + getSumFelter("Rab%2") + "," + getSumFelter("VerdiRabatt2") + "," + getSumFelter("VerdiSolgt2") + "|+" + getSumFelter("VerdiRabatt2") + ";"
              + "1," + getSumFelter("SolgtDiff%") + "," + getSumFelter("VerdiSolgt") + "|-" + getSumFelter("VerdiSolgt2") + "," + getSumFelter("VerdiSolgt2") + ";"
              + "1," + getSumFelter("DbKrDiff%") + "," + getSumFelter("DbKr") + "|-" + getSumFelter("DbKr2") + "," + getSumFelter("DbKr2")
         /* Col för SummaRadTxt, SUM = txt  */
         cSumString = getSumFelter("PerLinTxt") + ",SUM" .
  /* nästa rad måste stå före 'Summer' */
/*   RUN X%Solgt IN h_frapportgrid ("1" + "," + getSumFelter("Solgt%") + "," + getSumFelter("VerdiSolgt")). */
  IF cVisFelterTxt <> "" THEN DO:
      cExtrafelt = "".
      DO ii = 1 TO NUM-ENTRIES(cTilleggsFelter):
          IF CAN-DO(cFelter,ENTRY(ii,cTilleggsFelter)) THEN
              cExtraFelt = cExtraFelt + "," + ENTRY(ii,cTilleggsFelter).
      END.
      cVisFelterNr = getSumFelter(cVisFelterTxt + cExtraFelt).
  END.
  RUN Summer IN h_frapportgrid (cSumCols + ";" + cKalkCols,cSumString).
  IF cVisFelterNr <> "" THEN
      PUBLISH "VisKun" (cVisFelterNr,"SKJUL").
  qh:QUERY-CLOSE().
  TTH:EMPTY-TEMP-TABLE().
  RUN VisTxtBox IN h_frapportgrid ("").
  DELETE OBJECT TTH NO-ERROR.
  DELETE OBJECT qh NO-ERROR.
  ASSIGN TTH = ?
         qh  = ?.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Aktiver fFrameWin
ON TAB OF B-Aktiver IN FRAME fMain /* Aktiver */
DO:
  IF CB-ButikkTeam:SENSITIVE THEN
      APPLY "ENTRY" TO CB-ButikkTeam.
  ELSE
      APPLY "ENTRY" TO CB-PerId.
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
            ASSIGN SELF:PRIVATE-DATA   = ""
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
                   FI-HuvGr:BGCOLOR  = ?.
    END.
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
                   FI-LevNr:BGCOLOR  = ?.
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
        ASSIGN
          FI-Sesong:SCREEN-VALUE = IF cIdList = ""
                            THEN cAlle
                          ELSE "( " + STRING(NUM-ENTRIES(cIdList,"|")) + " )"
          FI-Sesong     = IF cIdList = ""
                            THEN "*"
                            ELSE REPLACE(cIdList,"|",",")
          FI-Sesong:TOOLTIP = IF FI-Sesong = "*" THEN "" ELSE FI-Sesong.
        IF FI-Sesong <> "*" THEN DO:
            ASSIGN SELF:PRIVATE-DATA = cRowIdList + CHR(1) + cIdList
                   FI-Sesong:BGCOLOR = 11.
            APPLY "CHOOSE" TO B-AvdelingBlank.
            APPLY "CHOOSE" TO B-HgBlank.
            APPLY "CHOOSE" TO B-VgBlank.
        END.
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
                   FI-VarGr:BGCOLOR  = ?.
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
    IF bOK THEN DO:
        RUN FixButikVis.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato1 fFrameWin
ON CHOOSE OF BUTTON-SokDato1 IN FRAME fMain /* ... */
or F10 of FI-Dato1
DO:
  do with frame DEFAULT-FRAME:  
    assign 
      FI-Dato1 = date(FI-Dato1:screen-value).

    wTittel = "Statistikkdato".
  
    /* Start søkeprogram */
    {soek.i
      &Felt        = FI-Dato1
      &Program     = kalender.w
      &Frame       = fMain
      &ExtraParam  = "input wTittel"
    }   
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato2 fFrameWin
ON CHOOSE OF BUTTON-SokDato2 IN FRAME fMain /* ... */
or F10 of FI-Dato2
DO:
  do with frame DEFAULT-FRAME:  
    assign 
      FI-Dato2 = date(FI-Dato2:screen-value).

    wTittel = "Statistikkdato".
  
    /* Start søkeprogram */
    {soek.i
      &Felt        = FI-Dato2
      &Program     = kalender.w
      &Frame       = fMain
      &ExtraParam  = "input wTittel"
    } 
    return no-apply.  
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SSokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SSokDato fFrameWin
ON CHOOSE OF BUTTON-SSokDato IN FRAME fMain /* ... */
or F10 of FI-SDato
DO:
  do with frame DEFAULT-FRAME:  
    assign 
      FI-SDato = date(FI-SDato:screen-value).

    wTittel = "Statistikkdato".
  
    /* Start søkeprogram */
    {soek.i
      &Felt        = FI-SDato
      &Program     = kalender.w
      &Frame       = fMain
      &ExtraParam  = "input wTittel"
    }   
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-ButikkTeam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-ButikkTeam fFrameWin
ON VALUE-CHANGED OF CB-ButikkTeam IN FRAME fMain /* Butikkteam */
DO:
  ASSIGN SELF:TOOLTIP = IF SELF:SCREEN-VALUE = "INGEN" THEN "" ELSE IF NUM-ENTRIES(SELF:SCREEN-VALUE,";") = 2 THEN
      ENTRY(1,SELF:SCREEN-VALUE,";") ELSE REPLACE(SELF:SCREEN-VALUE,CHR(1),",").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-PerId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-PerId fFrameWin
ON VALUE-CHANGED OF CB-PerId IN FRAME fMain /* Periodetype */
DO:
    RUN InitFillIns (SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-Sttype
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Sttype fFrameWin
ON VALUE-CHANGED OF RS-Sttype IN FRAME fMain
DO:
  DEFINE VARIABLE cTmpFieldDefs AS CHARACTER  NO-UNDO.
  
  IF NUM-ENTRIES(cXParam,CHR(1)) = 2 THEN
      ASSIGN ENTRY(2,cXParam,CHR(1)) = STRING(TG-VisPerBut:CHECKED,"J/N").
  ELSE
      ASSIGN cXParam = cXParam + CHR(1) + STRING(TG-VisPerBut:CHECKED,"J/N").
  
  ASSIGN cXParam = ""
         cTmpFieldDefs = cFieldDefs.
  IF CAN-DO("1,2,3,4",RS-Sttype:SCREEN-VALUE) AND TG-VisPerBut:CHECKED THEN
      ENTRY(2,cFieldDefs) = ENTRY(2,cFieldDefs) + ",Butik;Butikk;;,Butnamn;Navn;;".
  RUN FixStrings.
  CASE RS-Sttype:SCREEN-VALUE:
      WHEN "1" THEN DO:
          ASSIGN cStTypeId = "AVDELING"
                 ENTRY(1,cLabels) = "Avdeling".
      END.
      WHEN "2" THEN DO:
          ASSIGN cStTypeId = "HOVEDGR"
                 ENTRY(1,cLabels) = "Hovedgr".
      END.
      WHEN "3" THEN DO:
          ASSIGN cStTypeId = "VAREGR"
                 ENTRY(1,cLabels) = "Varegr".
      END.
      WHEN "4" THEN DO:
          ASSIGN cStTypeId = "LEVERAN"
                 ENTRY(1,cLabels) = "Leverandør".
      END.
      WHEN "5" THEN DO:
          ASSIGN cStTypeId = "LEVERAN-VG"
                 ENTRY(1,cFieldDefs) = "LevNr;Leverandør;;1,levnamn;Beskrivelse;;"
                 ENTRY(3,cFieldDefs) = "AvdelingNr;Avd;;1,AvdelingNavn;Avdeling;;"
                 ENTRY(4,cFieldDefs) = ENTRY(4,cFieldDefs) + ",Butik;Butikk;;,Butnamn;Navn;;".
                 cXParam = "Avd".
          RUN FixStrings.
      END.
      WHEN "6" THEN DO:
          ASSIGN cStTypeId = "LEVERAN-VG"
                 ENTRY(1,cFieldDefs) = "LevNr;Leverandør;;1,levnamn;Beskrivelse;;"
                 ENTRY(3,cFieldDefs) = "Hg;Hg;;1,HgBeskr;Hovedgruppe;;"
                 ENTRY(4,cFieldDefs) = ENTRY(4,cFieldDefs) + ",Butik;Butikk;;,Butnamn;Navn;;".
                 cXParam = "Hg".
          RUN FixStrings.
      END.
      WHEN "7" THEN DO:
          ASSIGN cStTypeId = "LEVERAN-VG"
                 ENTRY(1,cFieldDefs) = "LevNr;Leverandør;;1,levnamn;Beskrivelse;;"
                 ENTRY(3,cFieldDefs) = "Vg;Vg;;1,VgBeskr;Varegruppe;;"
                 ENTRY(4,cFieldDefs) = ENTRY(4,cFieldDefs) + ",Butik;Butikk;;,Butnamn;Navn;;".
                 cXParam = "Vg".
          RUN FixStrings.
      END.
      WHEN "8" THEN DO:
          ASSIGN cStTypeId = "LEVERAN-SA"
                 ENTRY(1,cFieldDefs) = "LevNr;Leverandør;;1,levnamn;Beskrivelse;;"
                 ENTRY(3,cFieldDefs) = "Sasong;Sesong;;1,SasBeskr;Sesongbeskr;;"
                 ENTRY(4,cFieldDefs) = ENTRY(4,cFieldDefs) + ",Butik;Butikk;;,Butnamn;Navn;;".
                 cXParam = "Vg".
          RUN FixStrings.
      END.
  END CASE.
  ASSIGN cXParam = cXParam + CHR(1) + STRING(TG-VisPerBut:CHECKED,"J/N").
  ASSIGN cFieldDefs = cTmpFieldDefs.
  IF VALID-HANDLE(h_frapportgrid) THEN
      RUN ClearGrid IN h_frapportgrid (cLabels).
  RUN EnaDisFields.
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
  DISPLAY FI-Butikker CB-ButikkTeam CB-PerId FI-FraAar FI-LinjeNr1 FI-Dato1 
          FI-Dato2 FI-TilAar FI-LinjeNr2 FI-SFraAar FI-SDato FI-SLinjeNr1 
          RS-Sttype FI-LevNr FI-Avdeling FI-HuvGr FI-VarGr FI-SammenlTxt 
          Tg-VisPerBut FI-Sesong 
      WITH FRAME fMain.
  ENABLE CB-ButikkTeam CB-PerId BUTTON-SSokDato FI-FraAar FI-LinjeNr1 FI-Dato1 
         FI-Dato2 FI-TilAar FI-LinjeNr2 FI-SFraAar FI-SDato FI-SLinjeNr1 
         RS-Sttype B-LevNrBlank B-Aktiver B-AvdelingBlank B-HgBlank B-VgBlank 
         BUTTON-SokBut B-LevNr B-Avdeling B-HuvGr B-VarGr BUTTON-SokDato1 
         BUTTON-SokDato2 FI-SammenlTxt Tg-VisPerBut B-SesongBlank B-Sesong 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnaDisFields fFrameWin 
PROCEDURE EnaDisFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cSV AS CHARACTER  NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        IF RS-Sttype:SCREEN-VALUE <> ? THEN DO:
            APPLY "CHOOSE" TO B-AvdelingBlank.
            APPLY "CHOOSE" TO B-HgBlank.
            APPLY "CHOOSE" TO B-VgBlank.
            APPLY "CHOOSE" TO B-LevNrBlank.
            APPLY "CHOOSE" TO B-SesongBlank.
            ASSIGN cSV = RS-Sttype:SCREEN-VALUE
/*                    FI-Avdeling:SENSITIVE     = TRUE */
                   B-Avdeling:SENSITIVE      = cSV <> "8"
                   B-AvdelingBlank:SENSITIVE = cSV <> "8"
/*                    FI-HuvGr:SENSITIVE        = CAN-DO("2,3,4",cSV) */
                   B-HuvGr:SENSITIVE         = CAN-DO("2,3,4,6",cSV)
                   B-HgBlank:SENSITIVE       = CAN-DO("2,3,4,6",cSV)
/*                    FI-VarGr:SENSITIVE        = CAN-DO("3,4",cSV) */
                   B-VarGr:SENSITIVE         = CAN-DO("3,4,7",cSV)
                   B-VgBlank:SENSITIVE       = CAN-DO("3,4,7",cSV)
/*                    FI-LevNr:SENSITIVE        = CAN-DO("4,5,6,7",cSV) */
                   B-LevNr:SENSITIVE         = CAN-DO("4,5,6,7,8",cSV)
                   B-LevNrBlank:SENSITIVE    = CAN-DO("4,5,6,7,8",cSV)
                   B-Sesong:SENSITIVE = cSV = "8"
                   B-SesongBlank:SENSITIVE = cSV = "8"
                .
        END.
    END.
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
                 CB-ButikkTeam:SCREEN-VALUE    = IF cUserDefaultBut <> "" AND CAN-DO(cListItemPairs,cUserDefaultBut) THEN 
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB2 fFrameWin 
PROCEDURE InitCB2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cStTypeId    AS CHARACTER INIT "AVDELING" NO-UNDO.
    DEFINE VARIABLE cButString   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cOkButiker   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cButStringListe AS CHARACTER  NO-UNDO.
    FOR EACH StDef WHERE StDef.StTypeId = cStTypeId /* AND StDef.PerId <> "Dag" */ NO-LOCK:
        ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") +
            StDef.Beskrivelse + "," + StDef.PerId.
    END.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN CB-PerId:LIST-ITEM-PAIRS = cListItemPairs
               CB-PerId:SCREEN-VALUE    = ENTRY(2,cListItemPairs).
        APPLY "VALUE-CHANGED" TO CB-PerId.
    END.
    FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK.
    ASSIGN cUserDefaultBut = STRING(Bruker.ButikkNr)
           cListItemPairs  = "".
    FOR EACH ButikkTeam NO-LOCK WHERE ButikkTeam.BrGrpNr = Bruker.BrGrpNr AND
                              ButikkTeam.TeamTypeId = 2 BY ButikkTeam.Beskrivelse.
        ASSIGN cButString = "".
        FOR EACH ButikkKobling OF ButikkTeam.
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
                             ButikkTeam.Beskrivelse + "," + cButString
               cButStringListe = cButStringListe + (IF cButStringListe <> "" THEN "," ELSE "") + cButString.
    END.
    FOR EACH TT_TillgButikker:
        ASSIGN cTillgButikker = cTillgButikker + (IF cTillgButikker <> "" THEN "," ELSE "") + STRING(TT_TillgButikker.Butik).
    END.
    /* Om vi har fått en butiklista genom proc SetIpButiker skall vi kontrollera att listan */
    /* innehåller butiker vi har tillgång till och ev strippa bort andra butiker */
    IF cAnropButiker <> "" AND cAnropButiker <> cUserDefaultBut THEN DO:
        DO iCount = 1 TO NUM-ENTRIES(cAnropButiker,"|"):
            IF CAN-DO(cTillgButikker,ENTRY(iCount,cAnropButiker,"|")) THEN
                ASSIGN cOkButiker = cOkButiker + (IF cOkButiker <> "" THEN "|" ELSE "") + ENTRY(iCount,cAnropButiker,"|").
        END.
        /* om cOkButiker finns i teamlistan i combo väljer vi det teamet som cUserdefault */
/*         MESSAGE "cTillgbutikker  : " cTillgbutikker SKIP              */
/*                 "cOkButiker     : " cOkButiker SKIP                   */
/*                 "cButStringListe:" cButStringListe                    */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                        */
/*         IF cOkButiker <> "" THEN DO:                                  */
/*             IF CAN-DO(cButStringListe,cOkButiker) THEN                */
/*                 ASSIGN cUserDefaultBut = REPLACE(cOkButiker,"|",","). */
/*             ELSE                                                      */
/*                 cButStringListe = cOkButiker.                         */
/*         END.                                                          */
        IF cOkButiker <> "" THEN DO:
            ASSIGN cButikerIdList = cOkButiker.
            DO iCount = 1 TO NUM-ENTRIES(cOkButiker,"|"):
                FIND Butiker WHERE Butiker.Butik = INT(ENTRY(iCOunt,cOkButiker,"|")) NO-LOCK NO-ERROR.
                IF AVAIL Butiker THEN
                   ASSIGN cButikerRowIdList = cButikerRowIdList + (IF cButikerRowIdList <> "" THEN "," ELSE "") + STRING(ROWID(Butiker)).
            END.
        END.
    END.
    RUN FixButikVis.
/*     ASSIGN CB-ButikkTeam:LIST-ITEM-PAIRS = cListItemPairs                                                           */
/*            CB-ButikkTeam:SCREEN-VALUE    = IF cUserDefaultBut <> "" AND CAN-DO(cTillgButikker,cUserDefaultBut) THEN */
/*                                              cUserDefaultBut ELSE ENTRY(2,cListItemPairs).                          */
/*     APPLY "VALUE-CHANGED" TO CB-ButikkTeam.                                                                         */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitFillIns fFrameWin 
PROCEDURE InitFillIns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cPerId AS CHARACTER  NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN FI-FraAar:HIDDEN       = TRUE
               FI-TilAar:HIDDEN       = TRUE
               FI-LinjeNr1:HIDDEN     = TRUE
               FI-LinjeNr2:HIDDEN     = TRUE
               FI-Dato1:HIDDEN        = TRUE
               FI-Dato2:HIDDEN        = TRUE
               FI-SDato:HIDDEN        = TRUE
               FI-SFraAar:HIDDEN      = TRUE
               FI-SLinjeNr1:HIDDEN    = TRUE
               BUTTON-SokDato1:HIDDEN = TRUE
               BUTTON-SokDato2:HIDDEN = TRUE
               BUTTON-SSokDato:HIDDEN = TRUE.
        CASE cPerId:
            WHEN "AAR" THEN DO:
                ASSIGN FI-FraAar:SCREEN-VALUE = STRING(YEAR(TODAY))
                       FI-TilAar:SCREEN-VALUE = STRING(YEAR(TODAY))
                       FI-SFraAar:SCREEN-VALUE   = STRING(YEAR(TODAY))
                       FI-SFraAar:HIDDEN        = FALSE
                       FI-FraAar:HIDDEN       = FALSE
                       FI-TilAar:HIDDEN       = FALSE.
            END.
            WHEN "MANED" THEN DO:
                ASSIGN FI-FraAar:SCREEN-VALUE   = STRING(YEAR(TODAY))
                       FI-TilAar:SCREEN-VALUE   = STRING(YEAR(TODAY))
                       FI-LinjeNr1:SCREEN-VALUE = "1"
                       FI-LinjeNr2:SCREEN-VALUE = "12"
                       FI-SLinjeNr1:SCREEN-VALUE = "1"
                       FI-SFraAar:SCREEN-VALUE   = STRING(YEAR(TODAY))
                       FI-SFraAar:HIDDEN        = FALSE
                       FI-FraAar:HIDDEN         = FALSE
                       FI-TilAar:HIDDEN         = FALSE
                       FI-LinjeNr1:HIDDEN       = FALSE
                       FI-LinjeNr2:HIDDEN       = FALSE
                       FI-SLinjeNr1:HIDDEN      = FALSE.
            END.
            WHEN "UKE" THEN DO:
                ASSIGN FI-FraAar:SCREEN-VALUE   = STRING(YEAR(TODAY))
                       FI-TilAar:SCREEN-VALUE   = STRING(YEAR(TODAY))
                       FI-LinjeNr1:SCREEN-VALUE = "1"
                       FI-LinjeNr2:SCREEN-VALUE = "53"
                       FI-SFraAar:HIDDEN        = FALSE
                       FI-SFraAar:SCREEN-VALUE   = STRING(YEAR(TODAY))
                       FI-SLinjeNr1:SCREEN-VALUE = "1"
                       FI-FraAar:HIDDEN         = FALSE
                       FI-TilAar:HIDDEN         = FALSE
                       FI-LinjeNr1:HIDDEN       = FALSE
                       FI-LinjeNr2:HIDDEN       = FALSE
                       FI-SLinjeNr1:HIDDEN      = FALSE.
            END.
            WHEN "DAG" THEN DO:
                ASSIGN FI-Dato1:SCREEN-VALUE  = STRING(DATE(1,1,YEAR(TODAY)))
                       FI-Dato2:SCREEN-VALUE  = STRING(TODAY)
                       FI-Dato1:HIDDEN        = FALSE
                       FI-Dato2:HIDDEN        = FALSE
                       FI-SDato:HIDDEN        = FALSE
                       FI-SDato:SCREEN-VALUE  = STRING(DATE(1,1,YEAR(TODAY)))
                       BUTTON-SokDato1:HIDDEN = FALSE
                       BUTTON-SokDato2:HIDDEN = FALSE
                       BUTTON-SSokDato:HIDDEN = FALSE.
            END.
            OTHERWISE DO:
            END.
        END CASE.
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
  {syspara.i 1 100 1 cAlle}
  {syspara.i 220 1 3 cVisFelterTxt}
  IF cAlle = "" THEN
      ASSIGN cAlle = "[Alle]".
  RUN FixStrings.
  RUN SUPER.
  PUBLISH "GetWindowH" (OUTPUT h_Window ).
  IF VALID-HANDLE(h_Window) THEN
      ASSIGN h_dstlinje   = DYNAMIC-FUNCTION('geth_dstlinje':U IN h_Window)
             h_frapportgrid = DYNAMIC-FUNCTION('geth_frapportgrid':U IN h_Window).
  /* Code placed here will execute AFTER standard behavior.    */
  RUN InitCB2.
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN cFilename = SESSION:TEMP-DIRECTORY + "gridstlinje.txt"
             FI-LevNr  = "*"
             FI-LevNr:SCREEN-VALUE = cAlle
             FI-Avdeling  = "*"
             FI-Avdeling:SCREEN-VALUE = cAlle
             FI-HuvGr  = "*"
             FI-HuvGr:SCREEN-VALUE = cAlle
             FI-VarGr  = "*"
             FI-VarGr:SCREEN-VALUE = cAlle.
  END.
  APPLY "VALUE-CHANGED" TO RS-Sttype.
  IF VALID-HANDLE(h_frapportgrid) THEN
      RUN ClearGrid IN h_frapportgrid (cLabels).
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
DEFINE        VARIABLE  cKriterier     AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cButikker      AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cPeriodeType   AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  cPeriode       AS CHARACTER  NO-UNDO.
    DYNAMIC-FUNCTION('getKriterier':U,
     OUTPUT cKriterier /* CHARACTER */).
    cKriterier = entry(2,cKriterier,CHR(1)).

    DO WITH FRAME {&FRAME-NAME}:
        CASE ENTRY(1,cKriterier):
            WHEN "AAR" THEN cPeriodeType = "ÅR".
            WHEN "MANED" THEN cPeriodeType = "MÅNED".
            WHEN "UKE" THEN cPeriodeType = "UKE".
            WHEN "DAG" THEN cPeriodeType = "DAG".
        END CASE.
        ASSIGN cPeriode = "Periode: " + cPeriodeType + " Fra: " + ENTRY(2,cKriterier) + (IF NUM-ENTRIES(cKriterier) = 5 AND 
                                                                             ENTRY(1,cKriterier) <> "AAR" THEN "-" + ENTRY(4,cKriterier) ELSE "") +
                                                       " Til: " + ENTRY(3,cKriterier) + (IF NUM-ENTRIES(cKriterier) = 5 AND
                                                                             ENTRY(1,cKriterier) <> "AAR" THEN "-" + ENTRY(5,cKriterier) ELSE "") +
                                                       "  Sammenligning start: " + (IF cPeriodetype = "DAG" THEN FI-SDato:SCREEN-VALUE ELSE FI-SFraAar:SCREEN-VALUE +
                                                                   (IF ENTRY(1,cKriterier) <> "AAR" THEN "-" + FI-SLinjeNr1:SCREEN-VALUE ELSE "")).
        ASSIGN cButikker = (IF FI-Butikker <> "" THEN REPLACE(FI-Butikker,"|",",") ELSE REPLACE(CB-ButikkTeam:SCREEN-VALUE,CHR(1),",")).
        RELEASE Butiker.
        IF NUM-ENTRIES(cButikker) = 1 THEN
            FIND Butiker WHERE Butiker.Butik = INT(cButikker) NO-LOCK NO-ERROR.
        ASSIGN cFilterVerdier = IF AVAIL Butiker THEN "Butikk: " + Butiker.Butnamn ELSE "Butikker: " + cButikker + "  " + cPeriode.
        ASSIGN cColAlign = cRightCols.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetIpButiker fFrameWin 
PROCEDURE SetIpButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipButiker AS CHARACTER  NO-UNDO.
        ASSIGN cAnropButiker = ipButiker.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetPer2 fFrameWin 
PROCEDURE SetPer2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER cType          AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER cFraAarPerLin1 AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER cTilAarPerLin1 AS CHARACTER  NO-UNDO.
   DEFINE INPUT  PARAMETER cFraAarPerLin2 AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER cTilAarPerLin2 AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE  iWeekNum   AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE  iCount     AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE  iAntper    AS INTEGER    NO-UNDO.
   IF cType = "MANED" THEN DO:
       DO iCount = INT(cFraAarPerLin1) TO INT(cTilAarPerLin1):
           IF iCount = INT(cTilAarPerLin1) THEN
               LEAVE.
           IF INT(SUBSTR(STRING(iCount),5)) = 13 THEN
               ASSIGN iCount = iCount + 988.
           ASSIGN iAntPer = iAntPer + 1.
       END.
       ASSIGN cTilAarPerLin2 = cFraAarPerLin2.
       IF iAntPer <> 0 THEN DO:
           ASSIGN iCount = INT(cTilAarPerLin2) + 1.
           REPEAT:
               IF INT(SUBSTR(STRING(iCount),5)) = 13 THEN
                   ASSIGN iCount = INT(STRING(INT(SUBSTR(STRING(iCount),1,4)) + 1) + "001").
               ASSIGN iAntPer = iAntPer - 1.
               IF iAntPer = 0 THEN
                   LEAVE.
               ASSIGN iCount = iCount + 1.
           END.
           ASSIGN cTilAarPerLin2 = STRING(iCount).
       END.
   END.
   IF cType = "UKE" THEN DO:
       DO iCount = INT(cFraAarPerLin1) TO INT(cTilAarPerLin1):
           IF iCount = INT(cTilAarPerLin1) THEN DO:
               IF SUBSTR(cTilAarPerLin1,5) = "053" THEN DO:
                   RUN weeknum.p (DATE(12,31,INT(SUBSTR(STRING(iCount),1,4))), OUTPUT iWeekNum).
                   IF SUBSTR(STRING(iWeekNum),5) = "53" THEN
                       ASSIGN iAntPer = iAntPer + 1.
               END.
               LEAVE.
           END.
           IF INT(SUBSTR(STRING(iCount),5)) = 53 THEN DO:
               RUN weeknum.p (DATE(12,31,INT(SUBSTR(STRING(iCount),1,4))), OUTPUT iWeekNum).
               IF SUBSTR(STRING(iWeekNum),5) = "53" THEN
                   ASSIGN iAntPer = iAntPer + 1.
               ASSIGN iCount = iCount + 948.
           END.
           ASSIGN iAntPer = iAntPer + 1.
       END.
       ASSIGN cTilAarPerLin2 = cFraAarPerLin2.
       IF iAntPer <> 0 THEN DO:
           ASSIGN iCount = INT(cTilAarPerLin2) + 1.
           REPEAT:
               IF INT(SUBSTR(STRING(iCount),5)) = 53 THEN DO:
                   RUN weeknum.p (DATE(12,31,INT(SUBSTR(STRING(iCount),1,4))), OUTPUT iWeekNum).
                   IF SUBSTR(STRING(iWeekNum),5) = "53" THEN
                       ASSIGN iAntPer = iAntPer - 1.
                   ASSIGN iCount = INT(STRING(INT(SUBSTR(STRING(iCount),1,4)) + 1) + "001").
                   IF iAntPer = 0 THEN
                       LEAVE.
                   ASSIGN iAntPer = iAntPer - 1.
                   IF iAntPer = 0 THEN
                       LEAVE.
               END.
               ASSIGN iAntPer = iAntPer - 1.
               IF iAntPer = 0 THEN
                   LEAVE.
               ASSIGN iCount = iCount + 1.
           END.
           ASSIGN cTilAarPerLin2 = STRING(iCount).
       END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Startsok fFrameWin 
PROCEDURE Startsok :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipKriterier   AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER opQry1        AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER opQry2        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraAar1   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilAar1   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraAar2   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilAar2   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraPerLinNr1 AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilPerLinNr1 AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cQryString   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraAarPer1   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilAarPer1   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFraAarPer2   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTilAarPer2   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iAntPerioder AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dTmp AS DATE       NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      CASE ENTRY(1,ipKriterier):
          WHEN "AAR" THEN DO:
              ASSIGN cFraAar1      = ENTRY(2,ipKriterier)
                     cTilAar1      = ENTRY(3,ipKriterier)
                     cFraPerLinNr1 = ENTRY(4,ipKriterier)
                     cTilPerLinNr1 = ENTRY(5,ipKriterier)
                     cFraAarPer1 = cFraAar1 + STRING(INT(cFraPerLinNr1),"999")
                     cTilAarPer1 = cTilAar1 + STRING(INT(cTilPerLinNr1),"999")
                     iAntPerioder  = INT(cTilAar1) - INT(cFraAar1)
                     cFraAar2      = FI-SFraAar:SCREEN-VALUE
                     cTilAar2      = STRING(INT(cFraAar2) + iAntPerioder)
                     cFraAarPer2 = cFraAar2 + STRING(INT(cFraPerLinNr1),"999")
                     cTilAarPer2 = cTilAar2 + STRING(INT(cTilPerLinNr1),"999").
          END.
          WHEN "MANED" THEN DO:
              ASSIGN cFraAar1      = ENTRY(2,ipKriterier)
                     cTilAar1      = ENTRY(3,ipKriterier)
                     cFraPerLinNr1 = ENTRY(4,ipKriterier)
                     cTilPerLinNr1 = ENTRY(5,ipKriterier)
                     cFraAarPer1 = cFraAar1 + STRING(INT(cFraPerLinNr1),"999")
                     cTilAarPer1 = cTilAar1 + STRING(INT(cTilPerLinNr1),"999")
                     cFraAarPer2 = FI-SFraAar:SCREEN-VALUE + STRING(INT(FI-SLinjeNr1:SCREEN-VALUE),"999").
              RUN SetPer2 ("MANED",cFraAarPer1,cTilAarPer1,cFraAarPer2,OUTPUT cTilAarPer2).
          END.
          WHEN "UKE" THEN DO:
              ASSIGN cFraAar1      = ENTRY(2,ipKriterier)
                     cTilAar1      = ENTRY(3,ipKriterier)
                     cFraPerLinNr1 = ENTRY(4,ipKriterier)
                     cTilPerLinNr1 = ENTRY(5,ipKriterier)
                     cFraAarPer1 = cFraAar1 + STRING(INT(cFraPerLinNr1),"999")
                     cTilAarPer1 = cTilAar1 + STRING(INT(cTilPerLinNr1),"999")
                     cFraAarPer2 = FI-SFraAar:SCREEN-VALUE + STRING(INT(FI-SLinjeNr1:SCREEN-VALUE),"999").
              RUN SetPer2 ("UKE",cFraAarPer1,cTilAarPer1,cFraAarPer2,OUTPUT cTilAarPer2).
          END.
          WHEN "DAG" THEN DO:
              ASSIGN cFraAar1      = STRING(YEAR(DATE(ENTRY(2,ipKriterier))))
                     cTilAar1      = STRING(YEAR(DATE(ENTRY(3,ipKriterier))))
                     cFraPerLinNr1 = STRING(DATE(ENTRY(2,ipKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(2,ipKriterier))) - 1))
                     cTilPerLinNr1 = STRING(DATE(ENTRY(3,ipKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(3,ipKriterier))) - 1))
                     cFraAarPer2   = STRING(YEAR(INPUT FI-SDato)) + STRING(INPUT FI-SDato - DATE(12,31,YEAR(INPUT FI-SDato) - 1),"999")
                     dTmp          = INPUT FI-SDato + (INPUT FI-Dato2 - INPUT FI-Dato1)
                  cTilAarPer2   = STRING(YEAR(dTmp)) + STRING(dTmp - DATE(12,31,YEAR(dTmp) - 1),"999").
/*                      cTilAarPer2   = STRING(YEAR(INPUT FI-SDato)) + STRING(dTmp - DATE(12,31,YEAR(dTmp) - 1),"999"). */
          END.
    /*       OTHERWISE DO:                                                                                                    */
    /*           ASSIGN cFraAar1      = STRING(YEAR(DATE(ENTRY(2,ipKriterier))))                                               */
    /*                  cTilAar1      = STRING(YEAR(DATE(ENTRY(3,ipKriterier))))                                               */
    /*                  cFraPerLinNr1 = STRING(DATE(ENTRY(2,ipKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(2,ipKriterier))) - 1))  */
    /*                  cTilPerLinNr1 = STRING(DATE(ENTRY(3,ipKriterier)) - DATE(12,31,YEAR(DATE(ENTRY(3,ipKriterier))) - 1)). */
    /*       END.                                                                                                             */
      END CASE.
      ASSIGN cQryString = 
          "FOR EACH StLinje WHERE SUBSTBUTIK AND StTypeId = '&1' AND PerId = '&2' AND AarPerLinNr >= &3 AND AarPerLinNr <= &4 use-index AarPerLinNr no-lock"
          cFraAarPer1 = cFraAar1 + STRING(INT(cFraPerLinNr1),"999")
          cTilAarPer1 = cTilAar1 + STRING(INT(cTilPerLinNr1),"999")
          opQry1 = SUBSTITUTE(cQryString,cStTypeId,ENTRY(1,ipKriterier),cFraAarPer1,cTilAarPer1)
          opQry2 = SUBSTITUTE(cQryString,cStTypeId,ENTRY(1,ipKriterier),cFraAarPer2,cTilAarPer2).
/*       DYNAMIC-FUNCTION('setQueryString':U IN h_dstlinje, */
/*          INPUT cQryString /* CHARACTER */).              */
  END.
/*   MESSAGE opQry1 SKIP                    */
/*           opQry2                         */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderKriterier fFrameWin 
PROCEDURE ValiderKriterier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    CASE CB-PerId:SCREEN-VALUE:
        WHEN "AAR" THEN DO:
            IF INPUT FI-FraAar > YEAR(TODAY) OR
               INPUT FI-TilAar > YEAR(TODAY) OR 
               INPUT FI-FraAar > INPUT FI-TilAar THEN DO:
                MESSAGE "Feil"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN "AVBRYT".
            END.
        END.
        WHEN "MANED" THEN DO:
            IF INPUT FI-FraAar > YEAR(TODAY) OR
               INPUT FI-TilAar > YEAR(TODAY) OR 
               INPUT FI-FraAar > INPUT FI-TilAar OR 
               INPUT FI-LinjeNr1 < 1 OR 
               INPUT FI-LinjeNr1 > 12 OR
               INPUT FI-LinjeNr2 < 1 OR 
               INPUT FI-LinjeNr2 > 12 OR 
               (INPUT FI-FraAar = INPUT FI-TilAar AND 
                      INPUT FI-LinjeNr1 > INPUT FI-LinjeNr2) 
                THEN DO:
                MESSAGE "Feil"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN "AVBRYT".
            END.
        END.
        WHEN "UKE" THEN DO:
            IF INPUT FI-FraAar > YEAR(TODAY) OR
               INPUT FI-TilAar > YEAR(TODAY) OR 
               INPUT FI-FraAar > INPUT FI-TilAar OR 
               INPUT FI-LinjeNr1 < 1 OR 
               INPUT FI-LinjeNr1 > 53 OR
               INPUT FI-LinjeNr2 < 1 OR 
               INPUT FI-LinjeNr2 > 53 OR 
               (INPUT FI-FraAar = INPUT FI-TilAar AND 
                      INPUT FI-LinjeNr1 > INPUT FI-LinjeNr2) 
                THEN DO:
                MESSAGE "Feil"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN "AVBRYT".
            END.
        END.
        WHEN "DAG" THEN DO:
            IF INPUT FI-Dato1 > TODAY OR
               INPUT FI-Dato2 > TODAY OR 
               INPUT FI-Dato1 > INPUT FI-Dato2 THEN DO:
                MESSAGE "Feil"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN "AVBRYT".
            END.
        END.
        OTHERWISE DO:
            IF INPUT FI-Dato1 > TODAY OR
               INPUT FI-Dato2 > TODAY OR 
               INPUT FI-Dato1 > INPUT FI-Dato2 THEN DO:
                MESSAGE "Feil"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN "AVBRYT".
            END.
        END.
    END CASE.
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
  APPLY "VALUE-CHANGED" TO RS-Sttype IN FRAME {&FRAME-NAME}.
  IF CB-ButikkTeam:SENSITIVE THEN
    APPLY "ENTRY" TO CB-ButikkTeam.
  ELSE
    APPLY "ENTRY" TO CB-PerId.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKriterier fFrameWin 
FUNCTION getKriterier RETURNS LOGICAL
  ( OUTPUT cKriterier AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cButiker AS CHARACTER  NO-UNDO.
  RUN ValiderKriterier.
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN FALSE.   /* Function return value. */
  ELSE DO WITH FRAME {&FRAME-NAME}:
      CASE CB-PerId:SCREEN-VALUE:
          WHEN "AAR" THEN DO:
              ASSIGN cKriterier = "AAR" + "," + STRING(INPUT FI-FraAar) + "," +
                                                 STRING(INPUT FI-TilAar) + "," + "1" + "," + "1".
          END.
          WHEN "MANED" THEN DO:
              ASSIGN cKriterier = "MANED" + "," + STRING(INPUT FI-FraAar) + "," +
                                                 STRING(INPUT FI-TilAar) + ","  +
                                                 STRING(INPUT FI-LinjeNr1) + "," +
                                                 STRING(INPUT FI-LinjeNr2).
          END.
          WHEN "UKE" THEN DO:
              ASSIGN cKriterier = "UKE" + "," + STRING(INPUT FI-FraAar) + "," +
                                                 STRING(INPUT FI-TilAar) + "," +
                                                 STRING(INPUT FI-LinjeNr1) + "," +
                                                 STRING(INPUT FI-LinjeNr2).
          END.
          WHEN "DAG" THEN DO:
              ASSIGN cKriterier = "DAG" + "," + STRING(INPUT FI-Dato1) + "," +
                                                 STRING(INPUT FI-Dato2).
          END.
          OTHERWISE DO:
              ASSIGN cKriterier = CB-PerId:SCREEN-VALUE + "," + STRING(INPUT FI-Dato1) + "," +
                                                 STRING(INPUT FI-Dato2).
          END.
      END CASE.
      IF FI-Butikker = "" THEN DO:
          IF NUM-ENTRIES(CB-ButikkTeam:SCREEN-VALUE,";") = 2 THEN DO:
              FIND TT_BigListItem WHERE ROWID(TT_BigListItem) = TO-ROWID(ENTRY(2,CB-ButikkTeam:SCREEN-VALUE,";")).
              ASSIGN cButiker = REPLACE(TT_BigListItem.Butiker,CHR(1),",").
          END.
          ELSE
              ASSIGN cButiker = REPLACE(CB-ButikkTeam:SCREEN-VALUE,CHR(1),",").
      END.
      ASSIGN cKriterier = (IF FI-Butikker <> "" THEN REPLACE(FI-Butikker,"|",",") ELSE cButiker) + CHR(1) + cKriterier.
/*       ASSIGN cKriterier = (IF FI-Butikker <> "" THEN REPLACE(FI-Butikker,"|",",") ELSE REPLACE(CB-ButikkTeam:SCREEN-VALUE,CHR(1),",")) + CHR(1) + cKriterier. */
      RETURN TRUE.
  END.
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

