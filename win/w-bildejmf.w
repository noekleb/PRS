&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*-----------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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
DEFINE VAR wError         AS LOGI        NO-UNDO.
DEFINE VAR wAntCol   AS INTE EXTENT 4 INIT [5,7,8,9] NO-UNDO.
DEFINE VAR WinWPix   AS INTE EXTENT 4 INIT [689,944,1075,1200] NO-UNDO.
DEFINE VAR WinHPix   AS INTE EXTENT 4 INIT [548,672,802,925] NO-UNDO.
DEFINE VAR CtrlWPix  AS INTE EXTENT 4 INIT [666,924,1055,1180] NO-UNDO.
DEFINE VAR CtrlHPix  AS INTE EXTENT 4 INIT [504,630,755,878] NO-UNDO.
def    var wRGB      as char no-undo.
DEFINE VARIABLE chBild      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE hBilde      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE iAntArt     AS INTEGER    NO-UNDO.
DEFINE VARIABLE cInputListe AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCL         AS INTEGER               NO-UNDO.
DEFINE VARIABLE iProfilNr LIKE Prisprofil.ProfilNr NO-UNDO.
DEFINE VARIABLE hFrame    AS WIDGET EXTENT 6       NO-UNDO.
DEFINE VARIABLE hValPris  AS WIDGET EXTENT 6      NO-UNDO.
DEFINE VARIABLE hValKod   AS WIDGET EXTENT 6      NO-UNDO.
DEFINE VARIABLE hInnPris  AS WIDGET EXTENT 6      NO-UNDO.
DEFINE VARIABLE hRab1     AS WIDGET EXTENT 6      NO-UNDO.
DEFINE VARIABLE hRab2     AS WIDGET EXTENT 6      NO-UNDO.
DEFINE VARIABLE hFrakt    AS WIDGET EXTENT 6      NO-UNDO.
DEFINE VARIABLE hDivKost  AS WIDGET EXTENT 6      NO-UNDO.
DEFINE VARIABLE hRab3     AS WIDGET EXTENT 6      NO-UNDO.
DEFINE VARIABLE hVareKost AS WIDGET EXTENT 6      NO-UNDO.
DEFINE VARIABLE hDBKr     AS WIDGET EXTENT 6      NO-UNDO.
DEFINE VARIABLE hMvaKr    AS WIDGET EXTENT 6      NO-UNDO.
DEFINE VARIABLE hPris     AS WIDGET EXTENT 6      NO-UNDO.
DEFINE VARIABLE hEuPris     AS WIDGET EXTENT 6      NO-UNDO.


DEFINE VAR wColor AS INTE EXTENT 3 INIT [5197823,65280,16711680] NO-UNDO.

/*
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    DEF TEMP-TABLE tBild
&ELSE
    /*DEF SHARED TEMP-TABLE tBild*/
    DEFINE SHARED TEMP-TABLE tbild /* LIKE Bild */
&ENDIF
  FIELD ArtikkelNr AS DECIMAL   DECIMALS 2 FORMAT "zzzzzzzzzzzz9"
  FIELD BestNr     AS INTEGER   FORMAT "zzzzz9"
  FIELD Bild       AS CHARACTER FORMAT "X(30)"
  FIELD BildTxt    AS CHARACTER FORMAT "X(30)"
  FIELD CellNr     AS INTEGER
  FIELD Farg       AS INTEGER   LABEL "Farge"
  FIELD InnkjPris  AS DECIMAL   DECIMALS 2 FORMAT "-zz,zzz,zz9" LABEL ""
  FIELD LevArtNr   AS CHARACTER FORMAT "X(30)" LABEL "Lev.ArtikkelNr"
  FIELD LevNr      AS INTEGER   FORMAT "zzzzz9" LABEL "LevNr"
  FIELD LevTid     AS CHARACTER FORMAT "X(6)" LABEL "Lev.Tid"
  FIELD ListeLinje AS RECID
  FIELD OrdreNr    AS INTEGER   FORMAT "zzzzz9" LABEL "OrdreNr"
  FIELD Valutapris AS DECIMAL   DECIMALS 2 FORMAT "-zz,zzz,zz9" LABEL "Valutapris"
  FIELD VareKost   AS DECIMAL   DECIMALS 2 FORMAT "-zz,zzz,zz9" LABEL "Varekost"
  FIELD VgLopNr    AS CHARACTER FORMAT "X(10)" LABEL "Vg/LøpeNr".
*/

/* DEF BUFFER bFraBild FOR tBild. */

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-50 B-Exit RECT-51 Btn_Help 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD BildText C-Win 
FUNCTION BildText RETURNS CHARACTER
  ( /* */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD BildText2 C-Win 
FUNCTION BildText2 RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Hex2Int C-Win 
FUNCTION Hex2Int RETURNS INTEGER
  ( INPUT wRGB AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Minneserror C-Win 
FUNCTION Minneserror RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE Grid AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chGrid AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE IMAGE-Sko AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chIMAGE-Sko AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Exit 
     IMAGE-UP FILE "icon/e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 10" 
     SIZE 4.6 BY 1.1 TOOLTIP "Avslutt".

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon/e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Hjelp" 
     SIZE 4.6 BY 1.1 TOOLTIP "Hjelp"
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 195.8 BY .1.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 195.8 BY .1.

DEFINE BUTTON BUTTON-SlettBest 
     IMAGE-UP FILE "icon/e-del":U
     LABEL "&Slette" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ta bort bestilling".

DEFINE VARIABLE FI-DBKr-A AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "DB[+]" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-DivKost-A AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Div. kost (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-EuPris-A AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris (Euro)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Frakt-A AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Frakt (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-InnPris-A AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Innkjøspris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MvaKr-A AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Mva[+]" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-Pris-A AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab1-A AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 1 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab2-A AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 2 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab3-A AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 3 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ValKod-A AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ValPris-A AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Valutapris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VareKost-A AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Varekost" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE BUTTON BUTTON-SlettBest-2 
     IMAGE-UP FILE "icon/e-del":U
     LABEL "&Slette" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ta bort bestilling".

DEFINE VARIABLE FI-DBKr-B AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "DB[+]" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-DivKost-B AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Div. kost (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-EuPris-B AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris (Euro)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Frakt-B AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Frakt (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-InnPris-B AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Innkjøpspris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MvaKr-B AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Mva[+]" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-Pris-B AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-Rab1-B AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 1 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab2-B AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 2 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab3-B AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 3 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ValKod-B AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ValPris-B AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Valutapris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VareKost-B AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Varekost" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE BUTTON BUTTON-SlettBest-3 
     IMAGE-UP FILE "icon/e-del":U
     LABEL "&Slette" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ta bort bestilling".

DEFINE VARIABLE FI-DBKr-C AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "DB[+]" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-DivKost-C AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Div. kost (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-EuPris-C AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris (Euro)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Frakt-C AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Frakt (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-InnPris-C AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Innkjøpspris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MvaKr-C AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Mva[+]" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-Pris-C AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab1-C AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 1 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab2-C AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 2 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab3-C AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 3 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ValKod-C AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ValPris-C AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Valutapris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VareKost-C AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Varekost" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE BUTTON BUTTON-SlettBest-4 
     IMAGE-UP FILE "icon/e-del":U
     LABEL "&Slette" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ta bort bestilling".

DEFINE VARIABLE FI-DBKr-D AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "DB[+]" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-DivKost-D AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Div. kost (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-EuPris-D AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris (Euro)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Frakt-D AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Frakt (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-InnPris-D AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Innkjøpspris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MvaKr-D AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Mva[+]" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-Pris-D AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab1-D AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 1 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab2-D AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 2 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab3-D AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 3 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ValKod-D AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ValPris-D AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Valutapris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VareKost-D AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Varekost" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE BUTTON BUTTON-SlettBest-5 
     IMAGE-UP FILE "icon/e-del":U
     LABEL "&Slette" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ta bort bestilling".

DEFINE VARIABLE FI-DBKr-E AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "DB[+]" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-DivKost-E AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Div. kost (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-EuPris-E AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris (Euro)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Frakt-E AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Frakt (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-InnPris-E AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Innkjøpspris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MvaKr-E AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Mva[+]" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-Pris-E AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab1-E AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 1 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab2-E AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 2 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab3-E AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 3 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ValKod-E AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ValPris-E AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Valutapris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VareKost-E AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Varekost" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE BUTTON BUTTON-SlettBest-6 
     IMAGE-UP FILE "icon/e-del":U
     LABEL "&Slette" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ta bort bestilling".

DEFINE VARIABLE FI-DBKr-F AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "DB[+]" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-DivKost-F AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Div. kost (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-EuPris-F AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris (Euro)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Frakt-F AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Frakt (+)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-InnPris-F AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Innkjøpspris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MvaKr-F AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Mva[+]" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-Pris-F AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab1-F AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 1 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab2-F AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 2 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Rab3-F AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Rabatt 3 (-)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ValKod-F AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ValPris-F AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Valutapris" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VareKost-F AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Varekost" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-Exit AT ROW 1.19 COL 190.8
     Btn_Help AT ROW 1.19 COL 186
     RECT-50 AT ROW 1.1 COL 1
     RECT-51 AT ROW 2.33 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 195.8 BY 28.43.

DEFINE FRAME FRAME-A
     FI-ValPris-A AT ROW 1.38 COL 13.8 COLON-ALIGNED
     FI-ValKod-A AT ROW 2.43 COL 13.8 COLON-ALIGNED NO-LABEL
     FI-InnPris-A AT ROW 3.71 COL 13.8 COLON-ALIGNED
     FI-Rab1-A AT ROW 4.71 COL 13.8 COLON-ALIGNED
     FI-Rab2-A AT ROW 5.76 COL 13.8 COLON-ALIGNED
     FI-Frakt-A AT ROW 6.76 COL 13.8 COLON-ALIGNED
     FI-DivKost-A AT ROW 7.71 COL 13.8 COLON-ALIGNED
     FI-Rab3-A AT ROW 8.67 COL 13.8 COLON-ALIGNED
     FI-VareKost-A AT ROW 10 COL 13.8 COLON-ALIGNED
     FI-DBKr-A AT ROW 11.05 COL 13.8 COLON-ALIGNED HELP
          "DB"
     FI-MvaKr-A AT ROW 12.1 COL 13.8 COLON-ALIGNED HELP
          "Mva"
     FI-Pris-A AT ROW 13.43 COL 13.8 COLON-ALIGNED
     FI-EuPris-A AT ROW 14.48 COL 13.8 COLON-ALIGNED
     BUTTON-SlettBest AT ROW 15.52 COL 26.6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.2 ROW 12.52
         SIZE 32 BY 16.67
         TITLE "Frame A".

DEFINE FRAME FRAME-B
     FI-ValPris-B AT ROW 1.38 COL 13.8 COLON-ALIGNED
     FI-ValKod-B AT ROW 2.43 COL 13.8 COLON-ALIGNED NO-LABEL
     FI-InnPris-B AT ROW 3.71 COL 13.8 COLON-ALIGNED
     FI-Rab1-B AT ROW 4.71 COL 13.8 COLON-ALIGNED
     FI-Rab2-B AT ROW 5.76 COL 13.8 COLON-ALIGNED
     FI-Frakt-B AT ROW 6.76 COL 13.8 COLON-ALIGNED
     FI-DivKost-B AT ROW 7.71 COL 13.8 COLON-ALIGNED
     FI-Rab3-B AT ROW 8.67 COL 13.8 COLON-ALIGNED
     FI-VareKost-B AT ROW 10 COL 13.8 COLON-ALIGNED
     FI-DBKr-B AT ROW 11.05 COL 13.8 COLON-ALIGNED HELP
          "DB"
     FI-MvaKr-B AT ROW 12.1 COL 13.8 COLON-ALIGNED HELP
          "Mva"
     FI-Pris-B AT ROW 13.43 COL 13.8 COLON-ALIGNED
     FI-EuPris-B AT ROW 14.48 COL 13.8 COLON-ALIGNED
     BUTTON-SlettBest-2 AT ROW 15.52 COL 26.6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 35.2 ROW 12.52
         SIZE 32 BY 16.67
         TITLE "Frame B".

DEFINE FRAME FRAME-C
     FI-ValPris-C AT ROW 1.38 COL 13.8 COLON-ALIGNED
     FI-ValKod-C AT ROW 2.43 COL 13.8 COLON-ALIGNED NO-LABEL
     FI-InnPris-C AT ROW 3.71 COL 13.8 COLON-ALIGNED
     FI-Rab1-C AT ROW 4.71 COL 13.8 COLON-ALIGNED
     FI-Rab2-C AT ROW 5.76 COL 13.8 COLON-ALIGNED
     FI-Frakt-C AT ROW 6.76 COL 13.8 COLON-ALIGNED
     FI-DivKost-C AT ROW 7.71 COL 13.8 COLON-ALIGNED
     FI-Rab3-C AT ROW 8.67 COL 13.8 COLON-ALIGNED
     FI-VareKost-C AT ROW 10 COL 13.8 COLON-ALIGNED
     FI-DBKr-C AT ROW 11.05 COL 13.8 COLON-ALIGNED HELP
          "DB"
     FI-MvaKr-C AT ROW 12.1 COL 13.8 COLON-ALIGNED HELP
          "Mva"
     FI-Pris-C AT ROW 13.43 COL 13.8 COLON-ALIGNED
     FI-EuPris-C AT ROW 14.48 COL 13.8 COLON-ALIGNED
     BUTTON-SlettBest-3 AT ROW 15.52 COL 26.6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 67.2 ROW 12.52
         SIZE 32 BY 16.67
         TITLE "Frame C".

DEFINE FRAME FRAME-D
     FI-ValPris-D AT ROW 1.38 COL 13.8 COLON-ALIGNED
     FI-ValKod-D AT ROW 2.43 COL 13.8 COLON-ALIGNED NO-LABEL
     FI-InnPris-D AT ROW 3.71 COL 13.8 COLON-ALIGNED
     FI-Rab1-D AT ROW 4.71 COL 13.8 COLON-ALIGNED
     FI-Rab2-D AT ROW 5.76 COL 13.8 COLON-ALIGNED
     FI-Frakt-D AT ROW 6.76 COL 13.8 COLON-ALIGNED
     FI-DivKost-D AT ROW 7.71 COL 13.8 COLON-ALIGNED
     FI-Rab3-D AT ROW 8.67 COL 13.8 COLON-ALIGNED
     FI-VareKost-D AT ROW 10 COL 13.8 COLON-ALIGNED
     FI-DBKr-D AT ROW 11.05 COL 13.8 COLON-ALIGNED HELP
          "DB"
     FI-MvaKr-D AT ROW 12.1 COL 13.8 COLON-ALIGNED HELP
          "Mva"
     FI-Pris-D AT ROW 13.43 COL 13.8 COLON-ALIGNED
     FI-EuPris-D AT ROW 14.48 COL 13.8 COLON-ALIGNED
     BUTTON-SlettBest-4 AT ROW 15.52 COL 26.6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 99.2 ROW 12.52
         SIZE 32 BY 16.67
         TITLE "Frame D".

DEFINE FRAME FRAME-E
     FI-ValPris-E AT ROW 1.38 COL 13.8 COLON-ALIGNED
     FI-ValKod-E AT ROW 2.43 COL 13.8 COLON-ALIGNED NO-LABEL
     FI-InnPris-E AT ROW 3.71 COL 13.8 COLON-ALIGNED
     FI-Rab1-E AT ROW 4.71 COL 13.8 COLON-ALIGNED
     FI-Rab2-E AT ROW 5.76 COL 13.8 COLON-ALIGNED
     FI-Frakt-E AT ROW 6.76 COL 13.8 COLON-ALIGNED
     FI-DivKost-E AT ROW 7.71 COL 13.8 COLON-ALIGNED
     FI-Rab3-E AT ROW 8.67 COL 13.8 COLON-ALIGNED
     FI-VareKost-E AT ROW 10 COL 13.8 COLON-ALIGNED
     FI-DBKr-E AT ROW 11.05 COL 13.8 COLON-ALIGNED HELP
          "DB"
     FI-MvaKr-E AT ROW 12.1 COL 13.8 COLON-ALIGNED HELP
          "Mva"
     FI-Pris-E AT ROW 13.43 COL 13.8 COLON-ALIGNED
     FI-EuPris-E AT ROW 14.48 COL 13.8 COLON-ALIGNED
     BUTTON-SlettBest-5 AT ROW 15.52 COL 26.6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 131.2 ROW 12.52
         SIZE 32 BY 16.67
         TITLE "Frame E".

DEFINE FRAME FRAME-F
     FI-ValPris-F AT ROW 1.38 COL 13.8 COLON-ALIGNED
     FI-ValKod-F AT ROW 2.43 COL 13.8 COLON-ALIGNED NO-LABEL
     FI-InnPris-F AT ROW 3.71 COL 13.8 COLON-ALIGNED
     FI-Rab1-F AT ROW 4.71 COL 13.8 COLON-ALIGNED
     FI-Rab2-F AT ROW 5.76 COL 13.8 COLON-ALIGNED
     FI-Frakt-F AT ROW 6.76 COL 13.8 COLON-ALIGNED
     FI-DivKost-F AT ROW 7.71 COL 13.8 COLON-ALIGNED
     FI-Rab3-F AT ROW 8.67 COL 13.8 COLON-ALIGNED
     FI-VareKost-F AT ROW 10 COL 13.8 COLON-ALIGNED
     FI-DBKr-F AT ROW 11.05 COL 13.8 COLON-ALIGNED HELP
          "DB"
     FI-MvaKr-F AT ROW 12.1 COL 13.8 COLON-ALIGNED HELP
          "Mva"
     FI-Pris-F AT ROW 13.43 COL 13.8 COLON-ALIGNED
     FI-EuPris-F AT ROW 14.48 COL 13.8 COLON-ALIGNED
     BUTTON-SlettBest-6 AT ROW 15.52 COL 26.6
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 163.2 ROW 12.52
         SIZE 32 BY 16.67
         TITLE "Frame F".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Sammenligning av artikkler"
         HEIGHT             = 28.43
         WIDTH              = 195.8
         MAX-HEIGHT         = 28.43
         MAX-WIDTH          = 202.4
         VIRTUAL-HEIGHT     = 28.43
         VIRTUAL-WIDTH      = 202.4
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-A:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-B:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-C:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-D:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-E:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-F:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* SETTINGS FOR FILL-IN FI-DBKr-A IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-DivKost-A IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-EuPris-A IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Frakt-A IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-InnPris-A IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-MvaKr-A IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Pris-A IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab1-A IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab2-A IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab3-A IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ValKod-A IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ValPris-A IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VareKost-A IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-B
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-B:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-DBKr-B IN FRAME FRAME-B
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-DivKost-B IN FRAME FRAME-B
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-EuPris-B IN FRAME FRAME-B
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Frakt-B IN FRAME FRAME-B
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-InnPris-B IN FRAME FRAME-B
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-MvaKr-B IN FRAME FRAME-B
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Pris-B IN FRAME FRAME-B
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab1-B IN FRAME FRAME-B
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab2-B IN FRAME FRAME-B
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab3-B IN FRAME FRAME-B
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ValKod-B IN FRAME FRAME-B
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ValPris-B IN FRAME FRAME-B
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VareKost-B IN FRAME FRAME-B
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-C
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-C:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-DBKr-C IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-DivKost-C IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-EuPris-C IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Frakt-C IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-InnPris-C IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-MvaKr-C IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Pris-C IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab1-C IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab2-C IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab3-C IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ValKod-C IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ValPris-C IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VareKost-C IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-D
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-D:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-DBKr-D IN FRAME FRAME-D
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-DivKost-D IN FRAME FRAME-D
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-EuPris-D IN FRAME FRAME-D
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Frakt-D IN FRAME FRAME-D
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-InnPris-D IN FRAME FRAME-D
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-MvaKr-D IN FRAME FRAME-D
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Pris-D IN FRAME FRAME-D
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab1-D IN FRAME FRAME-D
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab2-D IN FRAME FRAME-D
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab3-D IN FRAME FRAME-D
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ValKod-D IN FRAME FRAME-D
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ValPris-D IN FRAME FRAME-D
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VareKost-D IN FRAME FRAME-D
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-E
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-E:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-DBKr-E IN FRAME FRAME-E
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-DivKost-E IN FRAME FRAME-E
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-EuPris-E IN FRAME FRAME-E
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Frakt-E IN FRAME FRAME-E
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-InnPris-E IN FRAME FRAME-E
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-MvaKr-E IN FRAME FRAME-E
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Pris-E IN FRAME FRAME-E
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab1-E IN FRAME FRAME-E
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab2-E IN FRAME FRAME-E
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab3-E IN FRAME FRAME-E
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ValKod-E IN FRAME FRAME-E
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ValPris-E IN FRAME FRAME-E
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VareKost-E IN FRAME FRAME-E
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-F
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-F:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-DBKr-F IN FRAME FRAME-F
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-DivKost-F IN FRAME FRAME-F
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-EuPris-F IN FRAME FRAME-F
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Frakt-F IN FRAME FRAME-F
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-InnPris-F IN FRAME FRAME-F
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-MvaKr-F IN FRAME FRAME-F
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Pris-F IN FRAME FRAME-F
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab1-F IN FRAME FRAME-F
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab2-F IN FRAME FRAME-F
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rab3-F IN FRAME FRAME-F
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ValKod-F IN FRAME FRAME-F
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ValPris-F IN FRAME FRAME-F
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VareKost-F IN FRAME FRAME-F
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME IMAGE-Sko ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.71
       COLUMN          = 153
       HEIGHT          = 6.19
       WIDTH           = 27
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME Grid ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.52
       COLUMN          = 3
       HEIGHT          = 9.91
       WIDTH           = 192
       HIDDEN          = no
       SENSITIVE       = yes.
/* IMAGE-Sko OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
/* Grid OCXINFO:CREATE-CONTROL from: {0F026C11-5A66-4c2b-87B5-88DDEBAE72A1} type: VSFlexGrid */
      Grid:MOVE-AFTER(IMAGE-Sko).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Sammenligning av artikkler */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Sammenligning av artikkler */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Exit C-Win
ON CHOOSE OF B-Exit IN FRAME DEFAULT-FRAME /* Button 10 */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-A
&Scoped-define SELF-NAME BUTTON-SlettBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SlettBest C-Win
ON CHOOSE OF BUTTON-SlettBest IN FRAME FRAME-A /* Slette */
DO:
  RUN SlettJmf(1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-B
&Scoped-define SELF-NAME BUTTON-SlettBest-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SlettBest-2 C-Win
ON CHOOSE OF BUTTON-SlettBest-2 IN FRAME FRAME-B /* Slette */
DO:
    RUN SlettJmf(2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-C
&Scoped-define SELF-NAME BUTTON-SlettBest-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SlettBest-3 C-Win
ON CHOOSE OF BUTTON-SlettBest-3 IN FRAME FRAME-C /* Slette */
DO:
    RUN SlettJmf(3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-D
&Scoped-define SELF-NAME BUTTON-SlettBest-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SlettBest-4 C-Win
ON CHOOSE OF BUTTON-SlettBest-4 IN FRAME FRAME-D /* Slette */
DO:
    RUN SlettJmf(4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-E
&Scoped-define SELF-NAME BUTTON-SlettBest-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SlettBest-5 C-Win
ON CHOOSE OF BUTTON-SlettBest-5 IN FRAME FRAME-E /* Slette */
DO:
    RUN SlettJmf(5).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-F
&Scoped-define SELF-NAME BUTTON-SlettBest-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SlettBest-6 C-Win
ON CHOOSE OF BUTTON-SlettBest-6 IN FRAME FRAME-F /* Slette */
DO:
    RUN SlettJmf(6).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}
       {&WINDOW-NAME}:HIDDEN = YES.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
  do:
    RUN disable_UI.
    IF VALID-HANDLE(chGrid) THEN
        RELEASE OBJECT chGrid      NO-ERROR.
    IF VALID-HANDLE(Grid) THEN
        DELETE  OBJECT Grid        NO-ERROR.
    IF VALID-HANDLE(chIMAGE-Sko) THEN
        RELEASE OBJECT chIMAGE-Sko NO-ERROR.
    IF VALID-HANDLE(IMAGE-Sko) THEN
        DELETE  OBJECT IMAGE-Sko   NO-ERROR.
    IF VALID-HANDLE(chBild) THEN
        RELEASE OBJECT chBild NO-ERROR.
    IF VALID-HANDLE(hBilde) THEN
        RELEASE OBJECT hBilde NO-ERROR.
    ASSIGN chGrid      = ?
           chIMAGE-Sko = ?
           chBild      = ?
           hBilde      = ?.
  end.
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    {syspara.i 5 1 1 iCl INT}
    FIND Butiker WHERE Butiker.Butik = iCL NO-LOCK NO-ERROR.
    IF NOT AVAIL Butiker THEN DO:
        MESSAGE "Feil sentrallager."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    FIND Prisprofil WHERE Prisprofil.ProfilNr = Butiker.ProfilNr NO-LOCK NO-ERROR.
    IF NOT AVAIL Prisprofil THEN DO:
        MESSAGE "Prisprofil mangler for sentrallager."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    ASSIGN iProfilNr = Butiker.ProfilNr.
    ASSIGN
     C-Win:WIDTH-PIXELS = FRAME FRAME-A:X + FRAME FRAME-A:WIDTH-PIXELS + FRAME FRAME-A:X.
    RUN enable_UI.
    {lng.i} 

/*     RUN SettTittel (""). */
/*     ASSIGN COMBO-BOX-WinSize:SCREEN-VALUE = ENTRY(wScreenSize,COMBO-BOX-WinSize:LIST-ITEMS) */
/*           {&WINDOW-NAME}:HIDDEN = NO.                                                       */

/*     RUN FixWinSize. */
    RUN InitierHandles.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "w-bildejmf.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chGrid = Grid:COM-HANDLE
    UIB_S = chGrid:LoadControls( OCXFile, "Grid":U)
    Grid:NAME = "Grid":U
    chIMAGE-Sko = IMAGE-Sko:COM-HANDLE
    UIB_S = chIMAGE-Sko:LoadControls( OCXFile, "IMAGE-Sko":U)
    IMAGE-Sko:NAME = "IMAGE-Sko":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-bildejmf.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  RUN control_load.
  ENABLE RECT-50 B-Exit RECT-51 Btn_Help 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY FI-ValPris-A FI-ValKod-A FI-InnPris-A FI-Rab1-A FI-Rab2-A FI-Frakt-A 
          FI-DivKost-A FI-Rab3-A FI-VareKost-A FI-DBKr-A FI-MvaKr-A FI-Pris-A 
          FI-EuPris-A 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE BUTTON-SlettBest 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  DISPLAY FI-ValPris-B FI-ValKod-B FI-InnPris-B FI-Rab1-B FI-Rab2-B FI-Frakt-B 
          FI-DivKost-B FI-Rab3-B FI-VareKost-B FI-DBKr-B FI-MvaKr-B FI-Pris-B 
          FI-EuPris-B 
      WITH FRAME FRAME-B IN WINDOW C-Win.
  ENABLE BUTTON-SlettBest-2 
      WITH FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  DISPLAY FI-ValPris-C FI-ValKod-C FI-InnPris-C FI-Rab1-C FI-Rab2-C FI-Frakt-C 
          FI-DivKost-C FI-Rab3-C FI-VareKost-C FI-DBKr-C FI-MvaKr-C FI-Pris-C 
          FI-EuPris-C 
      WITH FRAME FRAME-C IN WINDOW C-Win.
  ENABLE BUTTON-SlettBest-3 
      WITH FRAME FRAME-C IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-C}
  DISPLAY FI-ValPris-D FI-ValKod-D FI-InnPris-D FI-Rab1-D FI-Rab2-D FI-Frakt-D 
          FI-DivKost-D FI-Rab3-D FI-VareKost-D FI-DBKr-D FI-MvaKr-D FI-Pris-D 
          FI-EuPris-D 
      WITH FRAME FRAME-D IN WINDOW C-Win.
  ENABLE BUTTON-SlettBest-4 
      WITH FRAME FRAME-D IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-D}
  DISPLAY FI-ValPris-E FI-ValKod-E FI-InnPris-E FI-Rab1-E FI-Rab2-E FI-Frakt-E 
          FI-DivKost-E FI-Rab3-E FI-VareKost-E FI-DBKr-E FI-MvaKr-E FI-Pris-E 
          FI-EuPris-E 
      WITH FRAME FRAME-E IN WINDOW C-Win.
  ENABLE BUTTON-SlettBest-5 
      WITH FRAME FRAME-E IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-E}
  DISPLAY FI-ValPris-F FI-ValKod-F FI-InnPris-F FI-Rab1-F FI-Rab2-F FI-Frakt-F 
          FI-DivKost-F FI-Rab3-F FI-VareKost-F FI-DBKr-F FI-MvaKr-F FI-Pris-F 
          FI-EuPris-F 
      WITH FRAME FRAME-F IN WINDOW C-Win.
  ENABLE BUTTON-SlettBest-6 
      WITH FRAME FRAME-F IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-F}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls C-Win 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR wCol AS INTE NO-UNDO.
/*     FIND LAST tBild WHERE tBild.Cellnr <> ?. */
    ASSIGN chGrid                   = chGrid:vsFlexGrid
           chGrid:ENABLED           = FALSE
           chGrid:Rows              = 6 
           chGrid:Cols              = 1 
           chGrid:FixedRows         = 0
           chGrid:FixedCols         = 0
           chGrid:BorderStyle       = 1
           chGrid:RowHeight(2)      = 1800
           chGrid:ColWidth(0)      = 2400
/*            chGrid:ColWidth(1)      = chGrid:ColWidth(0) */
/*            chGrid:ColWidth(2)      = chGrid:ColWidth(0) */
/*            chGrid:ColWidth(3)      = chGrid:ColWidth(0) */
/*            chGrid:ColWidth(4)      = chGrid:ColWidth(0) */
/*            chGrid:ColWidth(5)      = chGrid:ColWidth(0) */
           chGrid:AllowBigSelection = FALSE
           chGrid:AllowSelection    = FALSE
           chGrid:Redraw            = TRUE
           chGrid:GridLineWidth     = 1
/*            chGrid:GridLines         = 1 */
           chGrid:ScrollBars        = 2
           chGrid:FocusRect         = 3
           chGrid:ROW               = 1
/*            chIMAGE-Sko               = chIMAGE-Sko:Picbuf */
           chIMAGE-Sko:Picbuf:AutoScale     = TRUE
           .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitierHandles C-Win 
PROCEDURE InitierHandles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN hFrame[1]    = FRAME FRAME-A:HANDLE
           hFrame[2]    = FRAME FRAME-B:HANDLE
           hFrame[3]    = FRAME FRAME-C:HANDLE
           hFrame[4]    = FRAME FRAME-D:HANDLE
           hFrame[5]    = FRAME FRAME-E:HANDLE
           hFrame[6]    = FRAME FRAME-F:HANDLE
           hValPris[1] = FI-ValPris-A:HANDLE IN FRAME FRAME-A
           hValPris[2] = FI-ValPris-B:HANDLE IN FRAME FRAME-B
           hValPris[3] = FI-ValPris-C:HANDLE IN FRAME FRAME-C
           hValPris[4] = FI-ValPris-D:HANDLE IN FRAME FRAME-D
           hValPris[5] = FI-ValPris-E:HANDLE IN FRAME FRAME-E
           hValPris[6] = FI-ValPris-F:HANDLE IN FRAME FRAME-F
           hValKod[1] = FI-ValKod-A:HANDLE IN FRAME FRAME-A
           hValKod[2] = FI-ValKod-B:HANDLE IN FRAME FRAME-B
           hValKod[3] = FI-ValKod-C:HANDLE IN FRAME FRAME-C
           hValKod[4] = FI-ValKod-D:HANDLE IN FRAME FRAME-D
           hValKod[5] = FI-ValKod-E:HANDLE IN FRAME FRAME-E
           hValKod[6] = FI-ValKod-F:HANDLE IN FRAME FRAME-F
           hInnPris[1] = FI-InnPris-A:HANDLE IN FRAME FRAME-A
           hInnPris[2] = FI-InnPris-B:HANDLE IN FRAME FRAME-B
           hInnPris[3] = FI-InnPris-C:HANDLE IN FRAME FRAME-C
           hInnPris[4] = FI-InnPris-D:HANDLE IN FRAME FRAME-D
           hInnPris[5] = FI-InnPris-E:HANDLE IN FRAME FRAME-E
           hInnPris[6] = FI-InnPris-F:HANDLE IN FRAME FRAME-F
           hRab1[1] = FI-Rab1-A:HANDLE IN FRAME FRAME-A
           hRab1[2] = FI-Rab1-B:HANDLE IN FRAME FRAME-B
           hRab1[3] = FI-Rab1-C:HANDLE IN FRAME FRAME-C
           hRab1[4] = FI-Rab1-D:HANDLE IN FRAME FRAME-D
           hRab1[5] = FI-Rab1-E:HANDLE IN FRAME FRAME-E
           hRab1[6] = FI-Rab1-F:HANDLE IN FRAME FRAME-F
           hRab2[1] = FI-Rab2-A:HANDLE IN FRAME FRAME-A
           hRab2[2] = FI-Rab2-B:HANDLE IN FRAME FRAME-B
           hRab2[3] = FI-Rab2-C:HANDLE IN FRAME FRAME-C
           hRab2[4] = FI-Rab2-D:HANDLE IN FRAME FRAME-D
           hRab2[5] = FI-Rab2-E:HANDLE IN FRAME FRAME-E
           hRab2[6] = FI-Rab2-F:HANDLE IN FRAME FRAME-F
           hFrakt[1] = FI-Frakt-A:HANDLE IN FRAME FRAME-A
           hFrakt[2] = FI-Frakt-B:HANDLE IN FRAME FRAME-B
           hFrakt[3] = FI-Frakt-C:HANDLE IN FRAME FRAME-C
           hFrakt[4] = FI-Frakt-D:HANDLE IN FRAME FRAME-D
           hFrakt[5] = FI-Frakt-E:HANDLE IN FRAME FRAME-E
           hFrakt[6] = FI-Frakt-F:HANDLE IN FRAME FRAME-F
           hDivKost[1] = FI-DivKost-A:HANDLE IN FRAME FRAME-A
           hDivKost[2] = FI-DivKost-B:HANDLE IN FRAME FRAME-B
           hDivKost[3] = FI-DivKost-C:HANDLE IN FRAME FRAME-C
           hDivKost[4] = FI-DivKost-D:HANDLE IN FRAME FRAME-D
           hDivKost[5] = FI-DivKost-E:HANDLE IN FRAME FRAME-E
           hDivKost[6] = FI-DivKost-F:HANDLE IN FRAME FRAME-F
           hRab3[1] = FI-Rab3-A:HANDLE IN FRAME FRAME-A
           hRab3[2] = FI-Rab3-B:HANDLE IN FRAME FRAME-B
           hRab3[3] = FI-Rab3-C:HANDLE IN FRAME FRAME-C
           hRab3[4] = FI-Rab3-D:HANDLE IN FRAME FRAME-D
           hRab3[5] = FI-Rab3-E:HANDLE IN FRAME FRAME-E
           hRab3[6] = FI-Rab3-F:HANDLE IN FRAME FRAME-F
           hVarekost[1] = FI-Varekost-A:HANDLE IN FRAME FRAME-A
           hVarekost[2] = FI-Varekost-B:HANDLE IN FRAME FRAME-B
           hVarekost[3] = FI-Varekost-C:HANDLE IN FRAME FRAME-C
           hVarekost[4] = FI-Varekost-D:HANDLE IN FRAME FRAME-D
           hVarekost[5] = FI-Varekost-E:HANDLE IN FRAME FRAME-E
           hVarekost[6] = FI-Varekost-F:HANDLE IN FRAME FRAME-F
           hDBKr[1] = FI-DBKr-A:HANDLE IN FRAME FRAME-A
           hDBKr[2] = FI-DBKr-B:HANDLE IN FRAME FRAME-B
           hDBKr[3] = FI-DBKr-C:HANDLE IN FRAME FRAME-C
           hDBKr[4] = FI-DBKr-D:HANDLE IN FRAME FRAME-D
           hDBKr[5] = FI-DBKr-E:HANDLE IN FRAME FRAME-E
           hDBKr[6] = FI-DBKr-F:HANDLE IN FRAME FRAME-F
           hMvaKr[1] = FI-MvaKr-A:HANDLE IN FRAME FRAME-A
           hMvaKr[2] = FI-MvaKr-B:HANDLE IN FRAME FRAME-B
           hMvaKr[3] = FI-MvaKr-C:HANDLE IN FRAME FRAME-C
           hMvaKr[4] = FI-MvaKr-D:HANDLE IN FRAME FRAME-D
           hMvaKr[5] = FI-MvaKr-E:HANDLE IN FRAME FRAME-E
           hMvaKr[6] = FI-MvaKr-F:HANDLE IN FRAME FRAME-F
           hPris[1] = FI-Pris-A:HANDLE IN FRAME FRAME-A
           hPris[2] = FI-Pris-B:HANDLE IN FRAME FRAME-B
           hPris[3] = FI-Pris-C:HANDLE IN FRAME FRAME-C
           hPris[4] = FI-Pris-D:HANDLE IN FRAME FRAME-D
           hPris[5] = FI-Pris-E:HANDLE IN FRAME FRAME-E
           hPris[6] = FI-Pris-F:HANDLE IN FRAME FRAME-F
           hEuPris[1] = FI-EuPris-A:HANDLE IN FRAME FRAME-A
           hEuPris[2] = FI-EuPris-B:HANDLE IN FRAME FRAME-B
           hEuPris[3] = FI-EuPris-C:HANDLE IN FRAME FRAME-C
           hEuPris[4] = FI-EuPris-D:HANDLE IN FRAME FRAME-D
           hEuPris[5] = FI-EuPris-E:HANDLE IN FRAME FRAME-E
           hEuPris[6] = FI-EuPris-F:HANDLE IN FRAME FRAME-F
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyArtBas C-Win 
PROCEDURE NyArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
    DEFINE VARIABLE iIdx AS INTEGER    NO-UNDO.
    IF NUM-ENTRIES(cInputListe) = 6 THEN DO:
        MESSAGE "Listen er full"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    ELSE IF CAN-DO(cInputListe,STRING(dArtikkelNr)) THEN DO:
        MESSAGE "Allerede valgt."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    FIND ArtBas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK NO-ERROR.
    if not available ArtBas then
      return.
    find ArtPris no-lock where
      ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
      ArtPris.ProfilNr   = iProfilNr no-error.
    IF NOT AVAIL ArtPris THEN DO:
        MESSAGE "Artikkel mangler pris"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN.
    END.
    ASSIGN C-Win:WINDOW-STATE = 3
           cInputListe = cInputListe + (IF cInputListe = "" THEN "" ELSE ",")
                                 + STRING(dArtikkelnr)
           iAntArt     = NUM-ENTRIES(cInputListe).
    ASSIGN hFrame[iAntArt]:TITLE = IF ArtBas.LopNr = ? OR ArtBas.LopNr = 0 THEN
                  "Artnr: " + STRING(ArtBas.ArtikkelNr) ELSE
                  "Art: "   + STRING(ArtBas.Vg) + " / " +
                              STRING(ArtBas.LopNr).
    RUN VisBilde(1).

    ASSIGN iIdx = IF ArtPris.TilBud THEN 2 ELSE 1.

    RUN VisInfo (iIdx = 2,
                 string(ArtPris.ValPris[iIdx]),
                 ArtBas.valkod,
                 string(ArtPris.InnkjopsPris[iIdx]),
                 string(ArtPris.Rab1Kr[iIdx]),
                 string(ArtPris.Rab2Kr[iIdx]),
                 string(ArtPris.Frakt[iIdx]),
                 string(ArtPris.DivKostKr[iIdx]),
                 string(ArtPris.Rab3Kr[iIdx]),
                 string(ArtPris.VareKost[iIdx]),
                 string(ArtPris.DbKr[iIdx]),
                 string(ArtPris.MvaKr[iIdx]),
                 string(ArtPris.Pris[iIdx]),
                 string(ArtPris.EuroPris[iIdx])). 
    RUN SetVindu.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyBestHode C-Win 
PROCEDURE NyBestHode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iBestNr LIKE BestHode.BestNr       NO-UNDO.
    DEFINE VARIABLE iIdx AS INTEGER    NO-UNDO.
    IF NUM-ENTRIES(cInputListe) = 6 THEN DO:
        MESSAGE "Listen er full"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    ELSE IF CAN-DO(cInputListe,STRING(iBestNr)) THEN DO:
        MESSAGE "Allerede valgt."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    FIND BestHode WHERE BestHode.BestNr = iBestNr NO-LOCK NO-ERROR.
    FIND ArtBas OF BestHode NO-LOCK NO-ERROR.
    if not available ArtBas then
      return.
    find BestPris OF BestHode no-lock where
         BestPris.ProfilNr = iProfilNr AND
         BestPris.BestStat = BestHode.BestStat NO-ERROR.
    IF NOT AVAIL BestPris THEN
        find FIRST BestPris OF BestHode no-lock where
             BestPris.BestStat = BestHode.BestStat NO-ERROR.
    IF NOT AVAIL BestPris THEN DO:
        MESSAGE "Bestilling mangler pris"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN.
    END.
    ASSIGN C-Win:WINDOW-STATE = 3
           cInputListe = cInputListe + (IF cInputListe = "" THEN "" ELSE ",")
                                 + STRING(iBestNr)
           iAntArt     = NUM-ENTRIES(cInputListe).
    ASSIGN hFrame[iAntArt]:TITLE = "Best: " + STRING(iBestnr).
    RUN VisBilde(1).

    RUN VisInfo (FALSE,
                 string(BestPris.ValPris),         
                 ArtBas.valkod,
                 string(BestPris.InnkjopsPris),
                 string(BestPris.Rab1Kr),
                 string(BestPris.Rab2Kr),
                 string(BestPris.Frakt),
                 string(BestPris.DivKostKr),
                 string(BestPris.Rab3Kr),
                 string(BestPris.VareKost),
                 string(BestPris.DbKr),
                 string(BestPris.MvaKr),
                 string(BestPris.Pris),
                 string(BestPris.EuroPris)). 
    RUN SetVindu.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettTittel C-Win 
PROCEDURE SettTittel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcTittel AS CHAR NO-UNDO.

  ASSIGN
      C-Win:TITLE = "Bildehåndtering" + " " + pcTittel
      .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetVindu C-Win 
PROCEDURE SetVindu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DO WITH FRAME {&FRAME-NAME}:
   ASSIGN Grid:WIDTH-PIXELS = iAntArt * 160 + 5
          B-Exit:X   = iAntArt * 160 - B-Exit:WIDTH-PIXELS + FRAME FRAME-A:X
          Btn_Help:X = B-Exit:X - Btn_Help:WIDTH-PIXELS.
   CASE iAntArt:
       WHEN 1 THEN
           ASSIGN
            C-Win:WIDTH-PIXELS = FRAME FRAME-A:X + FRAME FRAME-A:WIDTH-PIXELS + FRAME FRAME-A:X.
       WHEN 2 THEN
           ASSIGN
            C-Win:WIDTH-PIXELS = FRAME FRAME-B:X + FRAME FRAME-B:WIDTH-PIXELS + FRAME FRAME-A:X
            FRAME FRAME-B:HIDDEN = FALSE.
       WHEN 3 THEN
           ASSIGN
           C-Win:WIDTH-PIXELS = FRAME FRAME-C:X + FRAME FRAME-C:WIDTH-PIXELS + FRAME FRAME-A:X
            FRAME FRAME-C:HIDDEN = FALSE.
       WHEN 4 THEN
           ASSIGN
            C-Win:WIDTH-PIXELS = FRAME FRAME-D:X + FRAME FRAME-D:WIDTH-PIXELS + FRAME FRAME-A:X
            FRAME FRAME-D:HIDDEN = FALSE.
       WHEN 5 THEN
           ASSIGN
            C-Win:WIDTH-PIXELS = FRAME FRAME-E:X + FRAME FRAME-E:WIDTH-PIXELS + FRAME FRAME-A:X
            FRAME FRAME-E:HIDDEN = FALSE.
       WHEN 6 THEN
           ASSIGN
            C-Win:WIDTH-PIXELS = FRAME FRAME-F:X + FRAME FRAME-F:WIDTH-PIXELS + FRAME FRAME-A:X
            FRAME FRAME-F:HIDDEN = FALSE.
   END CASE.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettJmf C-Win 
PROCEDURE SlettJmf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iNr     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE         iCount  AS INTEGER    NO-UNDO.
    DEFINE VARIABLE         cTmpStr AS CHARACTER  NO-UNDO.
    IF iAntArt = 1 THEN
        APPLY "CLOSE" TO THIS-PROCEDURE.
    ELSE IF iNr = iAntArt THEN DO:
              /* slett den siste */
        ASSIGN chGrid:Cols = iAntArt - 1.
        DO iCount = 1 TO iAntArt - 1:
            ASSIGN cTmpStr = cTmpStr + (IF cTmpStr = "" THEN "" ELSE ",") + ENTRY(iCount,cInputListe).
        END.
        ASSIGN cInputListe = cTmpStr
               hFrame[iNr]:HIDDEN = TRUE
               iAntArt = iAntArt - 1.
        RUN SetVindu.
    END.
    ELSE DO:
        DO iCount = iNr TO iAntArt - 1:
            chIMAGE-Sko:Picbuf:CLEAR(2).
            ASSIGN chBild = chGrid:Cell(3,2,iCount,1,iCount)
                   chIMAGE-Sko:Picbuf:Picture = chBild.
            RELEASE OBJECT chBild.
            ASSIGN chBild = chIMAGE-Sko:Picbuf:Picture
                chGrid:Cell(3,2,iCount - 1,2,iCount - 1) = chBild
                   chGrid:Cell(0,0,iCount - 1,0,iCount - 1) = chGrid:Cell(0,0,iCount,0,iCount)
                   chGrid:Cell(0,1,iCount - 1,1,iCount - 1) = chGrid:Cell(0,1,iCount,1,iCount)
                   chGrid:Cell(0,3,iCount - 1,3,iCount - 1) = chGrid:Cell(0,3,iCount,3,iCount)
                   chGrid:Cell(0,4,iCount - 1,4,iCount - 1) = chGrid:Cell(0,4,iCount,4,iCount)
/*                    chGrid:Cell(3,1,iCount - 1,1,iCount - 1) = chGrid:Cell(3,1,iCount,1,iCount) */

                   hValPris[iCount]:SCREEN-VALUE  = hValPris[iCount + 1]:SCREEN-VALUE 
                   hValKod[iCount]:SCREEN-VALUE   = hValKod[iCount + 1]:SCREEN-VALUE  
                   hInnPris[iCount]:SCREEN-VALUE  = hInnPris[iCount + 1]:SCREEN-VALUE 
                   hRab1[iCount]:SCREEN-VALUE     = hRab1[iCount + 1]:SCREEN-VALUE    
                   hRab2[iCount]:SCREEN-VALUE     = hRab2[iCount + 1]:SCREEN-VALUE    
                   hFrakt[iCount]:SCREEN-VALUE    = hFrakt[iCount + 1]:SCREEN-VALUE   
                   hDivKost[iCount]:SCREEN-VALUE  = hDivKost[iCount + 1]:SCREEN-VALUE 
                   hRab3[iCount]:SCREEN-VALUE     = hRab3[iCount + 1]:SCREEN-VALUE    
                   hVareKost[iCount]:SCREEN-VALUE = hVareKost[iCount + 1]:SCREEN-VALUE
                   hDBKr[iCount]:SCREEN-VALUE     = hDBKr[iCount + 1]:SCREEN-VALUE    
                   hMvaKr[iCount]:SCREEN-VALUE    = hMvaKr[iCount + 1]:SCREEN-VALUE   
                   hPris[iCount]:SCREEN-VALUE     = hPris[iCount + 1]:SCREEN-VALUE    
                   hPris[iCount]:BGCOLOR          = hPris[iCount + 1]:BGCOLOR
                   hEuPris[iCount]:SCREEN-VALUE   = hEuPris[iCount + 1]:SCREEN-VALUE  
                   hFrame[iCount]:TITLE = hFrame[iCount + 1]:TITLE.
            RELEASE OBJECT chBild.
        END.
        DO iCount = 1 TO iAntArt:
            IF iCount = iNr THEN
                NEXT.
            ASSIGN cTmpStr = cTmpStr + (IF cTmpStr = "" THEN "" ELSE ",") + ENTRY(iCount,cInputListe).
        END.
        ASSIGN cInputListe = cTmpStr
               hFrame[iAntArt]:HIDDEN = TRUE
               iAntArt = iAntArt - 1.
        RUN SetVindu.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisBilde C-Win 
PROCEDURE VisBilde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE wOK AS LOGICAL       NO-UNDO.
  if not available ArtBas then
    return.
do with frame DEFAULT-FRAME:  
  {visbilde.i
    &BldOcx = "chIMAGE-Sko"
    &BildNr = "ArtBas.BildNr"
  }
      FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
      FIND Farg OF ArtBas NO-LOCK NO-ERROR.
      ASSIGN hBilde = chIMAGE-Sko:Picbuf:Picture
             chGrid:Cols = iAntArt
             chGrid:ColWidth(chGrid:Cols - 1) = chGrid:ColWidth(0)
             chGrid:ROW = 2
             chGrid:COL = iAntArt - 1
             chGrid:CellPictureAlignment = 9
             chGrid:CellPicture = hBilde
             chGrid:Cell(2,0,iAntArt - 1,0,iAntArt - 1) = 1
             chGrid:Cell(0,0,iAntArt - 1,0,iAntArt - 1) = 
        IF ArtBas.LevKod <> "" THEN "Levkode: " +  ArtBas.LevKod ELSE "Artnr: " + STRING(ArtBas.ArtikkelNr)
            chGrid:Cell(2,1,iAntArt - 1,1,iAntArt - 1) = 1
            chGrid:Cell(0,1,iAntArt - 1,1,iAntArt - 1) = 
       IF ArtBas.LevFargKod <> "" THEN "Levfarge: " +  ArtBas.LevFargKod ELSE ""
            chGrid:Cell(2,3,iAntArt - 1,3,iAntArt - 1) = 1
            chGrid:Cell(0,3,iAntArt - 1,3,iAntArt - 1) = 
               IF AVAIL LevBas THEN LevBas.levnamn ELSE ""
            chGrid:Cell(2,4,iAntArt - 1,4,iAntArt - 1) = 1
            chGrid:Cell(0,4,iAntArt - 1,4,iAntArt - 1) = ArtBas.BongTekst
            chGrid:Cell(0,5,iAntArt - 1,5,iAntArt - 1) = 
        IF AVAIL Farg THEN Farg.FarBeskr ELSE "".
end.    
RELEASE OBJECT hBilde.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisInfo C-Win 
PROCEDURE VisInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER lTilbud   AS LOGICAL    NO-UNDO.
    DEFINE INPUT  PARAMETER cValPris  AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cValKod   AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cInnPris  AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cRab1     AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cRab2     AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cFrakt    AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cDivKost  AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cRab3     AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cVareKost AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cDBKr     AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cMvaKr    AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cPris     AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cEuPris     AS CHARACTER  NO-UNDO.
    ASSIGN hValPris[iAntArt]:SCREEN-VALUE  = cValPris 
           hValKod[iAntArt]:SCREEN-VALUE   = cValKod  
           hInnPris[iAntArt]:SCREEN-VALUE  = cInnPris 
           hRab1[iAntArt]:SCREEN-VALUE     = cRab1    
           hRab2[iAntArt]:SCREEN-VALUE     = cRab2    
           hFrakt[iAntArt]:SCREEN-VALUE    = cFrakt   
           hDivKost[iAntArt]:SCREEN-VALUE  = cDivKost 
           hRab3[iAntArt]:SCREEN-VALUE     = cRab3    
           hVareKost[iAntArt]:SCREEN-VALUE = cVareKost
           hDBKr[iAntArt]:SCREEN-VALUE     = cDBKr    
           hMvaKr[iAntArt]:SCREEN-VALUE    = cMvaKr   
           hPris[iAntArt]:SCREEN-VALUE     = cPris
           hPris[iAntArt]:BGCOLOR          = IF lTilbud THEN 12 ELSE ?
           hEuPris[iAntArt]:SCREEN-VALUE   = cEuPris  
           .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION BildText C-Win 
FUNCTION BildText RETURNS CHARACTER
  ( /* */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*   RETURN tBild.TekstRad1 + CHR(13) + */
/*          tBild.TekstRad2.            */
/*   RETURN tBild.BildTxt + CHR(13) +                               */
/*          string(tBild.ValutaPris) + " " +                        */
/*          string(tBild.VareKost) + " " +                          */
/*          string(tBild.InnkjPris).   /* Function return value. */ */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION BildText2 C-Win 
FUNCTION BildText2 RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
/*   RETURN bFraBild.TekstRad1 + CHR(13) + */
/*          bFraBild.TekstRad2.            */
/*                                                                     */
/*   RETURN bFraBild.BildTxt + CHR(13) +                               */
/*          string(bFraBild.ValutaPris) + " " +                        */
/*          string(bFraBild.VareKost) + " " +                          */
/*          string(bFraBild.InnkjPris).   /* Function return value. */ */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Hex2Int C-Win 
FUNCTION Hex2Int RETURNS INTEGER
  ( INPUT wRGB AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN INT(ENTRY(3,wRGB)) * 65536 +
         INT(ENTRY(2,wRGB)) * 256   +
         INT(ENTRY(1,wRGB)).         

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Minneserror C-Win 
FUNCTION Minneserror RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  MESSAGE "Minneområdet som benyttes for lagring av bilder er fullt!" SKIP
          "Bilder som ikke er lest inn er markert med hvite felt i bildegriden." SKIP
          "Du bør avslutte hele sesjonen. Dvs starte om SkoTex."
          VIEW-AS ALERT-BOX ERROR TITLE "Feil".
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

