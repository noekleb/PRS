&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
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
DEFINE VARIABLE cLanButiker       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFtpButiker       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPRSFtpButiker    AS CHAR       NO-UNDO.
DEFINE VARIABLE cOrgLanButiker    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cOrgFtpButiker    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lVare             AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lVarGr            AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lKKamp            AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lKasValuta        AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lKasserere        AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lSelgere          AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lKunde            AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lFarger           AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lStrKonv          AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lStrType          AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lTekster          AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lGaranti          AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lButiker          AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lSyspara          AS LOGICAL    NO-UNDO.
DEFINE VARIABLE iHPix             AS INTEGER    NO-UNDO.
DEFINE VARIABLE iWPix             AS INTEGER    NO-UNDO.
DEFINE VARIABLE cDatoTid          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lActiveTick       AS LOGICAL    NO-UNDO. /* */
DEFINE VARIABLE cFilNavn          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lTimerOff         AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lKlargjor         AS LOGICAL    NO-UNDO.

DEFINE VARIABLE lSupOrdTilMail    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lWebButEksport    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lKasseEksport     AS LOGICAL    NO-UNDO. /* Om aktiva kassor */
DEFINE VARIABLE lErpEksport       AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lWebMedlemEksport AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lOrdHK            AS LOGICAL    NO-UNDO.
DEFINE VARIABLE lPkSdlHK          AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cHKinst           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ipcHost           AS CHAR       NO-UNDO. 
DEFINE VARIABLE ipcPort           AS CHAR       NO-UNDO. 
DEFINE VARIABLE lHost             AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cTekst            AS CHAR       NO-UNDO.

/* DEFINE VARIABLE lErpFin    AS LOGICAL    NO-UNDO. */
/* DEFINE VARIABLE lErpOrd    AS LOGICAL    NO-UNDO. */
/* DEFINE VARIABLE lErpVpi    AS LOGICAL    NO-UNDO. */
DEFINE VARIABLE cWebButEksportRutine      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFerskVareEksportRutine   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFinEksportRutine         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFakEksportRutine         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKontEksportRutine        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cOrdEksportRutine         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVpiEksportRutine         AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVpiVareEksportRutine     AS CHAR       NO-UNDO.
DEFINE VARIABLE cMedWebEksportRutine      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cOrdButikkEksportRutine   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPkSdlButikkEksportRutine AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSendesKatalog            AS CHAR       NO-UNDO.
DEFINE VARIABLE cSendSupOrdTilMailRutine      AS CHARACTER  NO-UNDO.
DEF VAR hInstance AS INT NO-UNDO.
DEF VAR cULOGfil        AS CHAR INIT "C:\home\lindbak\kasse\ULOG" NO-UNDO.
DEFINE VARIABLE hServer AS HANDLE     NO-UNDO.
DEFINE VARIABLE cConnect AS CHARACTER INIT "-S elogg" NO-UNDO.
DEFINE VARIABLE cEksportkatalog AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lFlyttaTilSendes AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lIconExist      AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cRedIcon        AS CHARACTER INIT ".\icon\bullet_red.ico"    NO-UNDO.
DEFINE VARIABLE cGreenIcon      AS CHARACTER INIT ".\icon\bullet_green.ico"  NO-UNDO.
DEFINE VARIABLE cYellowIcon     AS CHARACTER INIT ".\icon\bullet_yellow.ico"    NO-UNDO.
DEFINE VARIABLE dSenasteDag     AS DATE        NO-UNDO.
{windows.i}
{initjukebox.i}

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
&Scoped-define BROWSE-NAME BR-File

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES DistributeFile

/* Definitions for BROWSE BR-File                                       */
&Scoped-define FIELDS-IN-QUERY-BR-File DistributeFile.ButikkNr ~
DistributeFile.Date getButliste() DistributeFile.FileName 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-File 
&Scoped-define QUERY-STRING-BR-File FOR EACH DistributeFile NO-LOCK ~
    BY DistributeFile.ButikkNr ~
       BY DistributeFile.Date INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-File OPEN QUERY BR-File FOR EACH DistributeFile NO-LOCK ~
    BY DistributeFile.ButikkNr ~
       BY DistributeFile.Date INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-File DistributeFile
&Scoped-define FIRST-TABLE-IN-QUERY-BR-File DistributeFile


/* Definitions for FRAME FRAME-Fildistr                                 */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-Fildistr ~
    ~{&OPEN-QUERY-BR-File}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-1 B-Oppdater B-Status CLanbutikerParam ~
B-Prisko cFtpbutikerParam BUTTON-Kontroller B-StartStop 
&Scoped-Define DISPLAYED-OBJECTS CLanbutikerParam cFtpbutikerParam ~
FI-Serverstatus FI-Aktiv 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD canfindElogg wWin 
FUNCTION canfindElogg RETURNS LOGICAL
  ( INPUT cTabell AS CHARACTER, INPUT cEksternt AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getButliste wWin 
FUNCTION getButliste RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFiltxt wWin 
FUNCTION getFiltxt RETURNS CHARACTER
  ( INPUT cFilNavn AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setError wWin 
FUNCTION setError RETURNS CHARACTER
  ( INPUT fiHandle AS HANDLE,INPUT-OUTPUT lSendError AS LOGICAL,INPUT-OUTPUT cToolTip AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD socketconnect wWin 
FUNCTION socketconnect RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD startup wWin 
FUNCTION startup RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE PSTimer AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chPSTimer AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Ftpbut  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-Klargjor 
     LABEL "Klargjør" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Lanbut  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-Oppdater 
     IMAGE-UP FILE "icon/oppdater.bmp":U
     LABEL "Oppdater" 
     SIZE 6 BY 1.14.

DEFINE BUTTON B-Prisko 
     LABEL "Klargjør priskø.." 
     SIZE 20 BY 1.14.

DEFINE BUTTON B-StartStop  NO-FOCUS
     LABEL "Start/Stopp" 
     SIZE 14.6 BY 1.14.

DEFINE BUTTON B-Status 
     LABEL "Kassestatus..." 
     SIZE 20 BY 1.14.

DEFINE BUTTON BUTTON-Kontroller 
     LABEL "Kontroller" 
     SIZE 14.6 BY 1.14.

DEFINE VARIABLE cFtpbutikerParam AS CHARACTER FORMAT "X(556)":U 
     LABEL "Ftpbutiker" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CLanbutikerParam AS CHARACTER FORMAT "X(556)":U 
     LABEL "Lanbutiker" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-Aktiv AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 33.6 BY .62
     FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Serverstatus AS CHARACTER FORMAT "X(256)":U INITIAL "Det finnes data å overføre" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "icon/redlight.jpg":U
     SIZE 32 BY 3.43.

DEFINE BUTTON B-FinErpAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-MedlemWebAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-OrdErpAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-OrdErpAlle-2 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-PkSdldErpAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendButikkOrd  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendButikkPkSdl  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendErpFin  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendErpOrd  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendErpVpi  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendMedlemWeb  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendSupOrdTilMail  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendWebBut  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-VpiErpAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-WebButAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON BUTTON-10 
     LABEL "Aktiv" 
     SIZE 8 BY .67.

DEFINE BUTTON BUTTON-11 
     LABEL "Hele register" 
     SIZE 17 BY .67.

DEFINE BUTTON BUTTON-12 
     LABEL "Antall eksporterte" 
     SIZE 17.8 BY .67.

DEFINE BUTTON BUTTON-13 
     LABEL "Kl." 
     SIZE 8.4 BY .67.

DEFINE BUTTON BUTTON-8 
     LABEL "Registertype" 
     SIZE 17.8 BY .67.

DEFINE BUTTON BUTTON-9 
     LABEL "Til eksport" 
     SIZE 17 BY .67.

DEFINE VARIABLE FI-AntButikkOrd AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntButikkPkSdl AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntErpFin AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntErpOrd AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntErpVpi AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntMedlemWeb AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntSupOrdTilMail AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntWebBut AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-ButikkOrdError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ButikkPkSdlError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ErpFinError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ErpOrdError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ErpVpiError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FinansErpDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-FinansErpTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-FinansErpTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Finans til ERP" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-MedlemTilWeb AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-MedlemTilWebTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Medlem til web" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-MedlemWebDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-MedlemWebError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-OrdreButikkDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-OrdreButikkTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-OrdreButikkTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Ordre til butikk" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-OrdreErpDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-OrdreErpTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-OrdreErpTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Ordre til ERP" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-OverfTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Til overføring" 
      VIEW-AS TEXT 
     SIZE 13 BY .62
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE FI-PkSdlButikkDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-PkSdlButikkTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-PkSdlButikkTxt AS CHARACTER FORMAT "X(256)":U INITIAL "PkSdl til butikk" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-SupOrdTilMail AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-SupOrdTilMailDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-SupOrdTilMailError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SupOrdTilMailTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Send eMail" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-TilExpInfo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rød = Poster til overføring" 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 12  NO-UNDO.

DEFINE VARIABLE FI-VareErpDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-VareErpTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-VarerErpTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Varer til ERP" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-WebButDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-WebButError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-WebButTilWeb AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-WebButTilWebTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Send til webbut" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-60
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE 101.2 BY 17.67.

DEFINE RECTANGLE RECT-61
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE .4 BY 17.67.

DEFINE RECTANGLE RECT-66
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 115 BY 19.76.

DEFINE VARIABLE TG-ButikkOrd AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-ButikkPkSdl AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-ErpFin AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-ErpOrd AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-ErpVpi AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-MedlemWeb AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SupOrdTilMail AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-WebBut AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE FI-FilTxt AS CHARACTER FORMAT "X(256)":U INITIAL " Filinfo" 
      VIEW-AS TEXT 
     SIZE 8 BY .62
     BGCOLOR 11  NO-UNDO.

DEFINE RECTANGLE RECT-62
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 114 BY 19.76.

DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 51 BY 8.48
     FONT 2 NO-UNDO.

DEFINE VARIABLE FI-ButikerFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikkfiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FargFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fargfiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-GarantiFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Garantifiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-HgrFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Hgruppefiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KassererFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kassererfiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KasValutaFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Valutafiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KKampFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kombinasjonskampanjefiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KundeFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kundefiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MixFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mixfiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SelgerFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Selgerfiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-StrKonvFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Størrelsefiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-StrTypeFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Størrelsetypefiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SysparaFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sysparafiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TeksterFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tekster filer" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VareFiler AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varefiler" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-59
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 109 BY 19.05.

DEFINE BUTTON B-ButikerAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-FargeAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-GarantiAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-KasserereAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-KasValutaAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-KKampAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-KundeAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SelgereAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendButiker  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendFarger  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendGaranti  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendKasserere  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendKasValuta  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendKKamp  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendKunde  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendSelgere  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendStrKonv  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendStrType  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendSyspara  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendTekster  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendVare  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SendVarGr  NO-FOCUS
     LABEL "Send" 
     SIZE 8 BY 1.

DEFINE BUTTON B-StrKonvAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-StrTypeAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-SysparaAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-TeksterAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-VareAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON B-VargrAlle 
     LABEL "Initier" 
     SIZE 8 BY 1.

DEFINE BUTTON BUTTON-1 
     LABEL "Registertype" 
     SIZE 17.8 BY .67.

DEFINE BUTTON BUTTON-2 
     LABEL "Til overføring" 
     SIZE 17 BY .67.

DEFINE BUTTON BUTTON-3 
     LABEL "Auto" 
     SIZE 8 BY .67.

DEFINE BUTTON BUTTON-4 
     LABEL "Hele register" 
     SIZE 17 BY .67.

DEFINE BUTTON BUTTON-6 
     LABEL "Antall overførte" 
     SIZE 17.8 BY .67.

DEFINE BUTTON BUTTON-7 
     LABEL "Kl." 
     SIZE 8.4 BY .67.

DEFINE VARIABLE FI-AntButiker AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntFarger AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntGaranti AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntKasserer AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntKasValuta AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntKKamp AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntKunder AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntSelger AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntStrKonv AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntStrType AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntSyspara AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntTekster AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntVarer AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-AntVarGr AS INTEGER FORMAT "-zzz,zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-ButikerDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-ButikerError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ButikerTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-ButikerTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Butikker" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-FargerDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-FargerError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FargerTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-FargerTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Farger" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-GarantiDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-GarantiError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-GarantiTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-GarantiTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Garanti" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-KassererDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-KassererError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KasserereTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Kasserere" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-KassererTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-KasValutaDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-KasValutaError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KasValutaTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-KasValutaTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Valuta" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-KKampDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-KKampError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KKampTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-KundeDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-KundeError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KunderTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Kunder/kort" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-KundeTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-PakkeMixTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Komb.kamp." 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-SelgerDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-SelgerError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SelgereTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Selgere" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-SelgerTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-StrKonvDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-StrKonvError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-StrKonvTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-StrKonvTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Størrelser" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-StrTypeDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-StrTypeError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-StrTypeTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-StrTypeTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Størrelsetyper" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-SysparaDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-SysparaError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SysparaTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-SysparaTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Syspara" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-TeksterDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-TeksterError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TeksterTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-TeksterTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Tekster" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-VareDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-VareError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VaregrupperTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Varegrupper" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-VarerTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Varer" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-VareTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FI-VarGrDatoTid AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 7.2 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI-VarGrError AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VarGrTilExp AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 4 BY .95
     BGCOLOR 14  NO-UNDO.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 109 BY 19.76.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE 101.2 BY 17.67.

DEFINE RECTANGLE RECT-58
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE .4 BY 17.67.

DEFINE VARIABLE TG-ButikerAuto AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-FargerAuto AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-GarantiAuto AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-KasserereAuto AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-KasValutaAuto AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-KKampAuto AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-KundeAuto AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SelgereAuto AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SendPRSPOS AS LOGICAL INITIAL yes 
     LABEL "Send til PRS POS" 
     VIEW-AS TOGGLE-BOX
     SIZE 26.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Skjul AS LOGICAL INITIAL yes 
     LABEL "Skjul send dialog" 
     VIEW-AS TOGGLE-BOX
     SIZE 26.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-StrKonvAuto AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-StrTypeAuto AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-SysparaAuto AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-TeksterAuto AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-VareAuto AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-VarGrAuto AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3.6 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BR-File FOR 
      DistributeFile SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BR-File
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-File wWin _STRUCTURED
  QUERY BR-File NO-LOCK DISPLAY
      DistributeFile.ButikkNr COLUMN-LABEL "Butikk" FORMAT ">>>>9":U
            WIDTH 7
      DistributeFile.Date FORMAT "99/99/9999 HH:MM:SS.SSS":U
      getButliste() COLUMN-LABEL "Kasser" FORMAT "x(10)":U WIDTH 9.4
      DistributeFile.FileName FORMAT "x(50)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 111.8 BY 19.19 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     B-Ftpbut AT ROW 2.81 COL 81 NO-TAB-STOP 
     B-Oppdater AT ROW 1.48 COL 88
     B-Status AT ROW 1.48 COL 96
     CLanbutikerParam AT ROW 1.57 COL 64 COLON-ALIGNED
     B-Prisko AT ROW 2.67 COL 96
     cFtpbutikerParam AT ROW 2.81 COL 64 COLON-ALIGNED
     BUTTON-Kontroller AT ROW 2.86 COL 35.6
     B-Klargjor AT ROW 4 COL 65.6
     FI-Serverstatus AT ROW 4.43 COL 1 NO-LABEL
     B-Lanbut AT ROW 1.57 COL 81 NO-TAB-STOP 
     B-StartStop AT ROW 1.48 COL 35.6
     FI-Aktiv AT ROW 4.29 COL 80 COLON-ALIGNED NO-LABEL
     IMAGE-1 AT ROW 1.05 COL 1
     SPACE(84.00) SKIP(22.95)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         DEFAULT-BUTTON B-StartStop.

DEFINE FRAME FRAME-Filinfo
     FI-VareFiler AT ROW 2.33 COL 70.4 COLON-ALIGNED
     EDITOR-1 AT ROW 2.52 COL 3.4 NO-LABEL
     FI-MixFiler AT ROW 3.43 COL 70.4 COLON-ALIGNED
     FI-HgrFiler AT ROW 4.57 COL 70.4 COLON-ALIGNED
     FI-KasValutaFiler AT ROW 5.67 COL 70.4 COLON-ALIGNED
     FI-KassererFiler AT ROW 6.86 COL 70.4 COLON-ALIGNED
     FI-SelgerFiler AT ROW 8 COL 70.4 COLON-ALIGNED
     FI-KundeFiler AT ROW 9.14 COL 70.4 COLON-ALIGNED
     FI-FargFiler AT ROW 10.33 COL 70.4 COLON-ALIGNED
     FI-StrKonvFiler AT ROW 11.43 COL 70.4 COLON-ALIGNED
     FI-StrTypeFiler AT ROW 12.57 COL 70.4 COLON-ALIGNED
     FI-TeksterFiler AT ROW 13.67 COL 70.4 COLON-ALIGNED
     FI-GarantiFiler AT ROW 14.81 COL 70.4 COLON-ALIGNED
     FI-ButikerFiler AT ROW 15.95 COL 70.4 COLON-ALIGNED
     FI-SysparaFiler AT ROW 17.05 COL 70.4 COLON-ALIGNED
     FI-KKampFiler AT ROW 18.19 COL 70.4 COLON-ALIGNED
     FI-FilTxt AT ROW 1 COL 37 COLON-ALIGNED NO-LABEL FORMAT "X(256)":U
           VIEW-AS TEXT 
          SIZE 8 BY .62
          BGCOLOR 11 
     RECT-59 AT ROW 1.24 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.8 ROW 7.14
         SIZE 115.2 BY 20.29.

DEFINE FRAME FRAME-Eksterne
     BUTTON-8 AT ROW 2.05 COL 4.4 NO-TAB-STOP 
     BUTTON-9 AT ROW 2.05 COL 23 NO-TAB-STOP 
     BUTTON-10 AT ROW 2.05 COL 40.2
     BUTTON-11 AT ROW 2.05 COL 48.2
     BUTTON-12 AT ROW 2.05 COL 65.2 NO-TAB-STOP 
     BUTTON-13 AT ROW 2.05 COL 83 NO-TAB-STOP 
     B-VpiErpAlle AT ROW 3.19 COL 51.6
     B-SendSupOrdTilMail AT ROW 14.33 COL 29 NO-TAB-STOP 
     FI-AntErpVpi AT ROW 3.19 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-VareErpDatoTid AT ROW 3.19 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-ErpVpiError AT ROW 3.19 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-ErpVpi AT ROW 3.33 COL 42.4
     B-OrdErpAlle AT ROW 4.43 COL 51.6
     FI-AntErpOrd AT ROW 4.43 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-OrdreErpDatoTid AT ROW 4.43 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-ErpOrdError AT ROW 4.48 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-ErpOrd AT ROW 4.57 COL 42.4
     B-FinErpAlle AT ROW 5.67 COL 51.6
     FI-AntErpFin AT ROW 5.67 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-FinansErpDatoTid AT ROW 5.67 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-ErpFinError AT ROW 5.67 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-ErpFin AT ROW 5.81 COL 42.4
     B-OrdErpAlle-2 AT ROW 8.67 COL 51.6
     FI-AntButikkOrd AT ROW 8.67 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-OrdreButikkDatoTid AT ROW 8.67 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-ButikkOrdError AT ROW 8.67 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-ButikkOrd AT ROW 8.81 COL 42.4
     FI-AntButikkPkSdl AT ROW 9.81 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-PkSdlButikkDatoTid AT ROW 9.86 COL 81.2 COLON-ALIGNED NO-LABEL
     B-PkSdldErpAlle AT ROW 9.91 COL 51.6
     FI-ButikkPkSdlError AT ROW 9.91 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-ButikkPkSdl AT ROW 9.95 COL 42.4
     TG-MedlemWeb AT ROW 12.05 COL 42.4
     B-MedlemWebAlle AT ROW 12.05 COL 52
     FI-AntMedlemWeb AT ROW 12.05 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-MedlemWebDatoTid AT ROW 12.05 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-MedlemWebError AT ROW 12.05 COL 91.2 COLON-ALIGNED NO-LABEL
     B-WebButAlle AT ROW 13.24 COL 52
     FI-AntWebBut AT ROW 13.24 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-WebButDatoTid AT ROW 13.24 COL 81.2 COLON-ALIGNED NO-LABEL
     TG-WebBut AT ROW 13.33 COL 42.4
     FI-WebButError AT ROW 13.33 COL 91.2 COLON-ALIGNED NO-LABEL
     FI-AntSupOrdTilMail AT ROW 14.38 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-SupOrdTilMailDatoTid AT ROW 14.38 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-SupOrdTilMailError AT ROW 14.43 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-SupOrdTilMail AT ROW 14.48 COL 42.4
     B-SendButikkOrd AT ROW 8.67 COL 29 NO-TAB-STOP 
     B-SendWebBut AT ROW 13.19 COL 29 NO-TAB-STOP 
     B-SendButikkPkSdl AT ROW 9.91 COL 29 NO-TAB-STOP 
     B-SendMedlemWeb AT ROW 12.05 COL 29 NO-TAB-STOP 
     B-SendErpOrd AT ROW 4.43 COL 29 NO-TAB-STOP 
     B-SendErpFin AT ROW 5.67 COL 29 NO-TAB-STOP 
     B-SendErpVpi AT ROW 3.19 COL 29 NO-TAB-STOP 
     FI-OverfTxt AT ROW 1 COL 37 COLON-ALIGNED NO-LABEL
     FI-VareErpTilExp AT ROW 3.19 COL 24 NO-LABEL
     FI-VarerErpTxt AT ROW 3.33 COL 3 COLON-ALIGNED NO-LABEL
     FI-OrdreErpTilExp AT ROW 4.43 COL 24 NO-LABEL
     FI-OrdreErpTxt AT ROW 4.57 COL 3 COLON-ALIGNED NO-LABEL
     FI-FinansErpTilExp AT ROW 5.67 COL 24 NO-LABEL
     FI-FinansErpTxt AT ROW 5.81 COL 3 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.8 ROW 7.14
         SIZE 115.2 BY 20.29.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-Eksterne
     FI-OrdreButikkTilExp AT ROW 8.67 COL 24 NO-LABEL
     FI-OrdreButikkTxt AT ROW 8.81 COL 3 COLON-ALIGNED NO-LABEL
     FI-PkSdlButikkTilExp AT ROW 9.86 COL 24 NO-LABEL
     FI-PkSdlButikkTxt AT ROW 10.05 COL 3 COLON-ALIGNED NO-LABEL
     FI-MedlemTilWeb AT ROW 12.05 COL 24 NO-LABEL
     FI-MedlemTilWebTxt AT ROW 12.33 COL 3 COLON-ALIGNED NO-LABEL
     FI-WebButTilWeb AT ROW 13.24 COL 24 NO-LABEL
     FI-WebButTilWebTxt AT ROW 13.48 COL 3 COLON-ALIGNED NO-LABEL
     FI-SupOrdTilMail AT ROW 14.38 COL 24 NO-LABEL
     FI-SupOrdTilMailTxt AT ROW 14.57 COL 3 COLON-ALIGNED NO-LABEL
     FI-TilExpInfo AT ROW 19.76 COL 4.2
     RECT-66 AT ROW 1.24 COL 1
     RECT-60 AT ROW 1.91 COL 3.8
     RECT-61 AT ROW 1.86 COL 22.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.8 ROW 7.14
         SIZE 115.2 BY 20.29.

DEFINE FRAME FRAME-Fildistr
     BR-File AT ROW 1.52 COL 2.2
     FI-FilTxt AT ROW 1 COL 37 COLON-ALIGNED NO-LABEL
     RECT-62 AT ROW 1.24 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.8 ROW 7.14
         SIZE 115.2 BY 20.29.

DEFINE FRAME FRAME-Para
     B-SendTekster AT ROW 14.52 COL 29 NO-TAB-STOP 
     BUTTON-1 AT ROW 2.05 COL 4.4 NO-TAB-STOP 
     BUTTON-2 AT ROW 2.05 COL 23 NO-TAB-STOP 
     BUTTON-3 AT ROW 2.05 COL 40.2
     BUTTON-4 AT ROW 2.05 COL 48.2
     BUTTON-6 AT ROW 2.05 COL 65.2 NO-TAB-STOP 
     BUTTON-7 AT ROW 2.05 COL 83 NO-TAB-STOP 
     B-VareAlle AT ROW 3 COL 51.6
     FI-AntVarer AT ROW 3 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-VareDatoTid AT ROW 3 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-VareError AT ROW 3 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-VareAuto AT ROW 3.14 COL 42.4
     B-KKampAlle AT ROW 4.14 COL 51.6
     FI-AntKKamp AT ROW 4.14 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-KKampDatoTid AT ROW 4.14 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-KKampError AT ROW 4.14 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-KKampAuto AT ROW 4.33 COL 42.6
     B-VargrAlle AT ROW 5.29 COL 51.6
     FI-AntVarGr AT ROW 5.29 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-VarGrDatoTid AT ROW 5.29 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-VarGrError AT ROW 5.29 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-VarGrAuto AT ROW 5.43 COL 42.4
     B-KasValutaAlle AT ROW 6.43 COL 51.6
     FI-AntKasValuta AT ROW 6.43 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-KasValutaDatoTid AT ROW 6.43 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-KasValutaError AT ROW 6.43 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-KasValutaAuto AT ROW 6.57 COL 42.4
     B-SendSyspara AT ROW 17.95 COL 29 NO-TAB-STOP 
     B-KasserereAlle AT ROW 7.57 COL 51.6
     FI-AntKasserer AT ROW 7.57 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-KassererDatoTid AT ROW 7.57 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-KassererError AT ROW 7.57 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-KasserereAuto AT ROW 7.76 COL 42.4
     B-SelgereAlle AT ROW 8.71 COL 51.6
     FI-AntSelger AT ROW 8.71 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-SelgerDatoTid AT ROW 8.71 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-SelgerError AT ROW 8.71 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-SelgereAuto AT ROW 8.86 COL 42.4
     B-KundeAlle AT ROW 9.86 COL 51.6
     FI-AntKunder AT ROW 9.86 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-KundeDatoTid AT ROW 9.86 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-KundeError AT ROW 9.91 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-KundeAuto AT ROW 10 COL 42.4
     B-FargeAlle AT ROW 11 COL 51.6
     FI-AntFarger AT ROW 11 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-FargerDatoTid AT ROW 11 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-FargerError AT ROW 11 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-FargerAuto AT ROW 11.14 COL 42.4
     B-StrKonvAlle AT ROW 12.19 COL 51.6
     FI-AntStrKonv AT ROW 12.19 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-StrKonvDatoTid AT ROW 12.19 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-StrKonvError AT ROW 12.19 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-StrKonvAuto AT ROW 12.29 COL 42.4
     B-StrTypeAlle AT ROW 13.29 COL 51.6
     FI-AntStrType AT ROW 13.29 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-StrTypeDatoTid AT ROW 13.29 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-StrTypeError AT ROW 13.29 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-StrTypeAuto AT ROW 13.43 COL 42.4
     FI-TeksterError AT ROW 14.48 COL 91.2 COLON-ALIGNED NO-LABEL
     B-TeksterAlle AT ROW 14.52 COL 51.6
     FI-AntTekster AT ROW 14.52 COL 64.8 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.8 ROW 7.14
         SIZE 115.2 BY 20.29.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-Para
     FI-TeksterDatoTid AT ROW 14.52 COL 81.2 COLON-ALIGNED NO-LABEL
     TG-TeksterAuto AT ROW 14.62 COL 42.4
     B-GarantiAlle AT ROW 15.62 COL 51.6
     FI-AntGaranti AT ROW 15.62 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-GarantiDatoTid AT ROW 15.62 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-GarantiError AT ROW 15.62 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-GarantiAuto AT ROW 15.76 COL 42.4
     B-ButikerAlle AT ROW 16.81 COL 51.6
     FI-AntButiker AT ROW 16.81 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-ButikerDatoTid AT ROW 16.81 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-ButikerError AT ROW 16.81 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-ButikerAuto AT ROW 17 COL 42.4
     B-SysparaAlle AT ROW 17.95 COL 51.6
     FI-AntSyspara AT ROW 17.95 COL 64.8 COLON-ALIGNED NO-LABEL
     FI-SysparaDatoTid AT ROW 17.95 COL 81.2 COLON-ALIGNED NO-LABEL
     FI-SysparaError AT ROW 17.95 COL 91.2 COLON-ALIGNED NO-LABEL
     TG-SysparaAuto AT ROW 18.14 COL 42.4
     TG-Skjul AT ROW 19.76 COL 42.4
     TG-SendPRSPOS AT ROW 19.81 COL 70.8
     B-SendKKamp AT ROW 4.19 COL 29 NO-TAB-STOP 
     B-SendButiker AT ROW 16.81 COL 29 NO-TAB-STOP 
     B-SendStrType AT ROW 13.29 COL 29 NO-TAB-STOP 
     B-SendFarger AT ROW 11 COL 29 NO-TAB-STOP 
     B-SendStrKonv AT ROW 12.19 COL 29 NO-TAB-STOP 
     B-SendSelgere AT ROW 8.71 COL 29 NO-TAB-STOP 
     B-SendKasValuta AT ROW 6.43 COL 29 NO-TAB-STOP 
     B-SendKunde AT ROW 9.86 COL 29 NO-TAB-STOP 
     B-SendKasserere AT ROW 7.57 COL 29 NO-TAB-STOP 
     B-SendVarGr AT ROW 5.29 COL 29 NO-TAB-STOP 
     B-SendGaranti AT ROW 15.62 COL 29 NO-TAB-STOP 
     B-SendVare AT ROW 3 COL 29 NO-TAB-STOP 
     FI-OverfTxt AT ROW 1 COL 37 COLON-ALIGNED NO-LABEL FORMAT "X(256)":U
           VIEW-AS TEXT 
          SIZE 13 BY .62
          BGCOLOR 11 
     FI-VareTilExp AT ROW 3 COL 24 NO-LABEL
     FI-VarerTxt AT ROW 3.14 COL 3 COLON-ALIGNED NO-LABEL
     FI-KKampTilExp AT ROW 4.14 COL 24 NO-LABEL
     FI-PakkeMixTxt AT ROW 4.19 COL 3 COLON-ALIGNED NO-LABEL
     FI-VarGrTilExp AT ROW 5.29 COL 24 NO-LABEL
     FI-VaregrupperTxt AT ROW 5.43 COL 3 COLON-ALIGNED NO-LABEL
     FI-KasValutaTilExp AT ROW 6.43 COL 24 NO-LABEL
     FI-KasValutaTxt AT ROW 6.57 COL 3 COLON-ALIGNED NO-LABEL
     FI-KassererTilExp AT ROW 7.57 COL 24 NO-LABEL
     FI-KasserereTxt AT ROW 7.76 COL 3 COLON-ALIGNED NO-LABEL
     FI-SelgerTilExp AT ROW 8.71 COL 24 NO-LABEL
     FI-SelgereTxt AT ROW 8.86 COL 3 COLON-ALIGNED NO-LABEL
     FI-KundeTilExp AT ROW 9.86 COL 24 NO-LABEL
     FI-KunderTxt AT ROW 10 COL 3 COLON-ALIGNED NO-LABEL
     FI-FargerTilExp AT ROW 11 COL 24 NO-LABEL
     FI-FargerTxt AT ROW 11.14 COL 3 COLON-ALIGNED NO-LABEL
     FI-StrKonvTilExp AT ROW 12.19 COL 24 NO-LABEL
     FI-StrKonvTxt AT ROW 12.29 COL 3 COLON-ALIGNED NO-LABEL
     FI-StrTypeTilExp AT ROW 13.29 COL 24 NO-LABEL
     FI-StrTypeTxt AT ROW 13.43 COL 3 COLON-ALIGNED NO-LABEL
     FI-TeksterTilExp AT ROW 14.52 COL 24 NO-LABEL
     FI-TeksterTxt AT ROW 14.62 COL 3 COLON-ALIGNED NO-LABEL
     FI-GarantiTilExp AT ROW 15.62 COL 24 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.8 ROW 7.14
         SIZE 115.2 BY 20.29.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-Para
     FI-GarantiTxt AT ROW 15.76 COL 3 COLON-ALIGNED NO-LABEL
     FI-ButikerTilExp AT ROW 16.81 COL 24 NO-LABEL
     FI-ButikerTxt AT ROW 17 COL 3 COLON-ALIGNED NO-LABEL
     FI-SysparaTilExp AT ROW 17.95 COL 24 NO-LABEL
     FI-SysparaTxt AT ROW 18.14 COL 3 COLON-ALIGNED NO-LABEL
     FI-TilExpInfo AT ROW 19.76 COL 4.2
          LABEL "Rød = Poster til overføring" FORMAT "X(256)":U
           VIEW-AS TEXT 
          SIZE 4 BY .95
          BGCOLOR 12 
     RECT-56 AT ROW 1.24 COL 1
     RECT-57 AT ROW 1.91 COL 3.8
     RECT-58 AT ROW 1.86 COL 22.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.8 ROW 7.14
         SIZE 115.2 BY 20.29.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Dataeksport til kasser/ERP/Web"
         HEIGHT             = 26.57
         WIDTH              = 116
         MAX-HEIGHT         = 26.57
         MAX-WIDTH          = 116
         VIRTUAL-HEIGHT     = 26.57
         VIRTUAL-WIDTH      = 116
         MAX-BUTTON         = no
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

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-Eksterne:FRAME = FRAME fMain:HANDLE
       FRAME FRAME-Fildistr:FRAME = FRAME fMain:HANDLE
       FRAME FRAME-Filinfo:FRAME = FRAME fMain:HANDLE
       FRAME FRAME-Para:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME fMain
   FRAME-NAME Size-to-Fit                                               */
ASSIGN 
       FRAME fMain:SCROLLABLE       = FALSE.

/* SETTINGS FOR BUTTON B-Ftpbut IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       B-Ftpbut:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR BUTTON B-Klargjor IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       B-Klargjor:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR BUTTON B-Lanbut IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       B-Lanbut:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       cFtpbutikerParam:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       CLanbutikerParam:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN FI-Aktiv IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Serverstatus IN FRAME fMain
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FI-Serverstatus:HIDDEN IN FRAME fMain           = TRUE.

/* SETTINGS FOR FRAME FRAME-Eksterne
                                                                        */
/* SETTINGS FOR BUTTON B-FinErpAlle IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
ASSIGN 
       B-FinErpAlle:HIDDEN IN FRAME FRAME-Eksterne           = TRUE.

/* SETTINGS FOR BUTTON B-MedlemWebAlle IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
ASSIGN 
       B-MedlemWebAlle:HIDDEN IN FRAME FRAME-Eksterne           = TRUE.

/* SETTINGS FOR BUTTON B-OrdErpAlle IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
ASSIGN 
       B-OrdErpAlle:HIDDEN IN FRAME FRAME-Eksterne           = TRUE.

/* SETTINGS FOR BUTTON B-OrdErpAlle-2 IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
ASSIGN 
       B-OrdErpAlle-2:HIDDEN IN FRAME FRAME-Eksterne           = TRUE.

/* SETTINGS FOR BUTTON B-PkSdldErpAlle IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
ASSIGN 
       B-PkSdldErpAlle:HIDDEN IN FRAME FRAME-Eksterne           = TRUE.

/* SETTINGS FOR BUTTON B-SendButikkOrd IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendButikkPkSdl IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendErpFin IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendErpOrd IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendErpVpi IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendMedlemWeb IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendSupOrdTilMail IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendWebBut IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-VpiErpAlle IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-WebButAlle IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntButikkOrd IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntButikkPkSdl IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntErpFin IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntErpOrd IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntErpVpi IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntMedlemWeb IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntSupOrdTilMail IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntWebBut IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ButikkOrdError IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ButikkPkSdlError IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ErpFinError IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ErpOrdError IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ErpVpiError IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-FinansErpDatoTid IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-FinansErpTilExp IN FRAME FRAME-Eksterne
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-MedlemTilWeb IN FRAME FRAME-Eksterne
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-MedlemWebDatoTid IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-MedlemWebError IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-OrdreButikkDatoTid IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-OrdreButikkTilExp IN FRAME FRAME-Eksterne
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-OrdreErpDatoTid IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-OrdreErpTilExp IN FRAME FRAME-Eksterne
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-PkSdlButikkDatoTid IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-PkSdlButikkTilExp IN FRAME FRAME-Eksterne
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-SupOrdTilMail IN FRAME FRAME-Eksterne
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-SupOrdTilMailDatoTid IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-SupOrdTilMailError IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-TilExpInfo IN FRAME FRAME-Eksterne
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-VareErpDatoTid IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VareErpTilExp IN FRAME FRAME-Eksterne
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-WebButDatoTid IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-WebButError IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-WebButTilWeb IN FRAME FRAME-Eksterne
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR TOGGLE-BOX TG-ButikkOrd IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-ButikkPkSdl IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-ErpFin IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-ErpOrd IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-ErpVpi IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-MedlemWeb IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-SupOrdTilMail IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-WebBut IN FRAME FRAME-Eksterne
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-Fildistr
                                                                        */
/* BROWSE-TAB BR-File RECT-62 FRAME-Fildistr */
/* SETTINGS FOR FRAME FRAME-Filinfo
                                                                        */
/* SETTINGS FOR FILL-IN FI-ButikerFiler IN FRAME FRAME-Filinfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-GarantiFiler IN FRAME FRAME-Filinfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-SysparaFiler IN FRAME FRAME-Filinfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-TeksterFiler IN FRAME FRAME-Filinfo
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-Para
                                                                        */
/* SETTINGS FOR BUTTON B-SendButiker IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendFarger IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendGaranti IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendKasserere IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendKasValuta IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendKKamp IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendKunde IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendSelgere IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendStrKonv IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendStrType IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendSyspara IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendTekster IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendVare IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SendVarGr IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-SysparaAlle IN FRAME FRAME-Para
   NO-ENABLE                                                            */
ASSIGN 
       B-SysparaAlle:HIDDEN IN FRAME FRAME-Para           = TRUE.

/* SETTINGS FOR FILL-IN FI-AntButiker IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntFarger IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntGaranti IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntKasserer IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntKasValuta IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntKKamp IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntKunder IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntSelger IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntStrKonv IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntStrType IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntSyspara IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntTekster IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntVarer IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntVarGr IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ButikerDatoTid IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ButikerError IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ButikerTilExp IN FRAME FRAME-Para
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-FargerDatoTid IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-FargerError IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-FargerTilExp IN FRAME FRAME-Para
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-GarantiDatoTid IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-GarantiError IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-GarantiTilExp IN FRAME FRAME-Para
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-KassererDatoTid IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KassererError IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KassererTilExp IN FRAME FRAME-Para
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-KasValutaDatoTid IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KasValutaError IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KasValutaTilExp IN FRAME FRAME-Para
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-KKampDatoTid IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KKampError IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KKampTilExp IN FRAME FRAME-Para
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-KundeDatoTid IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KundeError IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KundeTilExp IN FRAME FRAME-Para
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-SelgerDatoTid IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-SelgerError IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-SelgerTilExp IN FRAME FRAME-Para
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-StrKonvDatoTid IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-StrKonvError IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-StrKonvTilExp IN FRAME FRAME-Para
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-StrTypeDatoTid IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-StrTypeError IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-StrTypeTilExp IN FRAME FRAME-Para
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-SysparaDatoTid IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-SysparaError IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-SysparaTilExp IN FRAME FRAME-Para
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-TeksterDatoTid IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-TeksterError IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-TeksterTilExp IN FRAME FRAME-Para
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-TilExpInfo IN FRAME FRAME-Para
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-VareDatoTid IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VareError IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VareTilExp IN FRAME FRAME-Para
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-VarGrDatoTid IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VarGrError IN FRAME FRAME-Para
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VarGrTilExp IN FRAME FRAME-Para
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-File
/* Query rebuild information for BROWSE BR-File
     _TblList          = "SkoTex.DistributeFile"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "SkoTex.DistributeFile.ButikkNr|yes,SkoTex.DistributeFile.Date|yes"
     _FldNameList[1]   > SkoTex.DistributeFile.ButikkNr
"DistributeFile.ButikkNr" "Butikk" ? "integer" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = SkoTex.DistributeFile.Date
     _FldNameList[3]   > "_<CALC>"
"getButliste()" "Kasser" "x(10)" ? ? ? ? ? ? ? no ? no no "9.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > SkoTex.DistributeFile.FileName
"DistributeFile.FileName" ? "x(50)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BR-File */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME PSTimer ASSIGN
       FRAME           = FRAME fMain:HANDLE
       ROW             = 3.86
       COLUMN          = 51
       HEIGHT          = 1.91
       WIDTH           = 8
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      PSTimer:NAME = "PSTimer":U .
/* PSTimer OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      PSTimer:MOVE-AFTER(BUTTON-Kontroller:HANDLE IN FRAME fMain).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Dataeksport til kasser/ERP/Web */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Dataeksport til kasser/ERP/Web */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  IF chPSTimer:ENABLED = TRUE THEN DO:
      MESSAGE "Du må stoppe serveren for å kunne stenge vinduet."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Para
&Scoped-define SELF-NAME B-ButikerAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ButikerAlle wWin
ON CHOOSE OF B-ButikerAlle IN FRAME FRAME-Para /* Initier */
DO:
  RUN SkapELoggAlle ("Butiker",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").
  IF CAN-FIND(FIRST ekstbutiker) THEN
      RUN SkapEloggAlle ("ekstbutiker",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-FargeAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-FargeAlle wWin
ON CHOOSE OF B-FargeAlle IN FRAME FRAME-Para /* Initier */
DO:
  RUN SkapELoggAlle ("Farg",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Eksterne
&Scoped-define SELF-NAME B-FinErpAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-FinErpAlle wWin
ON CHOOSE OF B-FinErpAlle IN FRAME FRAME-Eksterne /* Initier */
DO:
/*     IF NOT lKlarGjor THEN                                                        */
/*         MESSAGE "Alle varer til kasse? Bekreft"                                  */
/*             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lOk AS LOGICAL.     */
/*     IF lOk OR lKlargjor THEN DO:                                                 */
/*         RUN SkapELoggAlle ("ArtBas",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").   */
/*         RUN SkapELoggAlle ("MixMatch",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE"). */
/*     END.                                                                         */
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME B-Ftpbut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Ftpbut wWin
ON CHOOSE OF B-Ftpbut IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iReturnValue AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cValgteFTP   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cEntryBut   AS CHARACTER   NO-UNDO.
    /* TN 31/5-10 Dette skal ikke gjøres. Hvis dette gjøres må man manuelt klikke bort alle butikkene .....
    DO iCount = 1 TO NUM-ENTRIES(cFtpButiker):
        cValgteFTP = cValgteFTP + (IF cValgteFTP <> "" THEN "," ELSE "") + STRING(iCount).
    END.
    */
    RUN JBoxDynFieldChoose.w (THIS-PROCEDURE:CURRENT-WINDOW,?,cOrgFtpButiker,"ROW",INPUT-OUTPUT cValgteFTP,OUTPUT iReturnValue).
    IF iReturnValue = 1 THEN DO:
        IF NUM-ENTRIES(cOrgFtpButiker) = 1 THEN DO:
            ASSIGN iReturnValue = 2
                   cValgteFTP   = "1".
        END.
        ELSE DO:
            MESSAGE "Välj endast 1 butik"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
    END.
    IF iReturnValue = 2 THEN DO:
        IF NUM-ENTRIES(cValgteFTP) <> 1 THEN DO:
            MESSAGE "Välj endast 1 butik"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        ELSE DO:
            cEntryBut = ENTRY(INT(cValgteFTP),cOrgFtpButiker) NO-ERROR.
            IF NOT ERROR-STATUS:ERROR THEN DO:
                IF CAN-DO(cFtpButiker,cEntryBut) THEN
                    ASSIGN cFtpButiker = cEntryBut.
                ELSE
                    ASSIGN cFtpButiker = ''.
                IF CAN-DO(cPRSFtpButiker,cEntryBut) THEN
                    ASSIGN cPRSFtpButiker = cEntryBut.
                ELSE
                    ASSIGN cPRSFtpButiker = ''.
            END.
            B-Ftpbut:SENSITIVE = FALSE.
            B-Lanbut:SENSITIVE = FALSE.
            ASSIGN cFtpButikerParam:SCREEN-VALUE = cFtpButiker + cPRSFtpButiker
                   CLanbutikerParam:SCREEN-VALUE = "".
            B-Klargjor:SENSITIVE = TRUE.
            B-Lanbut:SENSITIVE = FALSE.
            cLanButiker = "".
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Para
&Scoped-define SELF-NAME B-GarantiAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-GarantiAlle wWin
ON CHOOSE OF B-GarantiAlle IN FRAME FRAME-Para /* Initier */
DO:
  RUN SkapELoggAlle ("Garanti",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KasserereAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KasserereAlle wWin
ON CHOOSE OF B-KasserereAlle IN FRAME FRAME-Para /* Initier */
DO:
  RUN SkapELoggAlle ("ButikkForsalj",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KasValutaAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KasValutaAlle wWin
ON CHOOSE OF B-KasValutaAlle IN FRAME FRAME-Para /* Initier */
DO:
  RUN SkapELoggAlle ("KasValuta",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KKampAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KKampAlle wWin
ON CHOOSE OF B-KKampAlle IN FRAME FRAME-Para /* Initier */
DO:
    IF NOT lKlarGjor THEN
        MESSAGE "Alle kampanjer til kasse? Bekreft"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lOk AS LOGICAL.
    IF lOk OR lKlargjor THEN DO:
        RUN SkapELoggAlle ("KampanjeMixMatch",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME B-Klargjor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Klargjor wWin
ON CHOOSE OF B-Klargjor IN FRAME fMain /* Klargjør */
DO:
    IF cLanButiker = "" AND cFtpButiker = "" AND cPRSFtpButiker = "" THEN DO:
        MESSAGE
               "Ingen butikker er valgt"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    DO WITH FRAME FRAME-Para:
        IF TG-VareAuto:CHECKED THEN APPLY "CHOOSE" TO B-VareAlle.
        IF TG-KKampAuto:CHECKED THEN APPLY "CHOOSE" TO B-KKampAlle.
        IF TG-VarGrAuto:CHECKED THEN APPLY "CHOOSE" TO B-VargrAlle.
        IF TG-KasValutaAuto:CHECKED THEN APPLY "CHOOSE" TO B-KasValutaAlle.
        IF TG-KasserereAuto:CHECKED THEN APPLY "CHOOSE" TO B-KasserereAlle.
        IF TG-SelgereAuto:CHECKED THEN APPLY "CHOOSE" TO B-SelgereAlle.
        IF TG-KundeAuto:CHECKED THEN APPLY "CHOOSE" TO B-KundeAlle.
        IF TG-FargerAuto:CHECKED THEN APPLY "CHOOSE" TO B-FargeAlle.
        IF TG-StrKonvAuto:CHECKED THEN APPLY "CHOOSE" TO B-StrKonvAlle.
        IF TG-StrTypeAuto:CHECKED THEN APPLY "CHOOSE" TO B-StrTypeAlle.
        IF TG-TeksterAuto:CHECKED THEN APPLY "CHOOSE" TO B-TeksterAlle.
        IF TG-GarantiAuto:CHECKED THEN APPLY "CHOOSE" TO B-GarantiAlle.
        IF TG-ButikerAuto:CHECKED THEN APPLY "CHOOSE" TO B-ButikerAlle.
    END.
    ASSIGN lVare      = TG-VareAuto:CHECKED
           lVarGr     = TG-KKampAuto:CHECKED
           lKKamp     = TG-VarGrAuto:CHECKED
           lKasValuta = TG-KasValutaAuto:CHECKED
           lKasserere = TG-KasserereAuto:CHECKED
           lSelgere   = TG-SelgereAuto:CHECKED
           lKunde     = TG-KundeAuto:CHECKED
           lFarger    = TG-FargerAuto:CHECKED
           lStrKonv   = TG-StrKonvAuto:CHECKED  /* Både konv och strtype ksall ut samtidigt */
           lStrType   = TG-StrTypeAuto:CHECKED
           lTekster   = TG-TeksterAuto:CHECKED
           lGaranti   = TG-GarantiAuto:CHECKED
           lButiker   = TG-ButikerAuto:CHECKED
           lSyspara   = TRUE.
/*     ASSIGN lVare      = TRUE                                                 */
/*            lVarGr     = TRUE                                                 */
/*            lKKamp     = TRUE                                                 */
/*            lKasValuta = TRUE                                                 */
/*            lKasserere = TRUE                                                 */
/*            lSelgere   = TRUE                                                 */
/*            lKunde     = TRUE                                                 */
/*            lFarger    = TRUE                                                 */
/*            lStrKonv   = TRUE  /* Både konv och strtype ksall ut samtidigt */ */
/*            lStrType   = TRUE                                                 */
/*            lTekster   = TRUE                                                 */
/*            lGaranti   = TRUE                                                 */
/*            lButiker   = TRUE                                                 */
/*            lSyspara   = TRUE.                                                */
    RUN KontrollerElogg.
    RUN StartEksport.       
    FOR EACH ELogg WHERE ELogg.Verdier = "KLARGJOR":
        DELETE ELogg.
    END.
    ASSIGN cFtpButikerParam:SCREEN-VALUE = ''.
    RUN KlargjorModus (FALSE).
/*     RUN InitierButiker. görs i klargjor(false)*/
    RUN KontrollerElogg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Para
&Scoped-define SELF-NAME B-KundeAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KundeAlle wWin
ON CHOOSE OF B-KundeAlle IN FRAME FRAME-Para /* Initier */
DO:
  RUN SkapELoggAlle ("Kunde",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME B-Lanbut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Lanbut wWin
ON CHOOSE OF B-Lanbut IN FRAME fMain /* ... */
DO:
    DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iReturnValue AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cValgteLan   AS CHARACTER  NO-UNDO.
    /* TN 31/5-10 Dette skal ikke gjøres. Hvis dette gjøres må man manuelt klikke bort alle butikkene .....
    DO iCount = 1 TO NUM-ENTRIES(cLanButiker):
        cValgteLan = cValgteLan + (IF cValgteLan <> "" THEN "," ELSE "") + STRING(iCount).
    END.
    */
    RUN JBoxDynFieldChoose.w (THIS-PROCEDURE:CURRENT-WINDOW,?,cOrgLanButiker,"ROW",INPUT-OUTPUT cValgteLan,OUTPUT iReturnValue).
    IF iReturnValue = 1 THEN DO:
        IF NUM-ENTRIES(cOrgLanButiker) = 1 THEN DO:
            ASSIGN iReturnValue = 2
                   cValgteLan   = "1".
        END.
        ELSE DO:
            MESSAGE "Välj endast 1 butik"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
    END.
    IF iReturnValue = 2 THEN DO:
        IF NUM-ENTRIES(cValgteLan) <> 1 THEN DO:
            MESSAGE "Välj endast 1 butik"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        ELSE DO:
            cLanButiker    = cValgteLan.
            cFtpButiker    = "".
            cPRSFtpButiker = "".
            B-Ftpbut:SENSITIVE = FALSE.
            B-Lanbut:SENSITIVE = FALSE.
            ASSIGN cFtpButikerParam:SCREEN-VALUE = "".
                   CLanbutikerParam:SCREEN-VALUE = clanButiker.
            B-Klargjor:SENSITIVE = TRUE.
            B-Lanbut:SENSITIVE = FALSE.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Eksterne
&Scoped-define SELF-NAME B-MedlemWebAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-MedlemWebAlle wWin
ON CHOOSE OF B-MedlemWebAlle IN FRAME FRAME-Eksterne /* Initier */
DO:
/*     IF lKlarGjor THEN                                                        */
/*         RETURN NO-APPLY.                                                     */
/*     MESSAGE "Alle varer til ERP? Bekreft"                                    */
/*             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lOk AS LOGICAL. */
/*     IF lOk THEN                                                              */
/*         RUN SkapELoggAlleErp ("ArtBas","ALLE").                              */
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME B-Oppdater
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Oppdater wWin
ON CHOOSE OF B-Oppdater IN FRAME fMain /* Oppdater */
DO:
    CLOSE QUERY BR-File.
    {&OPEN-QUERY-BR-File}
    {&OPEN-QUERY-BR-Receiver}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Eksterne
&Scoped-define SELF-NAME B-OrdErpAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-OrdErpAlle wWin
ON CHOOSE OF B-OrdErpAlle IN FRAME FRAME-Eksterne /* Initier */
DO:
/*     IF NOT lKlarGjor THEN                                                        */
/*         MESSAGE "Alle varer til kasse? Bekreft"                                  */
/*             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lOk AS LOGICAL.     */
/*     IF lOk OR lKlargjor THEN DO:                                                 */
/*         RUN SkapELoggAlle ("ArtBas",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").   */
/*         RUN SkapELoggAlle ("MixMatch",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE"). */
/*     END.                                                                         */
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-OrdErpAlle-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-OrdErpAlle-2 wWin
ON CHOOSE OF B-OrdErpAlle-2 IN FRAME FRAME-Eksterne /* Initier */
DO:
/*     IF NOT lKlarGjor THEN                                                        */
/*         MESSAGE "Alle varer til kasse? Bekreft"                                  */
/*             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lOk AS LOGICAL.     */
/*     IF lOk OR lKlargjor THEN DO:                                                 */
/*         RUN SkapELoggAlle ("ArtBas",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").   */
/*         RUN SkapELoggAlle ("MixMatch",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE"). */
/*     END.                                                                         */
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-PkSdldErpAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-PkSdldErpAlle wWin
ON CHOOSE OF B-PkSdldErpAlle IN FRAME FRAME-Eksterne /* Initier */
DO:
/*     IF NOT lKlarGjor THEN                                                        */
/*         MESSAGE "Alle varer til kasse? Bekreft"                                  */
/*             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lOk AS LOGICAL.     */
/*     IF lOk OR lKlargjor THEN DO:                                                 */
/*         RUN SkapELoggAlle ("ArtBas",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").   */
/*         RUN SkapELoggAlle ("MixMatch",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE"). */
/*     END.                                                                         */
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME B-Prisko
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Prisko wWin
ON CHOOSE OF B-Prisko IN FRAME fMain /* Klargjør priskø.. */
DO:
    RUN KontrollerPrisko IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Para
&Scoped-define SELF-NAME B-SelgereAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SelgereAlle wWin
ON CHOOSE OF B-SelgereAlle IN FRAME FRAME-Para /* Initier */
DO:
  RUN SkapELoggAlle ("ButikkSelger",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SendButiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendButiker wWin
ON CHOOSE OF B-SendButiker IN FRAME FRAME-Para /* Send */
DO:
  ASSIGN lButiker = TRUE.
  RUN StartEksport IN THIS-PROCEDURE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Eksterne
&Scoped-define SELF-NAME B-SendButikkOrd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendButikkOrd wWin
ON CHOOSE OF B-SendButikkOrd IN FRAME FRAME-Eksterne /* Send */
DO:
    RUN StartEksportButikk ("ORDHK").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SendButikkPkSdl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendButikkPkSdl wWin
ON CHOOSE OF B-SendButikkPkSdl IN FRAME FRAME-Eksterne /* Send */
DO:
    RUN StartEksportButikk ("PKSDL").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SendErpFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendErpFin wWin
ON CHOOSE OF B-SendErpFin IN FRAME FRAME-Eksterne /* Send */
DO:
    RUN StartEksportErp ("FIN").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SendErpOrd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendErpOrd wWin
ON CHOOSE OF B-SendErpOrd IN FRAME FRAME-Eksterne /* Send */
DO:
    RUN StartEksportErp ("ORD").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SendErpVpi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendErpVpi wWin
ON CHOOSE OF B-SendErpVpi IN FRAME FRAME-Eksterne /* Send */
DO:
  RUN StartEksportErp ("VPI").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Para
&Scoped-define SELF-NAME B-SendFarger
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendFarger wWin
ON CHOOSE OF B-SendFarger IN FRAME FRAME-Para /* Send */
DO:
  ASSIGN lFarger = TRUE.
  RUN StartEksport IN THIS-PROCEDURE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SendGaranti
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendGaranti wWin
ON CHOOSE OF B-SendGaranti IN FRAME FRAME-Para /* Send */
DO:
  ASSIGN lGaranti = TRUE.
  RUN StartEksport IN THIS-PROCEDURE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SendKasserere
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendKasserere wWin
ON CHOOSE OF B-SendKasserere IN FRAME FRAME-Para /* Send */
DO:
  ASSIGN lKasserere = TRUE.
  RUN StartEksport IN THIS-PROCEDURE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SendKasValuta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendKasValuta wWin
ON CHOOSE OF B-SendKasValuta IN FRAME FRAME-Para /* Send */
DO:
  ASSIGN lKasValuta = TRUE.
  RUN StartEksport IN THIS-PROCEDURE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SendKKamp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendKKamp wWin
ON CHOOSE OF B-SendKKamp IN FRAME FRAME-Para /* Send */
DO:
  ASSIGN lKKamp = TRUE.
  RUN StartEksport IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SendKunde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendKunde wWin
ON CHOOSE OF B-SendKunde IN FRAME FRAME-Para /* Send */
DO:
  ASSIGN lKunde = TRUE.
  RUN StartEksport IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Eksterne
&Scoped-define SELF-NAME B-SendMedlemWeb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendMedlemWeb wWin
ON CHOOSE OF B-SendMedlemWeb IN FRAME FRAME-Eksterne /* Send */
DO:
    RUN StartEksportMedlemWeb ("WEBINIT").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Para
&Scoped-define SELF-NAME B-SendSelgere
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendSelgere wWin
ON CHOOSE OF B-SendSelgere IN FRAME FRAME-Para /* Send */
DO:
  ASSIGN lSelgere = TRUE.
  RUN StartEksport IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SendStrKonv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendStrKonv wWin
ON CHOOSE OF B-SendStrKonv IN FRAME FRAME-Para /* Send */
DO:
  ASSIGN lStrKonv = TRUE  /* Både konv och strtype ksall ut samtidigt */
         lStrType = TRUE.
  RUN StartEksport IN THIS-PROCEDURE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SendStrType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendStrType wWin
ON CHOOSE OF B-SendStrType IN FRAME FRAME-Para /* Send */
DO:
  ASSIGN lStrKonv = TRUE  /* Både konv och strtype ksall ut samtidigt */
         lStrType = TRUE.
  RUN StartEksport IN THIS-PROCEDURE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Eksterne
&Scoped-define SELF-NAME B-SendSupOrdTilMail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendSupOrdTilMail wWin
ON CHOOSE OF B-SendSupOrdTilMail IN FRAME FRAME-Eksterne /* Send */
DO:
    RUN StartSupOrdTilMail ("MAILSUPORD").
    ASSIGN B-SendSupOrdTilMail:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Para
&Scoped-define SELF-NAME B-SendSyspara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendSyspara wWin
ON CHOOSE OF B-SendSyspara IN FRAME FRAME-Para /* Send */
DO:
  ASSIGN lSyspara = TRUE.
  RUN StartEksport IN THIS-PROCEDURE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SendTekster
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendTekster wWin
ON CHOOSE OF B-SendTekster IN FRAME FRAME-Para /* Send */
DO:
  ASSIGN lTekster = TRUE.
  RUN StartEksport IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SendVare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendVare wWin
ON CHOOSE OF B-SendVare IN FRAME FRAME-Para /* Send */
DO:
  ASSIGN lVare = TRUE.
  RUN StartEksport IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SendVarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendVarGr wWin
ON CHOOSE OF B-SendVarGr IN FRAME FRAME-Para /* Send */
DO:
  ASSIGN lVarGr = TRUE.
  RUN StartEksport IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Eksterne
&Scoped-define SELF-NAME B-SendWebBut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SendWebBut wWin
ON CHOOSE OF B-SendWebBut IN FRAME FRAME-Eksterne /* Send */
DO:
    RUN StartEksportWebBut ("WEBBUT").
    ASSIGN B-SendWebBut:SENSITIVE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME B-StartStop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-StartStop wWin
ON CHOOSE OF B-StartStop IN FRAME fMain /* Start/Stopp */
DO:
    dSenasteDag = ?.
    ASSIGN cFtpButikerParam:SCREEN-VALUE = ''.
    RUN InitierButiker.
    RUN StartStopServer(NOT chPSTimer:ENABLED).
/*     DO WITH FRAME {&FRAME-NAME}:                                */
/*         ASSIGN FI-Oppstart:SCREEN-VALUE  = FI-Oppstart          */
/*                FI-Intervall:SCREEN-VALUE = STRING(FI-Intervall) */
/*                FI-FirstTid:SCREEN-VALUE  = FI-FirstTid          */
/*                FI-LastTid:SCREEN-VALUE   = FI-LastTid           */
/*                .                                                */
/*     END.                                                        */
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Status
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Status wWin
ON CHOOSE OF B-Status IN FRAME fMain /* Kassestatus... */
DO:
  RUN ShellExecute{&A} IN hpApi(0,
                                "open",
                                "C:\Home\Lindbak\kasse\menu.htm",
                                "",
                                "",
                                1,
                                OUTPUT hInstance).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Para
&Scoped-define SELF-NAME B-StrKonvAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-StrKonvAlle wWin
ON CHOOSE OF B-StrKonvAlle IN FRAME FRAME-Para /* Initier */
DO:
  RUN SkapELoggAlle ("StrKonv",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-StrTypeAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-StrTypeAlle wWin
ON CHOOSE OF B-StrTypeAlle IN FRAME FRAME-Para /* Initier */
DO:
  RUN SkapELoggAlle ("StrType",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SysparaAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SysparaAlle wWin
ON CHOOSE OF B-SysparaAlle IN FRAME FRAME-Para /* Initier */
DO:
/*   RUN SkapELoggAlle ("Butiker",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").         */
/*   IF CAN-FIND(FIRST ekstbutiker) THEN                                             */
/*       RUN SkapEloggAlle ("ekstbutiker",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE"). */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-TeksterAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-TeksterAlle wWin
ON CHOOSE OF B-TeksterAlle IN FRAME FRAME-Para /* Initier */
DO:
  RUN SkapELoggAlle ("FeilKode",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").
  RUN SkapELoggAlle ("KravKode",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").
  RUN SkapELoggAlle ("GaveKType",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").
  RUN SkapELoggAlle ("UtbetType",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").
  RUN SkapELoggAlle ("InnBetType",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VareAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VareAlle wWin
ON CHOOSE OF B-VareAlle IN FRAME FRAME-Para /* Initier */
DO:
    IF NOT lKlarGjor THEN
        MESSAGE "Alle varer til kasse? Bekreft"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lOk AS LOGICAL.
    IF lOk OR lKlargjor THEN DO:
        RUN SkapELoggAlle ("ArtPris",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").
        RUN SkapELoggAlle ("MixMatch",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VargrAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VargrAlle wWin
ON CHOOSE OF B-VargrAlle IN FRAME FRAME-Para /* Initier */
DO:
  RUN SkapELoggAlle ("VarGr",IF lKlargjor THEN "KLARGJOR" ELSE "ALLE").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Eksterne
&Scoped-define SELF-NAME B-VpiErpAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VpiErpAlle wWin
ON CHOOSE OF B-VpiErpAlle IN FRAME FRAME-Eksterne /* Initier */
DO:
    IF lKlarGjor THEN
        RETURN NO-APPLY.
    MESSAGE "Alle varer til ERP? Bekreft"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lOk AS LOGICAL.
    IF lOk THEN
        RUN SkapELoggAlleErp ("ArtBas","ALLE").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-WebButAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-WebButAlle wWin
ON CHOOSE OF B-WebButAlle IN FRAME FRAME-Eksterne /* Initier */
DO:
    IF lKlarGjor THEN
        RETURN NO-APPLY.
    MESSAGE "Alle faste registre til Web Butikk? Bekreft"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lOk AS LOGICAL.
    IF lOk THEN
        RUN SkapELoggAlleWebBut.
    ASSIGN B-SendWebBut:SENSITIVE = TRUE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR-File
&Scoped-define FRAME-NAME FRAME-Fildistr
&Scoped-define SELF-NAME BR-File
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR-File wWin
ON VALUE-CHANGED OF BR-File IN FRAME FRAME-Fildistr
DO:
    {&OPEN-QUERY-BR-Receiver}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Para
&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON ENTRY OF BUTTON-1 IN FRAME FRAME-Para /* Registertype */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON MOUSE-SELECT-DBLCLICK OF BUTTON-1 IN FRAME FRAME-Para /* Registertype */
DO:
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON MOUSE-SELECT-DOWN OF BUTTON-1 IN FRAME FRAME-Para /* Registertype */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Eksterne
&Scoped-define SELF-NAME BUTTON-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-10 wWin
ON MOUSE-SELECT-DOWN OF BUTTON-10 IN FRAME FRAME-Eksterne /* Aktiv */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-11 wWin
ON MOUSE-SELECT-DOWN OF BUTTON-11 IN FRAME FRAME-Eksterne /* Hele register */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-12 wWin
ON ENTRY OF BUTTON-12 IN FRAME FRAME-Eksterne /* Antall eksporterte */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-12 wWin
ON MOUSE-SELECT-DBLCLICK OF BUTTON-12 IN FRAME FRAME-Eksterne /* Antall eksporterte */
DO:
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-12 wWin
ON MOUSE-SELECT-DOWN OF BUTTON-12 IN FRAME FRAME-Eksterne /* Antall eksporterte */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-13 wWin
ON ENTRY OF BUTTON-13 IN FRAME FRAME-Eksterne /* Kl. */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-13 wWin
ON MOUSE-SELECT-DBLCLICK OF BUTTON-13 IN FRAME FRAME-Eksterne /* Kl. */
DO:
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-13 wWin
ON MOUSE-SELECT-DOWN OF BUTTON-13 IN FRAME FRAME-Eksterne /* Kl. */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Para
&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME FRAME-Para /* Til overføring */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON ENTRY OF BUTTON-2 IN FRAME FRAME-Para /* Til overføring */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON MOUSE-SELECT-DBLCLICK OF BUTTON-2 IN FRAME FRAME-Para /* Til overføring */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON MOUSE-SELECT-DOWN OF BUTTON-2 IN FRAME FRAME-Para /* Til overføring */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 wWin
ON MOUSE-SELECT-DOWN OF BUTTON-3 IN FRAME FRAME-Para /* Auto */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 wWin
ON MOUSE-SELECT-DOWN OF BUTTON-4 IN FRAME FRAME-Para /* Hele register */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 wWin
ON ENTRY OF BUTTON-6 IN FRAME FRAME-Para /* Antall overførte */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 wWin
ON MOUSE-SELECT-DBLCLICK OF BUTTON-6 IN FRAME FRAME-Para /* Antall overførte */
DO:
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 wWin
ON MOUSE-SELECT-DOWN OF BUTTON-6 IN FRAME FRAME-Para /* Antall overførte */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 wWin
ON ENTRY OF BUTTON-7 IN FRAME FRAME-Para /* Kl. */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 wWin
ON MOUSE-SELECT-DBLCLICK OF BUTTON-7 IN FRAME FRAME-Para /* Kl. */
DO:
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 wWin
ON MOUSE-SELECT-DOWN OF BUTTON-7 IN FRAME FRAME-Para /* Kl. */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Eksterne
&Scoped-define SELF-NAME BUTTON-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 wWin
ON ENTRY OF BUTTON-8 IN FRAME FRAME-Eksterne /* Registertype */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 wWin
ON MOUSE-SELECT-DBLCLICK OF BUTTON-8 IN FRAME FRAME-Eksterne /* Registertype */
DO:
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 wWin
ON MOUSE-SELECT-DOWN OF BUTTON-8 IN FRAME FRAME-Eksterne /* Registertype */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 wWin
ON CHOOSE OF BUTTON-9 IN FRAME FRAME-Eksterne /* Til eksport */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 wWin
ON ENTRY OF BUTTON-9 IN FRAME FRAME-Eksterne /* Til eksport */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 wWin
ON MOUSE-SELECT-DBLCLICK OF BUTTON-9 IN FRAME FRAME-Eksterne /* Til eksport */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 wWin
ON MOUSE-SELECT-DOWN OF BUTTON-9 IN FRAME FRAME-Eksterne /* Til eksport */
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BUTTON-Kontroller
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Kontroller wWin
ON CHOOSE OF BUTTON-Kontroller IN FRAME fMain /* Kontroller */
DO:
  RUN KontrollerPrisko IN THIS-PROCEDURE.
  IF lKasseEksport = TRUE THEN
      RUN KontrollerElogg IN THIS-PROCEDURE.
  IF lErpEksport THEN
      RUN KontrollerErp IN THIS-PROCEDURE.
  IF lOrdHK THEN
      RUN KontrollerOrdHK IN THIS-PROCEDURE.
  IF lWebMedlemEksport THEN
      RUN KontrollerMedlemWeb IN THIS-PROCEDURE.
  IF lPkSdlHK THEN
      RUN KontrollerPkSdl IN THIS-PROCEDURE.
  IF lWebButEksport THEN
      RUN KontrollerWebBut .
  IF lSupOrdTilMail THEN
      RUN KontrollerSupOrdTilMail .
  RUN RefreshDistBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IMAGE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-1 wWin
ON MOUSE-SELECT-DBLCLICK OF IMAGE-1 IN FRAME fMain
DO:
 ASSIGN
  {&WINDOW-NAME}:WIDTH-PIXELS  = IF SELF:PRIVATE-DATA = "MINI" THEN iWPix ELSE 160
  {&WINDOW-NAME}:HEIGHT-PIXELS = IF SELF:PRIVATE-DATA = "MINI" THEN iHPix ELSE 
              IF FI-Serverstatus:HIDDEN = TRUE THEN 72 ELSE 95
  SELF:PRIVATE-DATA = STRING(SELF:PRIVATE-DATA = "MINI","STOR/MINI").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME PSTimer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PSTimer wWin OCX.Tick
PROCEDURE PSTimer.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
    IF lActiveTick = TRUE THEN
        RETURN.
    ASSIGN lActiveTick = TRUE.

    /* Sjekker om ELogg server skal kjøre. Tidsintervall den skal kjøre */
    /* er angitt i systemparameter.                                     */
    RUN SjekkAktiv.
    IF RETURN-VALUE <> 'OK' THEN
    DO:
        ASSIGN lActiveTick = FALSE.
        RETURN.
    END.

    /* Kkjører klargjøring av priskø. */
    RUN KontrollerPrisko.

    /* Utlegg til kassene. */
    IF lKasseEksport = TRUE THEN DO:
        RUN KontrollerMix.
        RUN KontrollerElogg.

        RUN StartEksport. 
    END.

    IF lErpEksport THEN DO:
        RUN KontrollerErp.
        RUN StartEksportErp ("") .
    END.
    IF lOrdHK THEN DO:
        RUN KontrollerOrdHK.
        RUN StartEksportButikk ("").
    END.
    IF lWebMedlemEksport THEN DO:
        RUN KontrollerMedlemWeb.
        RUN StartEksportMedlemWeb ("") .
    END.
    IF lPkSdlHK THEN DO:
        RUN KontrollerPkSdl.
        RUN StartEksportButikk ("").
    END.
    IF lWebButEksport THEN DO:
        RUN KontrollerWebBut.
        RUN StartEksportWebBut ("") .
    END.
    IF lSupOrdTilMail THEN DO:
        RUN KontrollerSupOrdTilMail.
        RUN StartSupOrdTilMail ("") .
    END.
    /*IF lFersvarevekt THEN */ DO:
        RUN StartEksportFerskvarevekt ("").
    END.
    /* I butikk - nye artikler som opprettes logges for overføring til hk. */
    IF NOT CAN-DO("Ja,yes,true,1",cHKInst) THEN
    DO:
        /*RUN korrElogg_til_vpi.p.*/
    END.

    IF dSenasteDag <> ? THEN DO:
        IF dSenasteDag < TODAY THEN
            RUN SlettSendToWooComm.
    END.
    dSenasteDag = TODAY.

    ASSIGN lActiveTick = FALSE.
    APPLY "CHOOSE" TO B-Oppdater IN FRAME fmain.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Para
&Scoped-define SELF-NAME TG-StrKonvAuto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-StrKonvAuto wWin
ON VALUE-CHANGED OF TG-StrKonvAuto IN FRAME FRAME-Para
DO:
    ASSIGN TG-StrTypeAuto:CHECKED = SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-StrTypeAuto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-StrTypeAuto wWin
ON VALUE-CHANGED OF TG-StrTypeAuto IN FRAME FRAME-Para
DO:
  ASSIGN TG-StrKonvAuto:CHECKED = SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
    ON ALT-K ANYWHERE
    DO:
        IF NOT DYNAMIC-FUNCTION('getCurrentPage':U) = 1 THEN
            RETURN.
        IF chPSTimer:ENABLED THEN
            RETURN.
        IF NOT lKasseEksport THEN
            RETURN.
        ASSIGN lKlargjor = NOT lKlargjor.
        RUN KlargjorModus (lKlargjor).
    END.
    IF socketconnect() = TRUE THEN DO:
/*         MESSAGE "Programmet kjører allerede (kontroller)" */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.            */
/*         APPLY "CLOSE" TO THIS-PROCEDURE.                  */

        
        QUIT. 
    END.


{src/adm2/windowmn.i}
/* RUN InitierTimer. */

PROCEDURE ShellExecute{&A} EXTERNAL "shell32" :
     DEFINE INPUT PARAMETER HWND AS LONG.
     DEFINE INPUT PARAMETER lpOperation AS CHAR.
     DEFINE INPUT PARAMETER lpFile AS CHAR.
     DEFINE INPUT PARAMETER lpParameters AS CHAR.
     DEFINE INPUT PARAMETER lpDirectory AS CHAR.
     DEFINE INPUT PARAMETER nShowCmd AS LONG.
     DEFINE RETURN PARAMETER hInstance AS LONG.
END.

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
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Paremeter|Filinfo|Eksterne systemer|Filer til kasser' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 6.00 , 1.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 21.43 , 116.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_folder ,
             FI-Serverstatus:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButtonEnaDis wWin 
PROCEDURE ButtonEnaDis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER lDisable AS LOGICAL    NO-UNDO.
      ASSIGN BUTTON-Kontroller:SENSITIVE IN FRAME {&FRAME-NAME} = lDisable
             B-Prisko:SENSITIVE = lDisable.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConnectHandler wWin 
PROCEDURE ConnectHandler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ip_hSocket AS HANDLE NO-UNDO. /* handle to the client socket that just connected */
 ip_hSocket:SET-READ-RESPONSE-PROCEDURE("ReadMessage":U,THIS-PROCEDURE).
RETURN. /* all done */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load wWin  _CONTROL-LOAD
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

OCXFile = SEARCH( "w-ELoggserver.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chPSTimer = PSTimer:COM-HANDLE
    UIB_S = chPSTimer:LoadControls( OCXFile, "PSTimer":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-ELoggserver.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject wWin 
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  IF VALID-HANDLE(chPSTimer) THEN
      RELEASE OBJECT chPSTimer NO-ERROR.
  IF VALID-HANDLE(PSTimer) THEN
      DELETE OBJECT PSTimer NO-ERROR.
  ASSIGN PSTimer   = ?
         chPSTimer = ?.
  IF  (PROGRAM-NAME(1) + "," + 
       PROGRAM-NAME(2) + "," + 
       PROGRAM-NAME(3) + "," + 
       PROGRAM-NAME(4) + "," + 
       PROGRAM-NAME(5) + "," + 
       PROGRAM-NAME(6)) MATCHES "*modul*" THEN . /* Gjør ingenting */
  ELSE
      QUIT.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksportTyper wWin 
PROCEDURE EksportTyper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cErpModuler       AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cEmailELoggserver AS CHARACTER NO-UNDO.

    {syspara.i 50 50 6 cEmailELoggserver}

    IF cHKinst = "yes" THEN DO:
        ASSIGN lKasseEksport = FALSE.
    END.
    ELSE DO:
        /* InfoPos kassa */
        KASSEKOLL:
        FOR EACH Butiker WHERE 
            (Butiker.NedlagtDato = ? OR Butiker.NedlagtDato > TODAY) NO-LOCK:
            IF (/* InfoPOS 8.0 */
                CAN-FIND(FIRST kasse WHERE 
                        kasse.butik = butiker.butik AND 
                        kasse.modell = 10 AND 
                        kasse.aktiv = TRUE) 
                OR
                /* PRS POS 1.0 */
                CAN-FIND(FIRST kasse WHERE 
                        kasse.butik = butiker.butik AND 
                        kasse.modell = 5 AND 
                        kasse.aktiv = TRUE)
               )
                THEN 
              DO:
                ASSIGN lKasseEksport = TRUE.
                LEAVE KASSEKOLL.
              END.
        END. /* KASSEKOLL*/
    END.

    /* kontrollera vilka ERP-eksporttyper som är aktiva */
    FIND FIRST EkstEDBSystem WHERE EkstEDBSystem.DataType = "FIN" AND EkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
    IF AVAIL EkstEDBSystem THEN
        ASSIGN lErpEksport       = TRUE
               TG-ErpFin         = TRUE
               cFinEksportRutine = EkstEDBSystem.EksportRutine.
    FIND FIRST EkstEDBSystem WHERE EkstEDBSystem.DataType = "FAKTAUTO" AND EkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
    IF AVAIL EkstEDBSystem THEN
        ASSIGN lErpEksport       = TRUE
               TG-ErpFin         = TRUE
               cFakEksportRutine = EkstEDBSystem.EksportRutine.
    FIND FIRST EkstEDBSystem WHERE EkstEDBSystem.DataType = "KONTAUTO" AND EkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
    IF AVAIL EkstEDBSystem THEN
        ASSIGN lErpEksport       = TRUE
               TG-ErpFin         = TRUE
               cKontEksportRutine = EkstEDBSystem.EksportRutine.
    FIND FIRST EkstEDBSystem WHERE EkstEDBSystem.DataType = "ORD" AND EkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
    IF AVAIL EkstEDBSystem THEN
        ASSIGN lErpEksport       = TRUE
               TG-ErpOrd         = TRUE
               cOrdEksportRutine = EkstEDBSystem.EksportRutine.
    FIND FIRST EkstEDBSystem WHERE EkstEDBSystem.DataType = "VPI" AND EkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
    IF AVAIL EkstEDBSystem THEN
        ASSIGN lErpEksport       = TRUE
               TG-ErpVpi         = TRUE
               cVpiEksportRutine = EkstEDBSystem.EksportRutine.
    FIND FIRST EkstEDBSystem WHERE EkstEDBSystem.DataType = "VPIV" AND EkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
    IF AVAIL EkstEDBSystem THEN
        ASSIGN lErpEksport       = TRUE
               TG-ErpVpi         = TRUE
               cVpiVareEksportRutine = EkstEDBSystem.EksportRutine.
    FIND FIRST EkstEDBSystem WHERE EkstEDBSystem.DataType = "MEDW" AND EkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
    IF AVAIL EkstEDBSystem THEN
        ASSIGN lWebMedlemEksport    = TRUE
               TG-MedlemWeb         = TRUE
               cMedWebEksportRutine = EkstEDBSystem.EksportRutine.
    FIND FIRST EkstEDBSystem WHERE EkstEDBSystem.DataType = "WEBBUT" AND EkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
    IF AVAIL EkstEDBSystem THEN
        ASSIGN lWebButEksport    = TRUE
               TG-WebBut         = TRUE
               cWebButEksportRutine = EkstEDBSystem.EksportRutine.

    cFerskVareEksportRutine = 'eksportTilFerskvareVekt.p'.
    
    IF CAN-DO('1,J,Y,True,Yes',cEmailELoggserver) THEN 
    DO:
      ASSIGN lSupOrdTilMail       = TRUE
             TG-SupOrdTilMail     = TRUE
             cSendSupOrdTilMailRutine = 'sendSupOrdTilMail.p'.
    END.

    ASSIGN lOrdHK       = cHKinst = "yes"
           TG-ButikkOrd = cHKinst = "yes"
           cOrdButikkEksportRutine = "eksportHKordre.p".

    ASSIGN lPkSdlHK       = cHKinst  = "yes"
           TG-ButikkPkSdl = cHKinst  = "yes"
           cPkSdlButikkEksportRutine = "eksportPkSdl.p".
    /*        

cOrdEksportRutine
cVpiEksportRutine
    
        EkstEDBSystem.DataType 
        EkstEDBSystem.EDBSystem 
        EkstEDBSystem.EkspFrekvens 
        EkstEDBSystem.eksportRutine 
        EkstEDBSystem.FilEkstent 
        EkstEDBSystem.FilKatalog 
        EkstEDBSystem.FilPrefix 
        EkstEDBSystem.MaksSeq 
        EkstEDBSystem.SeqNr 
        EkstEDBSystem.SeqvAktiv 
        EkstEDBSystem.StartTid 
        EkstEDBSystem.StoppTid 
        EkstEDBSystem.SysBeskrivelse 
        EkstEDBSystem.TidsIntervall
*/

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
  DISPLAY CLanbutikerParam cFtpbutikerParam FI-Serverstatus FI-Aktiv 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE IMAGE-1 B-Oppdater B-Status CLanbutikerParam B-Prisko cFtpbutikerParam 
         BUTTON-Kontroller B-StartStop 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  DISPLAY FI-AntErpVpi FI-VareErpDatoTid FI-ErpVpiError TG-ErpVpi FI-AntErpOrd 
          FI-OrdreErpDatoTid FI-ErpOrdError TG-ErpOrd FI-AntErpFin 
          FI-FinansErpDatoTid FI-ErpFinError TG-ErpFin FI-AntButikkOrd 
          FI-OrdreButikkDatoTid FI-ButikkOrdError TG-ButikkOrd FI-AntButikkPkSdl 
          FI-PkSdlButikkDatoTid FI-ButikkPkSdlError TG-ButikkPkSdl TG-MedlemWeb 
          FI-AntMedlemWeb FI-MedlemWebDatoTid FI-MedlemWebError FI-AntWebBut 
          FI-WebButDatoTid TG-WebBut FI-WebButError FI-AntSupOrdTilMail 
          FI-SupOrdTilMailDatoTid FI-SupOrdTilMailError TG-SupOrdTilMail 
          FI-OverfTxt FI-VareErpTilExp FI-VarerErpTxt FI-OrdreErpTilExp 
          FI-OrdreErpTxt FI-FinansErpTilExp FI-FinansErpTxt FI-OrdreButikkTilExp 
          FI-OrdreButikkTxt FI-PkSdlButikkTilExp FI-PkSdlButikkTxt 
          FI-MedlemTilWeb FI-MedlemTilWebTxt FI-WebButTilWeb FI-WebButTilWebTxt 
          FI-SupOrdTilMail FI-SupOrdTilMailTxt FI-TilExpInfo 
      WITH FRAME FRAME-Eksterne IN WINDOW wWin.
  ENABLE RECT-66 RECT-60 RECT-61 BUTTON-8 BUTTON-9 BUTTON-10 BUTTON-11 
         BUTTON-12 BUTTON-13 FI-OverfTxt FI-VarerErpTxt FI-OrdreErpTxt 
         FI-FinansErpTxt FI-OrdreButikkTxt FI-PkSdlButikkTxt FI-MedlemTilWebTxt 
         FI-WebButTilWebTxt FI-SupOrdTilMailTxt 
      WITH FRAME FRAME-Eksterne IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Eksterne}
  DISPLAY FI-FilTxt 
      WITH FRAME FRAME-Fildistr IN WINDOW wWin.
  ENABLE RECT-62 BR-File FI-FilTxt 
      WITH FRAME FRAME-Fildistr IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Fildistr}
  DISPLAY FI-VareFiler EDITOR-1 FI-MixFiler FI-HgrFiler FI-KasValutaFiler 
          FI-KassererFiler FI-SelgerFiler FI-KundeFiler FI-FargFiler 
          FI-StrKonvFiler FI-StrTypeFiler FI-TeksterFiler FI-GarantiFiler 
          FI-ButikerFiler FI-SysparaFiler FI-KKampFiler FI-FilTxt 
      WITH FRAME FRAME-Filinfo IN WINDOW wWin.
  ENABLE RECT-59 FI-VareFiler EDITOR-1 FI-MixFiler FI-HgrFiler 
         FI-KasValutaFiler FI-KassererFiler FI-SelgerFiler FI-KundeFiler 
         FI-FargFiler FI-StrKonvFiler FI-StrTypeFiler FI-KKampFiler FI-FilTxt 
      WITH FRAME FRAME-Filinfo IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Filinfo}
  DISPLAY FI-AntVarer FI-VareDatoTid FI-VareError TG-VareAuto FI-AntKKamp 
          FI-KKampDatoTid FI-KKampError TG-KKampAuto FI-AntVarGr FI-VarGrDatoTid 
          FI-VarGrError TG-VarGrAuto FI-AntKasValuta FI-KasValutaDatoTid 
          FI-KasValutaError TG-KasValutaAuto FI-AntKasserer FI-KassererDatoTid 
          FI-KassererError TG-KasserereAuto FI-AntSelger FI-SelgerDatoTid 
          FI-SelgerError TG-SelgereAuto FI-AntKunder FI-KundeDatoTid 
          FI-KundeError TG-KundeAuto FI-AntFarger FI-FargerDatoTid 
          FI-FargerError TG-FargerAuto FI-AntStrKonv FI-StrKonvDatoTid 
          FI-StrKonvError TG-StrKonvAuto FI-AntStrType FI-StrTypeDatoTid 
          FI-StrTypeError TG-StrTypeAuto FI-TeksterError FI-AntTekster 
          FI-TeksterDatoTid TG-TeksterAuto FI-AntGaranti FI-GarantiDatoTid 
          FI-GarantiError TG-GarantiAuto FI-AntButiker FI-ButikerDatoTid 
          FI-ButikerError TG-ButikerAuto FI-AntSyspara FI-SysparaDatoTid 
          FI-SysparaError TG-SysparaAuto TG-Skjul TG-SendPRSPOS FI-OverfTxt 
          FI-VareTilExp FI-VarerTxt FI-KKampTilExp FI-PakkeMixTxt FI-VarGrTilExp 
          FI-VaregrupperTxt FI-KasValutaTilExp FI-KasValutaTxt FI-KassererTilExp 
          FI-KasserereTxt FI-SelgerTilExp FI-SelgereTxt FI-KundeTilExp 
          FI-KunderTxt FI-FargerTilExp FI-FargerTxt FI-StrKonvTilExp 
          FI-StrKonvTxt FI-StrTypeTilExp FI-StrTypeTxt FI-TeksterTilExp 
          FI-TeksterTxt FI-GarantiTilExp FI-GarantiTxt FI-ButikerTilExp 
          FI-ButikerTxt FI-SysparaTilExp FI-SysparaTxt FI-TilExpInfo 
      WITH FRAME FRAME-Para IN WINDOW wWin.
  ENABLE RECT-56 RECT-57 RECT-58 BUTTON-1 BUTTON-2 BUTTON-3 BUTTON-4 BUTTON-6 
         BUTTON-7 B-VareAlle TG-VareAuto B-KKampAlle TG-KKampAuto B-VargrAlle 
         TG-VarGrAuto B-KasValutaAlle TG-KasValutaAuto B-KasserereAlle 
         TG-KasserereAuto B-SelgereAlle TG-SelgereAuto B-KundeAlle TG-KundeAuto 
         B-FargeAlle TG-FargerAuto B-StrKonvAlle TG-StrKonvAuto B-StrTypeAlle 
         TG-StrTypeAuto B-TeksterAlle TG-TeksterAuto B-GarantiAlle 
         TG-GarantiAuto B-ButikerAlle TG-ButikerAuto TG-SysparaAuto TG-Skjul 
         TG-SendPRSPOS FI-OverfTxt FI-VarerTxt FI-PakkeMixTxt FI-VaregrupperTxt 
         FI-KasValutaTxt FI-KasserereTxt FI-SelgereTxt FI-KunderTxt 
         FI-FargerTxt FI-StrKonvTxt FI-StrTypeTxt FI-TeksterTxt FI-GarantiTxt 
         FI-ButikerTxt FI-SysparaTxt 
      WITH FRAME FRAME-Para IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Para}
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
    hServer:DISABLE-CONNECTIONS() NO-ERROR.
    DELETE OBJECT hServer NO-ERROR.

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FinnesIkkeOverforte wWin 
PROCEDURE FinnesIkkeOverforte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME FRAME-Para:
    IF FI-VareTilExp:BGCOLOR = 12      OR
       FI-VarGrTilExp:BGCOLOR = 12     OR
       FI-KasValutaTilExp:BGCOLOR = 12 OR
       FI-KassererTilExp:BGCOLOR = 12  OR
       FI-SelgerTilExp:BGCOLOR = 12    OR
       FI-KundeTilExp:BGCOLOR = 12     OR
       FI-FargerTilExp:BGCOLOR = 12    OR
       FI-StrKonvTilExp:BGCOLOR = 12   OR
       FI-StrTypeTilExp:BGCOLOR = 12   OR
       FI-TeksterTilExp:BGCOLOR = 12   OR
       FI-GarantiTilExp:BGCOLOR = 12   OR
       FI-KKampTilExp:BGCOLOR = 12     OR
       FI-SysparaTilExp:BGCOLOR = 12   OR
       FI-ButikerTilExp:BGCOLOR = 12   THEN 
        ASSIGN 
            /*   FI-Serverstatus:SCREEN-VALUE IN FRAME fMain = "Ikke overført data finnes" */
               FI-Serverstatus:HIDDEN IN FRAME fMain = FALSE.
    ELSE 
        ASSIGN FI-Serverstatus:HIDDEN = TRUE.
    RUN FixWindowSize.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixWindowSize wWin 
PROCEDURE FixWindowSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME fMain:
  IF {&WINDOW-NAME}:WIDTH-PIXELS <> iWPix THEN
      ASSIGN {&WINDOW-NAME}:HEIGHT-PIXELS = IF FI-Serverstatus:HIDDEN = TRUE THEN 72 ELSE 95.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlyttFiler wWin 
PROCEDURE FlyttFiler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cFtpSendes AS CHAR  NO-UNDO.

  IF lFlyttaTilSendes THEN 
  FLYTTFILER:
  DO:
      INPUT FROM OS-DIR(cEksportKatalog) NO-ECHO.
      LES_FILLISTE:
      REPEAT:
        SET cFilNavn FORMAT "X(50)".
        
        /* InfoPOS 8.0 filer vi ikke benytter */
        IF TRIM(cFilNavn) BEGINS "prisut" OR 
           TRIM(cFilNavn) BEGINS "PRSPos" OR
           TRIM(cFilNavn) BEGINS "journal" OR
           TRIM(cFilNavn) BEGINS "FINANSDA" OR
           TRIM(cFilNavn) BEGINS "HGRDAG" OR
           TRIM(cFilNavn) BEGINS "KASSDAG" OR
           TRIM(cFilNavn) BEGINS "KREDSALG" OR
           TRIM(cFilNavn) BEGINS "MEDDAG" OR
           TRIM(cFilNavn) BEGINS "TIMEDAG" OR
           TRIM(cFilNavn) BEGINS "VARESALG" OR
           TRIM(cFilNavn) BEGINS "VARETRAN"
            THEN
            NEXT.
        
        FILE-INFO:FILE-NAME = cEksportKatalog + "\" + TRIM(cFilNavn).
        
        /* Flytter InfoPOS 8.0 filer. */
        IF FILE-INFO:FILE-TYPE BEGINS "F" AND NUM-ENTRIES(cFilNavn,".") = 2 AND
          ENTRY(1,cFilNavn,".") <> "" AND CAN-DO(cFtpButiker,ENTRY(2,cFilNavn,".")) THEN 
        DO:
          ASSIGN cFtpSendes = cSendesKatalog + "\" + cFilNavn.
          IF SEARCH(cFtpSendes) = ? THEN
            OS-RENAME VALUE(FILE-INFO:FULL-PATHNAME) VALUE(cFtpSendes).
          ELSE DO:
            OS-COMMAND SILENT VALUE("TYPE " + FILE-INFO:FULL-PATHNAME + ">> " + cFtpSendes).
            OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).
          END.
        END.

        /* Flytter PRS Pos filer. */
        IF FILE-INFO:FILE-TYPE BEGINS "F" AND NUM-ENTRIES(cFilNavn,".") = 2 AND
          ENTRY(1,cFilNavn,".") <> "" AND CAN-DO(cPRSFtpButiker,ENTRY(2,cFilNavn,".")) THEN 
        DO:
          ASSIGN cFtpSendes = cSendesKatalog + "\" + cFilNavn.
          IF SEARCH(cFtpSendes) = ? THEN
            OS-RENAME VALUE(FILE-INFO:FULL-PATHNAME) VALUE(cFtpSendes).
          ELSE DO:
            OS-COMMAND SILENT VALUE("TYPE " + FILE-INFO:FULL-PATHNAME + ">> " + cFtpSendes).
            OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).
          END.
        END.

      END. /* LES_FILLISTE */
      INPUT CLOSE.
  END. /* FLYTTFILER */

  /* Midlertidig flytting av filer til PRS Pos.                                  */
  /* Denne flytterutinen utgår når ny rutine for utlegg til PRS Pos er på plass. */
  IF SEARCH(".\cmd\flyttafiler.cmd") <> ? THEN
      OS-COMMAND SILENT VALUE(".\cmd\flyttafiler.cmd").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitColors wWin 
PROCEDURE InitColors :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iFarge AS INTEGER    NO-UNDO.
    DO WITH FRAME FRAME-Para:
        ASSIGN FI-VareTilExp:BGCOLOR      = iFarge 
               FI-VarGrTilExp:BGCOLOR     = iFarge
               FI-KasValutaTilExp:BGCOLOR = iFarge
               FI-KassererTilExp:BGCOLOR  = iFarge
               FI-SelgerTilExp:BGCOLOR    = iFarge
               FI-KundeTilExp:BGCOLOR     = iFarge
               FI-FargerTilExp:BGCOLOR    = iFarge
               FI-StrKonvTilExp:BGCOLOR   = iFarge
               FI-StrTypeTilExp:BGCOLOR   = iFarge
               FI-TeksterTilExp:BGCOLOR   = iFarge 
               FI-GarantiTilExp:BGCOLOR   = iFarge
               FI-KKampTilExp:BGCOLOR     = iFarge
               FI-ButikerTilExp:BGCOLOR   = iFarge
               FI-SysparaTilExp:BGCOLOR   = iFarge.
    END.
    DO WITH FRAME FRAME-Eksterne:
        ASSIGN FI-VareErpTilExp:BGCOLOR     = iFarge 
               FI-OrdreErpTilExp:BGCOLOR    = iFarge
               FI-FinansErpTilExp:BGCOLOR   = iFarge
               FI-OrdreButikkTilExp:BGCOLOR = iFarge
               FI-MedlemTilWeb:BGCOLOR      = iFarge
               FI-PkSdlButikkTilExp:BGCOLOR = iFarge
               FI-WebButTilWeb:BGCOLOR      = iFarge
               FI-SupOrdTilMail:BGCOLOR     = iFarge
               .
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls wWin 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR iIntervall AS INT  NO-UNDO.
  DEF VAR cTekst AS CHAR NO-UNDO.

  {syspara.i 200 3 2 cTekst}
  ASSIGN iIntervall = INT(cTekst) NO-ERROR.
  IF ERROR-STATUS:ERROR OR iIntervall = 0 THEN iIntervall = 20.

  ASSIGN chPSTimer          = chPSTimer:PSTimer
         chPSTimer:ENABLED  = FALSE.
         chPSTimer:interval = iIntervall * 1000.
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
/*   IF socketconnect() = TRUE THEN DO:                    */
/*       MESSAGE "Programmet kjører allerede (kontroller)" */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.            */
/*       APPLY "CLOSE" TO THIS-PROCEDURE.                  */
/*       RETURN.                                           */
/*   END.                                                  */
/* eksport katalog */

{syspara.i 1 1 51 cSendesKatalog}
cSendesKatalog = TRIM(RIGHT-TRIM(cSendesKatalog,'\')).

{syspara.i 1 1 56 cEksportKatalog}
cEksportKatalog = TRIM(RIGHT-TRIM(cEksportKatalog,'\')).
/* Om vi lägger ut på kassekatalogen sp skall det flyttas */
lFlyttaTilSendes = cEksportKatalog = "c:\home\lindbak\kasse".
  
{syspara.i 200 10 1 cTekst}
IF CAN-DO('1,j,y,true,ja',cTekst) THEN
    lHost = TRUE.
ELSE
    lHost = FALSE.
{syspara.i 200 10 2 ipcHost}
{syspara.i 200 10 3 ipcPort}

  {syspara.i 1 1 18 cHKinst}
  FOR EACH ELogg WHERE ELogg.Verdier = "KLARGJOR":
      DELETE ELogg.
  END.

  /* Setter variabel lKasseEksport som sier om det finnes noen aktive kasser i systemet. */
  RUN EksportTyper.

  /* Setter opp bakgrunnsfarge på feltene */
  RUN InitColors(?).
  IF SEARCH(cRedIcon) <> ? AND SEARCH(cGreenIcon) <> ? AND SEARCH(cYellowIcon) <> ? THEN
      lIconExist = TRUE.

  RUN SUPER.
  B-Oppdater:HIDDEN IN FRAME fmain = TRUE.
  IF lIconExist THEN
      {&WINDOW-NAME}:LOAD-ICON(cRedIcon) NO-ERROR.

  /* Sjekker socket. */
  /*startup().*/

  IF lKasseEksport THEN
      RUN InitierButiker.
  ELSE IF NOT lKasseEksport THEN
      RUN ToggleOff.

  RUN SlettErpFiler. 
  IF lOrdHK = FALSE THEN
      RUN SlettHKord.
      
  IF TG-ErpVpi = TRUE THEN
      B-VpiErpAlle:SENSITIVE IN FRAME FRAME-Eksterne = TRUE.
  /* Code placed here will execute AFTER standard behavior.    */
  FRAME FRAME-Para:MOVE-TO-TOP().
  ASSIGN iWPix = {&WINDOW-NAME}:WIDTH-PIXELS 
         iHPix = {&WINDOW-NAME}:HEIGHT-PIXELS.
  RUN KontrollerPrisko IN THIS-PROCEDURE.

  /* Vid aktiv(a) kassa/kassor */
  IF lKasseEksport THEN DO:
      RUN KontrollerMix    IN THIS-PROCEDURE.
      RUN KontrollerElogg  IN THIS-PROCEDURE.
  END.
  /* Vid eksport ERP-system */
  IF lErpEksport THEN
      RUN KontrollerErp.
  IF lWebMedlemEksport THEN
      RUN KontrollerMedlemWeb.
  IF lOrdHK THEN
      RUN KontrollerOrdHK.
  IF lKasseEksport = FALSE AND (lErpEksport = TRUE OR lOrdHK = TRUE) THEN
      RUN SelectPage (3).
  IF lPkSdlHK THEN
      RUN KontrollerPkSdl.
  IF lWebButEksport THEN DO:
      RUN KontrollerWebBut.
      IF TG-WebBut = TRUE THEN
          B-WebButAlle:SENSITIVE IN FRAME FRAME-Eksterne = TRUE.
  END.
  IF lSupOrdTilMail THEN DO:
      RUN KontrollerSupOrdTilMail.
  END.

  ASSIGN
      TG-SendPRSPos:CHECKED IN FRAME FRAME-Para   = lHost
      TG-SendPRSPos:SENSITIVE IN FRAME FRAME-Para = lHost
      .

  /* I butikk - nye artikler som opprettes logges for overføring til hk. */
  /*
  IF NOT CAN-DO("Ja,yes,true,1",cHKInst) THEN
      RUN korrElogg_til_vpi.p.
  */
  PROCESS EVENTS.

  IF CAN-DO(SESSION:PARAMETER,"AUTOSTART") THEN
      APPLY "CHOOSE" TO B-StartStop IN FRAME fMain.
  ELSE
      RUN FinnesIkkeOverforte.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitierButiker wWin 
PROCEDURE InitierButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.
  
  DEF_FRAME:
  DO WITH FRAME {&FRAME-NAME}:
    
    ASSIGN 
        cLanbutikerParam = ""
        cPRSFtpButiker   = ""
        cLanbutikerParam = ""
        cFtpbutikerParam = ""
        cLanbutiker      = ""
        cFtpButiker      = ""
        .

    /* Lan butikker er InfoPOS 8.0 butikker. PRS POS butikker er alltid FTP butikker. */
    FOR EACH Butiker NO-LOCK WHERE 
        Butiker.LanButikk = TRUE AND 
        Butiker.ApningsDato > 01/01/2000 AND
        (Butiker.NedlagtDato = ? OR Butiker.NedlagtDato >= TODAY) AND
         Butiker.harButikksystem = TRUE:
        ASSIGN cLanbutikerParam = cLanbutikerParam + (IF cLanbutikerParam = "" THEN "" ELSE ",") + 
            STRING(Butiker.Butik).
    END.
    
    FRAME_BLOKK:
    DO WITH FRAME Frame-Filinfo:

      INFOPOS_8.0_BUTIKKER:
      FOR EACH butiker NO-LOCK WHERE 
          Butiker.ApningsDato > 01/01/2000 AND
          (Butiker.NedlagtDato = ? OR Butiker.NedlagtDato >= TODAY) AND
           Butiker.harButikksystem = TRUE AND 
          CAN-FIND(FIRST Kasse WHERE 
                   kasse.butik    = Butiker.butik AND 
                   Kasse.GruppeNr = 1 AND
                   Kasse.KasseNr < 99 AND
                   Kasse.ModellNr = 10 /* InfoPOS 8.0 */ AND
                   kasse.aktiv    = TRUE):

          /* Bygger streng med Lan butikker */
          IF CAN-DO(cLanbutikerParam,STRING(butiker.butik)) THEN 
          DO:
              ASSIGN cLanbutiker = cLanbutiker + (IF cLanbutiker = "" THEN "" ELSE ",") + STRING(Butiker.Butik).
              EDITOR-1:INSERT-STRING(STRING(butiker.butik) + " " + STRING(Butiker.butnamn,"x(15)") + " " + "<FILnavn>.txt" + CHR(10)).
          END.
          /* Bygger streng med ftp butikker for InfoPOS 8.0. */
          ELSE DO:
              ASSIGN cFtpButiker = cFtpButiker + (IF cFtpButiker = "" THEN "" ELSE ",") + STRING(Butiker.Butik).
              EDITOR-1:INSERT-STRING(STRING(butiker.butik) + " " + STRING(Butiker.butnamn,"x(15)") + " " + "<FILnavn>." + STRING(butiker.butik) + CHR(10)).
          END.
      END. /* INFOPOS_8.0_BUTIKKER: */

      PRS_POS_BUTIKKER:
      FOR EACH butiker NO-LOCK WHERE 
          Butiker.ApningsDato > 01/01/2000 AND
          (Butiker.NedlagtDato = ? OR Butiker.NedlagtDato >= TODAY) AND
           Butiker.harButikksystem = TRUE AND 
          CAN-FIND(FIRST Kasse WHERE 
                   kasse.butik    = Butiker.butik AND 
                   Kasse.GruppeNr = 1 AND 
                   Kasse.KasseNr < 99 AND
                   Kasse.ModellNr = 5 /* PRS POS 1.0 */ AND
                   kasse.aktiv    = TRUE):

          /* Bygger streng med ftp butikker for PRS POS. */
          ASSIGN cPRSFtpButiker = cPRSFtpButiker + (IF cPRSFtpButiker = "" THEN "" ELSE ",") + STRING(Butiker.Butik).
          EDITOR-1:INSERT-STRING(STRING(butiker.butik) + " " + STRING(Butiker.butnamn,"x(15)") + " " + "<FILnavn>." + STRING(butiker.butik) + CHR(10)).
      END. /* INFOPOS_8.0_BUTIKKER: */
      cPRSFtpButiker = TRIM(cPRSFtpButiker).

    END. /* FRAME_BLOKK*/

    ASSIGN cLanbutikerParam = cLanButiker
           cLanbutikerParam:SCREEN-VALUE = cLanbutikerParam
           cFtpbutikerParam:SCREEN-VALUE = cFtpButiker + (IF cFtpButiker <> '' THEN ',' ELSE '') + cPRSFtpButiker.


  END. /* DEF_FRAME */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitierTimer wWin 
PROCEDURE InitierTimer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     ASSIGN chPSTimer = chPSTimer:PSTimer
            chPSTimer:interval = 1000.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KlargjorModus wWin 
PROCEDURE KlargjorModus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cAktiv AS LOGICAL  NO-UNDO.
  
  DO WITH FRAME fMain:
      ASSIGN lKlargjor = cAktiv
             B-Klargjor:HIDDEN    = NOT cAktiv
             B-Klargjor:SENSITIVE = cAktiv
             B-Ftpbut:HIDDEN      = NOT cAktiv
             B-Ftpbut:SENSITIVE   = cAktiv
             B-Lanbut:HIDDEN      = NOT cAktiv
             B-Lanbut:SENSITIVE   = cAktiv
             B-Klargjor:SENSITIVE = NOT cAktiv.
  END.
  IF cAktiv THEN
      ASSIGN cOrgLanButiker = cLanButiker
             cOrgFtpButiker = ''
             cOrgFtpButiker = cFtpButiker + (IF cFtpButiker <> '' AND cPRSFtpButiker <> '' THEN ',' ELSE '') +  
                              cPRSFtpButiker.
  ELSE DO:
      RUN InitierButiker.
/*       ASSIGN  cLanButiker = cOrgLanButiker                 */
/*               cFtpButiker = cOrgFtpButiker                 */
/*               cLanButikerParam = cLanButiker               */
/*               cFtpButikerParam = cFtpButiker               */
/*               cLanButikerParam:SCREEN-VALUE = cLanButiker  */
/*               cFtpButikerParam:SCREEN-VALUE = cFtpButiker. */
  END.
   FRAME fMain:BGCOLOR = IF cAktiv THEN 13 ELSE ?.           

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerElogg wWin 
PROCEDURE KontrollerElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lFinnes AS LOGICAL    NO-UNDO.
  DO WITH FRAME FRAME-Para:
     IF FI-VareTilExp:BGCOLOR = ? AND (canfindElogg("RedeleteArtBas","POS") OR
                                       canfindElogg("ArtBas","POS")         OR 
                                       canfindElogg("ArtPris","POS")        OR 
                                       canfindElogg("PakkeLinje","POS")     OR
                                       canfindElogg("MixMatch","POS"))  THEN DO:
         ASSIGN FI-VareTilExp:BGCOLOR = 12
                B-SendVare:SENSITIVE  = TRUE.
     END.
     IF FI-KKampTilExp:BGCOLOR = ? AND canfindElogg("KampanjeMixMatch","POS") THEN DO:
         ASSIGN FI-KKampTilExp:BGCOLOR = 12
                B-SendKKamp:SENSITIVE  = TRUE.
     END.
     IF FI-VarGrTilExp:BGCOLOR = ? AND canfindElogg("VarGr","POS") THEN DO:
         ASSIGN FI-VarGrTilExp:BGCOLOR = 12
                B-SendVarGr:SENSITIVE  = TRUE.
     END.
     IF FI-KasValutaTilExp:BGCOLOR = ? AND canfindElogg("KasValuta","POS") THEN DO:
         ASSIGN FI-KasValutaTilExp:BGCOLOR = 12
                B-SendKasValuta:SENSITIVE  = TRUE.
     END.
     IF FI-KassererTilExp:BGCOLOR = ? AND (canfindElogg("ButikkForsalj","POS") OR 
                                           canfindElogg("Forsalj","POS")) THEN DO:
         ASSIGN FI-KassererTilExp:BGCOLOR = 12
                B-SendKasserere:SENSITIVE = TRUE.
     END.
     IF FI-SelgerTilExp:BGCOLOR = ? AND (canfindElogg("ButikkSelger","POS") OR 
                                         canfindElogg("Selger","POS")) THEN DO:
         ASSIGN FI-SelgerTilExp:BGCOLOR = 12
                B-SendSelgere:SENSITIVE = TRUE.
     END.
     IF FI-KundeTilExp:BGCOLOR = ? AND 
         (canfindElogg("Kunde","POS") OR canfindElogg("KundeKort","POS")) THEN DO:
         ASSIGN FI-KundeTilExp:BGCOLOR = 12
                B-SendKunde:SENSITIVE = TRUE.
     END.
     IF FI-FargerTilExp:BGCOLOR = ? AND canfindElogg("Farg","POS") THEN DO:
         ASSIGN FI-FargerTilExp:BGCOLOR = 12
                B-SendFarger:SENSITIVE = TRUE.
     END.
     IF FI-StrKonvTilExp:BGCOLOR = ? AND canfindElogg("StrKonv","POS") THEN DO:
         ASSIGN FI-StrKonvTilExp:BGCOLOR = 12
                B-SendStrKonv:SENSITIVE = TRUE.
     END.
     IF FI-StrTypeTilExp:BGCOLOR = ? AND canfindElogg("StrType","POS") THEN DO:
         ASSIGN FI-StrTypeTilExp:BGCOLOR = 12
                B-SendStrType:SENSITIVE = TRUE.
     END.
     IF FI-TeksterTilExp:BGCOLOR = ? AND 
         (canfindElogg("FeilKode","POS")  OR
          canfindElogg("KravKode","POS")  OR
          canfindElogg("GaveKType","POS") OR
          canfindElogg("UtbetType","POS") OR
          canfindElogg("InnBetType","POS")) THEN DO:
         ASSIGN FI-TeksterTilExp:BGCOLOR = 12
                B-SendTekster:SENSITIVE = TRUE.
     END.
     IF FI-GarantiTilExp:BGCOLOR = ? AND canfindElogg("Garanti","POS") THEN DO:
         ASSIGN FI-GarantiTilExp:BGCOLOR = 12
                B-SendGaranti:SENSITIVE = TRUE.
     END.
     IF FI-ButikerTilExp:BGCOLOR = ? AND (canfindElogg("Butiker","POS") OR
                                          canfindElogg("ekstbutiker","POS")) THEN DO:
         ASSIGN FI-ButikerTilExp:BGCOLOR = 12
                B-SendButiker:SENSITIVE = TRUE.
     END.
     IF FI-SysparaTilExp:BGCOLOR = ? AND canfindElogg("Syspara","POS") THEN DO:
         ASSIGN FI-SysparaTilExp:BGCOLOR = 12
                B-SendSyspara:SENSITIVE = TRUE.
     END.
     RUN FinnesIkkeOverforte.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerErp wWin 
PROCEDURE KontrollerErp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME FRAME-Eksterne:
      IF TG-ErpVpi = TRUE AND FI-VareErpTilExp:BGCOLOR = ? AND 
          ((canfindElogg( "ArtBas","ERP")) OR (canfindElogg( "VarebokVPI","ERP")))THEN DO:
          ASSIGN FI-VareErpTilExp:BGCOLOR = 12
                 B-SendErpVpi:SENSITIVE  = TRUE.
      END.
      IF TG-ErpOrd = TRUE AND FI-OrdreErpTilExp:BGCOLOR = ? AND (canfindElogg("Ordre","ERP")) THEN DO:
          ASSIGN FI-OrdreErpTilExp:BGCOLOR = 12
                 B-SendErpOrd:SENSITIVE  = TRUE.
      END.
      IF TG-ErpFin = TRUE AND FI-FinansErpTilExp:BGCOLOR = ? AND (
                                                                  canfindElogg("Bokforingsbilag","ERP") OR canfindElogg("FakturaHode","FAKTAUTO") OR 
                                                                  canfindElogg("Bonghode","KONTAUTO") 
                                                                 ) THEN DO:
          ASSIGN FI-FinansErpTilExp:BGCOLOR = 12
                 B-SendErpFin:SENSITIVE  = TRUE.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerMedlemWeb wWin 
PROCEDURE KontrollerMedlemWeb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME FRAME-Eksterne:
      IF TG-MedlemWeb = TRUE AND FI-MedlemTilWeb:BGCOLOR = ? AND (canfindElogg( "Medlem","WEBINIT")) THEN DO:
          ASSIGN FI-MedlemTilWeb:BGCOLOR = 12
                 B-SendMedlemWeb:SENSITIVE  = TRUE.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerMix wWin 
PROCEDURE KontrollerMix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* DEAKTIVERING */
    FOR EACH MixMatchHode WHERE MixMatchHode.MixAktiver = TRUE AND
                                MixMatchHode.TilDato < TODAY:
        ASSIGN MixMatchHode.MixAktiver        = FALSE
               MixMatchHode.KlarTilAktivering = FALSE.
    END.
    FOR EACH MixMatchHode WHERE MixMatchHode.KlarTilAktivering  = TRUE  AND
                                MixMatchHode.MixAktiver         = FALSE AND
                                MixMatchHode.FraDato           <= TODAY AND
                                MixMatchHode.TilDato           >= TODAY:
        ASSIGN MixMatchHode.MixAktiver = TRUE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerOrdHK wWin 
PROCEDURE KontrollerOrdHK :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME FRAME-Eksterne:
      IF TG-ButikkOrd = TRUE AND canfindElogg("ORDHK","POS") THEN DO:
          ASSIGN FI-OrdreButikkTilExp:BGCOLOR = 12
                 B-SendButikkOrd:SENSITIVE  = TRUE.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerPkSdl wWin 
PROCEDURE KontrollerPkSdl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME FRAME-Eksterne:
      IF TG-ButikkPkSdl = TRUE AND canfindElogg("PKSDLHode","POS") THEN DO:
          ASSIGN FI-PkSdlButikkTilExp:BGCOLOR = 12
                 B-SendButikkPkSdl:SENSITIVE  = TRUE.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerPrisko wWin 
PROCEDURE KontrollerPrisko :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF CAN-FIND(FIRST prisko WHERE PrisKo.AktiveresDato < TODAY) OR
       CAN-FIND(FIRST prisko WHERE PrisKo.AktiveresDato = TODAY AND 
                                   PrisKo.AktiveresTid < TIME) THEN DO:
        RUN StartPriskoOppdat.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerSupOrdTilMail wWin 
PROCEDURE KontrollerSupOrdTilMail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME FRAME-Eksterne:
      IF TG-SupOrdTilMail = TRUE AND FI-SupOrdTilMail:BGCOLOR = ? AND 
          canfindElogg( "plListeHode","MAILSUPORD") THEN DO:
          ASSIGN FI-SupOrdTilMail:BGCOLOR = 12
                 B-SendSupOrdTilMail:SENSITIVE  = TRUE.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerWebBut wWin 
PROCEDURE KontrollerWebBut :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME FRAME-Eksterne:
      IF TG-WebBut = TRUE AND 
          (
           canfindElogg("ArtBas","WEBBUTARTINFO") OR
           canfindElogg("ArtBas","WebBut") OR
           canfindElogg("Avdeling","WebBut") OR
           canfindElogg("HuvGr","WebBut") OR
           canfindElogg("VarGr","WebBut") OR
           canfindElogg("Kategori","WebBut") OR
           canfindElogg("Aktivitet","WebBut") OR
           canfindElogg("LevBas","WebBut") OR
           canfindElogg("Produsent","WebBut") OR
           canfindElogg("Varemerke","WebBut") OR
           canfindElogg("Sesong","WebBut") OR
           canfindElogg("Material","WebBut") OR 
           canfindElogg("Farg","WebBut") OR 
           canfindElogg("Lager","WebBut") OR
           canfindElogg("StrKonv","WebBut") OR
           canfindElogg("Moms","WebBut") OR 
           canfindElogg("VgKat","WebBut") OR 
           canfindElogg("VgAkt","WebBut") OR 
           canfindElogg("VgKundeGrpRabatt","WebBut") OR 
           canfindElogg("Kunde","WebBut") OR 
           canfindElogg("KundeGruppe","WebBut") OR 
           canfindElogg("KundeSaldo","WebBut") OR 
           canfindElogg("KundeKort","WebBut") OR 
           canfindElogg("Medlem","WebBut") OR 
           canfindElogg("MedlemsGruppe","WebBut") OR 
           canfindElogg("MedlemSaldo","WebBut") OR 
           canfindElogg("MedlemsKort","WebBut") OR 
           canfindElogg("StrType","WebBut") OR  
           canfindElogg("Hovedkategori","WebBut") OR  
           canfindElogg("Underkategori","WebBut") OR  
           canfindElogg("Klack","WebBut") OR  
           canfindElogg("Handtering","WebBut") OR  
           canfindElogg("Ovandel","WebBut") OR  
           canfindElogg("Slitsula","WebBut") OR  
           canfindElogg("Innersula","WebBut") OR  
           canfindElogg("Anv-Kod","WebBut") OR  
           canfindElogg("Last-Sko","WebBut") OR  
           canfindElogg("Regnskapsavdeling","WebBut") OR  
           canfindElogg("KOrdreHode","WebBut")                       
           ) THEN DO:
          ASSIGN FI-WebButTilWeb:BGCOLOR = 12
                 B-SendWebBut:SENSITIVE  = TRUE.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReadMessage wWin 
PROCEDURE ReadMessage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshDistBrowse wWin 
PROCEDURE RefreshDistBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CLOSE QUERY BR-File.
    {&OPEN-QUERY-BR-File}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectPage wWin 
PROCEDURE selectPage :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER piPageNum AS INTEGER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT piPageNum).
   B-Oppdater:HIDDEN IN FRAME fmain = piPageNum <> 4.
  /* Code placed here will execute AFTER standard behavior.    */
  CASE piPageNum:
      WHEN 1 THEN
          FRAME FRAME-Para:MOVE-TO-TOP().
      WHEN 2 THEN
          FRAME FRAME-Filinfo:MOVE-TO-TOP().
      WHEN 3 THEN
          FRAME FRAME-Eksterne:MOVE-TO-TOP().
      WHEN 4 THEN
          FRAME FRAME-Fildistr:MOVE-TO-TOP().
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetDatoTidColor wWin 
PROCEDURE SetDatoTidColor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcDatoTid AS CHARACTER  NO-UNDO.
  DO WITH FRAME FRAME-Para:
      
      ASSIGN FI-VareDatoTid:BGCOLOR     = INT(STRING(FI-VareDatoTid:SCREEN-VALUE     = ipcDatoTid,"10/15"))
             FI-KKampDatoTid:BGCOLOR    = INT(STRING(FI-KKampDatoTid:SCREEN-VALUE    = ipcDatoTid,"10/15"))
             FI-VarGrDatoTid:BGCOLOR    = INT(STRING(FI-VarGrDatoTid:SCREEN-VALUE    = ipcDatoTid,"10/15"))
             FI-KasValutaDatoTid:BGCOLOR   = INT(STRING(FI-KasValutaDatoTid:SCREEN-VALUE   = ipcDatoTid,"10/15"))
             FI-KassererDatoTid:BGCOLOR = INT(STRING(FI-KassererDatoTid:SCREEN-VALUE = ipcDatoTid,"10/15"))
             FI-SelgerDatoTid:BGCOLOR   = INT(STRING(FI-SelgerDatoTid:SCREEN-VALUE   = ipcDatoTid,"10/15"))
             FI-KundeDatoTid:BGCOLOR    = INT(STRING(FI-KundeDatoTid:SCREEN-VALUE    = ipcDatoTid,"10/15"))
             FI-FargerDatoTid:BGCOLOR   = INT(STRING(FI-FargerDatoTid:SCREEN-VALUE   = ipcDatoTid,"10/15"))
             FI-StrKonvDatoTid:BGCOLOR  = INT(STRING(FI-StrKonvDatoTid:SCREEN-VALUE  = ipcDatoTid,"10/15"))
             FI-StrTypeDatoTid:BGCOLOR  = INT(STRING(FI-StrTypeDatoTid:SCREEN-VALUE  = ipcDatoTid,"10/15"))
             FI-TeksterDatoTid:BGCOLOR = INT(STRING(FI-TeksterDatoTid:SCREEN-VALUE = ipcDatoTid,"10/15"))
             FI-GarantiDatoTid:BGCOLOR = INT(STRING(FI-GarantiDatoTid:SCREEN-VALUE = ipcDatoTid,"10/15"))
             FI-ButikerDatoTid:BGCOLOR = INT(STRING(FI-ButikerDatoTid:SCREEN-VALUE = ipcDatoTid,"10/15")).
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkAktiv wWin 
PROCEDURE SjekkAktiv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cIntervall AS CHAR NO-UNDO.
  DEF VAR iInt1      AS INT  NO-UNDO.
  DEF VAR iInt2      AS INT  NO-UNDO.
  DEF VAR cOK        AS CHAR NO-UNDO.

  {syspara.i 200 3 4 cIntervall}.

  cIntervall = TRIM(cIntervall).

  /* Parameter ikke angitt, eller er satt opp feil. Den skal kjøre hele døgnet. */
  ERROR_BLOKK:
  DO:
      IF (NUM-ENTRIES(cIntervall) < 2 OR cIntervall = '') 
          THEN cOk = 'OK'.
      ELSE DO:
          ASSIGN iInt1 = INT(ENTRY(1,cIntervall)) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN 
          DO:
              cOk = 'OK'.
              LEAVE ERROR_BLOKK.
          END.
          ASSIGN iInt2 = INT(ENTRY(2,cIntervall)) NO-ERROR.
          IF ERROR-STATUS:ERROR THEN 
          DO:
              cOk = 'OK'.
              LEAVE ERROR_BLOKK.
          END.
          IF iInt2 < iInt1 THEN cOk = 'OK'.
      END.
  END. /* ERROR_BLOKK */

  /* Sjekker om vi er i aktivt tidsintervallet */
  IF cOk = '' THEN
  DO:
      IF (TIME >= iInt1 AND TIME <= iInt2) THEN 
          cOk = 'OK'.
      ELSE 
          cOk = 'DEAKTIVERT til ' + STRING(iInt1,'HH:MM:SS').
  END.

  /* Viser resultatet på skjermen. */
  IF cOk = 'OK' THEN
      ASSIGN 
      FI-Aktiv:SCREEN-VALUE IN FRAME fMain = ''.
  ELSE 
      ASSIGN 
      FI-Aktiv:SCREEN-VALUE IN FRAME fMain = cOk.

  /* Returnerer resultat */
  RETURN cOk.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapELoggAlle wWin 
PROCEDURE SkapELoggAlle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cTabell AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cType AS CHARACTER  NO-UNDO. /* 'ALLE' eller 'KLARGJOR' */

    FIND ELogg WHERE ELogg.TabellNavn     = cTabell AND
                     ELogg.EksterntSystem = "POS"   AND
                     ELogg.Verdier        = cType NO-ERROR. 
    IF NOT AVAIL ELogg THEN DO:
        CREATE ELogg.
        ASSIGN ELogg.TabellNavn     = cTabell
               ELogg.EksterntSystem = "POS"   
               ELogg.Verdier        = cType.
    END.

    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
    
    RUN KontrollerElogg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapELoggAlleErp wWin 
PROCEDURE SkapELoggAlleErp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cTabell AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cType AS CHARACTER  NO-UNDO. /* 'ALLE' eller 'KLARGJOR' */
    FIND ELogg WHERE ELogg.TabellNavn     = cTabell AND
                     ELogg.EksterntSystem = "ERP"   AND
                     ELogg.Verdier        = cType NO-ERROR. 
    IF NOT AVAIL ELogg THEN DO:
        CREATE ELogg.
        ASSIGN ELogg.TabellNavn     = cTabell
               ELogg.EksterntSystem = "ERP"   
               ELogg.Verdier        = cType.
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
    RUN KontrollerErp.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapELoggAlleWebBut wWin 
PROCEDURE SkapELoggAlleWebBut :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR pbOk AS LOG NO-UNDO.

MESSAGE "Skal også artikler initieres?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
    UPDATE pbOk.

IF pbOk = ? THEN RETURN.

/* Rått og brutalt :) */
FOR EACH Farg EXCLUSIVE-LOCK:
    Farg.ETid = TIME.
END.
/*
FOR EACH StrType EXCLUSIVE-LOCK:
    StrType.ETid = TIME.
END.
*/
FOR EACH Avdeling EXCLUSIVE-LOCK:
    Avdeling.ETid = TIME.
END.
FOR EACH HuvGr EXCLUSIVE-LOCK:
    HuvGr.eTid = TIME.
END.
FOR EACH VarGr EXCLUSIVE-LOCK:
    VarGr.ETid = TIME.
END.
FOR EACH Kategori EXCLUSIVE-LOCK:
    Kategori.ETid = TIME.
END.
FOR EACH Aktivitet EXCLUSIVE-LOCK:
    Aktivitet.ETid = TIME.
END.
FOR EACH LevBas EXCLUSIVE-LOCK:
    LevBas.ETid = TIME.
END.
FOR EACH Produsent EXCLUSIVE-LOCK:
    Produsent.ETid = TIME.
END.
FOR EACH Varemerke EXCLUSIVE-LOCK:
    Varemerke.ETid = TIME.
END.
FOR EACH Sasong EXCLUSIVE-LOCK:
    Sasong.ETid = TIME.
END.
FOR EACH Material EXCLUSIVE-LOCK:
    Material.ETid = TIME.
END.
FOR EACH StrKonv EXCLUSIVE-LOCK:
    StrKonv.ETid = TIME.
END.
FOR EACH Moms EXCLUSIVE-LOCK:
        Moms.ETid = TIME.
END.
FOR EACH VgKat EXCLUSIVE-LOCK:
    VgKat.ETid = TIME.
END.
FOR EACH Hovedkategori EXCLUSIVE-LOCK:
    Hovedkategori.ETid = TIME.
END.
FOR EACH Underkategori EXCLUSIVE-LOCK:
    underkategori.Etid = TIME.
END.
FOR EACH VgAkt EXCLUSIVE-LOCK:
    VgAkt.ETid = TIME.
END.
FOR EACH VgKundeGrpRabatt EXCLUSIVE-LOCK:
    VgKundeGrpRabatt.ETid = TIME.
END.
FOR EACH Kunde EXCLUSIVE-LOCK WHERE
  Kunde.WebKunde = TRUE:
    Kunde.ETid = TIME.
  FOR EACH KundeKort EXCLUSIVE-LOCK WHERE
    KundeKort.KundeNr = Kunde.KundeNr:
    KundeKort.ETid = TIME.    
  END. 
  FOR EACH KundeSaldo EXCLUSIVE-LOCK WHERE
    KundeSaldo.KundeNr = Kunde.KundeNr:
            KundeSaldo.ETid = TIME.    
  END.
END.
FOR EACH KundeGruppe EXCLUSIVE-LOCK:
    KundeGruppe.ETid = TIME.
END.
FOR EACH Medlem EXCLUSIVE-LOCK:
    Medlem.ETid = TIME.
  FOR EACH MedlemsKort EXCLUSIVE-LOCK WHERE
    MedlemsKort.MedlemsNr = Medlem.MedlemsNr:
    MedlemsKort.ETid = TIME.    
  END. 
  FOR EACH MedlemSaldo EXCLUSIVE-LOCK WHERE
    MedlemSaldo.MedlemsNr = Medlem.MedlemsNr:
    MedlemSaldo.ETid = TIME.    
  END.
END.
FOR EACH MedlemsGruppe EXCLUSIVE-LOCK:
    MedlemsGruppe.ETid = TIME.
END.

FOR EACH Klack EXCLUSIVE-LOCK:
    Klack.ETid = TIME.
END.
FOR EACH Handtering EXCLUSIVE-LOCK:
    Handtering.ETid = TIME.
END.
FOR EACH Slitsula EXCLUSIVE-LOCK:
    Slitsula.ETid = TIME.
END.
FOR EACH Ovandel EXCLUSIVE-LOCK:
    Ovandel.ETid = TIME.
END.
FOR EACH Innersula EXCLUSIVE-LOCK:
    Innersula.ETid = TIME.
END.
FOR EACH Last-Sko EXCLUSIVE-LOCK:
    Last-Sko.ETid = TIME.
END.
FOR EACH Regnskapsavdeling EXCLUSIVE-LOCK:
    Regnskapsavdeling.ETid = TIME.
END.


IF pbOk THEN
FOR EACH ArtBas EXCLUSIVE-LOCK WHERE
    ArtBas.WebButikkArtikkel = TRUE:
    ArtBas.ETid = TIME.
    FOR EACH Lager OF ArtBas EXCLUSIVE-LOCK:
        Lager.ETid = TIME.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettErpFiler wWin 
PROCEDURE SlettErpFiler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF TG-ErpVpi = FALSE THEN DO:
        FOR EACH ELogg WHERE ELogg.TabellNavn     = "ArtBas" AND
                             ELogg.EksterntSystem = "ERP":
            DELETE ELogg.
        END.
    END.
    IF TG-ErpFin = FALSE THEN DO:
        FOR EACH ELogg WHERE ELogg.TabellNavn     = "Bokforingsbilag" AND
                             ELogg.EksterntSystem = "ERP":
            DELETE ELogg.
        END.
    END.
    IF TG-ErpOrd = FALSE THEN DO:
        FOR EACH ELogg WHERE ELogg.TabellNavn     = "Ordre" AND
                             ELogg.EksterntSystem = "ERP":
            DELETE ELogg.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettHKord wWin 
PROCEDURE SlettHKord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH ELogg WHERE ELogg.Tabellnavn = "ORDHK" AND
                         ELogg.EksterntSystem = "POS":
        DELETE ELogg.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettSendToWooComm wWin 
PROCEDURE SlettSendToWooComm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH sendtowoocomm:
        IF sendtowoocomm.fetched = TRUE AND DATE(sendtowoocomm.fetcheddt) < TODAY - 2 THEN
            DELETE sendtowoocomm.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartEksport wWin 
PROCEDURE StartEksport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 {starteksport.i}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartEksportButikk wWin 
PROCEDURE StartEksportButikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cSend AS CHARACTER  NO-UNDO.
  DEF VAR iCount          AS INTE  NO-UNDO.
  DEF VAR cXXFiler      AS CHAR  NO-UNDO.
  DEF VAR lOverfort  AS LOGI  NO-UNDO.
  DEF VAR cLoadedJpg AS CHAR  NO-UNDO.
  DEF VAR cLoadedIcon     AS CHAR  NO-UNDO.
  DEF VAR cToolTip   AS CHAR  NO-UNDO.
  DEF VAR lSendError AS LOGI  NO-UNDO.
  DEF VAR ocRetur    AS CHAR NO-UNDO.

  ASSIGN lTimerOff = NOT chPSTimer:ENABLED.
  DO WITH FRAME FRAME-Eksterne:
      DO WITH FRAME fMain:
          ASSIGN cLoadedJpg = IMAGE-1:IMAGE.
          IMAGE-1:LOAD-IMAGE(".\icon\yellowlight.jpg").
          IF lIconExist THEN DO:
               cLoadedIcon = {&WINDOW-NAME}:ICON.
              {&WINDOW-NAME}:LOAD-ICON(cYellowIcon) NO-ERROR.
          END.
      END.
      {sww.i}
      ASSIGN cDatoTid = STRING(TIME,"HH:MM").

      /* Eksport av ordre til butikk */
      IF TG-ButikkOrd = TRUE AND canfindElogg("ORDHK","POS") THEN ORDHK: DO:
          IF cSend <> "OrdHK" AND lTimerOff THEN
              LEAVE ORDHK.
          IF cOrdButikkEksportRutine = "" OR SEARCH(cOrdButikkEksportRutine)  = ? THEN DO:
              ASSIGN lSendError = TRUE
                     cToolTip   = "Feil eksportrutine".
              setError(FI-ButikkOrdError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              LEAVE ORDHK.
          END.
          ELSE DO:
              RUN VALUE(cOrdButikkEksportRutine) (OUTPUT ocRetur) NO-ERROR.
              IF ERROR-STATUS:ERROR THEN DO:
                  ASSIGN lSendError = TRUE.
                  IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
                      ERROR-STATUS:GET-NUMBER(1).
                      cTooltip = ERROR-STATUS:GET-MESSAGE(1).
                  END.
                  ELSE
                      ASSIGN cToolTip = "Feil i rutine: " + cOrdButikkEksportRutine.
                  setError(FI-ButikkOrdError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE IF NOT ocRetur BEGINS "OK" THEN DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = TRUE.
                  setError(FI-ButikkOrdError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = FALSE
                         FI-AntButikkOrd = IF NUM-ENTRIES(cTooltip) = 2 THEN INT(ENTRY(2,cTooltip)) ELSE 0
                         FI-OrdreButikkDatoTid:SCREEN-VALUE = STRING(TIME,"HH:MM").
                  setError(FI-ButikkOrdError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
          END.
          ASSIGN FI-OrdreButikkTilExp:BGCOLOR  = ?
                 B-SendButikkOrd:SENSITIVE     = FALSE
                 FI-AntButikkOrd:SCREEN-VALUE  = STRING(FI-AntButikkOrd).
      END. /* ORDHK */

      /* Eksport av pakksedler til butikk */
      IF TG-ButikkPkSdl = TRUE AND canfindElogg("PKSDLHode","POS") THEN PKSDL: DO:
          IF cSend <> "PKSDL" AND lTimerOff THEN
              LEAVE PKSDL.
          IF cPkSdlButikkEksportRutine = "" OR SEARCH(cPkSdlButikkEksportRutine)  = ? THEN DO:
              ASSIGN lSendError = TRUE
                     cToolTip   = "Feil eksportrutine for pakkseddler".
              setError(FI-ButikkPkSdlError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              LEAVE PKSDL.
          END.
          ELSE DO:
              RUN VALUE(cPkSdlButikkEksportRutine) (OUTPUT ocRetur) NO-ERROR.
              IF ERROR-STATUS:ERROR THEN DO:
                  ASSIGN lSendError = TRUE.
                  IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
                      ERROR-STATUS:GET-NUMBER(1).
                      cTooltip = ERROR-STATUS:GET-MESSAGE(1).
                  END.
                  ELSE
                      ASSIGN cToolTip = "Feil i rutine: " + cPkSdlButikkEksportRutine.
                  setError(FI-ButikkPkSdlError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE IF NOT ocRetur BEGINS "OK" THEN DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = TRUE.
                  setError(FI-ButikkPkSdlError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = FALSE
                         FI-AntButikkPkSdl = IF NUM-ENTRIES(cTooltip) = 2 THEN INT(ENTRY(2,cTooltip)) ELSE 0
                         FI-PkSdlButikkDatoTid:SCREEN-VALUE = STRING(TIME,"HH:MM").
                  setError(FI-ButikkPkSdlError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
          END.
          ASSIGN FI-PkSdlButikkTilExp:BGCOLOR  = ?
                 B-SendButikkPkSdl:SENSITIVE     = FALSE
                 FI-AntButikkPkSdl:SCREEN-VALUE  = STRING(FI-AntButikkPkSdl).
      END. /* PKSDL */

  END.
  {swn.i}
  IMAGE-1:LOAD-IMAGE(cLoadedJpg).
  IF lIconExist THEN
      {&WINDOW-NAME}:LOAD-ICON(cLoadedIcon) NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartEksportErp wWin 
PROCEDURE StartEksportErp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cSend AS CHARACTER  NO-UNDO.
  DEF VAR iCount          AS INTE  NO-UNDO.
  DEF VAR cXXFiler      AS CHAR  NO-UNDO.
  DEF VAR lOverfort  AS LOGI  NO-UNDO.
  DEF VAR cLoadedJpg AS CHAR  NO-UNDO.
  DEF VAR cLoadedIcon     AS CHAR  NO-UNDO.
  DEF VAR cToolTip   AS CHAR  NO-UNDO.
  DEF VAR lSendError AS LOGI  NO-UNDO.
  DEF VAR ocRetur    AS CHAR NO-UNDO.

  ASSIGN lTimerOff = NOT chPSTimer:ENABLED.
  DO WITH FRAME FRAME-Eksterne:
      DO WITH FRAME fMain:
          ASSIGN cLoadedJpg = IMAGE-1:IMAGE.
          IMAGE-1:LOAD-IMAGE(".\icon\yellowlight.jpg").
          IF lIconExist THEN DO:
               cLoadedIcon = {&WINDOW-NAME}:ICON.
              {&WINDOW-NAME}:LOAD-ICON(cYellowIcon) NO-ERROR.
          END.
      END.
      /*{sww.i}*/
      ASSIGN cDatoTid = STRING(TIME,"HH:MM").

      /* Overfør til ERP - "wtmpartbas.w" */
      IF TG-ErpVpi = TRUE AND (canfindElogg("ArtBas","ERP")) THEN 
      ERPVPI: 
      DO:
          FI-AntErpVpi:SCREEN-VALUE = "0".
          IF cSend <> "VPI" AND lTimerOff THEN
              LEAVE ERPVPI.
          IF cVpiEksportRutine = "" OR SEARCH(cVpiEksportRutine)  = ? THEN DO:
              ASSIGN lSendError = TRUE
                     cToolTip   = "Feil eksportrutine".
              setError(FI-ErpVpiError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              LEAVE ERPVPI.
          END.
          ELSE DO:
              RUN VALUE(cVpiEksportRutine) (OUTPUT ocRetur) NO-ERROR.
              IF ERROR-STATUS:ERROR THEN DO:
                  ASSIGN lSendError = TRUE.
                  IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
                      ERROR-STATUS:GET-NUMBER(1).
                      cTooltip = ERROR-STATUS:GET-MESSAGE(1).
                  END.
                  ELSE
                      ASSIGN cToolTip = "Feil i rutine: " + cVpiEksportRutine.
                  setError(FI-ErpVpiError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE IF NOT ocRetur BEGINS "OK" THEN DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = TRUE.
                  setError(FI-ErpVpiError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = FALSE
                         FI-AntErpVpi = IF NUM-ENTRIES(cTooltip) = 2 THEN INT(ENTRY(2,cTooltip)) ELSE 0
                         FI-VareErpDatoTid:SCREEN-VALUE = STRING(TIME,"HH:MM").
                  setError(FI-ErpVpiError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
          END.
          ASSIGN FI-VareErpTilExp:BGCOLOR     = ?
                 B-SendErpVpi:SENSITIVE      = FALSE.
                 FI-AntErpVpi:SCREEN-VALUE  = STRING(FI-AntErpVpi).
      END.

      /* Eksport av varebok til Visma Global - fra "varebokhode.w". */
      IF TG-ErpVpi = TRUE AND (canfindElogg("VarebokVPI","ERP")) THEN 
      ERPVPI: 
      DO:
          FI-AntErpVpi:SCREEN-VALUE = "0".
          IF cSend <> "VPI" AND lTimerOff THEN
              LEAVE ERPVPI.
          IF cVpiVareEksportRutine = "" OR SEARCH(cVpiVareEksportRutine)  = ? THEN DO:
              ASSIGN lSendError = TRUE
                     cToolTip   = "Feil eksportrutine".
              setError(FI-ErpVpiError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              LEAVE ERPVPI.
          END.
          ELSE DO:
              RUN VALUE(cVpiVareEksportRutine) (OUTPUT ocRetur) NO-ERROR.
              IF ERROR-STATUS:ERROR THEN DO:
                  ASSIGN lSendError = TRUE.
                  IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
                      ERROR-STATUS:GET-NUMBER(1).
                      cTooltip = ERROR-STATUS:GET-MESSAGE(1).
                  END.
                  ELSE
                      ASSIGN cToolTip = "Feil i rutine: " + cVpiVareEksportRutine.
                  setError(FI-ErpVpiError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE IF NOT ocRetur BEGINS "OK" THEN DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = TRUE.
                  setError(FI-ErpVpiError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = FALSE
                         FI-AntErpVpi = IF NUM-ENTRIES(cTooltip) = 2 THEN INT(ENTRY(2,cTooltip)) ELSE 0
                         FI-VareErpDatoTid:SCREEN-VALUE = STRING(TIME,"HH:MM").
                  setError(FI-ErpVpiError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
          END.
          ASSIGN FI-VareErpTilExp:BGCOLOR     = ?
                 B-SendErpVpi:SENSITIVE      = FALSE.
                 FI-AntErpVpi:SCREEN-VALUE  = STRING(FI-AntErpVpi).
      END.

      IF TG-ErpOrd = TRUE AND (canfindElogg("Ordre","ERP")) THEN 
      ERPORD: 
      DO:
          IF cSend <> "ORD" AND lTimerOff THEN
              LEAVE ERPORD.
          IF cOrdEksportRutine = "" OR SEARCH(cOrdEksportRutine)  = ? THEN DO:
              ASSIGN lSendError = TRUE
                     cToolTip   = "Feil eksportrutine".
              setError(FI-ErpOrdError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              LEAVE ERPORD.
          END.
          ELSE DO:
              RUN VALUE(cOrdEksportRutine) (OUTPUT ocRetur) NO-ERROR.
              IF ERROR-STATUS:ERROR THEN DO:
                  ASSIGN lSendError = TRUE.
                  IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
                      ERROR-STATUS:GET-NUMBER(1).
                      cTooltip = ERROR-STATUS:GET-MESSAGE(1).
                  END.
                  ELSE
                      ASSIGN cToolTip = "Feil i rutine: " + cOrdEksportRutine.
                  setError(FI-ErpOrdError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE IF NOT ocRetur BEGINS "OK" THEN DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = TRUE.
                  setError(FI-ErpOrdError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = FALSE
                         FI-AntErpOrd = IF NUM-ENTRIES(cTooltip) = 2 THEN INT(ENTRY(2,cTooltip)) ELSE 0
                         FI-OrdreErpDatoTid:SCREEN-VALUE = STRING(TIME,"HH:MM").
                  setError(FI-ErpOrdError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
          END.
          ASSIGN FI-OrdreErpTilExp:BGCOLOR     = ?
                 B-SendErpOrd:SENSITIVE      = FALSE
                 FI-AntErpOrd:SCREEN-VALUE  = STRING(FI-AntErpOrd).
      END.

      IF TG-ErpFin = TRUE AND (canfindElogg("Bokforingsbilag","ERP")) THEN 
      ERPFIN: 
      DO:
          IF cSend <> "FIN" AND lTimerOff THEN
              LEAVE ERPFIN.
          IF cFinEksportRutine = "" OR SEARCH(cFinEksportRutine)  = ? THEN DO:
              ASSIGN lSendError = TRUE
                     cToolTip   = "Feil eksportrutine".
              setError(FI-ErpFinError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              LEAVE ERPFIN.
          END.
          ELSE DO:
              RUN VALUE(cFinEksportRutine) (OUTPUT ocRetur) NO-ERROR.
              IF ERROR-STATUS:ERROR THEN DO:
                  ASSIGN lSendError = TRUE.
                  IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
                      ERROR-STATUS:GET-NUMBER(1).
                      cTooltip = ERROR-STATUS:GET-MESSAGE(1).
                  END.
                  ELSE
                      ASSIGN cToolTip = "Feil i rutine: " + cFinEksportRutine.
                  setError(FI-ErpFinError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE IF NOT ocRetur BEGINS "OK" THEN DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = TRUE.
                  setError(FI-ErpFinError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = FALSE
                         FI-AntErpFin = IF NUM-ENTRIES(cTooltip) = 2 THEN INT(ENTRY(2,cTooltip)) ELSE 0
                         FI-FinansErpDatoTid:SCREEN-VALUE = STRING(TIME,"HH:MM").
                  setError(FI-ErpFinError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
          END.
          ASSIGN FI-FinansErpTilExp:BGCOLOR     = ?
                 B-SendErpFin:SENSITIVE      = FALSE
                 FI-AntErpFin:SCREEN-VALUE  = STRING(FI-AntErpFin).
      END.

      IF TG-ErpFin = TRUE AND (canfindElogg("FakturaHode","FAKTAUTO")) THEN 
      ERPFIN: 
      DO:
          IF cSend <> "FIN" AND lTimerOff THEN
              LEAVE ERPFIN.
          IF cFakEksportRutine = "" OR SEARCH(cFakEksportRutine)  = ? THEN DO:
              ASSIGN lSendError = TRUE
                     cToolTip   = "Feil fakturaeksportrutine".
              setError(FI-ErpFinError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              LEAVE ERPFIN.
          END.
          ELSE DO:
              RUN VALUE(cFakEksportRutine) (?,?,OUTPUT ocRetur) NO-ERROR.
              IF ERROR-STATUS:ERROR THEN DO:
                  ASSIGN lSendError = TRUE.
                  IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
                      ERROR-STATUS:GET-NUMBER(1).
                      cTooltip = ERROR-STATUS:GET-MESSAGE(1).
                  END.
                  ELSE
                      ASSIGN cToolTip = "Feil i rutine: " + cFakEksportRutine.
                  setError(FI-ErpFinError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE IF NOT ocRetur BEGINS "OK" THEN DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = TRUE.
                  setError(FI-ErpFinError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = FALSE
                         FI-AntErpFin = IF NUM-ENTRIES(cTooltip) = 2 THEN INT(ENTRY(2,cTooltip)) ELSE 0
                         FI-FinansErpDatoTid:SCREEN-VALUE = STRING(TIME,"HH:MM").
                  setError(FI-ErpFinError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
          END.
          ASSIGN FI-FinansErpTilExp:BGCOLOR     = ?
                 B-SendErpFin:SENSITIVE      = FALSE
                 FI-AntErpFin:SCREEN-VALUE  = STRING(FI-AntErpFin).
      END.

      IF TG-ErpFin = TRUE AND (canfindElogg("Bonghode","KONTAUTO")) THEN 
      ERPFIN: 
      DO:
          IF cSend <> "FIN" AND lTimerOff THEN
              LEAVE ERPFIN.
          IF cKontEksportRutine = "" OR SEARCH(cKontEksportRutine)  = ? THEN DO:
              ASSIGN lSendError = TRUE
                     cToolTip   = "Feil kontantsalgeksportrutine".
              setError(FI-ErpFinError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              LEAVE ERPFIN.
          END.
          ELSE DO:
              RUN VALUE(cKontEksportRutine) (OUTPUT ocRetur) NO-ERROR.
              IF ERROR-STATUS:ERROR THEN DO:
                  ASSIGN lSendError = TRUE.
                  IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
                      ERROR-STATUS:GET-NUMBER(1).
                      cTooltip = ERROR-STATUS:GET-MESSAGE(1).
                  END.
                  ELSE
                      ASSIGN cToolTip = "Feil i rutine: " + cKontEksportRutine.
                  setError(FI-ErpFinError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE IF NOT ocRetur BEGINS "OK" THEN DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = TRUE.
                  setError(FI-ErpFinError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = FALSE
                         FI-AntErpFin = IF NUM-ENTRIES(cTooltip) = 2 THEN INT(ENTRY(2,cTooltip)) ELSE 0
                         FI-FinansErpDatoTid:SCREEN-VALUE = STRING(TIME,"HH:MM").
                  setError(FI-ErpFinError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
          END.
          ASSIGN FI-FinansErpTilExp:BGCOLOR = ?
                 B-SendErpFin:SENSITIVE     = FALSE
                 FI-AntErpFin:SCREEN-VALUE  = STRING(FI-AntErpFin).
      END.
  END.
  /*{swn.i}*/
  IMAGE-1:LOAD-IMAGE(cLoadedJpg).
  IF lIconExist THEN
      {&WINDOW-NAME}:LOAD-ICON(cLoadedIcon) NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartEksportFerskvarevekt wWin 
PROCEDURE StartEksportFerskvarevekt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cSend AS CHARACTER  NO-UNDO.
  DEF VAR iCount          AS INTE  NO-UNDO.
  DEF VAR cXXFiler      AS CHAR  NO-UNDO.
  DEF VAR lOverfort  AS LOGI  NO-UNDO.
  DEF VAR cLoadedJpg AS CHAR  NO-UNDO.
  DEF VAR cLoadedIcon     AS CHAR  NO-UNDO.
  DEF VAR cToolTip   AS CHAR  NO-UNDO.
  DEF VAR lSendError AS LOGI  NO-UNDO.
  DEF VAR ocRetur    AS CHAR NO-UNDO.

  /*ASSIGN lTimerOff = NOT chPSTimer:ENABLED.*/
  DO WITH FRAME FRAME-Eksterne:
      /*
      DO WITH FRAME fMain:
          ASSIGN cLoadedJpg = IMAGE-1:IMAGE.
          IMAGE-1:LOAD-IMAGE(".\icon\yellowlight.jpg").
          IF lIconExist THEN DO:
               cLoadedIcon = {&WINDOW-NAME}:ICON.
              {&WINDOW-NAME}:LOAD-ICON(cYellowIcon) NO-ERROR.
          END.
      END.
      /*{sww.i}*/
      ASSIGN cDatoTid = STRING(TIME,"HH:MM").
      */
      IF  CAN-FIND (FIRST SysPara WHERE SysPAra.SysHId = 23 AND SysPara.SysGr = 1 AND SysPara.Parameter1 > '0') AND 
          (
           canfindElogg("ArtBas","FVEKT")
          )
          THEN 
      SENDFERSKVARE: 
      DO:
          IF cSend <> "FVEKT" AND lTimerOff THEN
              LEAVE SENDFERSKVARE.
          IF cFerskVareEksportRutine = "" OR SEARCH(cFerskVareEksportRutine)  = ? THEN DO:
              /*
              ASSIGN lSendError = TRUE
                     cToolTip   = "Feil eksportrutine for ferskvarevekt".
              setError(FI-WebButTilWeb:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              */
              LEAVE SENDFERSKVARE.
          END.

          ELSE DO:
              RUN VALUE(cFerskVareEksportRutine) NO-ERROR.
              /*
              IF ERROR-STATUS:ERROR THEN DO:
                  ASSIGN lSendError = TRUE.
                  IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
                      ERROR-STATUS:GET-NUMBER(1).
                      cTooltip = ERROR-STATUS:GET-MESSAGE(1).
                  END.
                  ELSE
                      ASSIGN cToolTip = "Feil i rutine: " + cWebButEksportRutine.
                  setError(FI-WebButTilWeb:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE IF NOT ocRetur BEGINS "OK" THEN DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = TRUE.
                  setError(FI-WebButTilWeb:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = FALSE
                         FI-AntWebBut = IF NUM-ENTRIES(cTooltip) = 2 THEN INT(ENTRY(2,cTooltip)) ELSE 0
                         FI-WebButDatoTid:SCREEN-VALUE = STRING(TIME,"HH:MM").
                  setError(FI-WebButError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              */
          END.
          /*
          ASSIGN FI-WebButTilWeb:BGCOLOR  = ?
                 B-SendWebBut:SENSITIVE     = FALSE
                 FI-AntWebBut:SCREEN-VALUE  = STRING(FI-AntWebBut).
          */
      END. /* SENDFERSKVARE */
      
  END.
  /*
  /*{swn.i}*/
  IMAGE-1:LOAD-IMAGE(cLoadedJpg).
  IF lIconExist THEN
      {&WINDOW-NAME}:LOAD-ICON(cLoadedIcon) NO-ERROR.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartEksportMedlemWeb wWin 
PROCEDURE StartEksportMedlemWeb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cSend AS CHARACTER  NO-UNDO.
  DEF VAR iCount          AS INTE  NO-UNDO.
  DEF VAR cXXFiler      AS CHAR  NO-UNDO.
  DEF VAR lOverfort  AS LOGI  NO-UNDO.
  DEF VAR cLoadedJpg AS CHAR  NO-UNDO.
  DEF VAR cLoadedIcon     AS CHAR  NO-UNDO.
  DEF VAR cToolTip   AS CHAR  NO-UNDO.
  DEF VAR lSendError AS LOGI  NO-UNDO.
  DEF VAR ocRetur    AS CHAR NO-UNDO.

  ASSIGN lTimerOff = NOT chPSTimer:ENABLED.
  DO WITH FRAME FRAME-Eksterne:
      DO WITH FRAME fMain:
          ASSIGN cLoadedJpg = IMAGE-1:IMAGE.
          IMAGE-1:LOAD-IMAGE(".\icon\yellowlight.jpg").
          IF lIconExist THEN DO:
               cLoadedIcon = {&WINDOW-NAME}:ICON.
              {&WINDOW-NAME}:LOAD-ICON(cYellowIcon) NO-ERROR.
          END.
      END.
      /*{sww.i}*/
      ASSIGN cDatoTid = STRING(TIME,"HH:MM").
      IF TG-MedlemWeb = TRUE AND canfindElogg("Medlem","WEBINIT") THEN 
      SENDMEDLEM: 
      DO:
          IF cSend <> "WEBINIT" AND lTimerOff THEN
              LEAVE SENDMEDLEM.
          IF cMedWebEksportRutine = "" OR SEARCH(cMedWebEksportRutine)  = ? THEN DO:
              ASSIGN lSendError = TRUE
                     cToolTip   = "Feil eksportrutine".
              setError(FI-MedlemTilWeb:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              LEAVE SENDMEDLEM.
          END.

          ELSE DO:
              RUN VALUE(cMedWebEksportRutine) (OUTPUT ocRetur) NO-ERROR.
              IF ERROR-STATUS:ERROR THEN DO:
                  ASSIGN lSendError = TRUE.
                  IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
                      ERROR-STATUS:GET-NUMBER(1).
                      cTooltip = ERROR-STATUS:GET-MESSAGE(1).
                  END.
                  ELSE
                      ASSIGN cToolTip = "Feil i rutine: " + cMedWebEksportRutine.
                  setError(FI-MedlemTilWeb:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE IF NOT ocRetur BEGINS "OK" THEN DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = TRUE.
                  setError(FI-MedlemTilWeb:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = FALSE
                         FI-AntMedlemWeb = IF NUM-ENTRIES(cTooltip) = 2 THEN INT(ENTRY(2,cTooltip)) ELSE 0
                         FI-MedlemWebDatoTid:SCREEN-VALUE = STRING(TIME,"HH:MM").
                  setError(FI-MedlemWebError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
          END.
          ASSIGN FI-MedlemTilWeb:BGCOLOR  = ?
                 B-SendMedlemWeb:SENSITIVE     = FALSE
                 FI-AntMedlemWeb:SCREEN-VALUE  = STRING(FI-AntMedlemWeb).
      END. /* SENDMEDLEM */
      
  END.
  /*{swn.i}*/
  IMAGE-1:LOAD-IMAGE(cLoadedJpg).
  IF lIconExist THEN
      {&WINDOW-NAME}:LOAD-ICON(cLoadedIcon) NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartEksportWebBut wWin 
PROCEDURE StartEksportWebBut :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cSend AS CHARACTER  NO-UNDO.
  DEF VAR iCount          AS INTE  NO-UNDO.
  DEF VAR cXXFiler      AS CHAR  NO-UNDO.
  DEF VAR lOverfort  AS LOGI  NO-UNDO.
  DEF VAR cLoadedJpg AS CHAR  NO-UNDO.
  DEF VAR cLoadedIcon     AS CHAR  NO-UNDO.
  DEF VAR cToolTip   AS CHAR  NO-UNDO.
  DEF VAR lSendError AS LOGI  NO-UNDO.
  DEF VAR ocRetur    AS CHAR NO-UNDO.

  ASSIGN lTimerOff = NOT chPSTimer:ENABLED.
  DO WITH FRAME FRAME-Eksterne:
      DO WITH FRAME fMain:
          ASSIGN cLoadedJpg = IMAGE-1:IMAGE.
          IMAGE-1:LOAD-IMAGE(".\icon\yellowlight.jpg").
          IF lIconExist THEN DO:
               cLoadedIcon = {&WINDOW-NAME}:ICON.
              {&WINDOW-NAME}:LOAD-ICON(cYellowIcon) NO-ERROR.
          END.
      END.
      /*{sww.i}*/
      ASSIGN cDatoTid = STRING(TIME,"HH:MM").
      IF TG-WebBut = TRUE AND
          (
           canfindElogg("ArtBas","WEBBUTARTINFO") OR
           canfindElogg("ArtBas","WebBut") OR
           canfindElogg("Avdeling","WebBut") OR
           canfindElogg("HuvGr","WebBut") OR
           canfindElogg("VarGr","WebBut") OR
           canfindElogg("Kategori","WebBut") OR
           canfindElogg("Aktivitet","WebBut") OR
           canfindElogg("LevBas","WebBut") OR
           canfindElogg("Produsent","WebBut") OR
           canfindElogg("Varemerke","WebBut") OR
           canfindElogg("Sesong","WebBut") OR
           canfindElogg("Material","WebBut") OR 
           canfindElogg("Farg","WebBut") OR 
           canfindElogg("Lager","WebBut") OR
           canfindElogg("StrKonv","WebBut") OR
           canfindElogg("Moms","WebBut") OR 
           canfindElogg("VgKat","WebBut") OR 
           canfindElogg("VgAkt","WebBut") OR
           canfindElogg("VgKundeGrpRabatt","WebBut") OR            
           canfindElogg("Kunde","WebBut") OR 
           canfindElogg("KundeGruppe","WebBut") OR 
           canfindElogg("KundeSaldo","WebBut") OR 
           canfindElogg("KundeKort","WebBut") OR 
           canfindElogg("Medlem","WebBut") OR 
           canfindElogg("MedlemsGruppe","WebBut") OR 
           canfindElogg("MedlemSaldo","WebBut") OR 
           canfindElogg("MedlemsKort","WebBut") OR                        
           canfindElogg("StrType","WebBut") OR                        
           canfindElogg("Hovedkategori","WebBut") OR                        
           canfindElogg("Underkategori","WebBut") OR                        
           canfindElogg("Klack","WebBut") OR  
           canfindElogg("Handtering","WebBut") OR  
           canfindElogg("Ovandel","WebBut") OR  
           canfindElogg("Slitsula","WebBut") OR  
           canfindElogg("Innersula","WebBut") OR  
           canfindElogg("Anv-Kod","WebBut") OR  
           canfindElogg("Last-Sko","WebBut") OR  
           canfindElogg("Regnskapsavdeling","WebBut") OR  
           canfindElogg("KOrdreHode","WebBut")                       
          )
          THEN 
      SENDWEBBUT: 
      DO:
          IF cSend <> "WEBBUT" AND lTimerOff THEN
              LEAVE SENDWEBBUT.
          IF cWebButEksportRutine = "" OR SEARCH(cWebButEksportRutine)  = ? THEN DO:
              ASSIGN lSendError = TRUE
                     cToolTip   = "Feil eksportrutine".
              setError(FI-WebButTilWeb:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              LEAVE SENDWEBBUT.
          END.

          ELSE DO:
              RUN VALUE(cWebButEksportRutine) (OUTPUT ocRetur) NO-ERROR.
              IF ERROR-STATUS:ERROR THEN DO:
                  ASSIGN lSendError = TRUE.
                  IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
                      ERROR-STATUS:GET-NUMBER(1).
                      cTooltip = ERROR-STATUS:GET-MESSAGE(1).
                  END.
                  ELSE
                      ASSIGN cToolTip = "Feil i rutine: " + cWebButEksportRutine.
                  setError(FI-WebButTilWeb:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE IF NOT ocRetur BEGINS "OK" THEN DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = TRUE.
                  setError(FI-WebButTilWeb:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = FALSE
                         FI-AntWebBut = IF NUM-ENTRIES(cTooltip) = 2 THEN INT(ENTRY(2,cTooltip)) ELSE 0
                         FI-WebButDatoTid:SCREEN-VALUE = STRING(TIME,"HH:MM").
                  setError(FI-WebButError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
          END.
          ASSIGN FI-WebButTilWeb:BGCOLOR  = ?
                 B-SendWebBut:SENSITIVE     = FALSE
                 FI-AntWebBut:SCREEN-VALUE  = STRING(FI-AntWebBut).
      END. /* SENDMEDLEM */
      
  END.
  /*{swn.i}*/
  IMAGE-1:LOAD-IMAGE(cLoadedJpg).
  IF lIconExist THEN
      {&WINDOW-NAME}:LOAD-ICON(cLoadedIcon) NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartPriskoOppdat wWin 
PROCEDURE StartPriskoOppdat :
DEFINE VARIABLE cLoadedJpg      AS CHARACTER  NO-UNDO.
DEF    VAR      cLoadedIcon     AS CHAR  NO-UNDO.
  DEFINE VARIABLE cOrgTxt         AS CHARACTER  NO-UNDO.
  DO WITH FRAME FRAME-Para:
      {sww.i}
     DO WITH FRAME fMain:
         ASSIGN cLoadedJpg = IMAGE-1:IMAGE
                cOrgTxt    = FI-Serverstatus:SCREEN-VALUE
                FI-Serverstatus:SCREEN-VALUE = "Prisoppdatering pågår"
                FI-Serverstatus:HIDDEN IN FRAME fMain = FALSE.
         IMAGE-1:LOAD-IMAGE(".\icon\yellowlight.jpg").
         IF lIconExist THEN DO:
              cLoadedIcon = {&WINDOW-NAME}:ICON.
             {&WINDOW-NAME}:LOAD-ICON(cYellowIcon) NO-ERROR.
         END.
         RUN FixWindowSize.
         RUN x-klargjorprisko.w (?).
         ASSIGN FI-Serverstatus:SCREEN-VALUE = cOrgTxt.
         IMAGE-1:LOAD-IMAGE(cLoadedJpg).
         IF lIconExist THEN
             {&WINDOW-NAME}:LOAD-ICON(cLoadedIcon) NO-ERROR.
     END.
     {swn.i}
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartStopServer wWin 
PROCEDURE StartStopServer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER lStart AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE iSek   AS INTEGER    NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    IF lStart = TRUE THEN DO:
        ASSIGN chPSTimer:ENABLED = TRUE
            .
    END.
    ELSE DO:
        ASSIGN chPSTimer:ENABLED         = FALSE
            .
    END.
  END.
  RUN ButtonEnaDis(NOT chPSTimer:ENABLED).
  IMAGE-1:LOAD-IMAGE(IF NOT chPSTimer:ENABLED THEN ".\icon\redlight.jpg" ELSE ".\icon\greenlight.jpg").
  IF lIconExist THEN DO:
      IF NOT chPSTimer:ENABLED THEN
          {&WINDOW-NAME}:LOAD-ICON(cRedIcon) NO-ERROR.
      ELSE
          {&WINDOW-NAME}:LOAD-ICON(cGreenIcon) NO-ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSupOrdTilMail wWin 
PROCEDURE StartSupOrdTilMail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cSend AS CHARACTER  NO-UNDO.
  DEF VAR iCount          AS INTE  NO-UNDO.
  DEF VAR cXXFiler      AS CHAR  NO-UNDO.
  DEF VAR lOverfort  AS LOGI  NO-UNDO.
  DEF VAR cLoadedJpg AS CHAR  NO-UNDO.
  DEF VAR cLoadedIcon     AS CHAR  NO-UNDO.
  DEF VAR cToolTip   AS CHAR  NO-UNDO.
  DEF VAR lSendError AS LOGI  NO-UNDO.
  DEF VAR ocRetur    AS CHAR NO-UNDO.

  ASSIGN lTimerOff = NOT chPSTimer:ENABLED.
  DO WITH FRAME FRAME-Eksterne:
      DO WITH FRAME fMain:
          ASSIGN cLoadedJpg = IMAGE-1:IMAGE.
          IMAGE-1:LOAD-IMAGE(".\icon\yellowlight.jpg").
          IF lIconExist THEN DO:
               cLoadedIcon = {&WINDOW-NAME}:ICON.
              {&WINDOW-NAME}:LOAD-ICON(cYellowIcon) NO-ERROR.
          END.
      END.
      /*{sww.i}*/
      ASSIGN cDatoTid = STRING(TIME,"HH:MM").

      /* Overfør til ERP - "wtmpartbas.w" */
      IF TG-SupOrdTilMail = TRUE AND (canfindElogg("plListeHode","MAILSUPORD")) THEN 
      ERPVPI: 
      DO:
          FI-AntSupOrdTilMail:SCREEN-VALUE = "0".
          IF cSend <> "MAILSUPORD" AND lTimerOff THEN
              LEAVE ERPVPI.
          IF cSendsupOrdTilMailRutine = "" OR SEARCH(cSendsupOrdTilMailRutine)  = ? THEN DO:
              ASSIGN lSendError = TRUE
                     cToolTip   = "Feil eksportrutine".
              setError(FI-SupOrdTilMailError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              LEAVE ERPVPI.
          END.
          ELSE DO:
              RUN VALUE(cSendsupOrdTilMailRutine) (OUTPUT ocRetur) NO-ERROR.
              IF ERROR-STATUS:ERROR THEN DO:
                  ASSIGN lSendError = TRUE.
                  IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
                      ERROR-STATUS:GET-NUMBER(1).
                      cTooltip = ERROR-STATUS:GET-MESSAGE(1).
                  END.
                  ELSE
                      ASSIGN cToolTip = "Feil i rutine: " + cSendsupOrdTilMailRutine.
                  setError(FI-SupOrdTilMailError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE IF NOT ocRetur BEGINS "OK" THEN DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = TRUE.
                  setError(FI-SupOrdTilMailError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
              ELSE DO:
                  ASSIGN cTooltip   = ocRetur
                         lSendError = FALSE
                         FI-AntSupOrdTilMail = IF NUM-ENTRIES(cTooltip) = 2 THEN INT(ENTRY(2,cTooltip)) ELSE 0
                         FI-SupOrdTilMailDatoTid:SCREEN-VALUE = STRING(TIME,"HH:MM").
                  setError(FI-SupOrdTilMailError:HANDLE,INPUT-OUTPUT lSendError,INPUT-OUTPUT cToolTip).
              END.
          END.
          ASSIGN FI-SupOrdTilMail:BGCOLOR     = ?
                 B-SendSupOrdTilMail:SENSITIVE     = FALSE.
                 FI-AntSupOrdTilMail:SCREEN-VALUE  = STRING(FI-AntSupOrdTilMail).
      END.

  END.
  /*{swn.i}*/
  IMAGE-1:LOAD-IMAGE(cLoadedJpg).
  IF lIconExist THEN
      {&WINDOW-NAME}:LOAD-ICON(cLoadedIcon) NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToggleOff wWin 
PROCEDURE ToggleOff :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME FRAME-Para:
       ASSIGN TG-ButikerAuto:CHECKED = FALSE 
              TG-FargerAuto:CHECKED = FALSE 
              TG-GarantiAuto:CHECKED = FALSE 
              TG-KasserereAuto:CHECKED = FALSE 
              TG-KasValutaAuto:CHECKED = FALSE 
              TG-KundeAuto:CHECKED = FALSE 
              TG-SelgereAuto:CHECKED = FALSE 
              TG-StrKonvAuto:CHECKED = FALSE 
              TG-StrTypeAuto:CHECKED = FALSE 
              TG-TeksterAuto:CHECKED = FALSE 
              TG-VareAuto:CHECKED = FALSE 
              TG-VarGrAuto:CHECKED = FALSE
              TG-KKampAuto:CHECKED = FALSE
              TG-Skjul:CHECKED = FALSE
              TG-SendPRSPos:CHECKED = FALSE
/*               FRAME FRAME-Para:SENSITIVE = FALSE    */
/*               FRAME FRAME-Filinfo:SENSITIVE = FALSE */
              TG-ButikerAuto:SENSITIVE = FALSE
              TG-FargerAuto:SENSITIVE = FALSE
              TG-GarantiAuto:SENSITIVE = FALSE
              TG-KasserereAuto:SENSITIVE = FALSE
              TG-KasValutaAuto:SENSITIVE = FALSE
              TG-KundeAuto:SENSITIVE = FALSE
              TG-SelgereAuto:SENSITIVE = FALSE
              TG-StrKonvAuto:SENSITIVE = FALSE
              TG-StrTypeAuto:SENSITIVE = FALSE
              TG-TeksterAuto:SENSITIVE = FALSE
              TG-VareAuto:SENSITIVE = FALSE
              TG-VarGrAuto:SENSITIVE = FALSE
              TG-KKampAuto:SENSITIVE = FALSE
              TG-Skjul:SENSITIVE = FALSE
              TG-SendPRSPos:SENSITIVE = FALSE
              B-ButikerAlle:SENSITIVE = FALSE
              B-FargeAlle:SENSITIVE = FALSE
              B-GarantiAlle:SENSITIVE = FALSE
              B-KasserereAlle:SENSITIVE = FALSE
              B-KasValutaAlle:SENSITIVE = FALSE
              B-KundeAlle:SENSITIVE = FALSE
              B-SelgereAlle:SENSITIVE = FALSE
              B-StrKonvAlle:SENSITIVE = FALSE
              B-StrTypeAlle:SENSITIVE = FALSE
              B-TeksterAlle:SENSITIVE = FALSE
              B-VareAlle:SENSITIVE = FALSE
              B-VargrAlle:SENSITIVE = FALSE.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION canfindElogg wWin 
FUNCTION canfindElogg RETURNS LOGICAL
  ( INPUT cTabell AS CHARACTER, INPUT cEksternt AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 RETURN CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn = cTabell AND ELogg.EksterntSystem = cEksternt).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getButliste wWin 
FUNCTION getButliste RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
      DEFINE VARIABLE cKasser AS CHARACTER   NO-UNDO.
      FOR EACH SkoTex.DistributeFileReceiver WHERE 
          DistributeFileReceiver.DistributeFileId = DistributeFile.Id NO-LOCK:
          cKasser = cKasser + (IF cKasser <> "" THEN "," ELSE "") + STRING(DistributeFileReceiver.Kassenr).
      END.
  RETURN cKasser.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFiltxt wWin 
FUNCTION getFiltxt RETURNS CHARACTER
  ( INPUT cFilNavn AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cReturTxt AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cInRad    AS CHARACTER  NO-UNDO.
  IF SEARCH(cFilNavn) <> ? THEN DO:
      INPUT FROM VALUE(cFilNavn).
      REPEAT:
          IMPORT UNFORMATTED cInrad.
          IF cInrad <> "" THEN
              ASSIGN cReturTxt = cReturTxt + (IF cReturTxt = "" THEN "" ELSE CHR(10)) +
                                 cInrad.
      END.
      INPUT CLOSE.
  END.
  RETURN cReturTxt.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setError wWin 
FUNCTION setError RETURNS CHARACTER
  ( INPUT fiHandle AS HANDLE,INPUT-OUTPUT lSendError AS LOGICAL,INPUT-OUTPUT cToolTip AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
           ASSIGN fiHandle:SCREEN-VALUE = STRING(lSendError,"ERROR/OK")
                  fiHandle:BGCOLOR      = IF lSendError THEN 12 ELSE 10
                  fiHandle:TOOLTIP      = cToolTip
                  lSendError            = FALSE
                  cToolTip              = "".

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION socketconnect wWin 
FUNCTION socketconnect RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 CREATE SOCKET hServer NO-ERROR.
/*  hServer:CONNECT("-H ken1lap3 -S 9101":U). */
 hServer:CONNECT(cConnect) NO-ERROR.
 IF hServer:CONNECTED() THEN DO:
     /* Detta är ett serverprogram */
     /* Om vi får kontakt så måste vi avsluta. Vi kan inte starta 2 instanser */
     hServer:DISCONNECT() NO-ERROR.
     DELETE OBJECT hServer NO-ERROR.
     RETURN TRUE.
 END.
 ELSE
     RETURN FALSE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION startup wWin 
FUNCTION startup RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 DEFINE VARIABLE lOkEnable AS LOGICAL    NO-UNDO.
 CREATE SERVER-SOCKET hServer. /* create the server socket and store the handle in hServer */

 /* ensure that the server can received connection requests. In this example, the server
    is using the localmachine and a service of 1234. Obviously the host and service
    can be replaced with any machine or service that you need */
 ASSIGN lOkEnable = hServer:ENABLE-CONNECTIONS(cConnect) NO-ERROR.

 IF lOkEnable THEN DO:
     /* when a client connects to this server, run the procedure "ConnectHandler" in THIS-PROCEDURE */
     hServer:SET-CONNECT-PROCEDURE("ConnectHandler":U,THIS-PROCEDURE).
 END.
 ELSE RETURN FALSE.
 RETURN TRUE. /* all's ok !*/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

