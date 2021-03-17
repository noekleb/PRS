&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  Forfatter:   SJ, 25.06.98
  Beskrivelse: Programinformasjon 
  Parametere:  
  Endringer:
  GUI-versjon av proginfo.p. Mye NYTTIG informasjon kan legges inn. 
  Foreløpig er programmet kun en "kopi" av proginfo.p.
  
  23.02.99 SJ Lagt inn startmappe, Progress EXE m/path, mer versjonsinfo
              og serienummer.
------------------------------------------------------------------------*/

CREATE WIDGET-POOL.
/*{shared.i &New = " "}*/
/* ***************************  Definitions  ************************** */

/* Parameter Definisjoner ---                                           */
/*
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
   DEF VAR wLokalIniFil      AS CHAR NO-UNDO INIT "VS90.INI".
   DEF VAR wMappeLokalIniFil AS CHAR NO-UNDO INIT "C:\WINDOWS".
&ELSE
   DEF SHARED VAR wLokalIniFil      AS CHAR NO-UNDO.
   DEF SHARED VAR wMappeLokalIniFil AS CHAR NO-UNDO.
&ENDIF
*/
DEF VAR wLokalIniFil      AS CHAR NO-UNDO INIT "VS90.INI".
DEF VAR wMappeLokalIniFil AS CHAR NO-UNDO INIT "C:\WINDOWS".

/* Preprossessor direktiver ---                                         */

/* Buffer og Temp-Table Definisjoner ---                                */

/* Lokale variabler ---                                                 */
def var retur-verdi as char initial "<avbryt>" no-undo.

define var m-ja              as logical no-undo.
define var m-i               as integer no-undo.
define var m-x               as character no-undo.
define var m-handle          as handle no-undo.
define var m-wh              as widget-handle no-undo.

DEFINE TEMP-TABLE tPrg NO-UNDO
   FIELD tNummer     AS INTE 
   FIELD tProgram    AS CHAR
   INDEX tNummer IS PRIMARY UNIQUE
             tNummer ASCENDING. 

DEFINE TEMP-TABLE tDb NO-UNDO
   FIELD tLdb    AS CHAR 
   FIELD tPdb    AS CHAR
   FIELD tVer    AS CHAR
   INDEX tLdb IS PRIMARY 
           tLdb ASCENDING. 

DEFINE TEMP-TABLE tPath NO-UNDO
   FIELD tNummer AS INTE 
   FIELD tPath   AS CHAR
   INDEX tNummer IS PRIMARY UNIQUE
             tNummer ASCENDING.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tPath tPrg tDb

/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 tPath.tNummer tPath.tPath   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3   
&Scoped-define SELF-NAME BROWSE-3
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH tPath NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY {&SELF-NAME} FOR EACH tPath NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 tPath
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 tPath


/* Definitions for BROWSE BROWSE-Prg                                    */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Prg tPrg.tNummer tPrg.tProgram   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Prg   
&Scoped-define SELF-NAME BROWSE-Prg
&Scoped-define QUERY-STRING-BROWSE-Prg FOR EACH tPrg NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-Prg OPEN QUERY {&SELF-NAME} FOR EACH tPrg NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-Prg tPrg
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Prg tPrg


/* Definitions for BROWSE BROWSE-tDb                                    */
&Scoped-define FIELDS-IN-QUERY-BROWSE-tDb tdb.tLdb tdb.tPdb tdb.tVer   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-tDb   
&Scoped-define SELF-NAME BROWSE-tDb
&Scoped-define QUERY-STRING-BROWSE-tDb FOR EACH tDb NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-tDb OPEN QUERY {&SELF-NAME} FOR EACH tDb NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-tDb tDb
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-tDb tDb


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-3}~
    ~{&OPEN-QUERY-BROWSE-Prg}~
    ~{&OPEN-QUERY-BROWSE-tDb}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-Prg B-Sprak BUTTON-1 BROWSE-tDb ~
BROWSE-3 Btn_OK Btn_Help RECT-1 RECT-2 RECT-24 RECT-3 RECT-4 RECT-5 RECT-6 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-Info FILL-IN-StartDir ~
FILL-IN-IniFil FILL-IN-SysIni FILL-IN-Exe FILL-IN-Para FILL-IN-Progress ~
FILL-IN-Serial FILL-IN-Opsys FILL-IN-Terminal FILL-IN-Brukerid ~
FILL-IN-LdNavn FILL-IN-Dictdb FILL-IN-FrameName FILL-IN-FrameDb ~
FILL-IN-FrameFile FILL-IN-FrameField FILL-IN-FrameValue 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CommandLine Dialog-Frame 
FUNCTION CommandLine RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Sprak 
     LABEL "Språk..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 14 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Lukk" 
     SIZE 14 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1 
     LABEL "&Sesjonsinnstillinger..." 
     SIZE 23 BY 1.1.

DEFINE BUTTON BUTTON-Persist 
     LABEL "&Persistente programmer..." 
     SIZE 28 BY 1.1.

DEFINE VARIABLE FILL-IN-Brukerid AS CHARACTER FORMAT "X(256)":U 
     LABEL "Br.navn" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Brukernavn"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-Dictdb AS CHARACTER FORMAT "X(256)":U 
     LABEL "DictDB" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-Exe AS CHARACTER FORMAT "X(256)":U 
     LABEL "Progress EXE" 
     VIEW-AS FILL-IN 
     SIZE 95 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-FrameDb AS CHARACTER FORMAT "X(256)":U 
     LABEL "DB" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-FrameField AS CHARACTER FORMAT "X(256)":U 
     LABEL "Field" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-FrameFile AS CHARACTER FORMAT "X(256)":U 
     LABEL "File" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-FrameName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-FrameValue AS CHARACTER FORMAT "X(256)":U 
     LABEL "Value" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-Info AS CHARACTER FORMAT "X(256)":U 
     LABEL "Status" 
     VIEW-AS FILL-IN 
     SIZE 95 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-IniFil AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lokal INI-fil" 
     VIEW-AS FILL-IN 
     SIZE 95 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-LdNavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Delsys" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-Opsys AS CHARACTER FORMAT "X(256)":U 
     LABEL "Opsys" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-Para AS CHARACTER FORMAT "X(256)":U 
     LABEL "Parametere" 
     VIEW-AS FILL-IN 
     SIZE 95 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-Progress AS CHARACTER FORMAT "X(256)":U 
     LABEL "Versjon" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-Serial AS CHARACTER FORMAT "X(256)":U 
     LABEL "Serienummer" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-StartDir AS CHARACTER FORMAT "X(256)":U 
     LABEL "Startmappe" 
     VIEW-AS FILL-IN 
     SIZE 95 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-SysIni AS CHARACTER FORMAT "X(256)":U 
     LABEL "Progress INI-fil" 
     VIEW-AS FILL-IN 
     SIZE 95 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-Terminal AS CHARACTER FORMAT "X(256)":U 
     LABEL "Terminal" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 5.95.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 5.95.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 152 BY 6.19.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 10.24.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67 BY 10.24.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 10.24.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY 5.95.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-3 FOR 
      tPath SCROLLING.

DEFINE QUERY BROWSE-Prg FOR 
      tPrg SCROLLING.

DEFINE QUERY BROWSE-tDb FOR 
      tDb SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 Dialog-Frame _FREEFORM
  QUERY BROWSE-3 DISPLAY
      tPath.tNummer FORMAT "zz9-"   LABEL "Nr"
      tPath.tPath   FORMAT "X(100)" LABEL " Path"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS SIZE 40 BY 9.05 ROW-HEIGHT-CHARS .62.

DEFINE BROWSE BROWSE-Prg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Prg Dialog-Frame _FREEFORM
  QUERY BROWSE-Prg DISPLAY
      tPrg.tNummer  FORMAT "zzzz-"   LABEL "Nr "
         tPrg.tProgram FORMAT "X(200)"  LABEL " Navn"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS SIZE 35 BY 9.05.

DEFINE BROWSE BROWSE-tDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-tDb Dialog-Frame _FREEFORM
  QUERY BROWSE-tDb DISPLAY
      tdb.tLdb    FORMAT "X(18)" LABEL "Lo"
tdb.tPdb    FORMAT "X(35)" LABEL "Fys"
tdb.tVer    FORMAT "X(5)" LABEL "Ver"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 63 BY 9.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-Prg AT ROW 15.05 COL 4
     B-Sprak AT ROW 1.48 COL 138
     BUTTON-1 AT ROW 24.81 COL 2
     BUTTON-Persist AT ROW 24.81 COL 27
     FILL-IN-Info AT ROW 1.48 COL 10.8
     FILL-IN-StartDir AT ROW 2.43 COL 16 COLON-ALIGNED
     FILL-IN-IniFil AT ROW 3.38 COL 16 COLON-ALIGNED
     FILL-IN-SysIni AT ROW 4.33 COL 16 COLON-ALIGNED
     FILL-IN-Exe AT ROW 5.29 COL 16 COLON-ALIGNED
     FILL-IN-Para AT ROW 6.24 COL 16 COLON-ALIGNED
     FILL-IN-Progress AT ROW 8.38 COL 16 COLON-ALIGNED
     FILL-IN-Serial AT ROW 9.33 COL 16 COLON-ALIGNED
     FILL-IN-Opsys AT ROW 10.29 COL 16 COLON-ALIGNED
     FILL-IN-Terminal AT ROW 11.24 COL 16 COLON-ALIGNED
     BROWSE-tDb AT ROW 15.05 COL 44
     FILL-IN-Brukerid AT ROW 8.38 COL 50 COLON-ALIGNED
     FILL-IN-LdNavn AT ROW 9.33 COL 50 COLON-ALIGNED
     FILL-IN-Dictdb AT ROW 10.29 COL 50 COLON-ALIGNED
     BROWSE-3 AT ROW 15.05 COL 112
     FILL-IN-FrameName AT ROW 8.38 COL 79 COLON-ALIGNED
     FILL-IN-FrameDb AT ROW 9.57 COL 79 COLON-ALIGNED
     FILL-IN-FrameFile AT ROW 10.52 COL 79 COLON-ALIGNED
     FILL-IN-FrameField AT ROW 11.48 COL 79 COLON-ALIGNED
     FILL-IN-FrameValue AT ROW 12.43 COL 79 COLON-ALIGNED
     Btn_OK AT ROW 24.81 COL 124
     Btn_Help AT ROW 24.81 COL 139
     "Progress info" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 7.67 COL 3
     "Aktive programmoduler" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 14.1 COL 4
     "VS90" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 7.67 COL 43
     "Databaser" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 14.1 COL 44
     "Frame info" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 7.67 COL 73
     "Programpath" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 14.1 COL 112
     RECT-1 AT ROW 7.91 COL 2
     RECT-2 AT ROW 7.91 COL 71
     RECT-24 AT ROW 1.24 COL 2
     RECT-3 AT ROW 14.33 COL 2
     RECT-4 AT ROW 14.33 COL 42
     RECT-5 AT ROW 14.33 COL 110
     RECT-6 AT ROW 7.91 COL 42
     SPACE(84.99) SKIP(12.23)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "VS90 Programinformasjon"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BROWSE-Prg 1 Dialog-Frame */
/* BROWSE-TAB BROWSE-tDb FILL-IN-Terminal Dialog-Frame */
/* BROWSE-TAB BROWSE-3 FILL-IN-Dictdb Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Persist IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Brukerid IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Dictdb IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Exe IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-FrameDb IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-FrameField IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-FrameFile IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-FrameName IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-FrameValue IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Info IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-IniFil IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-LdNavn IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Opsys IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Para IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Progress IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Serial IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-StartDir IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-SysIni IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Terminal IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tPath NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Prg
/* Query rebuild information for BROWSE BROWSE-Prg
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tPrg NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-Prg */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-tDb
/* Query rebuild information for BROWSE BROWSE-tDb
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tDb NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-tDb */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* VS90 Programinformasjon */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Sprak
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Sprak Dialog-Frame
ON CHOOSE OF B-Sprak IN FRAME Dialog-Frame /* Språk... */
DO:
    RUN VisLng.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {diahelp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 Dialog-Frame
ON CHOOSE OF BUTTON-1 IN FRAME Dialog-Frame /* Sesjonsinnstillinger... */
DO:
  RUN session.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Persist
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Persist Dialog-Frame
ON CHOOSE OF BUTTON-Persist IN FRAME Dialog-Frame /* Persistente programmer... */
DO:
  RUN d-bproc.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-3
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{syspara.i 1 1 5 wLokalIniFil}
{syspara.i 1 1 6 wMappeLokalIniFil}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN Initiering.
  {lng.i} RUN enable_UI.
  WAIT-FOR GO, END-ERROR, ENDKEY OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
RETURN NO-APPLY.

/* Denne benyttes for å finne Porgress exe-fil. Egentlig 
   unødvendig fordi den kan hentes ut fra GetCommandLineA.
*/   
PROCEDURE GetModuleFileNameA EXTERNAL "kernel32.dll" :
  DEFINE INPUT  PARAMETER hModule    AS LONG.
  DEFINE OUTPUT PARAMETER lpFilename AS CHAR.
  DEFINE INPUT  PARAMETER nSize      AS LONG.
  DEFINE RETURN PARAMETER ReturnSize AS LONG.
END PROCEDURE.

PROCEDURE GetCommandLineA EXTERNAL "kernel32.dll":
  DEF RETURN PARAMETER mCmdLine AS MEMPTR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-Info FILL-IN-StartDir FILL-IN-IniFil FILL-IN-SysIni 
          FILL-IN-Exe FILL-IN-Para FILL-IN-Progress FILL-IN-Serial FILL-IN-Opsys 
          FILL-IN-Terminal FILL-IN-Brukerid FILL-IN-LdNavn FILL-IN-Dictdb 
          FILL-IN-FrameName FILL-IN-FrameDb FILL-IN-FrameFile FILL-IN-FrameField 
          FILL-IN-FrameValue 
      WITH FRAME Dialog-Frame.
  ENABLE BROWSE-Prg B-Sprak BUTTON-1 BROWSE-tDb BROWSE-3 Btn_OK Btn_Help RECT-1 
         RECT-2 RECT-24 RECT-3 RECT-4 RECT-5 RECT-6 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initiering Dialog-Frame 
PROCEDURE Initiering :
/*------------------------------------------------------------------------------
  Purpose:     Initierer skjermbildet
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR i         AS INTE NO-UNDO.
   DEF VAR wStartDir AS CHAR NO-UNDO.
   DEF VAR wFil      AS CHAR NO-UNDO.
   DEF VAR hModule   AS INTE NO-UNDO.
   DEF VAR FileName  AS CHAR NO-UNDO.
   DEF VAR RetVal    AS INTE NO-UNDO.
   DEF VAR wCl       AS CHAR NO-UNDO. /* Command-line */
   DEF VAR wStr      AS CHAR NO-UNDO.
   DEF VAR wSysIni   AS CHAR NO-UNDO.
   def var wSysNavn  as char no-undo.

   {syspara.i 1 1 102 wSysNavn}
   
   ASSIGN hModule  = 0
          FileName = FILL(" ",256)
          wCl      = CommandLine()
          wStr     = REPLACE(wCL,"-ininame ","#")
          wSysIni  = ENTRY(2,wStr,"#") NO-ERROR.

   /* Finner Progress exe */       
   RUN GetModuleFileNameA(hModule, OUTPUT FileName, 256, OUTPUT RetVal).

   IF wSysIni = "" THEN 
        ASSIGN wStr    = REPLACE(wCL,"-ini ","#")
               wSysIni = ENTRY(2,wStr,"#") NO-ERROR.
    
   ASSIGN wSysIni = ENTRY(1,wSysIni," ").

   /* Ini-fil er ikke oppgitt: Let etter den etter følgende Progress-
      regler:
         1) Check the current working directory of the user. This is set in
            the property sheet of the PROGRESS Client icon.
         2) Check the directory where windows is installed (i.e.:  C:\WINDOWS).
         3) Check the directory where _prowin resides.  This information comes
            from the property sheet of the icon as well and is by default the bin
            directory under DLC.
         4) Check the directories in the users PATH.
      NB! Viser teksten "I henhold til PATH-variabelen" hvis 4).   
   */   
   IF wSysIni = "" THEN DO:
      ASSIGN wSysIni = SEARCH(".\progress.ini").
      IF wSysIni = ? THEN DO:
         ASSIGN wSysIni = SEARCH(OS-GETENV("WINDIR") + "\progress.ini") NO-ERROR.
         IF wSysIni = ? THEN 
            wSysIni = SUBSTR(FileName,1,R-INDEX(FileName,"\")) + "progress.ini" NO-ERROR.
      END.
   END.         
   
   IF wSysIni = "" THEN ASSIGn wSysIni = "I henhold til PATH-variabelen".
   
   ASSIGN i = 2.
   REPEAT:
      ASSIGN i = i + 1.
      IF PROGRAM-NAME(i) = ? THEN LEAVE.
      CREATE tPrg.
      ASSIGN tPrg.tNummer  = i - 2
             tPrg.tProgram = PROGRAM-NAME(i).
   END.
   
   DO i = 1 TO NUM-DBS:
      CREATE tDb.
      ASSIGN tDb.tLdb = LDBNAME(i)
             tDb.tPdb = PDBNAME(i)
             tDb.tVer = DBVERSION(i).
   END. 
   
   DO i = 1 TO NUM-ENTRIES(PROPATH):
      CREATE tPath.
      ASSIGN tPath.tNummer = i
             tPath.tPath   = ENTRY(i,PROPATH).
   END.          
   
   /* Finner startdir */
   INPUT FROM OS-DIR(".\").
   REPEAT:
      IMPORT wFil wStartDir.
      IF wFil = "." THEN LEAVE.
   END.
   INPUT CLOSE. 
   ASSIGN wStartDir = REPLACE(wStartDir,".","")
          wStartDir = SUBSTR(wStartDir,1,LENGTH(wStartDir) - 1).
         
   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN FILL-IN-StartDir      = wStartDir
             FILL-IN-IniFil        = wMappeLokalIniFil + "\" + wLokalIniFil
             FILL-IN-Exe           = SUBSTR(FileName, 1, RetVal)
             FILL-IN-Para          = TRIM(ENTRY(3,wCl,'"'))
             FILL-IN-SysIni        = CAPS(wSysIni)
             FILL-IN-Progress      = PROVERSION + ", " + PROGRESS
             FILL-IN-Serial        = STRING(_SERIAL)
             FILL-IN-Opsys         = OPSYS
             FILL-IN-Terminal      = TERMINAL
             FILL-IN-DictDb        = LDBNAME("dictdb")
             FILL-IN-BrukerId      = userid("dictdb")
             FILL-IN-LdNavn        = wSysNavn
             FILL-IN-FrameDb       = FRAME-DB
             FILL-IN-FrameFile     = FRAME-FILE
             FILL-IN-FrameField    = FRAME-FIELD
             FILL-IN-FrameName     = FRAME-NAME
             FILL-IN-FrameValue    = FRAME-VALUE
             FILL-IN-Info          = IF TRANSACTION THEN 
                                          "En transaksjon er aktiv"
                                     ELSE "Ingen aktiv transaksjon"
             FILL-IN-INFO:BGCOLOR  = INT(STRING(TRANSACTION,"12/15"))
             FILL-IN-INFO:FGCOLOR  = INT(STRING(TRANSACTION,"14/0"))
             BUTTON-Persist:SENSITIVE = true /*AVAIL sh-BrTilh AND sh-BrTilh.BrGrNr = 0 */.                           
   END.          
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisLng Dialog-Frame 
PROCEDURE VisLng :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  MESSAGE 
      "wCurrLng:"   wCurrLng SKIP
      "wLngHandle:" wLngHandle valid-handle(wLngHandle) SKIP
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CommandLine Dialog-Frame 
FUNCTION CommandLine RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VAR mCmdLine AS MEMPTR NO-UNDO.
  RUN GetCommandLineA (OUTPUT mCmdLine).
  RETURN GET-STRING(mCmdLine,1).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

