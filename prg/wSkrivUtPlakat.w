&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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


/* Local Variable Definitions ---                                       */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN /* V=Vedlikehold och Knapparna syns. */
    DEFINE VARIABLE dArtikkelnr AS DECIMAL    NO-UNDO.
    dArtikkelnr = 9801658.
&ELSE
/* Parameters Definitions ---                                           */
    DEFINE INPUT  PARAMETER dArtikkelnr AS DECIMAL    NO-UNDO.
&ENDIF


DEFINE VARIABLE cBildefil AS CHARACTER  NO-UNDO.
def var wOk                as log    no-undo.

DEFINE VARIABLE hParentHandle AS HANDLE     NO-UNDO.
DEFINE VARIABLE lInit AS LOGICAL INIT TRUE   NO-UNDO.
DEFINE VARIABLE iMax AS INTEGER  INIT 99  NO-UNDO.

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
&Scoped-Define ENABLED-FIELDS ArtBas.ArtikkelNr 
&Scoped-define ENABLED-TABLES ArtBas
&Scoped-define FIRST-ENABLED-TABLE ArtBas
&Scoped-Define ENABLED-OBJECTS RECT-62 FI-Varemerke FI-Beskr FI-Logofil ~
BUTTON-Next TG-Logofil TG-InkluderBilde FI-Etiketttxt1 FI-Etiketttxt2 ~
ED-Varefakta FI-Ordinarie TG-Velgordinarie FI-Tilbud TG-Velgtilbud B-SpinUp ~
FI-AntEx B-SpinDown B-Skrivut TG-FHVisning BUTTON-SokFil BUTTON-Prev 
&Scoped-Define DISPLAYED-FIELDS ArtBas.ArtikkelNr 
&Scoped-define DISPLAYED-TABLES ArtBas
&Scoped-define FIRST-DISPLAYED-TABLE ArtBas
&Scoped-Define DISPLAYED-OBJECTS FI-Varemerke FI-Beskr FI-Logofil ~
TG-Logofil TG-InkluderBilde FI-Etiketttxt1 FI-Etiketttxt2 ED-Varefakta ~
FI-Ordinarie TG-Velgordinarie FI-Tilbud TG-Velgtilbud FI-AntEx TG-FHVisning ~
FI-VarefaktaTxt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBildefil C-Win 
FUNCTION getBildefil RETURNS CHARACTER
  ( INPUT ipBildNr AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE IMAGE-Sko AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chIMAGE-Sko AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Skrivut 
     LABEL "Skrivut" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-SpinDown 
     LABEL "-" 
     SIZE 2.6 BY .62
     FONT 6.

DEFINE BUTTON B-SpinUp 
     LABEL "+" 
     SIZE 2.6 BY .62
     FONT 6.

DEFINE BUTTON BUTTON-Next 
     IMAGE-UP FILE "icon/next.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Neste" 
     SIZE 4.6 BY 1.05 TOOLTIP "Neste post (Alt-PilNed)".

DEFINE BUTTON BUTTON-Prev 
     IMAGE-UP FILE "icon/prev.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Forrige" 
     SIZE 4.6 BY 1.05 TOOLTIP "Forrige post (Alt-PilOpp)".

DEFINE BUTTON BUTTON-SokFil 
     IMAGE-UP FILE "icon/e-open.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 4.6 BY 1.1 TOOLTIP "Henter bilde fra fil" DROP-TARGET.

DEFINE VARIABLE ED-Varefakta AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 53 BY 8.33 NO-UNDO.

DEFINE VARIABLE FI-AntEx AS INTEGER FORMAT ">9":U INITIAL 1 
     LABEL "Antal" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Beskr AS CHARACTER FORMAT "x(20)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI-Etiketttxt1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Etikett1" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Etiketttxt2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Etikett2" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Logofil AS CHARACTER FORMAT "x(20)" 
     LABEL "Logofil" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE FI-Ordinarie AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Ordinær pris" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Tilbud AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Tilbudspris" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VarefaktaTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Varefakta" 
      VIEW-AS TEXT 
     SIZE 14 BY .95
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Varemerke AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varemerke" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-62
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 122 BY 19.05.

DEFINE VARIABLE TG-FHVisning AS LOGICAL INITIAL no 
     LABEL "Forhåndvisning" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE TG-InkluderBilde AS LOGICAL INITIAL no 
     LABEL "Inkluder bilde" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Logofil AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Velgordinarie AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Velgtilbud AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     ArtBas.ArtikkelNr AT ROW 2.24 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
          BGCOLOR 15  NO-TAB-STOP 
     FI-Varemerke AT ROW 3.43 COL 18 COLON-ALIGNED
     FI-Beskr AT ROW 4.62 COL 18 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen" NO-TAB-STOP 
     FI-Logofil AT ROW 5.91 COL 18 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen" NO-TAB-STOP 
     BUTTON-Next AT ROW 2.24 COL 46.6 NO-TAB-STOP 
     TG-Logofil AT ROW 6 COL 74
     TG-InkluderBilde AT ROW 7.43 COL 89
     FI-Etiketttxt1 AT ROW 7.71 COL 18 COLON-ALIGNED
     FI-Etiketttxt2 AT ROW 9.1 COL 18 COLON-ALIGNED
     ED-Varefakta AT ROW 10.95 COL 20 NO-LABEL
     FI-Ordinarie AT ROW 10.95 COL 87.4 COLON-ALIGNED NO-TAB-STOP 
     TG-Velgordinarie AT ROW 11 COL 105
     FI-Tilbud AT ROW 12.38 COL 87.4 COLON-ALIGNED
     TG-Velgtilbud AT ROW 12.43 COL 105
     B-SpinUp AT ROW 14.05 COL 95.4
     FI-AntEx AT ROW 14.19 COL 87.4 COLON-ALIGNED
     B-SpinDown AT ROW 14.71 COL 95.4
     B-Skrivut AT ROW 16.14 COL 89.2
     TG-FHVisning AT ROW 17.91 COL 90
     BUTTON-SokFil AT ROW 1.95 COL 118 NO-TAB-STOP 
     BUTTON-Prev AT ROW 2.24 COL 42 NO-TAB-STOP 
     FI-VarefaktaTxt AT ROW 11.05 COL 2.6 COLON-ALIGNED NO-LABEL
     RECT-62 AT ROW 1.24 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 123.6 BY 19.62.


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
         TITLE              = "Plakatutskrift"
         HEIGHT             = 19.62
         WIDTH              = 123.6
         MAX-HEIGHT         = 31.67
         MAX-WIDTH          = 128.2
         VIRTUAL-HEIGHT     = 31.67
         VIRTUAL-WIDTH      = 128.2
         RESIZE             = yes
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       ArtBas.ArtikkelNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-Logofil:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FI-VarefaktaTxt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME IMAGE-Sko ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.95
       COLUMN          = 89
       HEIGHT          = 4.81
       WIDTH           = 27
       TAB-STOP        = no
       HIDDEN          = no
       SENSITIVE       = yes.
      IMAGE-Sko:NAME = "IMAGE-Sko":U .
/* IMAGE-Sko OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Plakatutskrift */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Plakatutskrift */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Skrivut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Skrivut C-Win
ON CHOOSE OF B-Skrivut IN FRAME DEFAULT-FRAME /* Skrivut */
DO:
    ASSIGN 
        FI-Logofil
        TG-Logofil
        FI-Varemerke
        FI-Beskr 
        FI-Etiketttxt1 
        FI-Etiketttxt2 
        ED-Varefakta 
        FI-Ordinarie 
        FI-Tilbud
        FI-AntEx.
  IF FI-AntEx = 0 OR FI-AntEx = ? THEN
      ASSIGN FI-AntEx = 1
             FI-AntEx:SCREEN-VALUE = "1".
  RUN skrivplakat.p (FI-Varemerke,
                     FI-Beskr,
                     FI-Etiketttxt1,
                     FI-Etiketttxt2,
                     ED-Varefakta,
                     IF TG-Velgordinarie:CHECKED THEN FI-Ordinarie ELSE 0,
                     IF TG-Velgtilbud:CHECKED THEN FI-Tilbud ELSE 0,
                     TG-InkluderBilde:CHECKED, /* skrivutbild */
                     chIMAGE-Sko:Picbuf:filename,
                     IF TG-Logofil = TRUE THEN FI-LogoFil ELSE "",
                     FI-AntEx,
                     IF TG-FHVisning:CHECKED THEN 1 ELSE 0).    /* typ         */
    IF VALID-HANDLE(hParentHandle) THEN DO:
        RUN PrevNext IN hParentHandle ("Next").
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SpinDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SpinDown C-Win
ON CHOOSE OF B-SpinDown IN FRAME DEFAULT-FRAME /* - */
DO:
    IF FI-AntEx > 1 THEN
        ASSIGN FI-AntEx = FI-AntEx - 1
               FI-AntEx:SCREEN-VALUE = STRING(FI-AntEx).
    RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SpinUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SpinUp C-Win
ON CHOOSE OF B-SpinUp IN FRAME DEFAULT-FRAME /* + */
DO:
    IF FI-AntEx < iMax THEN
        ASSIGN FI-AntEx = FI-AntEx + 1
               FI-AntEx:SCREEN-VALUE = STRING(FI-AntEx).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Next C-Win
ON CHOOSE OF BUTTON-Next IN FRAME DEFAULT-FRAME /* Neste */
DO:
  /* Det er ikke lov å bytte artikkel hvis artikkelen ikke har en ArtPrisPost. */
  IF VALID-HANDLE(hParentHandle) THEN DO:
      RUN PrevNext IN hParentHandle ("Next").
  END.
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Prev C-Win
ON CHOOSE OF BUTTON-Prev IN FRAME DEFAULT-FRAME /* Forrige */
DO:
  IF VALID-HANDLE(hParentHandle) THEN DO:
      RUN PrevNext IN hParentHandle ("Prev").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokFil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokFil C-Win
ON CHOOSE OF BUTTON-SokFil IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE cFilename AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE OKpressed AS LOGICAL    NO-UNDO.
    DO:
        IF SELF:PRIVATE-DATA <> "" THEN DO:
            cFileName = SELF:PRIVATE-DATA.
            SELF:PRIVATE-DATA = "".
            IF SEARCH(cFileName) <> ? THEN DO:
                OKpressed = TRUE.
            END.
        END.
        ELSE DO:
            ASSIGN SELF:PRIVATE-DATA = "".
            SYSTEM-DIALOG GET-FILE cFileName
                TITLE      "Velg bildefil ..."
                FILTERS    "Type (*.gif,*.bmp,*.jpg)"   "*.gif,*.bmp,*.jpg"
    /*             INITIAL-DIR cSisteBildedir */
                MUST-EXIST
                USE-FILENAME
                RETURN-TO-START-DIR
                UPDATE OKpressed.
        END.
        IF OKpressed = TRUE THEN DO:
/*             ASSIGN FILE-INFO:FILENAME = cFileName */
/*             IF ENTRY(NUM-ENTRIES(cFileName,"\"),cFileName,"\") BEGINS "mini" THEN DO:                     */
/*                 MESSAGE "Fil med navn 'mini...' kann ikke leses inn."                                     */
/*                     VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
/*                 LEAVE NYBILDE.                                                                            */
/*             END.                                                                                          */
/*             ASSIGN cSisteBildeDir = REPLACE(cFileName,ENTRY(NUM-ENTRIES(cFileName,"\"),cFileName,"\"),"") */
/*                    SELF:PRIVATE-DATA = cFilename.                                                         */
                   chImage-Sko:Picbuf:clear(2).
                   assign
                     chImage-Sko:Picbuf:filename  = cFilename
                     chImage-Sko:Picbuf:AutoScale = True no-error.
                     chImage-Sko:Picbuf:LOAD NO-ERROR.
        END.
    END.
/*     RUN NyttBilde (SELF:PRIVATE-DATA). */
    
    
    
    
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokFil C-Win
ON DROP-FILE-NOTIFY OF BUTTON-SokFil IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE cFilnavn AS CHARACTER  NO-UNDO.
    IF SELF:NUM-DROPPED-FILES = 1 THEN DO:
        ASSIGN cFilNavn = SELF:GET-DROPPED-FILE(1).
        IF NOT CAN-DO("gif,bmp,jpg",ENTRY(NUM-ENTRIES(cFilNavn,"."),cFilNavn,".")) THEN
            MESSAGE "Tillatte filtyper: '.gif,.bmp,.jpg'"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ELSE DO:
            ASSIGN SELF:PRIVATE-DATA = cFilnavn.
            APPLY "CHOOSE" TO SELF.
        END.
    END.
    ELSE DO:
        SELF:PRIVATE-DATA = "".
        MESSAGE "Endast 1 fil tillatt!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-AntEx
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-AntEx C-Win
ON TAB OF FI-AntEx IN FRAME DEFAULT-FRAME /* Antal */
OR RETURN OF FI-AntEx DO:
    IF INPUT FI-AntEx = 0 OR INPUT FI-AntEx = ? THEN
        FI-AntEx:SCREEN-VALUE = "1".
    ASSIGN FI-AntEx.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IMAGE-Sko
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-Sko C-Win OCX.DblClick
PROCEDURE IMAGE-Sko.Picbuf.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  if available ArtBAs then
    do:
      find BildeRegister of ArtBas no-error.
      if available BildeRegister then
        run d-visbil.w (input recid(BildeRegister)).
    end.
  return no-apply.    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
IF CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"PrevNext") THEN DO:
    hParentHandle = SOURCE-PROCEDURE.
    SUBSCRIBE TO "ByttArtikkel" IN hParentHandle.
END.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    FIND Artbas WHERE Artbas.artikkelnr = dArtikkelnr NO-LOCK.
    FIND bilderegister OF artbas NO-LOCK NO-ERROR.
    RUN initFelter.
    RUN InitDiv.
  RUN enable_UI.
  BUTTON-Next:HIDDEN = hParentHandle = ?.
  BUTTON-Prev:HIDDEN = hParentHandle = ?.
  
  IF AVAILABLE Bilderegister THEN
      RUN VisBilde (1).
  ELSE
      RUN VisBilde (2).
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttArtikkel C-Win 
PROCEDURE ByttArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
  FIND ArtBas NO-LOCK WHERE
    ArtBas.ArtikkelNr = dArtikkelNr NO-ERROR.
  FIND Artbas WHERE Artbas.artikkelnr = dArtikkelnr NO-LOCK.
  FIND bilderegister OF artbas NO-LOCK NO-ERROR.
  Artbas.artikkelnr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(Artbas.artikkelnr).
  RUN initFelter.
  RUN InitDiv.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

OCXFile = SEARCH( "wSkrivUtPlakat.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chIMAGE-Sko = IMAGE-Sko:COM-HANDLE
    UIB_S = chIMAGE-Sko:LoadControls( OCXFile, "IMAGE-Sko":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "wSkrivUtPlakat.wrx":U SKIP(1)
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
  DISPLAY FI-Varemerke FI-Beskr FI-Logofil TG-Logofil TG-InkluderBilde 
          FI-Etiketttxt1 FI-Etiketttxt2 ED-Varefakta FI-Ordinarie 
          TG-Velgordinarie FI-Tilbud TG-Velgtilbud FI-AntEx TG-FHVisning 
          FI-VarefaktaTxt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE ArtBas THEN 
    DISPLAY ArtBas.ArtikkelNr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-62 ArtBas.ArtikkelNr FI-Varemerke FI-Beskr FI-Logofil BUTTON-Next 
         TG-Logofil TG-InkluderBilde FI-Etiketttxt1 FI-Etiketttxt2 ED-Varefakta 
         FI-Ordinarie TG-Velgordinarie FI-Tilbud TG-Velgtilbud B-SpinUp 
         FI-AntEx B-SpinDown B-Skrivut TG-FHVisning BUTTON-SokFil BUTTON-Prev 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitDiv C-Win 
PROCEDURE InitDiv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initFelter C-Win 
PROCEDURE initFelter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND FIRST artpris OF artbas NO-LOCK.
DO WITH FRAME {&FRAME-NAME}:
    FI-Ordinarie = Artpris.pris[1].
    IF FI-Ordinarie = ? THEN
        FI-Ordinarie = 0.
    FI-Tilbud    = IF Artpris.tilbud THEN Artpris.pris[2] ELSE 0.
    IF FI-Tilbud = ? THEN
        FI-Tilbud = 0.
    TG-Velgordinarie = FI-Ordinarie > 0.
    TG-Velgtilbud    = FI-Tilbud    > 0.
    IF lInit THEN DO:
        FI-Logofil = SEARCH(".\icon\plakat.jpg").
        FI-Logofil = IF FI-Logofil = ? THEN "" ELSE FI-Logofil.
        TG-Logofil = FI-Logofil <> "".
    END.
    FIND varemerke OF artbas NO-LOCK NO-ERROR.

    ASSIGN 
        FI-Beskr     = Artbas.beskr
        ED-Varefakta = artbas.varefakta
        FI-Etiketttxt1  = TRIM(ArtBas.Etikettekst1)
        FI-Etiketttxt2  = TRIM(ArtBas.Etikettekst2)
        FI-Varemerke = IF AVAIL Varemerke THEN TRIM(Varemerke.Beskrivelse) ELSE "".
        cBildeFil = getbildefil(ArtBas.bildnr).
        IF lInit = FALSE THEN DO:
            DISPLAY
                FI-Ordinarie
                FI-Tilbud
                TG-Velgordinarie
                TG-Velgtilbud
                FI-Beskr
                ED-Varefakta
                FI-Etiketttxt1
                FI-Etiketttxt2
                FI-Varemerke.
            IF AVAILABLE Bilderegister THEN
                RUN VisBilde (1).
            ELSE
                RUN VisBilde (2).
        END.
    ASSIGN lInit = FALSE.
END.

    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Visbilde C-Win 
PROCEDURE Visbilde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT AVAILABLE ArtBas THEN
    RETURN.
    DO WITH FRAME DEFAULT-FRAME:  
      {visbilde.i
        &BldOcx = "chIMAGE-Sko"
        &BildNr = "ArtBas.BildNr"
    /*     &BildNr = "input ArtBas.BildNr" */
      }
    END.
    TG-InkluderBilde:CHECKED = artbas.bildnr > 0 AND SEARCH(wBildePeker) <> ?.
    TG-InkluderBilde:SENSITIVE = TG-InkluderBilde:CHECKED.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBildefil C-Win 
FUNCTION getBildefil RETURNS CHARACTER
  ( INPUT ipBildNr AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ocBildeFil AS CHARACTER  NO-UNDO.
  FIND BildeRegister NO-LOCK WHERE
    BildeRegister.BildNr = ipBildNr NO-ERROR.
  IF AVAIL BildeRegister AND TRIM(BildeRegister.FilNavn) <> "" THEN DO:
    IF VALID-HANDLE(wLibHandle) THEN
      RUN HentBildePeker IN wLibHandle (INPUT ipBildNr, 1, BildeRegister.FilNavn, OUTPUT ocBildeFil).
  END.
  /* cBlanktBilde */
  RETURN ocBildeFil.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

