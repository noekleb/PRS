&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
/* Procedure Description
"Basic Window Template

Use this template to create a new window. Alter this default template or create new ones to accomodate your needs for different default sizes and/or attributes."
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*********************************************************************
* Copyright (C) 2001 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOK             AS LOG NO-UNDO.
DEF VAR ix              AS INT NO-UNDO.

DEF VAR hToolbar        AS HANDLE NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR hFieldMap       AS HANDLE NO-UNDO.
DEF VAR iLoop           AS INT    NO-UNDO.
DEF VAR cTekst          AS CHAR   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolBar rectWinToolbar RECT-3 ~
RECT-4 RECT-5 RECT-6 EDBSystem DataType SysBeskrivelse Aktiv FilKatalog ~
FilPrefix FilEkstent EDB-System SeqvAktiv SeqNr MaksSeq EkspFrekvens ~
cFromTime cToTime TidsIntervall eksportRutine 
&Scoped-Define DISPLAYED-OBJECTS EDBSystem DataType SysBeskrivelse Aktiv ~
FilKatalog FilPrefix FilEkstent EDB-System SeqvAktiv SeqNr MaksSeq ~
EkspFrekvens cFromTime cToTime TidsIntervall eksportRutine FI-3 FI-1 FI-2 ~
FI-4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE DataType AS CHARACTER FORMAT "X(8)" 
     LABEL "DataType" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE EDB-System AS CHARACTER FORMAT "X(12)" 
     LABEL "Mappingtbl." 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 42 BY 1.

DEFINE VARIABLE EkspFrekvens AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Frekvens" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "0",1
     DROP-DOWN-LIST
     SIZE 32.8 BY 1 NO-UNDO.

DEFINE VARIABLE cFromTime AS CHARACTER FORMAT "99:99":U 
     LABEL "Start/stopp" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Starttidspunkt" NO-UNDO.

DEFINE VARIABLE cToTime AS CHARACTER FORMAT "99:99":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Stopptidspunkt" NO-UNDO.

DEFINE VARIABLE EDBSystem AS CHARACTER FORMAT "X(8)" 
     LABEL "EDBSystem" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1.

DEFINE VARIABLE eksportRutine AS CHARACTER FORMAT "X(40)" 
     LABEL "Eksp. rutine" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1.

DEFINE VARIABLE FI-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Sekvensnr i filnavn" 
      VIEW-AS TEXT 
     SIZE 26 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Frekvens og intervall" 
      VIEW-AS TEXT 
     SIZE 26 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-3 AS CHARACTER FORMAT "X(256)":U INITIAL "EDB system og filnavn" 
      VIEW-AS TEXT 
     SIZE 30 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-4 AS CHARACTER FORMAT "X(256)":U INITIAL "Eksportrutine" 
      VIEW-AS TEXT 
     SIZE 26 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FilEkstent AS CHARACTER FORMAT "X(8)" 
     LABEL "Ekstent" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1.

DEFINE VARIABLE FilKatalog AS CHARACTER FORMAT "X(40)" 
     LABEL "Filkatalog" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE FilPrefix AS CHARACTER FORMAT "X(8)" 
     LABEL "Fil prefix" 
     VIEW-AS FILL-IN 
     SIZE 13.6 BY 1.

DEFINE VARIABLE MaksSeq AS INTEGER FORMAT ">>>>>>>9" INITIAL 99999999 
     LABEL "MaksSeq" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE SeqNr AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Sekv.nr" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE SysBeskrivelse AS CHARACTER FORMAT "X(255)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE TidsIntervall AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "TidsIntervall" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Angi tidsintervall i minutter.".

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 2.38.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 3.81.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 8.86.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 1.67.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 77 BY 19.05.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE VARIABLE Aktiv AS LOGICAL INITIAL no 
     LABEL "Aktiv" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE SeqvAktiv AS LOGICAL INITIAL no 
     LABEL "Sekvens" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     EDBSystem AT ROW 3.1 COL 91.2 COLON-ALIGNED HELP
          "Betegnelse på eksternt EDB system"
     DataType AT ROW 4.14 COL 91.2 COLON-ALIGNED HELP
          "Data type som det eksterne EDB systemet skal ha."
     SysBeskrivelse AT ROW 5.19 COL 91.2 COLON-ALIGNED HELP
          "Kort beskrivelse av eksternt EDB system"
     Aktiv AT ROW 6.29 COL 93.4 HELP
          "Aktiv."
     FilKatalog AT ROW 7.19 COL 91.2 COLON-ALIGNED HELP
          "Navn på filkatalog som eksportfil skal legges i."
     FilPrefix AT ROW 8.24 COL 91.2 COLON-ALIGNED HELP
          "Fil prefix på eksportfil."
     FilEkstent AT ROW 9.29 COL 91.2 COLON-ALIGNED HELP
          "Navn på fliekstent"
     EDB-System AT ROW 10.33 COL 91 COLON-ALIGNED HELP
          "Peker på mapping i ImpKonv."
     SeqvAktiv AT ROW 12.76 COL 93 HELP
          "Sekvens skal være endel av filnavnet"
     SeqNr AT ROW 13.71 COL 91 COLON-ALIGNED HELP
          "Sekvensnummer siste eksport"
     MaksSeq AT ROW 13.71 COL 119 COLON-ALIGNED HELP
          "Maks seqv.nr. Starter om fra 1."
     EkspFrekvens AT ROW 16.14 COL 91 COLON-ALIGNED HELP
          "0=Tidsintervall, 1=Daglig , 2=Ukentlig, 3=Måned, 4=År"
     cFromTime AT ROW 17.19 COL 91 COLON-ALIGNED
     cToTime AT ROW 17.19 COL 107.8 COLON-ALIGNED NO-LABEL
     TidsIntervall AT ROW 18.24 COL 91 COLON-ALIGNED HELP
          "Angi tidsintervall i minutter."
     eksportRutine AT ROW 20.67 COL 91 COLON-ALIGNED HELP
          "Programnavn på rutine som eksporterer dataene."
     FI-3 AT ROW 2.19 COL 77.4 COLON-ALIGNED NO-LABEL
     FI-1 AT ROW 12 COL 77.8 COLON-ALIGNED NO-LABEL
     FI-2 AT ROW 15.1 COL 78 COLON-ALIGNED NO-LABEL
     FI-4 AT ROW 19.62 COL 77.6 COLON-ALIGNED NO-LABEL
     rectBrowse AT ROW 2.91 COL 2
     rectToolBar AT ROW 1.14 COL 1.8
     rectWinToolbar AT ROW 1.1 COL 128.4
     RECT-3 AT ROW 12.67 COL 79
     RECT-4 AT ROW 15.76 COL 79
     RECT-5 AT ROW 2.86 COL 79
     RECT-6 AT ROW 20.29 COL 79
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 138.6 BY 21.24.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window Template
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Vedlikehold eksport register"
         HEIGHT             = 21.24
         WIDTH              = 138.6
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("ico/cntrlhry.ico":U) THEN
    MESSAGE "Unable to load icon: ico/cntrlhry.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN FI-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Vedlikehold eksport register */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vedlikehold eksport register */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Vedlikehold eksport register */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

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
ON CLOSE OF THIS-PROCEDURE DO:
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN enable_UI.
  RUN InitWindow.
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteRecord C-Win 
PROCEDURE deleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN SUPER.
    APPLY "ENTRY" TO hBrowse.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayRecord C-Win 
PROCEDURE displayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN SUPER.

    ASSIGN
        EDBSystem:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
        SeqNr:SENSITIVE IN FRAME {&FRAME-NAME} = IF SeqvAktiv:SCREEN-VALUE = "no" THEN FALSE ELSE TRUE
        MaksSeq:SENSITIVE IN FRAME {&FRAME-NAME} = IF SeqvAktiv:SCREEN-VALUE = "no" THEN FALSE ELSE TRUE
        .

  IF hFieldMap:AVAIL THEN 
    ASSIGN cFromTime:SCREEN-VALUE = hFieldMap:BUFFER-FIELD("cFromTime"):BUFFER-VALUE
           cToTime:SCREEN-VALUE   = hFieldMap:BUFFER-FIELD("cToTime"):BUFFER-VALUE
           cFromTime:MODIFIED     = FALSE
           cToTime:MODIFIED       = FALSE
           .

  ASSIGN
      cFromTime:SENSITIVE IN FRAME {&FRAME-NAME} = IF int(EkspFrekvens:SCREEN-VALUE) <> 0 THEN FALSE ELSE TRUE
      cToTime:SENSITIVE IN FRAME {&FRAME-NAME} = IF int(EkspFrekvens:SCREEN-VALUE) <> 0 THEN FALSE ELSE TRUE
      cFromTime:SCREEN-VALUE = IF int(EkspFrekvens:SCREEN-VALUE) <> 0 THEN "00:00" ELSE cFromTime:SCREEN-VALUE
      cToTime:SCREEN-VALUE   = IF int(EkspFrekvens:SCREEN-VALUE) <> 0 THEN "00:00" ELSE cToTime:SCREEN-VALUE
      cFromTime:MODIFIED     = FALSE
      cToTime:MODIFIED       = FALSE
      .

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
  DISPLAY EDBSystem DataType SysBeskrivelse Aktiv FilKatalog FilPrefix 
          FilEkstent EDB-System SeqvAktiv SeqNr MaksSeq EkspFrekvens cFromTime 
          cToTime TidsIntervall eksportRutine FI-3 FI-1 FI-2 FI-4 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectBrowse rectToolBar rectWinToolbar RECT-3 RECT-4 RECT-5 RECT-6 
         EDBSystem DataType SysBeskrivelse Aktiv FilKatalog FilPrefix 
         FilEkstent EDB-System SeqvAktiv SeqNr MaksSeq EkspFrekvens cFromTime 
         cToTime TidsIntervall eksportRutine 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow C-Win 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

  /* Oppsett av combo-box'er. */
  ASSIGN 
      EDB-System:DELIMITER         = "|"
      EDB-System:LIST-ITEM-PAIRS   = DYNAMIC-FUNCTION("GetFieldList","ImpHode;EDB-System|SystemBeskrivelse;EDB-System","WHERE TRUE")
      DataType:DELIMITER           = "|"
      DataType:LIST-ITEM-PAIRS     = DYNAMIC-FUNCTION("GetFieldList","EDBDataType;TypeBeskrivelse;DataType","WHERE TRUE BY DataType")
      EkspFrekvens:DELIMITER       = "|"
      EkspFrekvens:LIST-ITEM-PAIRS = "Tidsintervall|0|Dag|1|Uke|2|Måned|3|År|4"
      .

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",          
                    rectBrowse:HANDLE,             
                    100,                           
                    "",                            
                    "EkstEDBSystem;EDBSystem;DataType;SysBeskrivelse;EDB-System|MappingTbl;Aktiv|Aktiv|*/ ;FilKatalog;FilPrefix;FilEkstent;SeqvAktiv;SeqNr;MaksSeq;EkspFrekvens" +
                               ";+cFromTime|CHARACTER|x(5)|jbserv_int_to_hhmm_time.p (StartTid)|Fra" +
                               ";+cToTime|CHARACTER|x(5)|jbserv_int_to_hhmm_time.p (StoppTid)|Til" +
                               ";Tidsintervall;eksportRutine",
                    "WHERE true", 
                    "").                    
  hBrowse:NAME = "brwLeveringsform". 

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",    
                                                 
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE,  
                    "EDBSystem,DataType,SysBeskrivelse,EDB-System,Aktiv,FilKatalog,FilPrefix,FilEkstent,SeqvAktiv,SeqNr,MaksSeq,EkspFrekvens,Tidsintervall,eksportRutine",              
                    "",              
                    "","",                       
                    "cFromTime,cToTime"). 
  /* Legger på disse feltene i listen som skal oppdateres.       */
  /* Verdien som lagres i disse feltene legges inn i SaveRecord. */
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextrafields",                   
                   "StartTid,StoppTid").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customUpdateValProc","ignore").

  DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,            
                    "Fil",                         
                    "new;Ny,undo;Angre,delete;Slett,save;Lagre,excel;Eksporter til E&xcel",
                                                     
                                                    
                    "maxborder").                   
  

  DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE,          
                    "Fil",                          
                    "close;Avslutt",
                                                     
                                                    
                    "right,enable").                
  /* Link objects: */

  DYNAMIC-FUNCTION("LinkAllObjects",                
                                                    
                    THIS-PROCEDURE:CURRENT-WINDOW,  
                    TRUE,                           
                    STRING(rectWinToolBar:HANDLE)). 


  APPLY "value-changed" TO hBrowse.

END.

DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,150,0,250).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{&WINDOW-NAME}:WINDOW-STATE = 3.
{&WINDOW-NAME}:MOVE-TO-TOP().
APPLY "entry" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveRecord C-Win 
PROCEDURE saveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:
    IF DYNAMIC-FUNCTION("getToolbarState",rectToolBar:HANDLE) = "NEW" AND
       EDBSystem:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","EkstEDBSystem",
                                              "WHERE EDBSystem = '" + EDBSystem:SCREEN-VALUE + "'","EDBSystem") THEN
    DO:
        MESSAGE 
        "EDB system allerede registrert."         
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    IF EDBSystem:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Ugyldig verdi: '<Blank>'. Navn på EDBsystem må angis."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO EDBSystem.
        RETURN.
    END.
    IF DataType:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Ugyldig verdi: '<Blank>'. Datatype må angis."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO EDBSystem.
        RETURN.
    END.
    IF int(MaksSeq:SCREEN-VALUE) < 9 THEN DO:
        MESSAGE "Ugyldig verdi: '" + MaksSeq:SCREEN-VALUE + "'. Maks verdi for sekvens må være mellom 9 og 99999999."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO EDBSystem.
        RETURN.
    END.
    IF TRIM(SysBeskrivelse:SCREEN-VALUE) = "" THEN DO:
        MESSAGE "Ugyldig verdi. Beskrivelse må angis."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO SysBeskrivelse.
        RETURN.
    END.
    IF TRIM(FilKatalog:SCREEN-VALUE) = "" THEN DO:
        MESSAGE "Ugyldig verdi. Filkatalog må angis."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO Filkatalog.
        RETURN.
    END.
    IF eksportRutine:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Ugyldig verdi: '<Blank>'. Navn på eksportrutine må angis."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO EDBSystem.
        RETURN.
    END.
    /* Sjekker at eksportproceduren finnes og setter på riktig ekstent */
    IF search(entry(1,eksportRutine:SCREEN-VALUE,".") + ".p") = ? THEN DO:
      IF search(entry(1,eksportRutine:SCREEN-VALUE,".") + ".r") = ? THEN DO:
        IF search(entry(1,eksportRutine:SCREEN-VALUE,".") + ".w") = ? THEN DO:

            MESSAGE "Ugyldig eksportfilnavn. Angitte procedure finnes ikke (" + eksportRutine:SCREEN-VALUE + ")." SKIP
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "ENTRY" TO EDBSystem.
            RETURN.
        END.
        ELSE DO: 
            iLoop = NUM-ENTRIES(search(entry(1,eksportRutine:SCREEN-VALUE,".") + ".w"),"\").
            eksportRutine:SCREEN-VALUE = entry(iLoop,search(entry(1,eksportRutine:SCREEN-VALUE,".") + ".w"),"\").
        END.
      END.
      ELSE DO: 
          iLoop = NUM-ENTRIES(search(entry(1,eksportRutine:SCREEN-VALUE,".") + ".r"),"\").
          eksportRutine:SCREEN-VALUE = entry(iLoop,search(entry(1,eksportRutine:SCREEN-VALUE,".") + ".r"),"\").
      END.
    END.
    ELSE DO: 
        iLoop = NUM-ENTRIES(search(entry(1,eksportRutine:SCREEN-VALUE,".") + ".p"),"\").
        eksportRutine:SCREEN-VALUE = entry(iLoop,search(entry(1,eksportRutine:SCREEN-VALUE,".") + ".p"),"\").
    END.

/*     IF INPUT Aktiv = TRUE THEN                                                                                            */
/*     DO:                                                                                                                   */
/*         cTekst = DYNAMIC-FUNCTION("GetFieldList","EkstEDBSystem;EDBSystem","WHERE DataType = '" + DataType:SCREEN-VALUE + */
/*                                   "' and Aktiv = true and EDBSystem <> '" + EDBSystem:SCREEN-VALUE + "'").                */
/*         IF cTekst <> "" THEN DO:                                                                                          */
/*           MESSAGE                                                                                                         */
/*           "Følgende EDB system med samme datatype er allerede aktivert: " cTekst SKIP                                     */
/*           "Disse må deaktiveres før denne kan aktiveres."                                                                 */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                              */
/*           RETURN.                                                                                                         */
/*         END.                                                                                                              */
/*     END.                                                                                                                  */

END.

/* Legger inn verdier i tidsfeltene slik at skjermverdiene lagres i databasen. */
DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues",
                  STRING(INT(ENTRY(1,cFromTime:SCREEN-VALUE,":")) * 3600 + INT(ENTRY(2,cFromTime:SCREEN-VALUE,":")) * 60) + "|" 
                + STRING(INT(ENTRY(1,cToTime:SCREEN-VALUE,":")) * 3600 + INT(ENTRY(2,cToTime:SCREEN-VALUE,":")) * 60)
                  ).

RUN SUPER.

APPLY "ENTRY" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE undoRecord C-Win 
PROCEDURE undoRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN SUPER.
    APPLY "ENTRY" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChangedField C-Win 
PROCEDURE ValueChangedField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcFieldName AS CHAR NO-UNDO.

  CASE pcFieldName:
      WHEN "SeqvAktiv" THEN DO:
          ASSIGN
              SeqNr:SENSITIVE IN FRAME {&FRAME-NAME} = IF SeqvAktiv:SCREEN-VALUE = "no" THEN FALSE ELSE TRUE
              MaksSeq:SENSITIVE IN FRAME {&FRAME-NAME} = IF SeqvAktiv:SCREEN-VALUE = "no" THEN FALSE ELSE TRUE
              .
          IF SeqvAktiv:SCREEN-VALUE = "no" THEN
              ASSIGN
              SeqNr:SCREEN-VALUE   = "0"
              MaksSeq:SCREEN-VALUE = "99999999"
              .
      END.
      WHEN "EkspFrekvens" THEN DO:
          ASSIGN
              cFromTime:SENSITIVE IN FRAME {&FRAME-NAME} = IF int(EkspFrekvens:SCREEN-VALUE) <> 0 THEN FALSE ELSE TRUE
              cToTime:SENSITIVE IN FRAME {&FRAME-NAME} = IF int(EkspFrekvens:SCREEN-VALUE) <> 0 THEN FALSE ELSE TRUE
              cFromTime:SCREEN-VALUE = IF int(EkspFrekvens:SCREEN-VALUE) <> 0 THEN "00:00" ELSE cFromTime:SCREEN-VALUE
              cToTime:SCREEN-VALUE   = IF int(EkspFrekvens:SCREEN-VALUE) <> 0 THEN "00:00" ELSE cToTime:SCREEN-VALUE
              cFromTime:MODIFIED     = FALSE
              cToTime:MODIFIED       = FALSE
              .
      END.
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

