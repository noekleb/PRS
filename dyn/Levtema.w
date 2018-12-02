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
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEF VAR icVarebehNr AS CHAR NO-UNDO INIT "10000029".
  DEF VAR icLevNr     AS CHAR NO-UNDO INIT "1".
&ELSE
  DEF INPUT PARAM icVarebehNr AS CHAR NO-UNDO.
  DEF INPUT PARAM icLevNr     AS CHAR NO-UNDO.
&ENDIF


/* Local Variable Definitions ---                                       */

DEF VAR bOK             AS LOG NO-UNDO.
DEF VAR ix              AS INT NO-UNDO.

DEF VAR hToolbar        AS HANDLE NO-UNDO.
DEF VAR hWinToolbar     AS HANDLE NO-UNDO.
DEF VAR hBrowse         AS HANDLE NO-UNDO.
DEF VAR hFieldMap       AS HANDLE NO-UNDO.
DEF VAR hBrwArtikler    AS HANDLE NO-UNDO.
DEF VAR hBuffArt        AS HANDLE NO-UNDO.
DEF VAR hTbArtikler     AS HANDLE NO-UNDO.

DEF VAR cAllCompIds     AS CHAR NO-UNDO.
DEF VAR cAllCompNames   AS CHAR NO-UNDO.
DEF VAR cDeleteList     AS CHAR NO-UNDO.
DEF VAR cNewList        AS CHAR NO-UNDO.
DEF VAR cSelected       AS CHAR NO-UNDO.

DEF VAR cVarebehNr      AS CHAR NO-UNDO.
DEF VAR cLevNr          AS CHAR NO-UNDO.

DEF VAR hRedArtSeq      AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolBar WinToolBar ~
BrwArtikler tbArtikler VbBeskrivelse RegistrertDato RegistrertAv EDato ~
BrukerID 
&Scoped-Define DISPLAYED-OBJECTS VbBeskrivelse RegistrertDato RegistrertAv ~
EDato BrukerID 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReposTo C-Win 
FUNCTION ReposTo RETURNS LOGICAL
  ( INPUT icRowId    AS CHAR,
    INPUT iiReposRow AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE BrukerID AS CHARACTER FORMAT "X(10)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE EDato AS DATE FORMAT "99/99/9999" 
     LABEL "Endret" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE RegistrertAv AS CHARACTER FORMAT "X(10)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE RegistrertDato AS DATE FORMAT "99/99/9999" 
     LABEL "Registrert" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE VbBeskrivelse AS CHARACTER FORMAT "X(40)" 
     LABEL "Tema" 
     VIEW-AS FILL-IN 
     SIZE 36.4 BY 1.

DEFINE RECTANGLE BrwArtikler
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 115 BY 8.91.

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 70 BY 7.38.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 28 BY .95.

DEFINE RECTANGLE tbArtikler
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 10.6 BY .91.

DEFINE RECTANGLE WinToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 8.8 BY 1.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     VbBeskrivelse AT ROW 2.62 COL 77.4 COLON-ALIGNED HELP
          "Kort beskrivelse av aktivitet"
     RegistrertDato AT ROW 7.62 COL 81.4 COLON-ALIGNED HELP
          "Dato da posten ble registrert i registeret"
     RegistrertAv AT ROW 7.62 COL 98.4 COLON-ALIGNED HELP
          "Brukerid på den som registrerte posten" NO-LABEL
     EDato AT ROW 8.71 COL 81.4 COLON-ALIGNED HELP
          "Endret dato"
     BrukerID AT ROW 8.71 COL 98.4 COLON-ALIGNED HELP
          "Bruker som registrerte/endret posten" NO-LABEL
     rectBrowse AT ROW 2.43 COL 2
     rectToolBar AT ROW 1.14 COL 1.8
     WinToolBar AT ROW 1.14 COL 107.4
     BrwArtikler AT ROW 11.71 COL 2
     tbArtikler AT ROW 10.57 COL 2.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 116.8 BY 19.76.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window Template
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Vedlikehold tema"
         HEIGHT             = 19.76
         WIDTH              = 116.8
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
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
ON END-ERROR OF C-Win /* Vedlikehold tema */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Vedlikehold tema */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Vedlikehold tema */
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
  IF VALID-HANDLE(hRedArtSeq) THEN APPLY "close" TO hRedArtSeq.
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
  DISPLAY VbBeskrivelse RegistrertDato RegistrertAv EDato BrukerID 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectBrowse rectToolBar WinToolBar BrwArtikler tbArtikler VbBeskrivelse 
         RegistrertDato RegistrertAv EDato BrukerID 
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
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                    rectBrowse:HANDLE,       
                    100,                     
                    "",                      
                    "VarebokTemaHode"
                    + ";VbBeskrivelse|Tema;Levnr;!RegistrertAv;!RegistrertDato;!BrukerID;!EDato;!VarebehNr;!VbTemeNr"
                  + ",LevBas"
                    + ";Levnamn"
                    ,"WHERE VarebehNr = " + icVarebehNr 
                      + (IF icLevNr NE "0" THEN " AND LevNr = " + icLevNr ELSE "")
                   + ",FIRST LevBas NO-LOCK OF VarebokTemaHode OUTER-JOIN"
                    ,"sort|VbBeskrivelse").            
  hBrowse:NAME = "brwVarebokTemaHode". 
  hBrowse:GET-BROWSE-COLUMN(1):WIDTH-PIXELS = 170.

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap", 
                    hBrowse:QUERY,
                    FRAME {&FRAME-NAME}:HANDLE, 
                    "VbBeskrivelse",            
                    "",                         
                    "RegistrertAv,RegistrertDato,BrukerID,EDato",
                    "",                             /* Additional buffer and displ.fields - not updateable*/
                    "").                            /* Misc - for something I might need in next version.. */
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customCreateProc","create_vareboktemahode.p").

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hBrowse).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,         
                    "Fil",                      
                    "new;Ny,copy;Kopier,undo;Angre,delete;Slett,save;Lagre,excel;Eksporter til E&xcel"
                   ,"maxborder"). 

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hBrowse).

  hBrwArtikler = DYNAMIC-FUNCTION("NewBrowse",
                    BrwArtikler:HANDLE,       
                    300,                     
                    "",                      
                    "VarebokTemaLinje;SeqNr|Sekv"
                   + ";!VbTemaNr;!VarebehNr"
                  + ",VarebehLinje"
                   + ";ArtikkelNr|SE Art.nr"
                   + ";Levkod|Lev.art.nr| x(15)"
                   + ";Beskr|Art.navn"
                   + ";Vg"
                   + ";VgBeskr"
                    ,"WHERE false"
                   + ",FIRST VarebehLinje OF VarebokTemaLinje NO-LOCK", 
                    "sort|SeqNr").            
  DYNAMIC-FUNCTION("CreateParentLink",hBrwArtikler,hBrowse,"VarebehNr,VbTemaNr;VbTemeNr").
  hBuffArt = hBrwArtikler:QUERY:GET-BUFFER-HANDLE(1).

  hTbArtikler = DYNAMIC-FUNCTION("NewToolBar",
                    tbArtikler:HANDLE,         
                    "Fil",  
                    "delete;Slett artikkel fra tema"
                  + ",MoveUp;Flytt opp;Flytt artikkel opp;MoveUp;gif/moveup.gif"
                  + ",MoveDown;Flytt ned;Flytt artikkel ned;MoveDown;gif/movedown.gif"
                   ,"maxborder"). 
  
  DYNAMIC-FUNCTION("setNoColumnSort",hBrwArtikler,"SeqNr,ArtikkelNr,LevKod,Beskr,Vg,VgBeskr").
  DYNAMIC-FUNCTION("setAttribute",hBrwArtikler,"customDeleteValProc","ignore").
  DYNAMIC-FUNCTION("CreateObjectLink",hTbArtikler,hBrwArtikler).


  hWinToolBar = DYNAMIC-FUNCTION("NewToolBar",
                    WinToolBar:HANDLE,             
                    "Fil",                          
                    "Help|Hjelp,Close;Avslutt",
                    "enable").   

  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar,tbArtikler,brwVarebokTemaHode,rectBrowse").
  DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,250,0,0).

  DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

  APPLY "value-changed" TO hBrowse.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveDown C-Win 
PROCEDURE MoveDown :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowId AS CHAR NO-UNDO.

cRowId = hBrwArtikler:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE.

IF NOT DYNAMIC-FUNCTION("runproc","varebeh_levtema_moveart.p",
                 cRowId + ",down",
                 ?) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
ELSE
  ReposTo(cRowId,hBrwArtikler:FOCUSED-ROW + 1).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveUp C-Win 
PROCEDURE MoveUp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cRowId AS CHAR NO-UNDO.

cRowId = hBrwArtikler:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE.

IF NOT DYNAMIC-FUNCTION("runproc","varebeh_levtema_moveart.p",
                 cRowId + ",up",
                 ?) THEN 
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
ELSE
  ReposTo(cRowId,hBrwArtikler:FOCUSED-ROW - 1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RedArtSeqRecord C-Win 
PROCEDURE RedArtSeqRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hRedArtSeq) THEN DO:
  RUN RedigerLevTema.w PERSIST SET hRedArtSeq.
  RUN InitializeObject IN hRedArtSeq.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getToolbarState",hToolbar) = "new" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraFields","VarebehNr,LevNr").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraValues",icVarebehNr + "|" + icLevNr).
END.
ELSE DO:
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraFields","").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraValues","").
END.
RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReposTo C-Win 
FUNCTION ReposTo RETURNS LOGICAL
  ( INPUT icRowId    AS CHAR,
    INPUT iiReposRow AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
APPLY "value-changed" TO hBrowse.
  
bOK = hBuffArt:FIND-FIRST("WHERE RowIdent1 = '" + icRowId + "'") NO-ERROR.
IF bOk THEN DO:
  hBrwArtikler:SET-REPOSITIONED-ROW(iiReposRow,"conditional").
  hBrwArtikler:QUERY:REPOSITION-TO-ROWID(hBuffArt:ROWID) NO-ERROR.
END.
  
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

