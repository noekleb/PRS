&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports114        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        Container for a JukeBox window program

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           18.oct.2006

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
&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1 
&ENDIF   
/* Uncomment to enable use of .Net components: */
/* &SCOPED-DEFINE AdvGuiWin */ 

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk           AS LOG    NO-UNDO.
DEF VAR ix            AS INT    NO-UNDO.
DEF VAR hSmallPicture AS HANDLE NO-UNDO.
DEF VAR hLargePicture AS HANDLE NO-UNDO.
DEF VAR hPictureFrame AS HANDLE NO-UNDO.
DEF VAR iCurrDocId    AS INT    NO-UNDO.
DEF VAR cOpenDirs     AS CHAR   NO-UNDO.
 

/*** Start instance property definitions for JBoxBrowse object oBrwItem ***/
DEF VAR oBrwItem AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE Item
    FIELD Itemnum AS integer
    FIELD ItemName AS character
    FIELD Category1 AS character
    FIELD Price AS decimal
    FIELD Onhand AS integer
    FIELD Allocated AS integer
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_Item FOR Item.


FUNCTION getBuffersAndFieldsBrwItem RETURNS CHARACTER():
  RETURN
    'Item'
     + ';Itemnum'
     + ';ItemName'
     + ';Category1'
     + ';Price'
     + ';Onhand'
     + ';Allocated'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwItem RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.


DEF VAR oFmItem AS JBoxFieldMap NO-UNDO.
DEF VAR otbItem AS JBoxToolbar NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwItem

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Item

/* Definitions for BROWSE BrwItem                                       */
&Scoped-define FIELDS-IN-QUERY-BrwItem Item.Itemnum Item.ItemName ~
Item.Category1 Item.Price Item.Onhand Item.Allocated 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwItem Item.Itemnum 
&Scoped-define QUERY-STRING-BrwItem FOR EACH Item NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwItem OPEN QUERY BrwItem FOR EACH Item NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwItem Item
&Scoped-define FIRST-TABLE-IN-QUERY-BrwItem Item


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS imgEiendom tbItem new_tbItem copy_tbItem ~
undo_tbItem delete_tbItem save_tbItem excel_tbItem LoadImage_tbItem ~
SaveImageToDisk_tbItem DeleteImage_tbItem BrwItem Itemnum ItemName ~
Category1 Price Onhand Allocated 
&Scoped-Define DISPLAYED-OBJECTS Itemnum ItemName Category1 Price Onhand ~
Allocated 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON copy_tbItem 
     IMAGE-UP FILE "bmp/copy.bmp":U
     LABEL "Kopier" 
     SIZE 6 BY 1.52 TOOLTIP "Kopier (ALT-K)".

DEFINE BUTTON DeleteImage_tbItem 
     LABEL "Delete image" 
     SIZE 14 BY 1.52.

DEFINE BUTTON delete_tbItem 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 6 BY 1.52 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON excel_tbItem 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 6 BY 1.52 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON LoadImage_tbItem 
     LABEL "Load image" 
     SIZE 13.2 BY 1.52.

DEFINE BUTTON new_tbItem 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 6 BY 1.52 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON SaveImageToDisk_tbItem 
     LABEL "Save image to disk" 
     SIZE 19 BY 1.52.

DEFINE BUTTON save_tbItem 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Lagre" 
     SIZE 6 BY 1.52 TOOLTIP "Lagre (CTRL-S)".

DEFINE BUTTON undo_tbItem 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Angre" 
     SIZE 6 BY 1.52 TOOLTIP "Angre (CTRL-Z)".

DEFINE VARIABLE Allocated AS INTEGER FORMAT "->>>>9" INITIAL 0 
     LABEL "Allocated" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1 TOOLTIP "Please enter allocated".

DEFINE VARIABLE Category1 AS CHARACTER FORMAT "x(30)" 
     LABEL "Category1" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 TOOLTIP "Please enter a category".

DEFINE VARIABLE ItemName AS CHARACTER FORMAT "x(25)" 
     LABEL "Item Name" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 TOOLTIP "Please enter an item name.".

DEFINE VARIABLE Itemnum AS INTEGER FORMAT "zzzzzzzzz9" INITIAL 0 
     LABEL "Item Num" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Please enter the item  number .".

DEFINE VARIABLE Onhand AS INTEGER FORMAT "->>>>9" INITIAL 0 
     LABEL "On Hand" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1 TOOLTIP "Please enter On Hand Qty".

DEFINE VARIABLE Price AS DECIMAL FORMAT "->,>>>,>>9.99" INITIAL 0 
     LABEL "Price" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 TOOLTIP "Please enter a Price.".

DEFINE RECTANGLE imgEiendom
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 51 BY 7.38.

DEFINE RECTANGLE tbItem
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 114 BY 1.71.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwItem FOR 
      Item SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwItem C-Win _STRUCTURED
  QUERY BrwItem NO-LOCK DISPLAY
      Item.Itemnum COLUMN-LABEL "Item Num" FORMAT "zzzzzzzzz9":U
            WIDTH 10.2
      Item.ItemName COLUMN-LABEL "Item Name" FORMAT "x(25)":U
      Item.Category1 COLUMN-LABEL "Category1" FORMAT "x(30)":U
      Item.Price COLUMN-LABEL "Price" FORMAT "->,>>>,>>9.99":U
      Item.Onhand COLUMN-LABEL "On Hand" FORMAT "->>>>9":U
      Item.Allocated COLUMN-LABEL "Allocated" FORMAT "->>>>9":U
  ENABLE
      Item.Itemnum HELP "Please enter the item  number ."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 52 BY 16.19 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     new_tbItem AT ROW 1.33 COL 2.2 WIDGET-ID 18
     copy_tbItem AT ROW 1.33 COL 8.4 WIDGET-ID 20
     undo_tbItem AT ROW 1.33 COL 14.4 WIDGET-ID 22
     delete_tbItem AT ROW 1.33 COL 20.4 WIDGET-ID 24
     save_tbItem AT ROW 1.33 COL 26.4 WIDGET-ID 26
     excel_tbItem AT ROW 1.33 COL 32.4 WIDGET-ID 28
     LoadImage_tbItem AT ROW 1.33 COL 38.4 WIDGET-ID 30
     SaveImageToDisk_tbItem AT ROW 1.33 COL 51.6 WIDGET-ID 34
     DeleteImage_tbItem AT ROW 1.33 COL 70.6 WIDGET-ID 32
     BrwItem AT ROW 3.62 COL 3 WIDGET-ID 200
     Itemnum AT ROW 3.62 COL 73 COLON-ALIGNED
     ItemName AT ROW 4.62 COL 73 COLON-ALIGNED
     Category1 AT ROW 5.62 COL 73 COLON-ALIGNED
     Price AT ROW 6.62 COL 73 COLON-ALIGNED
     Onhand AT ROW 7.62 COL 73 COLON-ALIGNED
     Allocated AT ROW 8.62 COL 73 COLON-ALIGNED
     imgEiendom AT ROW 11.71 COL 62 WIDGET-ID 2
     tbItem AT ROW 1.24 COL 2 WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 115.4 BY 19.52 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<Insert window title>"
         HEIGHT             = 19.52
         WIDTH              = 115.4
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 115.4
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 115.4
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
/* BROWSE-TAB BrwItem DeleteImage_tbItem DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 19.52
       FRAME DEFAULT-FRAME:WIDTH            = 115.4.

ASSIGN 
       tbItem:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "new;Ny,copy;Kopier,undo;Angre,delete;Slett,save;Lagre,excel;Eksporter til E&xcel,LoadImage;Load image,SaveImageToDisk;Save image to disk,DeleteImage;Delete imagemaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwItem
/* Query rebuild information for BROWSE BrwItem
     _TblList          = "sports114.Item"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"Item.Itemnum" "Item Num" "zzzzzzzzz9" "integer" ? ? ? ? ? ? yes "Please enter the item  number ." no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"Item.ItemName" "Item Name" "x(25)" "character" ? ? ? ? ? ? no "Please enter an item name." no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"Item.Category1" "Category1" "x(30)" "character" ? ? ? ? ? ? no "Please enter a category" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"Item.Price" "Price" "->,>>>,>>9.99" "decimal" ? ? ? ? ? ? no "Please enter a Price." no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"Item.Onhand" "On Hand" "->>>>9" "integer" ? ? ? ? ? ? no "Please enter On Hand Qty" no no "8.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"Item.Allocated" "Allocated" "->>>>9" "integer" ? ? ? ? ? ? no "Please enter allocated" no no "8.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwItem */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <Insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwItem
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
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
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

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteImageRecord C-Win 
PROCEDURE DeleteImageRecord :
IF NOT DYNAMIC-FUNCTION("runproc","jbdoc_delete_one_doc.p",STRING(iCurrDocId),?) THEN
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
ELSE RUN ViewImage.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
RUN SUPER.


RUN ViewImage.


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
  DISPLAY Itemnum ItemName Category1 Price Onhand Allocated 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE imgEiendom tbItem new_tbItem copy_tbItem undo_tbItem delete_tbItem 
         save_tbItem excel_tbItem LoadImage_tbItem SaveImageToDisk_tbItem 
         DeleteImage_tbItem BrwItem Itemnum ItemName Category1 Price Onhand 
         Allocated 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeComponents C-Win 
PROCEDURE InitializeComponents :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
        
  RUN JBoxSmallPicture.w PERSIST SET hSmallPicture.
  DYNAMIC-FUNCTION("setContext" IN hSmallPicture,"Eiendom").
  DYNAMIC-FUNCTION("setAsDropTarget" IN hSmallPicture,YES).
  RUN InitializeObject IN hSmallPicture (imgEiendom:HANDLE).
  hPictureFrame = DYNAMIC-FUNCTION("getFrameHandle" IN hSmallPicture).

  SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "ShowLargePicture" IN hSmallPicture.
  SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "CloseFromPictureView" IN hSmallPicture.
  SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "LoadDroppedFiles" IN hSmallPicture.
    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
RUN enable_UI.

DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DO WITH FRAME {&FRAME-NAME}:

  oBrwItem = NEW JBoxBrowse(brwItem:HANDLE).

  oFmItem = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmItem:updateFields = 'Itemnum,ItemName,Category1,Price,Onhand,Allocated'.
  oFmItem:primaryKeyFields = 'Itemnum'.

  oFmItem:BROWSE-OBJECT = oBrwItem.
  otbItem = NEW JBoxToolbar(tbItem:HANDLE).

  oBrwItem:TOOLBAR-OBJECT = otbItem.
  oFmItem:TOOLBAR-OBJECT = otbItem.
  
  RUN InitializeComponents.
END.
oBrwItem:OpenQuery().

DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"DeleteImage_tbItem").
DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,300,400,0,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadDroppedFiles C-Win
PROCEDURE LoadDroppedFiles:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihDropFrame AS HANDLE NO-UNDO.

DEF VAR ix AS INT NO-UNDO.
DEF VAR cFileNames AS CHAR   NO-UNDO.

IF NOT AVAIL Item THEN RETURN.

IF ihDropFrame:NUM-DROPPED-FILES > 1 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Max one image for item","","").
  RETURN.
END.
IF iCurrDocId NE 0 AND
   DYNAMIC-FUNCTION("DoMessage",0,4,"Replace image?","","") = 7
  THEN RETURN.

cFileNames = ihDropFrame:GET-DROPPED-FILE(1).

RUN JBoxPictureScale.w (INPUT-OUTPUT cFileNames,"").

DYNAMIC-FUNCTION("setDocLoadParam","replaceDocPair").
DYNAMIC-FUNCTION("LoadDocs",cFileNames,
                 "Item",STRING(Item.Itemnum),"").

iCurrDocId = 0.
RUN ViewImage.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadImageRecord C-Win 
PROCEDURE LoadImageRecord :
DEF VAR ix AS INT NO-UNDO.
DEF VAR cFileName AS CHAR   NO-UNDO.
DEF VAR bOK AS LOG NO-UNDO.

SYSTEM-DIALOG GET-FILE cFileName UPDATE bOk.

RUN JBoxPictureScale.w (INPUT-OUTPUT cFileName,"").

DYNAMIC-FUNCTION("setDocLoadParam","replaceDocPair").
DYNAMIC-FUNCTION("LoadDocs",cFileName,
                 "Item",STRING(Item.Itemnum),"").

iCurrDocId = 0.
RUN ViewImage.


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
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
  RUN ShowForm("").
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveImageToDiskRecord C-Win 
PROCEDURE SaveImageToDiskRecord :

DEF VAR cDir AS CHAR NO-UNDO.
DEF VAR oServer AS COM-HANDLE NO-UNDO.
 
  IF iCurrDocId = 0 THEN RETURN.

  SYSTEM-DIALOG GET-DIR cDir
         TITLE "Select directory".
  IF cDir NE "" THEN DO:   
    IF NOT DYNAMIC-FUNCTION("ViewDocs","Item",STRING(Item.Itemnum) + "|!small_",FALSE,cDir) THEN 
      MESSAGE DYNAMIC-FUNCTION ("getTransactionMessage")
      VIEW-AS ALERT-BOX.
    ELSE IF NOT CAN-DO(cOpenDirs,cDir) THEN DO:
      CREATE 'Shell.Application' oServer.
      NO-RETURN-VALUE oServer:Explore(cDir).
      RELEASE OBJECT oServer.
      cOpenDirs = cOpenDirs + (IF cOpenDirs NE "" THEN "," ELSE "") + cDir.
    END.      
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShowLargePicture C-Win 
PROCEDURE ShowLargePicture :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM  icSmallPicture AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOk           AS LOG NO-UNDO.

IF hSmallPicture NE SOURCE-PROCEDURE OR NOT AVAIL Item THEN RETURN.

IF VALID-HANDLE(hLargePicture) THEN DO:   
   hLargePicture:CURRENT-WINDOW:WINDOW-STATE = 3.
   hLargePicture:CURRENT-WINDOW:MOVE-TO-TOP().
END.
DYNAMIC-FUNCTION("ViewDocs","Item",STRING(Item.Itemnum) + "|!small_",FALSE,""). 

IF NOT VALID-HANDLE(hLargePicture) THEN DO:
  RUN JBoxLargePicture.w PERSIST SET hLargePicture.
  DYNAMIC-FUNCTION("setParent" IN hLargePicture,THIS-PROCEDURE).
  RUN InitializeObject IN hLargePicture.
END.

hLargePicture:CURRENT-WINDOW:TITLE = STRING(Item.Itemnum) + " - " + Item.ItemName. 
DYNAMIC-FUNCTION("LoadPictureFile" IN hLargePicture,ENTRY(1,DYNAMIC-FUNCTION("getTmpDocFileNames"),"|")). 

obOk = TRUE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ViewImage C-Win 
PROCEDURE ViewImage :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR iDocId AS INT NO-UNDO.

IF NOT VALID-HANDLE(hSmallPicture) THEN RETURN.

hPictureFrame:MOVE-TO-TOP().

IF AVAIL Item THEN DO WITH FRAME {&FRAME-NAME}:
  IF DYNAMIC-FUNCTION("getFieldValues","JBoxDocRel",
                      "WHERE JBoxDocRel.cContext = 'Item' AND cEntityId = '" + STRING(Item.Itemnum) + "' AND iJBoxDocumentId = " + STRING(iCurrDocId),
                      "iJboxDocumentId") NE ? THEN DO:  
    hPictureFrame:MOVE-TO-TOP().
    RETURN.
  END.
  IF DYNAMIC-FUNCTION("ViewDocs","Item",STRING(Item.Itemnum) + "|small_",FALSE,"") THEN
    iCurrDocId = INTEGER(ENTRY(1,DYNAMIC-FUNCTION("getDocFileIds"))).  
  ELSE iCurrDocId = 0.
       
  DYNAMIC-FUNCTION("ShowPicture" IN hSmallPicture,iCurrDocId).
END.  

ELSE DO:
  DYNAMIC-FUNCTION("ShowPicture" IN hSmallPicture,0).
  iCurrDocId = 0.
END. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

