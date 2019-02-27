&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{JukeBoxControlsGeneral.i}
{JukeBoxNavBar.i}
{JukeBoxTreeView.i}

DEF VAR hControlsLibrary    AS HANDLE NO-UNDO.
DEF VAR iTree               AS INTEGER NO-UNDO.

DEF VAR ix                  AS INT    NO-UNDO.
DEF VAR bOk                 AS LOG    NO-UNDO.
DEF VAR hParent             AS HANDLE NO-UNDO.
DEF VAR iTreeStyle          AS INT    NO-UNDO.
DEF VAR cImageList          AS CHAR   NO-UNDO.
DEF VAR cImagePath          AS CHAR   NO-UNDO.
DEF VAR cLargeImageList     AS CHAR   NO-UNDO.
DEF VAR cLargeImagePath     AS CHAR   NO-UNDO.
DEF VAR iImage              AS INT    NO-UNDO.
DEF VAR iImgSelected        AS INT    NO-UNDO.
DEF VAR iStatus             AS INT    NO-UNDO.
DEF VAR iTreeNodeIndex      AS INT    NO-UNDO.
DEF VAR iNavBarNodeIndex    AS INT    NO-UNDO.
DEF VAR cColorNameList      AS CHAR   NO-UNDO
    INIT "AQUA,BLACK,BLUE,CREAM,DARKGRAY,FUCHSIA,GRAY,GREEN,LIMEGREEN,LIGHTGRAY,MAROON,MEDIUMGRAY,MINTGREEN,NAVYBLUE,OLIVE,PURPLE,RED,SILVER,SKYBLUE,TEAL,WHITE,YELLOW".
DEF VAR cColorNumList       AS CHAR
    INIT "16776960,0,16711680,15793151,8421504,16711935,8421504,32768,65280,12632256,128,10789024,12639424,8388608,32896,8388736,255,12632256,15780518,8421376,16777215,65535".


DEF TEMP-TABLE ttTree
    FIELD cKeyValue         AS CHAR
    FIELD cParentKeyValue   AS CHAR
    FIELD cNodeLabel        AS CHAR
    FIELD cImage            AS CHAR
    FIELD cSelImage         AS CHAR
    FIELD cNodeContext      AS CHAR
    FIELD iNodeKey          AS INT
    FIELD iParentNodeKey    AS INT
    FIELD iParentTree       AS INT      
    FIELD iNodeIndex        AS INT
    FIELD iNodeIndexNavBar  AS INT
    INDEX idxNodeKey         iNodeKey
    INDEX idxParentNodeKey   iParentNodeKey
    INDEX idxKeyValue        cKeyValue
    INDEX idxParentKeyValue  cParentKeyValue
    INDEX idxNodeIndex       iNodeIndex
    INDEX idxNodeIndexNavBar iNodeIndexNavBar
    .
DEF BUFFER bttTree FOR ttTree.
DEF VAR httTree AS HANDLE NO-UNDO.
httTree = BUFFER ttTree:HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME tree-frame

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddTreeListColumn C-Win 
FUNCTION AddTreeListColumn RETURNS LOGICAL
  ( INPUT icLabel       AS CHAR,
    INPUT icImage       AS CHAR,
    INPUT icFont        AS CHAR,
    INPUT ibSensitive   AS LOG,
    INPUT icDataType    AS CHAR,
    INPUT iiColumnWidth AS INT,
    INPUT iiJustify     AS INT,
    INPUT iiPosition    AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddTreeListFont C-Win 
FUNCTION AddTreeListFont RETURNS LOGICAL
  ( INPUT icFontName  AS CHAR,
    INPUT iiSize      AS INT,
    INPUT ibBold      AS LOG,
    INPUT ibItalic    AS LOG,
    INPUT ibUnderline AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DeleteNodes C-Win 
FUNCTION DeleteNodes RETURNS LOGICAL
  ( INPUT icNodeIdList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD dumpTT C-Win 
FUNCTION dumpTT RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ExpandNode C-Win 
FUNCTION ExpandNode RETURNS LOGICAL
  ( INPUT icKeyValue AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ExpandTree C-Win 
FUNCTION ExpandTree RETURNS LOGICAL
  ( INPUT icKeyList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getImageList C-Win 
FUNCTION getImageList RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getImgIndex C-Win 
FUNCTION getImgIndex RETURNS INTEGER
  ( INPUT icImage AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLargeImageList C-Win 
FUNCTION getLargeImageList RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTreeHandle C-Win 
FUNCTION getTreeHandle RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTreeViewFrame C-Win 
FUNCTION getTreeViewFrame RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitTree C-Win 
FUNCTION InitTree RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitTreeView C-Win 
FUNCTION InitTreeView RETURNS HANDLE
  ( INPUT ihTreeRect AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SelectNode C-Win 
FUNCTION SelectNode RETURNS LOGICAL
  ( INPUT icKeyValue AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setImage C-Win 
FUNCTION setImage RETURNS LOGICAL
  ( INPUT icImage AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setImageList C-Win 
FUNCTION setImageList RETURNS LOGICAL
  ( INPUT icImageList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setLargeImageList C-Win 
FUNCTION setLargeImageList RETURNS LOGICAL
  ( INPUT icLargeImageList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setNavBarImage C-Win 
FUNCTION setNavBarImage RETURNS LOGICAL
  ( INPUT icNavBarImage AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSelImage C-Win 
FUNCTION setSelImage RETURNS LOGICAL
  ( INPUT icImage AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTreeStyle C-Win 
FUNCTION setTreeStyle RETURNS LOGICAL
  ( INPUT iiTreeStyle AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setViewType C-Win 
FUNCTION setViewType RETURNS LOGICAL
  ( INPUT iViewType AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ShowTreeList C-Win 
FUNCTION ShowTreeList RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD showTreeView C-Win 
FUNCTION showTreeView RETURNS INTEGER
  ( INPUT iiTreeStyle AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME tree-frame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 38.8 BY 6.67.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 6.71
         WIDTH              = 39.2
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME tree-frame
   FRAME-NAME                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  RUN CloseWindow IN hParent NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
    APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tree-frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tree-frame C-Win
ON MOUSE-SELECT-UP OF FRAME tree-frame
DO:
  DEFINE VARIABLE cChar        AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cNodeContext AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cRowId       AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iCode        AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iLevel       AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iNode        AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iRecId       AS INTEGER     NO-UNDO.
  DEFINE VARIABLE oiNodeKey    AS INTEGER     NO-UNDO.

  IF iTreeStyle = 1 THEN DO:
    IF {&NAVBAR_GROUPCHANGED} THEN DO:
      PUBLISH "NavBarGroupChanged".
    END.
    ELSE DO:
      FIND FIRST ttTree
           WHERE ttTree.iNodeIndexNavBar = LAST-EVENT:X
             AND ttTree.cNodeContext     = "navbar-node"
           NO-ERROR.
      IF AVAIL ttTree THEN
        PUBLISH "tvNodeEvent" ("select",ttTree.cKeyValue).
      ELSE 
        MESSAGE PROGRAM-NAME(1) SKIP
                "Not avail" SKIP
                "mouse-select-up" SKIP
                "LAST-EVENT:X: " LAST-EVENT:X
                VIEW-AS ALERT-BOX.
    END.
  END.

  ELSE IF iTreeStyle = 2 THEN DO:
    IF LAST-EVENT:X = {&TREE_EVENT_ENDDRAG} THEN 
      RUN DropNode.
    ELSE DO:
      RUN TreeGetSelectedItem(iTree,OUTPUT iNode).
      cNodeContext = FILL(" ",50).
      cRowId     = FILL(" ",10). 
      RUN TreeGetSelExtraInfoEx(iTree,
                                OUTPUT cNodeContext,
                                OUTPUT cChar,
                                OUTPUT iCode,
                                OUTPUT iRecId,
                                OUTPUT cRowId,
                                OUTPUT oiNodeKey,
                                OUTPUT iLevel).
      
      FIND FIRST ttTree
           WHERE ttTree.iNodeKey = oiNodeKey
           NO-ERROR.
      IF AVAIL ttTree THEN DO:
        IF LAST-EVENT:X = {&TREE_EVENT_SINGLEEXPAND} THEN
          PUBLISH "tvNodeEvent" ("expand",ttTree.cKeyValue).
        ELSE DO:
          FIND FIRST bttTree
               WHERE bttTree.iParentNodeKey = ttTree.iNodeKey
               NO-ERROR.
          IF AVAIL bttTree THEN
            PUBLISH "tvNodeEvent" ("parentselect",ttTree.cKeyValue).
          ELSE
            PUBLISH "tvNodeEvent" ("select",ttTree.cKeyValue).
        END.
      END.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tree-frame C-Win
ON RIGHT-MOUSE-DOWN OF FRAME tree-frame
DO:
  DEFINE VARIABLE cString      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE hGroupHandle AS INTEGER     NO-UNDO.
  DEFINE VARIABLE oiNodeKey    AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iControlType AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iGroupId     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iMessage     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iParam1      AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iParam2      AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iRecordId    AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iSuccess     AS INTEGER     NO-UNDO.

  IF iTreeStyle = 1 THEN DO:
    ASSIGN iGroupId = LAST-EVENT:Y cString = FILL(" ",255).
    
    IF iGroupId > 0 THEN DO:
        RUN NavBarEmbGetMsg(iTree,OUTPUT iGroupId, OUTPUT hGroupHandle,
                            OUTPUT iControlType, OUTPUT iMessage,
                            OUTPUT iParam1, OUTPUT iParam2, OUTPUT iSuccess).
  
    END.
  
    RUN NavBarEmbGetSelNode(iTree,hGroupHandle,OUTPUT oiNodeKey, OUTPUT iRecordId,
                            OUTPUT cString, 255, OUTPUT iSuccess).
  

/*     MESSAGE "right-mouse-down" cString SKIP LAST-EVENT:Y SKIP oiNodeKey.  */

    FIND FIRST ttTree
         WHERE ttTree.iNodeIndex   = iRecordId
           AND ttTree.cNodeContext = "navbar-emb-tree-node"
         NO-ERROR.
    IF AVAIL ttTree THEN DO:
      PUBLISH "tvNodeEvent" ("select",ttTree.cKeyValue).

/*       IF LAST-EVENT:X = {&TREE_EVENT_SINGLEEXPAND} THEN            */
/*         PUBLISH "tvNodeEvent" ("expand",ttTree.cKeyValue).         */
/*       ELSE DO:                                                     */
/*         FIND FIRST bttTree                                         */
/*              WHERE bttTree.iParentNodeKey = ttTree.iNodeKey        */
/*              NO-ERROR.                                             */
/*         IF AVAIL bttTree THEN                                      */
/*           PUBLISH "tvNodeEvent" ("parentselect",ttTree.cKeyValue). */
/*         ELSE                                                       */
/*           PUBLISH "tvNodeEvent" ("select",ttTree.cKeyValue).       */
/*       END.                                                         */
    END.
    ELSE 
      MESSAGE PROGRAM-NAME(1) SKIP
              "Not avail" SKIP
              "right-mouse-select" SKIP
              "iRecordId: " iRecordId
              VIEW-AS ALERT-BOX.

  END.
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
  RUN disable_UI.

/*   RUN ReleaseDll IN hControlsLibrary. */
/*   DELETE PROCEDURE hControlsLibrary.  */
  APPLY "close" TO hParent.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  hParent = SOURCE-PROCEDURE.
  RUN enable_UI.
/*   RUN Controls.p PERSISTENT SET hControlsLibrary. */

  SUBSCRIBE TO "JlwTreeViewToExcel" ANYWHERE.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddNode C-Win 
PROCEDURE AddNode :
DEF INPUT PARAM icKeyValue        AS CHAR NO-UNDO.
DEF INPUT PARAM icParentKeyValue  AS CHAR NO-UNDO.
DEF INPUT PARAM ibHasChildSubMenu AS LOG  NO-UNDO.
DEF INPUT PARAM icNodeLabel       AS CHAR NO-UNDO.
DEF INPUT PARAM icTooltip         AS CHAR NO-UNDO.
DEF INPUT PARAM icImage           AS CHAR NO-UNDO.
DEF INPUT PARAM icSelImage        AS CHAR NO-UNDO.
DEF INPUT PARAM icStateImage      AS CHAR NO-UNDO.
DEF INPUT PARAM icStyle           AS CHAR NO-UNDO.
DEF INPUT PARAM icColor           AS CHAR NO-UNDO.
DEF INPUT PARAM icEnableDisable   AS CHAR NO-UNDO. /* enable/disable */
DEF INPUT PARAM icNodeType        AS CHAR NO-UNDO. /* sub-menu/menu-item */
/*------------------------------------------------------------------------------
------------------------------------------------------------------------------*/
DEF VAR iNodeIdx   AS INT NO-UNDO.
DEF VAR iImgIdx    AS INT NO-UNDO.
DEF VAR iSelImgIdx AS INT NO-UNDO.
DEF VAR iParentKey AS INT NO-UNDO.
DEF VAR iTreeKey   AS INT NO-UNDO.

IF icImage NE "" THEN DO:    
  iImgIdx = getImgIndex(icImage).
  IF iImgIdx < 0 THEN DO:
    MESSAGE "Image " icImage " not in image-list" SKIP
            "Node not created" SKIP(1) 
            "Image path: " cImagePath SKIP(1)
            "Image list: " SKIP 
             cImageList SKIP            
             VIEW-AS ALERT-BOX.
    RETURN.
  END.
END.
ELSE iImgIdx = iImage.

/*      MESSAGE "Image " icImage SKIP            */
/*              SEARCH(icImage) SKIP(1)          */
/*              "Image path: " cImagePath SKIP(1)*/
/*              "Image list: " cImageList SKIP(1)*/
/*              "iImgIdx: " iImgIdx              */
/*               VIEW-AS ALERT-BOX.              */


IF icSelImage NE "" THEN DO:
  iSelImgIdx = getImgIndex(icSelImage).
  IF iSelImgIdx < 0 THEN 
    iSelImgIdx  = iImgSelected.
END.
ELSE iSelImgIdx  = iImgSelected.

FIND FIRST bttTree
     WHERE bttTree.cKeyValue = icParentKeyValue
     NO-ERROR.
IF AVAIL bttTree THEN
  ASSIGN iParentKey = bttTree.iNodeKey
         iTreeKey   = bttTree.iParentTree.

CREATE ttTree.
ASSIGN ttTree.cKeyValue        = icKeyValue
       ttTree.cParentKeyValue  = icParentKeyValue
       ttTree.cNodeLabel       = icNodeLabel
       ttTree.cImage           = icImage
       ttTree.cSelImage        = icSelImage
       ttTree.iParentNodeKey   = iParentKey
       ttTree.iParentTree      = iTreeKey
       .

IF iTreeStyle = 1 THEN DO:       /* NavBar */
  IF NOT AVAIL bttTree THEN DO:      

    IF icNodeType = "sub-menu" AND ibHasChildSubMenu THEN DO:

      RUN NavBarAddGroup(iTree,icNodeLabel,iImgIdx,{&NAVBAR_GROUP-EXPANDED},
                         {&EMBEDDED-TREEVIEW},OUTPUT ttTree.iNodeKey).

      RUN NavBarEmbSetBooleanProperty(iTree,ttTree.iNodeKey, {&EMB_TV-SHOWROOT},1, OUTPUT iStatus).
      RUN NavBarEmbSetBooleanProperty(iTree,ttTree.iNodeKey, {&EMB_TV-AUTOEXPAND},1, OUTPUT iStatus).  
      RUN NavBarEmbSetBooleanProperty(iTree,ttTree.iNodeKey, {&EMB_TV-ROWSELECT},1, OUTPUT iStatus).
      RUN NavBarEmbSetBooleanProperty(iTree,ttTree.iNodeKey, {&EMB_TV-HOTTRACK},1, OUTPUT iStatus).
      RUN NavBarEmbSetBooleanProperty(iTree,ttTree.iNodeKey,{&EMB_TV-TOOLTIPS},0,OUTPUT iStatus).
      ttTree.iParentTree = ttTree.iNodeKey.
    END.
    ELSE
      RUN NavBarAddGroup(iTree,icNodeLabel,iImgIdx,{&NAVBAR_GROUP-EXPANDED},-1,OUTPUT ttTree.iNodeKey).

    ttTree.iNodeIndex  = -1.

  END.
  ELSE DO:      
    IF iTreeKey NE 0 THEN DO:
      RUN NavBarEmbAddTreeNode(iTree,
                               iTreeKey,
                               (IF iTreeKey = iParentKey THEN 0 ELSE iParentKey),
                               iTreeNodeIndex,
                               "",
                               icNodeLabel,
                               "",
                               iImgIdx,
                               -1,
                               -1,
                               {&COLOR-BLACK},
                               0,
                               1,
                               {&NAVBAR_VAL-ID} + {&NAVBAR_VAL-ENABLED} + {&NAVBAR_VAL-COLOR} + {&NAVBAR_VAL-IMAGE} + {&NAVBAR_VAL-ATTRIBUTE},
                               OUTPUT ttTree.iNodeKey).

     ASSIGN ttTree.iNodeIndex   = iTreeNodeIndex
            iTreeNodeIndex      = iTreeNodeIndex + 1
            ttTree.cNodeContext = "navbar-emb-tree-node".
   END.
   ELSE DO:
     RUN NavBarAddGroupItem(iTree,iParentKey,icNodeLabel,iImgIdx,OUTPUT ttTree.iNodeKey).
     ASSIGN ttTree.iNodeIndexNavBar = iNavBarNodeIndex
            iNavBarNodeIndex        = iNavBarNodeIndex + 1
            ttTree.cNodeContext     = "navbar-node".
   END.

  END.

END.
ELSE IF iTreeStyle = 2 THEN DO:  /* TreeView */
  RUN TreeAddLastEx (iTree,icNodeLabel,iParentKey,iSelImgIdx,iImgIdx,
                     IF icStyle MATCHES "*bold*" THEN {&TREE_ITEM_BOLD} ELSE {&TREE_ITEM_NO-BOLD},
                     OUTPUT ttTree.iNodeKey).
  RUN TreeSetSelExtraInfoEx(iTree,"",ASC("A":U),0,0,"",ttTree.iNodeKey).
END.
ELSE IF iTreeStyle = 3 THEN DO:  /* TreeListView */
  ASSIGN iTreeNodeIndex    = iTreeNodeIndex + 1
         ttTree.iNodeIndex = iTreeNodeIndex
         .
  RUN TreeListAddLast(iTree,iParentKey,icNodeLabel,iTreeNodeIndex,iSelImgIdx,iImgIdx,0,
/*                       IF icStyle MATCHES "*bold*" THEN {&TREE_ITEM_BOLD} ELSE {&TREE_ITEM_NO-BOLD}, */
                      OUTPUT ttTree.iNodeKey).
/*   MESSAGE PROGRAM-NAME(1) SKIP                       */
/*           "iParentKey: " iParentKey skip             */
/*           "icNodeLabel: " icNodeLabel SKIP           */
/*           "iImgIdx: " iImgIdx icImage SKIP           */
/*           "iSelImgIdx: " iSelImgIdx icSelImage SKIP  */
/*           "ttTree.iNodeKey: " ttTree.iNodeKey        */
/*           VIEW-AS ALERT-BOX.                         */
END.
ELSE DO:
  MESSAGE "TreeView style not set: " SKIP
          "Use function setTreeStyle()" SKIP
          "1: Navbar, 2: Standard"
          VIEW-AS ALERT-BOX ERROR.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AddNodeColumn C-Win 
PROCEDURE AddNodeColumn :
DEF INPUT PARAM icKeyValue       AS CHAR NO-UNDO.
DEF INPUT PARAM icData           AS CHAR NO-UNDO.
DEF INPUT PARAM iiColumnIndex    AS INT  NO-UNDO.
DEF INPUT PARAM iiFontIndex      AS INT  NO-UNDO.
DEF INPUT PARAM iiColorIndex     AS INT  NO-UNDO.
/*------------------------------------------------------------------------------
------------------------------------------------------------------------------*/


IF iTreeStyle NE 3 THEN DO:
  MESSAGE "Invalid tree-style " iTreeStyle " for tree-list columns" SKIP
          PROGRAM-NAME(1)
          VIEW-AS ALERT-BOX ERROR.
  RETURN.   
END.

FIND FIRST bttTree
     WHERE bttTree.cKeyValue = icKeyValue
     NO-ERROR.
IF NOT AVAIL bttTree THEN DO:
  MESSAGE "Invalid node key " icKeyValue " for tree-list columns" SKIP
          PROGRAM-NAME(1)
          VIEW-AS ALERT-BOX ERROR.
  RETURN.   
END.


RUN TreeListSetSubItem(iTree,bttTree.iNodeKey,iiColumnIndex,icData,iiFontIndex,iiColorIndex,OUTPUT iStatus).

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
  /* Hide all frames. */
  HIDE FRAME tree-frame.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DropNode C-Win 
PROCEDURE DropNode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iDropNode   AS INTEGER NO-UNDO.  
DEF VAR iDragItem     AS INTEGER NO-UNDO.
DEF VAR iDropItem   AS INTEGER NO-UNDO.
DEF VAR iError        AS INTEGER NO-UNDO.
DEF VAR iDragNode     AS INTEGER NO-UNDO.

DEF VAR cDragTableName    AS CHARACTER NO-UNDO.
DEF VAR cDragChar         AS INTEGER   NO-UNDO.
DEF VAR iDragCode         AS INTEGER   NO-UNDO.
DEF VAR iDragRecId        AS INTEGER   NO-UNDO.
DEF VAR cDragRowId        AS CHARACTER NO-UNDO.
DEF VAR iDragNodeKey      AS INTEGER   NO-UNDO.
DEF VAR iDragLevel        AS INTEGER   NO-UNDO.

DEF VAR cDropTableName    AS CHARACTER NO-UNDO.
DEF VAR cDropChar         AS INTEGER   NO-UNDO.
DEF VAR iDropCode         AS INTEGER   NO-UNDO.
DEF VAR iDropRecId        AS INTEGER   NO-UNDO.
DEF VAR cDropRowId        AS CHARACTER NO-UNDO.
DEF VAR iDropNodeKey      AS INTEGER   NO-UNDO.
DEF VAR iDropLevel        AS INTEGER   NO-UNDO.


RUN TreeGetDragDropItems (iTree, INPUT-OUTPUT iDragItem, INPUT-OUTPUT iDropItem, OUTPUT iError).

IF iError = 0 THEN RETURN.

IF (iDragItem = 0 OR iDragItem = ?) OR
   (iDropItem = 0 OR iDropItem = ?)    
   THEN RETURN.

RUN TreeSetSelectedItem(iTree, iDragItem).
RUN TreeGetSelectedItem(iTree, OUTPUT iDragNode).

ASSIGN cDragTableName = FILL(" ",50)
       cDragRowId     = FILL(" ",10)
       cDropTableName = FILL(" ",50)
       cDropRowId     = FILL(" ",10)
       .
RUN TreeGetSelExtraInfoEx(iTree,
                        OUTPUT cDragTableName,
                        OUTPUT cDragChar,
                        OUTPUT iDragCode,
                        OUTPUT iDragRecId,
                        OUTPUT cDragRowId,
                        OUTPUT iDragNodeKey,
                        OUTPUT iDragLevel).

RUN TreeSetSelectedItem(iTree, iDropItem).
RUN TreeGetSelectedItem(iTree, OUTPUT iDropNode).
RUN TreeGetSelExtraInfoEx(iTree,
                        OUTPUT cDropTableName,
                        OUTPUT cDropChar,
                        OUTPUT iDropCode,
                        OUTPUT iDropRecId,
                        OUTPUT cDropRowId,
                        OUTPUT iDropNodeKey,
                        OUTPUT iDropLevel).

IF iDropNodeKey = 0 THEN RETURN.

FIND FIRST ttTree
     WHERE ttTree.iNodeKey = iDragNodeKey
     NO-ERROR.
FIND FIRST bttTree
     WHERE bttTree.iNodeKey = iDropNodeKey
     NO-ERROR.

IF AVAIL ttTree AND AVAIL bttTree THEN
  PUBLISH "tvNodeEvent" ("dragdrop",ttTree.cKeyValue + "|" + bttTree.cKeyValue).

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
  VIEW FRAME tree-frame.
  {&OPEN-BROWSERS-IN-QUERY-tree-frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JlwTreeViewToExcel C-Win 
PROCEDURE JlwTreeViewToExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("ToExcelViaFile",httTree,0).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddTreeListColumn C-Win 
FUNCTION AddTreeListColumn RETURNS LOGICAL
  ( INPUT icLabel       AS CHAR,
    INPUT icImage       AS CHAR,
    INPUT icFont        AS CHAR,
    INPUT ibSensitive   AS LOG,
    INPUT icDataType    AS CHAR,
    INPUT iiColumnWidth AS INT,
    INPUT iiJustify     AS INT,
    INPUT iiPosition    AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iImgIdx    AS INT NO-UNDO.
DEF VAR iDataType  AS INT NO-UNDO.

IF iTreeStyle NE 3 THEN DO:
  MESSAGE "Invalid tree-style " iTreeStyle " for tree-list columns" SKIP
          PROGRAM-NAME(1)
          VIEW-AS ALERT-BOX ERROR.
  RETURN FALSE.   
END.

IF icImage NE "" THEN DO:    
  iImgIdx = getImgIndex(icImage).
  IF iImgIdx < 0 THEN DO:
    MESSAGE "Image " icImage " not in image-list" SKIP
            "Node not created"
             VIEW-AS ALERT-BOX.
    RETURN NO.
  END.
END.
ELSE iImgIdx = iImage.

CASE icDataType:
  WHEN "CHARACTER" THEN iDataType = 0.
  WHEN "DECIMAL"   THEN iDataType = 1.
  OTHERWISE        iDataType = 0.
END CASE.

RUN TreeListInsertColumn (iTree,icLabel,iImgIdx,
                          0,  /* FONT index */
                          0,  /* Read-only */
                          0,  /* Data-type */
                          iiColumnWidth,iiJustify,5,OUTPUT iStatus).

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddTreeListFont C-Win 
FUNCTION AddTreeListFont RETURNS LOGICAL
  ( INPUT icFontName  AS CHAR,
    INPUT iiSize      AS INT,
    INPUT ibBold      AS LOG,
    INPUT ibItalic    AS LOG,
    INPUT ibUnderline AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

IF iTreeStyle NE 3 THEN DO:
  MESSAGE "Invalid tree-style " iTreeStyle " for tree-list" SKIP
          PROGRAM-NAME(1)
          VIEW-AS ALERT-BOX ERROR.
  RETURN FALSE.   
END.


RUN TreeListAddFont (iTree,icFontName,iiSize,
                     IF ibBold THEN 1 ELSE 0,  
                     IF ibItalic THEN 1 ELSE 0,  
                     IF ibUnderline THEN 1 ELSE 0,
                     OUTPUT iStatus).

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DeleteNodes C-Win 
FUNCTION DeleteNodes RETURNS LOGICAL
  ( INPUT icNodeIdList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: The navbar must be destroyed - individual nodes cannot be deleted 
------------------------------------------------------------------------------*/
IF iTreeStyle = 1 THEN DO:
  RUN NavBarDestroy(iTree,OUTPUT iStatus).
  EMPTY TEMP-TABLE ttTree.
  InitTree().
END.
ELSE DO:
  IF icNodeIdList NE "" THEN DO ix = 1 TO NUM-ENTRIES(icNodeIdList,"|"):
    FIND FIRST ttTree
         WHERE ttTree.cKeyValue = ENTRY(ix,icNodeIdList)
         NO-ERROR.
    IF AVAIL ttTree THEN DO:
      RUN TreeDeleteItem (iTree,ttTree.iNodeKey).
      DELETE ttTree.
    END.
  END.
  ELSE DO: 
    FOR EACH ttTree:
      RUN TreeDeleteItem (iTree,ttTree.iNodeKey).
      DELETE ttTree.
    END.
    IF cImagePath <> "" THEN
      RUN TreeInitImageList(iTree,cImagePath,cImageList).
  END.
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION dumpTT C-Win 
FUNCTION dumpTT RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("ToExcelViaFile",httTree,0).

RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ExpandNode C-Win 
FUNCTION ExpandNode RETURNS LOGICAL
  ( INPUT icKeyValue AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FIND FIRST ttTree 
     WHERE ttTree.cKeyValue = icKeyValue
     NO-ERROR.
IF AVAIL ttTree THEN
  RUN TreeExpandBranch (iTree, ttTree.iNodeKey).
ELSE
  RETURN NO.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ExpandTree C-Win 
FUNCTION ExpandTree RETURNS LOGICAL
  ( INPUT icKeyList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icKeyList = "" THEN
  FOR EACH ttTree 
      WHERE ttTree.cParentKeyValue NE "":
    RUN TreeExpandBranch (iTree, ttTree.iNodeKey).
  END.
ELSE DO ix = 1 TO NUM-ENTRIES(icKeyList,"|"):
  IF TRIM(ENTRY(ix,icKeyList,"|")) NE "" THEN
    ExpandNode(TRIM(ENTRY(ix,icKeyList,"|"))).
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getImageList C-Win 
FUNCTION getImageList RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN cImageList.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getImgIndex C-Win 
FUNCTION getImgIndex RETURNS INTEGER
  ( INPUT icImage AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iImgIdx AS INT NO-UNDO.

IF icImage NE "" THEN DO:  
  IF cLargeImageList NE "" THEN DO:
    iImgIdx = LOOKUP(icImage,cLargeImageList,";") - 1.
    IF iImgIdx < 0 THEN DO:
      IF INDEX(icImage,"\") > 0 THEN 
        icImage = SUBSTR(icImage,R-INDEX(icImage,"\") + 1).  
      ELSE IF INDEX(icImage,"/") > 0 THEN  
        icImage = SUBSTR(icImage,R-INDEX(icImage,"/") + 1).  
      iImgIdx = LOOKUP(icImage,cLargeImageList,";") - 1.
    END.
  END.
  ELSE iImgIdx = -1.

  IF iImgIdx < 0 THEN DO:
    iImgIdx = LOOKUP(icImage,cImageList,";") - 1.
    IF iImgIdx < 0 THEN DO:
      IF INDEX(icImage,"\") > 0 THEN 
        icImage = SUBSTR(icImage,R-INDEX(icImage,"\") + 1).  
      ELSE IF INDEX(icImage,"/") > 0 THEN  
        icImage = SUBSTR(icImage,R-INDEX(icImage,"/") + 1).  
      iImgIdx = LOOKUP(icImage,cImageList,";") - 1.
    END.
  END.
END.

RETURN iImgIdx.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLargeImageList C-Win 
FUNCTION getLargeImageList RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN cLargeImageList.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTreeHandle C-Win 
FUNCTION getTreeHandle RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN iTree.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTreeViewFrame C-Win 
FUNCTION getTreeViewFrame RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN FRAME tree-frame:HANDLE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitTree C-Win 
FUNCTION InitTree RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF iTreeStyle = 1 THEN DO:      
  RUN NavBarCreate(FRAME tree-frame:HWND,OUTPUT iTree).

/*   RUN NavBarSetDefaultStyle(iTree,{&NAVBAR_DEFAULTELEMENT-BACKGROUND},                                   */
/*                             0,0,0,0,"",0,0,0,0,0,0,0,"C:\appdir\91d\skotex\img\icon\KundeLogo.bmp",0,0,  */
/*                             {&NAVBAR_STYLEVALID-ALPHA1} + {&NAVBAR_STYLEVALID-ALPHA2} +                  */
/*                             {&NAVBAR_STYLEVALID-BACKGROUNDIMAGE},OUTPUT iStatus).                        */
/*   RUN NavBarSetDefaultStyle(iTree,{&NAVBAR_DEFAULTELEMENT-GROUPBACKGROUND},                        */
/*                             0,0,0,0,"",0,0,0,0,0,0,0,"",0,0,                                       */
/*                             {&NAVBAR_STYLEVALID-ALPHA2},OUTPUT iStatus).                           */
/*   RUN NavBarSetDefaultStyle(iTree,{&NAVBAR_DEFAULTELEMENT-GROUPHEADER},                            */
/*                             0,0,0,0,"",0,0,0,0,0,0,0,"",0,0,                                       */
/*                             {&NAVBAR_STYLEVALID-ALPHA2},OUTPUT iStatus).                           */
/*   RUN NavBarSetDefaultStyle(iTree,{&NAVBAR_DEFAULTELEMENT-GROUPHEADERHOTTRACKED},                  */
/*                             0,0,0,0,"",0,0,0,0,0,0,0,"",0,0,                                       */
/*                             {&NAVBAR_STYLEVALID-ALPHA2},OUTPUT iStatus).                           */


  RUN NavBarLoadLargeImageList(iTree,cLargeImagePath,cLargeImageList,
                               {&NAVBAR_LARGE-IMAGELIST-32X32},
                               OUTPUT iStatus).
  RUN NavBarLoadSmallImageList(iTree,cImagePath,cImageList,OUTPUT iStatus).

/*   RUN NavBarSetDefaultStyle(iTree,{&NAVBAR_DEFAULTELEMENT-GROUPBACKGROUND},                     */
/*                           0,0,700,1048560,"",0,0,0,0,0,0,{&NAVBAR_GRADIENT-HORIZONTAL},"",0,0,  */
/*                           {&NAVBAR_STYLEVALID-BACKCOLOR1} +                                     */
/*                           {&NAVBAR_STYLEVALID-BACKCOLOR2} +                                     */
/*                           {&NAVBAR_STYLEVALID-GRADIENTMODE} ,OUTPUT iStatus).                   */

  
  RUN NavBarSetView(iTree,16,OUTPUT iStatus).
  RUN NavBarSetBooleanProperty(iTree,{&NAVBAR_WANT-MSG-GROUPCHANGE},{&GEN_ENABLE},OUTPUT iStatus).
END.
ELSE IF iTreeStyle = 2 THEN DO:
  RUN TreeCreate (FRAME tree-frame:HWND,0,OUTPUT iTree).
  IF cImagePath <> "" THEN
    RUN TreeInitImageList(iTree,cImagePath,cImageList).
  RUN TreeSetStyle(iTree,5,1).
  RUN TreeSetStyle(iTree, 7, 1).
  RUN TreeInitFont(iTree,"MS Sans Serif",8).

  RUN TreeShow(iTree).
END.
ELSE IF iTreeStyle = 3 THEN DO:    
  RUN TreeListCreate(FRAME tree-frame:HWND,OUTPUT iTree).
  IF cImagePath <> "" THEN
    RUN TreeListInitImageList(iTree,1,cImagePath,cImageList,OUTPUT iStatus).
END.

  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitTreeView C-Win 
FUNCTION InitTreeView RETURNS HANDLE
  ( INPUT ihTreeRect AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hParent) THEN hParent = SOURCE-PROCEDURE.

DO WITH FRAME tree-frame:
  ASSIGN FRAME tree-frame:X = ihTreeRect:X 
         FRAME tree-frame:Y = ihTreeRect:Y 
         FRAME tree-frame:HEIGHT-PIXELS = ihTreeRect:HEIGHT-PIXELS
         FRAME tree-frame:WIDTH-PIXELS  = ihTreeRect:WIDTH-PIXELS
         .

  InitTree().
END.

RETURN FRAME tree-frame:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SelectNode C-Win 
FUNCTION SelectNode RETURNS LOGICAL
  ( INPUT icKeyValue AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF iTreeStyle = 2 THEN DO:
  FIND FIRST ttTree 
       WHERE ttTree.cKeyValue = TRIM(icKeyValue)
       NO-ERROR.
  IF AVAIL ttTree THEN 
    RUN TreeSetSelectedItem (iTree, ttTree.iNodeKey).
  ELSE
    RETURN NO.
END.
ELSE RETURN NO.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setImage C-Win 
FUNCTION setImage RETURNS LOGICAL
  ( INPUT icImage AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
iImage = getImgIndex(icImage).
IF iImage < 0 THEN DO:
  MESSAGE "Image " icImage " not found in image-list"
          VIEW-AS ALERT-BOX WARNING.
  iImage = 0.
  RETURN NO.
END.
ELSE RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setImageList C-Win 
FUNCTION setImageList RETURNS LOGICAL
  ( INPUT icImageList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cNewImgList AS CHAR NO-UNDO.
DEF VAR cImgEntry   AS CHAR NO-UNDO.
DEF VAR cImg        AS CHAR NO-UNDO.

cImageList = icImageList.

IF cImageList NE "" THEN DO ix = 1 TO NUM-ENTRIES(cImageList,";"):
  
  cImgEntry = ENTRY(ix,cImageList,";").

  IF cImgEntry = "" THEN NEXT.

  IF R-INDEX(cImgEntry,"/") > 0 THEN
    cImg = ENTRY(NUM-ENTRIES(cImgEntry,"/"),cImgEntry,"/").
  ELSE IF R-INDEX(cImgEntry,"\") > 0 THEN
    cImg = ENTRY(NUM-ENTRIES(cImgEntry,"\"),cImgEntry,"\").
  ELSE IF SEARCH(cImgEntry) = ? THEN DO:
    IF SEARCH("bmp\" + cImgEntry) NE ? THEN
      cImgEntry = "bmp\" + cImgEntry.
    ELSE IF SEARCH("ico\" + cImgEntry) NE ? THEN
      cImgEntry = "ico\" + cImgEntry.
    ELSE IF SEARCH("newbmp\" + cImgEntry) NE ? THEN
      cImgEntry = "newbmp\" + cImgEntry.
    ELSE IF SEARCH("tvpics\" + cImgEntry) NE ? THEN
      cImgEntry = "tvpics\" + cImgEntry.
  END.

  IF SEARCH(SESSION:TEMP-DIR + cImg) = ? THEN DO:
    IF  SEARCH(cImgEntry) NE ? THEN DO:
      FILE-INFO:FILE-NAME = cImgEntry.
      OS-COPY VALUE(FILE-INFO:FULL-PATHNAME) VALUE(SESSION:TEMP-DIR).
    END.
    ELSE 
      MESSAGE "Invalid image entry: " cImgEntry SKIP
              "Must be in PROPATH"
              VIEW-AS ALERT-BOX WARNING.
  END.

  IF R-INDEX(cImgEntry,"/") > 0 THEN DO:    
    IF LOOKUP(SUBSTR(cImgEntry,R-INDEX(cImgEntry,"/") + 1),cNewImgList,";") = 0 THEN
      cNewImgList = cNewImgList + SUBSTR(cImgEntry,R-INDEX(cImgEntry,"/") + 1) + ";".
  END.
  ELSE IF R-INDEX(cImgEntry,"\") > 0 THEN DO:
    IF LOOKUP(SUBSTR(cImgEntry,R-INDEX(cImgEntry,"\") + 1),cNewImgList,";") = 0 THEN
      cNewImgList = cNewImgList + SUBSTR(cImgEntry,R-INDEX(cImgEntry,"\") + 1) + ";".
  END.
  ELSE IF LOOKUP(cImgEntry,cNewImgList,";") = 0 THEN
      cNewImgList = cNewImgList + cImgEntry + ";".
END.
ASSIGN cNewImgList = TRIM(cNewImgList,";")
       cImagePath  = TRIM(SESSION:TEMP-DIR,"\")
       cImageList  = cNewImgList.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setLargeImageList C-Win 
FUNCTION setLargeImageList RETURNS LOGICAL
  ( INPUT icLargeImageList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cNewImgList AS CHAR NO-UNDO.
DEF VAR cImgEntry   AS CHAR NO-UNDO.
DEF VAR cImg        AS CHAR NO-UNDO.

cLargeImageList = icLargeImageList.

IF cLargeImageList NE "" THEN DO ix = 1 TO NUM-ENTRIES(cLargeImageList,";"):

  cImgEntry = ENTRY(ix,cLargeImageList,";").

  IF cImgEntry = "" THEN NEXT.

  IF R-INDEX(cImgEntry,"/") > 0 THEN
    cImg = ENTRY(NUM-ENTRIES(cImgEntry,"/"),cImgEntry,"/").
  ELSE IF R-INDEX(cImgEntry,"\") > 0 THEN
    cImg = ENTRY(NUM-ENTRIES(cImgEntry,"\"),cImgEntry,"\").
  ELSE IF SEARCH(cImgEntry) = ? THEN DO:
    IF SEARCH("bmp\" + cImgEntry) NE ? THEN
      cImgEntry = "bmp\" + cImgEntry.
    ELSE IF SEARCH("ico\" + cImgEntry) NE ? THEN
      cImgEntry = "ico\" + cImgEntry.
  END.

  IF SEARCH(SESSION:TEMP-DIR + cImg) = ? AND SEARCH(cImgEntry) NE ? THEN DO:
    FILE-INFO:FILE-NAME = cImgEntry.
    OS-COPY VALUE(FILE-INFO:FULL-PATHNAME) VALUE(SESSION:TEMP-DIR).
  END.
  ELSE IF SEARCH(cImgEntry) = ? THEN
    MESSAGE "Invalid image entry: " cImgEntry SKIP
            "Must be in PROPATH"
            VIEW-AS ALERT-BOX WARNING.


  IF R-INDEX(cImgEntry,"/") > 0 THEN DO:    
    IF LOOKUP(SUBSTR(cImgEntry,R-INDEX(cImgEntry,"/") + 1),cNewImgList,";") = 0 THEN
      cNewImgList = cNewImgList + SUBSTR(cImgEntry,R-INDEX(cImgEntry,"/") + 1) + ";".
  END.
  ELSE IF R-INDEX(cImgEntry,"\") > 0 THEN DO:
    IF LOOKUP(SUBSTR(cImgEntry,R-INDEX(cImgEntry,"\") + 1),cNewImgList,";") = 0 THEN
      cNewImgList = cNewImgList + SUBSTR(cImgEntry,R-INDEX(cImgEntry,"\") + 1) + ";".
  END.
  ELSE IF LOOKUP(cImgEntry,cNewImgList,";") = 0 THEN
      cNewImgList = cNewImgList + cImgEntry + ";".
END.

ASSIGN cNewImgList = TRIM(cNewImgList,";")
       cLargeImagePath  = TRIM(SESSION:TEMP-DIR,"\")
       cLargeImageList  = cNewImgList.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setNavBarImage C-Win 
FUNCTION setNavBarImage RETURNS LOGICAL
  ( INPUT icNavBarImage AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF SEARCH(icNavBarImage) NE ? THEN DO:
  FILE-INFO:FILE-NAME = SEARCH(icNavBarImage).
  RUN NavBarSetStringProperty(iTree,0,FILE-INFO:FULL-PATHNAME,OUTPUT iStatus).
  RETURN YES.
END.
ELSE RETURN NO.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParent = ihParent.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSelImage C-Win 
FUNCTION setSelImage RETURNS LOGICAL
  ( INPUT icImage AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
iImgSelected = getImgIndex(icImage).
IF iImgSelected < 0 THEN DO:
  MESSAGE "Image " icImage " not found in image-list"
          VIEW-AS ALERT-BOX WARNING.
  iImgSelected = 0.
  RETURN NO.
END.
ELSE RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTreeStyle C-Win 
FUNCTION setTreeStyle RETURNS LOGICAL
  ( INPUT iiTreeStyle AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  1: Nav.bar style
            2: Treeview style
------------------------------------------------------------------------------*/
iTreeStyle = iiTreeStyle.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setViewType C-Win 
FUNCTION setViewType RETURNS LOGICAL
  ( INPUT iViewType AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    RUN NavBarSetView(iTree,iViewType,OUTPUT iStatus).
   
    /* View Style 9 had non default styles, so we must apply them again after the view assignment.
       This only needs to be done for the DEFAULT STYLES, not the custom styles. */
    IF iViewType GE 9 AND iViewType LE 11 THEN DO:

      RUN NavBarSetDefaultStyle(iTree,{&NAVBAR_DEFAULTELEMENT-GROUPBACKGROUND},
                              0,0,700,1048560,"",0,0,0,0,0,0,{&NAVBAR_GRADIENT-VERTICAL},"",0,0,
                              {&NAVBAR_STYLEVALID-BACKCOLOR1} + 
                              {&NAVBAR_STYLEVALID-BACKCOLOR2} + 
                              {&NAVBAR_STYLEVALID-GRADIENTMODE} ,OUTPUT iStatus).

      /*First, set some default properties that apply to the entire NavBar control*/
/*        RUN NavBarSetDefaultStyle(iTree,{&NAVBAR_DEFAULTELEMENT-BACKGROUND},                  */
/*                                  0,0,0,0,"",0,0,0,0,0,0,0,".\Hubble.jpg",0,0,                */
/*                                  {&NAVBAR_STYLEVALID-ALPHA1} + {&NAVBAR_STYLEVALID-ALPHA2} + */
/*                                  {&NAVBAR_STYLEVALID-BACKGROUNDIMAGE},OUTPUT iStatus).       */
/*        RUN NavBarSetDefaultStyle(iTree,{&NAVBAR_DEFAULTELEMENT-GROUPBACKGROUND}, */
/*                                  0,0,0,0,"",0,0,0,0,0,0,0,"",0,0,                */
/*                                  {&NAVBAR_STYLEVALID-ALPHA2},OUTPUT iStatus).    */
       /* RUN NavBarSetDefaultStyle(iTree,{&NAVBAR_DEFAULTELEMENT-GROUPHEADER},
                                    0,0,0,0,"",0,0,0,0,0,0,0,"",0,0,
                                   {&NAVBAR_STYLEVALID-ALPHA2},OUTPUT bStatus).*/
/*        RUN NavBarSetDefaultStyle(iTree,{&NAVBAR_DEFAULTELEMENT-GROUPHEADERHOTTRACKED}, */
/*                                  0,0,0,0,"",0,0,0,0,0,0,0,"",0,0,                      */
/*                                  {&NAVBAR_STYLEVALID-ALPHA2},OUTPUT iStatus).          */
    END.
    
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ShowTreeList C-Win 
FUNCTION ShowTreeList RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF iTreeStyle NE 3 THEN DO:
  MESSAGE "Invalid tree-style " iTreeStyle " for show of tree-list" SKIP
          PROGRAM-NAME(1)
          VIEW-AS ALERT-BOX ERROR.
  RETURN NO.   
END.

RUN TreeListShow(iTree,OUTPUT iStatus).
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION showTreeView C-Win 
FUNCTION showTreeView RETURNS INTEGER
  ( INPUT iiTreeStyle AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF iTreeStyle = 1 THEN
  RUN NavBarShow(iTree,OUTPUT iStatus).  
  
RETURN iStatus.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

