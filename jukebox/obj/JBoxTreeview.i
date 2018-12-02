&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF VAR hDynTreeView     AS HANDLE NO-UNDO.
DEF VAR hTreeTable       AS HANDLE NO-UNDO.
DEF VAR hTreeTableBuffer AS HANDLE NO-UNDO.
DEF VAR hTreeTableQuery  AS HANDLE NO-UNDO.
DEF VAR hTreeViewFrame   AS HANDLE NO-UNDO.
                                         
DEF VAR cImageOpen       AS CHAR   NO-UNDO.
DEF VAR cImageClosed     AS CHAR   NO-UNDO.
DEF VAR cCurrNode        AS CHAR   NO-UNDO.
DEF VAR cExpandNode      AS CHAR   NO-UNDO.

DEF TEMP-TABLE ttNodes NO-UNDO
    FIELD iSeq         AS INT
    FIELD cNodeKey     AS CHAR /*Unique id for each node in the tree*/
    FIELD iLevel       AS INT
    FIELD cParentKey   AS CHAR
    FIELD cText        AS CHAR
    FIELD bOpen        AS LOG
    FIELD cImageOpen   AS CHAR
    FIELD cImageClosed AS CHAR
    FIELD bHeader      AS LOG
    FIELD cKey         AS CHAR
    INDEX idxSeq IS PRIMARY UNIQUE iSeq
    .

DEF BUFFER bttNodes for ttNodes.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setExpandNode Include 
FUNCTION setExpandNode RETURNS LOGICAL
  ( INPUT ipcExpandNode AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildTree Include 
PROCEDURE buildTree :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ipcParentNode AS CHAR NO-UNDO.
DEF INPUT PARAM ipcParentKey  AS CHAR NO-UNDO.

FOR EACH ttNodes 
    WHERE ttNodes.cParentKey = ipcParentNode
       BY ttNodes.iSeq:    

  hTreeTableBuffer:BUFFER-CREATE().
  ASSIGN
    hTreeTableBuffer:BUFFER-FIELD('node_key'):BUFFER-VALUE        = DYNAMIC-FUNCTION('getNextNodeKey':U IN hDynTreeView)
    hTreeTableBuffer:BUFFER-FIELD('parent_node_key'):BUFFER-VALUE = ipcParentKey
    hTreeTableBuffer:BUFFER-FIELD('node_sort'):BUFFER-VALUE       = FALSE
    hTreeTableBuffer:BUFFER-FIELD('node_insert'):BUFFER-VALUE     = 4
    hTreeTableBuffer:BUFFER-FIELD('node_label'):BUFFER-VALUE      = ttNodes.cText
    hTreeTableBuffer:BUFFER-FIELD('image'):BUFFER-VALUE           = ttNodes.cImageClosed
    hTreeTableBuffer:BUFFER-FIELD('selected_image'):BUFFER-VALUE  = ttNodes.cImageOpen    
    hTreeTableBuffer:BUFFER-FIELD('private_data'):BUFFER-VALUE    = ttNodes.cNodeKey
    .

  RUN BuildTree (ttNodes.cNodeKey,hTreeTableBuffer:BUFFER-FIELD('node_key'):BUFFER-VALUE).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateNodes Include 
PROCEDURE CreateNodes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ipiLevel        AS INT  NO-UNDO.
DEF INPUT PARAM ipcParent       AS CHAR NO-UNDO.
DEF INPUT PARAM ipcText         AS CHAR NO-UNDO.
DEF INPUT PARAM ipbOpen         AS LOG  NO-UNDO.
DEF INPUT PARAM ipbHeader       AS LOG  NO-UNDO.
DEF INPUT PARAM ipcUniqueKey    AS CHAR NO-UNDO.
  
FIND LAST bttNodes NO-LOCK NO-ERROR.
CREATE ttNodes.
ASSIGN 
  ttNodes.iSeq         = IF AVAIL bttNodes THEN bttNodes.iSeq + 1 ELSE 1
  ttNodes.cNodeKey     = ipcUniqueKey
  ttNodes.iLevel       = ipiLevel
  ttNodes.cParentKey   = ipcParent
  ttNodes.cText        = ipcText
  ttNodes.bOpen        = ipbOpen
  ttNodes.cImageOpen   = cImageOpen
  ttNodes.cImageClosed = cImageClosed
  ttNodes.bHeader      = ipbHeader
  .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openTreeNode Include 
PROCEDURE openTreeNode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cExpandKey AS CHAR NO-UNDO.

hTreeTableQuery:QUERY-OPEN().
hTreeTableQuery:GET-FIRST().
REPEAT WHILE NOT hTreeTableQuery:QUERY-OFF-END:
  DYNAMIC-FUNCTION('ExpandNode',hTreeTableBuffer:BUFFER-FIELD('node_key'):BUFFER-VALUE,TRUE).
  IF hTreeTableBuffer:BUFFER-FIELD('private_data'):BUFFER-VALUE = cExpandNode THEN
    cExpandKey = hTreeTableBuffer:BUFFER-FIELD('node_key'):BUFFER-VALUE.
  hTreeTableQuery:GET-NEXT().
END.

IF cExpandKey NE "" THEN DO:
  DYNAMIC-FUNCTION('selectNode',cExpandKey).
  RUN tvNodeEvent ("myclick",cExpandKey).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setExpandNode Include 
FUNCTION setExpandNode RETURNS LOGICAL
  ( INPUT ipcExpandNode AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Set the expand node for the tree-view after re-open 
    Notes: The key here is referes to private_data for the treeview temp-table
           (which always should contain the primary key for the real data)
------------------------------------------------------------------------------*/
cExpandNode = ipcExpandNode.

RETURN FALSE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

