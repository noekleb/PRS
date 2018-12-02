 
 /*------------------------------------------------------------------------
    File        : TreeView
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Fri Sep 22 16:06:33 CEST 2017
    Notes       : Remember to set AllowDrop for drag/drop function 
  ----------------------------------------------------------------------*/



ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS uc.JBoxMsTreeView INHERITS uc.JBoxUCbase: 

  DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE treeView1 AS System.Windows.Forms.TreeView NO-UNDO.
  DEFINE PRIVATE VARIABLE imageList1  AS System.Windows.Forms.ImageList NO-UNDO.

  DEF VAR idxNode AS INT NO-UNDO. 
  DEF VAR oDefaultContextMenuStrip AS System.Windows.Forms.ContextMenuStrip NO-UNDO.
  DEF VAR draggedNode AS System.Windows.Forms.TreeNode NO-UNDO.
    		    		
  DEF TEMP-TABLE ttNode NO-UNDO 
      FIELD cNodeName       AS CHAR 
      FIELD cParentNodeName AS CHAR 
      FIELD cNodeText       AS CHAR 
      FIELD cNodeType       AS CHAR
      FIELD cNodeImage      AS CHAR
      FIELD iNodeIdx        AS INT 
      FIELD oNodeObj        AS Progress.Lang.Object
      INDEX idxNodeIdx IS PRIMARY UNIQUE iNodeIdx
      .

  DEF TEMP-TABLE ttContextMenu NO-UNDO 
      FIELD cMenuName       AS CHAR 
      FIELD cMenuType       AS CHAR
      FIELD oMenuObj        AS Progress.Lang.Object
      .
      		
  DEFINE PUBLIC PROPERTY ttNodeBuffer AS HANDLE GET(): RETURN BUFFER ttNode:HANDLE. END GET. PROTECTED SET.
    
  CONSTRUCTOR PUBLIC JBoxMsTreeView (INPUT ihWinSuper    AS HANDLE, /* Window super proc - unknown if object created from window */
                                     INPUT ihWinProc     AS HANDLE,
                                     INPUT ihPlaceHolder AS HANDLE, /* Design widget (rectangle, field, etc) */
                                     INPUT iiDeltaWidth  AS INTEGER):

     SUPER(ihWinSuper,ihWinProc,ihPlaceHolder,iiDeltaWidth).

     THIS-OBJECT:InitializeComponent("").   
  END CONSTRUCTOR.
  
  
  CONSTRUCTOR PUBLIC JBoxMsTreeView ( INPUT ihWinProc     AS HANDLE,
                                      INPUT ihPlaceHolder AS HANDLE ): /* Design widget (rectangle, field, etc) */
                                   
    SUPER(?,ihWinProc,ihPlaceHolder,0).
   
    InitializeComponent(""). 
  END CONSTRUCTOR.


  CONSTRUCTOR PUBLIC JBoxMsTreeView ( INPUT ihWinProc     AS HANDLE,
                                      INPUT ihPlaceHolder AS HANDLE, /* Design widget (rectangle, field, etc) */
                                      INPUT icImageList   AS CHAR  ):
  
    SUPER(?,ihWinProc,ihPlaceHolder,0).
  
    InitializeComponent(icImageList). 
  END CONSTRUCTOR.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID ExpandNodes(INPUT icNodeName AS CHAR  ):
    DEF VAR ix AS INT NO-UNDO.
    
    IF icNodeName = "" THEN
      THIS-OBJECT:treeView1:ExpandAll().
    ELSE DO ix = 1 TO NUM-ENTRIES(icNodeName,"|"):
      FIND FIRST ttNode
           WHERE ttNode.cNodeName = TRIM(ENTRY(ix,icNodeName,"|"))
           NO-ERROR.
      IF AVAIL ttNode THEN 
        CAST(ttNode.oNodeObj,"System.Windows.Forms.TreeNode"):Expand().
    END.
        
    RETURN.

  END METHOD.

  METHOD PUBLIC VOID CollapseNodes(INPUT icNodeName AS CHAR  ):
    
    IF icNodeName = "" THEN
      THIS-OBJECT:treeView1:CollapseAll().
    ELSE DO:
      FIND FIRST ttNode
           WHERE ttNode.cNodeName = icNodeName
           NO-ERROR.
      IF AVAIL ttNode THEN 
        CAST(ttNode.oNodeObj,"System.Windows.Forms.TreeNode"):Collapse().
    END.
        
    RETURN.

  END METHOD.

  METHOD PUBLIC VOID InitializeComponent( INPUT icImageList AS CHAR ):
		
    DEF VAR ix             AS INT NO-UNDO.
    DEF VAR cImgFile       AS CHAR NO-UNDO.
    
    DEFINE VARIABLE treeNode1 AS System.Windows.Forms.TreeNode NO-UNDO.
    treeNode1 = NEW System.Windows.Forms.TreeNode("Rot").
    THIS-OBJECT:components = NEW System.ComponentModel.Container().
    THIS-OBJECT:treeView1 = NEW System.Windows.Forms.TreeView().

    IF icImageList NE "" THEN DO:
      THIS-OBJECT:imageList1 = NEW System.Windows.Forms.ImageList(THIS-OBJECT:components).
      icImageList = REPLACE(icImageList,",",";").
      DO ix = 1 TO NUM-ENTRIES(icImageList,";"):
        cImgFile = REPLACE(ENTRY(ix,icImageList,";"),"\","/").
        IF SEARCH(cImgFile) NE ? THEN DO:
          FILE-INFO:FILE-NAME = cImgFile.
          imageList1:Images:Add(System.Drawing.Image:FromFile(FILE-INFO:FULL-PATHNAME)).
          imageList1:Images:SetKeyName(ix - 1, ENTRY(NUM-ENTRIES(cImgFile,"/"),cImgFile,"/")).
        END.    
      END.  
      THIS-OBJECT:treeView1:ImageIndex = 0.
      THIS-OBJECT:treeView1:ImageList = THIS-OBJECT:imageList1.
      THIS-OBJECT:treeView1:SelectedImageIndex = 1.
      
    END.
    
    THIS-OBJECT:SuspendLayout().
    /*  */
    /* treeView1 */
    /*  */
    THIS-OBJECT:treeView1:Location = NEW System.Drawing.Point(0, 0).
    THIS-OBJECT:treeView1:Name = "treeView1".
/*    THIS-OBJECT:treeView1:Size = NEW System.Drawing.Size(211, 136).*/
    THIS-OBJECT:treeView1:TabIndex = 0.
    THIS-OBJECT:treeView1:AllowDrop = YES.
    THIS-OBJECT:treeView1:DragDrop:Subscribe(THIS-OBJECT:DropNode).
    THIS-OBJECT:treeView1:ItemDrag:Subscribe(THIS-OBJECT:DragNode).
    THIS-OBJECT:treeView1:DragEnter:Subscribe(THIS-OBJECT:DragEnter).
    THIS-OBJECT:treeView1:DragOver:Subscribe(THIS-OBJECT:DragOver).
    THIS-OBJECT:treeView1:AfterSelect:Subscribe(THIS-OBJECT:SelectChange).
    /*  */
    /* MsTVForm */
    /*  */
    THIS-OBJECT:ClientSize = NEW System.Drawing.Size(357, 326).
    THIS-OBJECT:Controls:Add(THIS-OBJECT:treeView1).
    THIS-OBJECT:Name = "JBoxMsTreeView".
    THIS-OBJECT:Text = "JBoxMsTreeView".
    THIS-OBJECT:ResumeLayout(FALSE).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.
END METHOD.

/* Mandatory methods: */
  METHOD PUBLIC System.Windows.Forms.TreeView getDotNetWidget():
    RETURN TreeView1.
  END METHOD.

  METHOD PUBLIC CHARACTER getDotNetWidgetType():
    RETURN STRING(TreeView1:GetType()).
  END METHOD.

  METHOD PUBLIC CHARACTER DEFAULT():
    RETURN DYNAMIC-INVOKE(THIS-OBJECT,ENTRY(1,PROGRAM-NAME(2)," ")).
  END METHOD.

/* TreeView methods: */
  METHOD PUBLIC INTEGER AddNode (icParentNodeName     AS CHAR 
                                ,icNodeName           AS CHAR 
                                ,icNodeText           AS CHAR
                                ,icNodeType           AS CHAR
                                ,iiImageIndex         AS INT
                                ,iiSelectedImageIndex AS INT
                                 ):
                                     
    DEFINE VARIABLE treeNode1 AS System.Windows.Forms.TreeNode NO-UNDO.
    
    FIND FIRST ttNode 
         WHERE ttNode.cNodeName = icParentNodeName
         NO-ERROR.
        
    IF AVAIL ttNode THEN
      treeNode1 = CAST(ttNode.oNodeObj,"System.Windows.Forms.TreeNode"):Nodes:Add(icNodeText).
    ELSE
      treeNode1 = treeView1:Nodes:Add(icNodeText).
    
    IF iiImageIndex GE 0 THEN  
      treeNode1:ImageIndex = iiImageIndex.  
    IF iiSelectedImageIndex GE 0 THEN  
      treeNode1:SelectedImageIndex = iiSelectedImageIndex.  
    
    idxNode = idxNode + 1.

    CREATE ttNode.
    ASSIGN ttNode.iNodeIdx        = idxNode
           ttNode.cNodeName       = icNodeName
           ttNode.cParentNodeName = icParentNodeName
           ttNode.cNodeText       = icNodeText
           ttNode.cNodeType       = icNodeType
           ttNode.oNodeObj        = CAST(treeNode1,"Progress.Lang.Object")
           .
    
    treeNode1:Name = icNodeName.
    
    RETURN idxNode.                              
  END METHOD.                            

  METHOD PUBLIC CHARACTER AddContextMenuStrip (icNodeName  AS CHAR
                                              ,icNodeType  AS CHAR):
                                         
    DEFINE VARIABLE contextMenuStrip1 AS CLASS System.Windows.Forms.ContextMenuStrip NO-UNDO.
    contextMenuStrip1 = NEW System.Windows.Forms.ContextMenuStrip().
  
    IF NOT VALID-OBJECT(oDefaultContextMenuStrip) THEN oDefaultContextMenuStrip = contextMenuStrip1.
    
    IF icNodeName NE "" THEN DO:
      FIND FIRST ttNode
           WHERE ttNode.cNodeName = icNodeName
           NO-ERROR
           .
      IF AVAIL ttNode THEN
        ASSIGN CAST(ttNode.oNodeObj,"System.Windows.Forms.TreeNode"):ContextMenuStrip = contextMenuStrip1
               contextMenuStrip1:Name = icNodeName.     
    END.
    ELSE DO:
      FOR EACH ttNode
          WHERE (IF icNodeType NE "" THEN ttNode.cNodeType = icNodeType ELSE TRUE)
          :
        CAST(ttNode.oNodeObj,"System.Windows.Forms.TreeNode"):ContextMenuStrip = contextMenuStrip1.    
      END.
      contextMenuStrip1:Name = icNodeType.
    END.  
    
    CREATE ttContextMenu.
    ASSIGN ttContextMenu.cMenuName = contextMenuStrip1:Name
           ttContextMenu.cMenuType = "PopupMenu"
           ttContextMenu.oMenuObj  = CAST(contextMenuStrip1,"Progress.Lang.Object")
           .
    
    RETURN contextMenuStrip1:Name.                                 
  END.
                    
  METHOD PUBLIC CHARACTER AddContextSubMenu (icMenuStrip  AS CHAR
                                            ,icSubMenu    AS CHAR
                                            ,icName       AS CHAR
                                            ,icText       AS CHAR
                                            ,icImageFile  AS CHAR
                                             ):
  
    DEF VAR contextMenuItem1 AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
          
    contextMenuItem1 = NEW System.Windows.Forms.ToolStripMenuItem().       
    ASSIGN contextMenuItem1:Name = icName
           contextMenuItem1:Text = icText
           .
  
    IF icSubMenu NE "" THEN DO:
      FIND FIRST ttContextMenu 
           WHERE ttContextMenu.cMenuName = icSubMenu
             AND ttContextMenu.cMenuType = "SubMenu"
           NO-ERROR.
      IF AVAIL ttContextMenu THEN
        CAST(ttContextMenu.oMenuObj,"System.Windows.Forms.ToolStripMenuItem"):DropDownItems:Add(contextMenuItem1).
    END.    
    ELSE DO:    
      FIND FIRST ttContextMenu 
           WHERE ttContextMenu.cMenuName = icMenuStrip
             AND ttContextMenu.cMenuType = "PopupMenu"
           NO-ERROR.
      IF AVAIL ttContextMenu THEN
        CAST(ttContextMenu.oMenuObj,"System.Windows.Forms.ContextMenuStrip"):Items:Add(contextMenuItem1).
    END.
    
    IF icImageFile NE "" AND SEARCH(icImageFile) NE ? THEN DO:
      FILE-INFO:FILE-NAME = SEARCH(icImageFile).
      contextMenuItem1:Image = System.Drawing.Image:FromFile(FILE-INFO:FULL-PATHNAME).
    END.  
    
    CREATE ttContextMenu.
    ASSIGN ttContextMenu.cMenuName = contextMenuItem1:Name
           ttContextMenu.cMenuType = "SubMenu"
           ttContextMenu.oMenuObj  = CAST(contextMenuItem1,"Progress.Lang.Object")
           .
    RETURN contextMenuItem1:Name.     
  END.
    
  METHOD PUBLIC VOID AddContextMenuItem (icMenuStrip  AS CHAR
                                        ,icSubMenu    AS CHAR
                                        ,icProcedure  AS CHAR
                                        ,icText       AS CHAR
                                        ,icImageFile  AS CHAR
                                         ):
  
    DEF VAR contextMenuItem1 AS System.Windows.Forms.ToolStripItem NO-UNDO.

    IF icSubMenu NE "" THEN DO:
      FIND FIRST ttContextMenu 
           WHERE ttContextMenu.cMenuName = icSubMenu
             AND ttContextMenu.cMenuType = "SubMenu"
           NO-ERROR.
      IF AVAIL ttContextMenu THEN
        contextMenuItem1 = CAST(ttContextMenu.oMenuObj,"System.Windows.Forms.ToolStripMenuItem"):DropDownItems:Add(icText).
    END.    
    ELSE DO:    
      FIND FIRST ttContextMenu 
           WHERE ttContextMenu.cMenuName = icMenuStrip
             AND ttContextMenu.cMenuType = "PopupMenu"
           NO-ERROR.
      IF AVAIL ttContextMenu THEN
        contextMenuItem1 = CAST(ttContextMenu.oMenuObj,"System.Windows.Forms.ContextMenuStrip"):Items:Add(icText).
    END.
    
    IF icImageFile NE "" AND SEARCH(icImageFile) NE ? THEN DO:
      FILE-INFO:FILE-NAME = SEARCH(icImageFile).
      contextMenuItem1:Image = System.Drawing.Image:FromFile(FILE-INFO:FULL-PATHNAME).
    END.  
            
    CREATE ttContextMenu.
    ASSIGN contextMenuItem1:Name    = icProcedure
           ttContextMenu.cMenuName  = contextMenuItem1:Name
           ttContextMenu.cMenuType  = "MenuItem"
           ttContextMenu.oMenuObj   = CAST(contextMenuItem1,"Progress.Lang.Object")
           .
    contextMenuItem1:Click:Subscribe(THIS-OBJECT:MenuItemClick).
         
  END.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID SelectNode( icNodeName AS CHAR ):
    
    FIND FIRST ttNode
         WHERE ttNode.cNodeName = icNodeName
         NO-ERROR.
    IF AVAIL ttNode THEN DO: 
      THIS-OBJECT:treeView1:SelectedNode = CAST(ttNode.oNodeObj,"System.Windows.Forms.TreeNode").
      IF CAN-DO(THIS-OBJECT:hWinProc:INTERNAL-ENTRIES,"TreeNodeSelectedRecord") THEN 
        RUN TreeNodeSelectedRecord IN hWinProc(treeView1:selectedNode:Name). 
    END.
    RETURN.

  END METHOD.

  METHOD PUBLIC LOGICAL setAllowDrop (ibAllowDrop AS LOG):
    THIS-OBJECT:treeView1:AllowDrop = ibAllowDrop.
  END.  

  METHOD PUBLIC LOGICAL clearNodes ():
    EMPTY TEMP-TABLE ttNode.
    EMPTY TEMP-TABLE ttContextMenu.
    idxNode = 0.
    THIS-OBJECT:treeView1:Nodes:Clear().
    THIS-OBJECT:treeView1:ImageList = THIS-OBJECT:imageList1.
  END.  
  
  METHOD PRIVATE VOID MenuItemClick( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
        
    DEF VAR oMenuItem AS System.Windows.Forms.ToolStripItem NO-UNDO. 
    
    oMenuItem = CAST(sender,"System.Windows.Forms.ToolStripItem").

    FIND FIRST ttContextMenu 
         WHERE ttContextMenu.cMenuName = oMenuItem:Name
           AND ttContextMenu.cMenuType = "MenuItem"
         NO-ERROR.
    IF AVAIL ttContextMenu 
       AND CAN-DO(THIS-OBJECT:hWinProc:INTERNAL-ENTRIES,ttContextMenu.cMenuName) THEN 
      RUN VALUE(ttContextMenu.cMenuName) IN hWinProc(treeView1:SelectedNode:Name).    
    ELSE IF CAN-DO(THIS-OBJECT:hWinProc:INTERNAL-ENTRIES,"TreeMenuItemRecord") THEN 
      RUN TreeMenuItemRecord IN hWinProc(oMenuItem:Name,treeView1:SelectedNode:Name). 
       
    RETURN.

  END METHOD.

  METHOD PRIVATE VOID SelectChange( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
        
    
    IF CAN-DO(THIS-OBJECT:hWinProc:INTERNAL-ENTRIES,"TreeNodeSelectedRecord") THEN 
      RUN TreeNodeSelectedRecord IN hWinProc(treeView1:selectedNode:Name). 
       
    RETURN.

  END METHOD.
  
  METHOD PRIVATE VOID DropNode( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):
    
    DEF VAR targetPoint AS System.Drawing.Point NO-UNDO. 
    DEF VAR targetNode  AS System.Windows.Forms.TreeNode NO-UNDO.
    
    /*  Retrieve the client coordinates of the drop location.*/
    targetPoint = treeView1:PointToClient(NEW System.Drawing.Point(e:X, e:Y)).

    /*  Retrieve the node at the drop location.*/
    targetNode = treeView1:GetNodeAt(targetPoint).
        
    IF targetNode:Name = draggedNode:Name THEN RETURN.

    IF CAN-DO(THIS-OBJECT:hWinProc:INTERNAL-ENTRIES,"TreeDropNode") THEN 
      RUN TreeDropNode IN hWinProc(draggedNode:Name,targetNode:Name). 

    RETURN.

  END METHOD.  
  
  METHOD PRIVATE VOID DragNode( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.ItemDragEventArgs ):
    
    draggedNode = CAST(e:Item,"System.Windows.Forms.TreeNode"). 
    THIS-OBJECT:DoDragDrop(e:Item, System.Windows.Forms.DragDropEffects:Move).

  END METHOD.
  
  METHOD PRIVATE VOID DragOver( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):

    DEF VAR targetPoint AS System.Drawing.Point NO-UNDO. 
    // Retrieve the client coordinates of the mouse position.
    targetPoint = treeView1:PointToClient(new System.Drawing.Point(e:X, e:Y)).

    // Select the node at the mouse position.
    treeView1:SelectedNode = treeView1:GetNodeAt(targetPoint).
    
    RETURN.

  END METHOD.
  
  METHOD PRIVATE VOID DragEnter( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):
    
    e:Effect = e:AllowedEffect.
    RETURN.

  END METHOD.
  
	DESTRUCTOR PUBLIC JBoxMsTreeView ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.

END CLASS.
