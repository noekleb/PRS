 
 /*------------------------------------------------------------------------
    File        : MsTVForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Fri Dec 09 12:52:31 CET 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.


CLASS MsTVForm INHERITS Form: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE contextMenuStrip1 AS System.Windows.Forms.ContextMenuStrip NO-UNDO.
  DEFINE PRIVATE VARIABLE imageList1 AS System.Windows.Forms.ImageList NO-UNDO.
  DEFINE PRIVATE VARIABLE submenu1ToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
  DEFINE PRIVATE VARIABLE menuUnderSubmenuToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
  DEFINE PRIVATE VARIABLE menu2ToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
  DEFINE PRIVATE VARIABLE treeView1 AS System.Windows.Forms.TreeView NO-UNDO.
  
  DEF VAR draggedNode AS System.Windows.Forms.TreeNode NO-UNDO.

		
	CONSTRUCTOR PUBLIC MsTVForm (  ):
		
		
    SUPER().
    InitializeComponent().
    THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.

	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID DragEnter( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):
		
		e:Effect = e:AllowedEffect.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID DragNode( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.ItemDragEventArgs ):
		
	
	  draggedNode = CAST(e:Item,"System.Windows.Forms.TreeNode").	
		THIS-OBJECT:DoDragDrop(e:Item, System.Windows.Forms.DragDropEffects:Move).
		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID DragOver( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):

    DEF VAR targetPoint AS System.Drawing.Point NO-UNDO. 
    // Retrieve the client coordinates of the mouse position.
    targetPoint = treeView1:PointToClient(new System.Drawing.Point(e:X, e:Y)).

    // Select the node at the mouse position.
    treeView1:SelectedNode = treeView1:GetNodeAt(targetPoint).
		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID DropNode( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):
		
		DEF VAR targetPoint AS System.Drawing.Point NO-UNDO. 
		DEF VAR targetNode  AS System.Windows.Forms.TreeNode NO-UNDO.
		
    /*  Retrieve the client coordinates of the drop location.*/
    targetPoint = treeView1:PointToClient(NEW System.Drawing.Point(e:X, e:Y)).

    /*  Retrieve the node at the drop location.*/
    targetNode = treeView1:GetNodeAt(targetPoint).
    
    IF targetNode:Name = draggedNode:Name THEN RETURN.
/*
    // Confirm that the node at the drop location is not 
    // the dragged node or a descendant of the dragged node.
    if (!draggedNode.Equals(targetNode) && !ContainsNode(draggedNode, targetNode))
    {
        // If it is a move operation, remove the node from its current 
        // location and add it to the node at the drop location.
        if (e.Effect == DragDropEffects.Move)
        {
            draggedNode.Remove();
            targetNode.Nodes.Add(draggedNode);
        }

        // If it is a copy operation, clone the dragged node 
        // and add it to the node at the drop location.
        else if (e.Effect == DragDropEffects.Copy)
        {
            targetNode.Nodes.Add((TreeNode)draggedNode.Clone());
        }

        // Expand the node at the location 
        // to show the dropped node.
        targetNode.Expand();
    }
*/
	 	
/*   Position.X = e.X.                            */
/*   Position.Y = e.Y.                            */
/*   Position = treeView1.PointToClient(Position).*/
   /*
   TreeNode DropNode = this.treeView1.GetNodeAt(Position);
   if (DropNode != null && DropNode.Parent == this.NodeToBeDeleted.Parent )
   {
    TreeNode DragNode = this.NodeToBeDeleted;
    DropNode.Parent.Nodes.Remove(this.NodeToBeDeleted);
    DropNode.Parent.Nodes.Insert(DropNode.Index+1, DragNode);
   }	
   */
   	
	  RETURN.

	END METHOD.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
    /* NOTE: The following method is automatically generated.
    
    We strongly suggest that the contents of this method only be modified using the
    Visual Designer to avoid any incompatible modifications.
    
    Modifying the contents of this method using a code editor will invalidate any support for this file. */
    THIS-OBJECT:components = NEW System.ComponentModel.Container().
    @VisualDesigner.FormMember (NeedsInitialize="true").
    DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
    resources = NEW Progress.Util.ResourceManager("MsTVForm").
    @VisualDesigner.FormMember (NeedsInitialize="true").
    DEFINE VARIABLE treeNode1 AS System.Windows.Forms.TreeNode NO-UNDO.
    treeNode1 = NEW System.Windows.Forms.TreeNode("Node0").
    @VisualDesigner.FormMember (NeedsInitialize="true").
    DEFINE VARIABLE treeNode2 AS System.Windows.Forms.TreeNode NO-UNDO.
    treeNode2 = NEW System.Windows.Forms.TreeNode("Node1").
    @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
    DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.TreeNode EXTENT 2 NO-UNDO.
    arrayvar0[1] = treeNode1.
    arrayvar0[2] = treeNode2.
    @VisualDesigner.FormMember (NeedsInitialize="true").
    DEFINE VARIABLE treeNode3 AS System.Windows.Forms.TreeNode NO-UNDO.
    treeNode3 = NEW System.Windows.Forms.TreeNode("Rot", arrayvar0).
    @VisualDesigner.FormMember (NeedsInitialize="true").
    DEFINE VARIABLE treeNode4 AS System.Windows.Forms.TreeNode NO-UNDO.
    treeNode4 = NEW System.Windows.Forms.TreeNode("Node3").
    @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
    DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.TreeNode EXTENT 1 NO-UNDO.
    arrayvar1[1] = treeNode4.
    @VisualDesigner.FormMember (NeedsInitialize="true").
    DEFINE VARIABLE treeNode5 AS System.Windows.Forms.TreeNode NO-UNDO.
    treeNode5 = NEW System.Windows.Forms.TreeNode("Node2", arrayvar1).
    THIS-OBJECT:contextMenuStrip1 = NEW System.Windows.Forms.ContextMenuStrip(THIS-OBJECT:components).
    THIS-OBJECT:submenu1ToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
    THIS-OBJECT:menuUnderSubmenuToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
    THIS-OBJECT:menu2ToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem().
    THIS-OBJECT:treeView1 = NEW System.Windows.Forms.TreeView().
    THIS-OBJECT:imageList1 = NEW System.Windows.Forms.ImageList(THIS-OBJECT:components).
    THIS-OBJECT:contextMenuStrip1:SuspendLayout().
    THIS-OBJECT:SuspendLayout().
    /*  */
    /* contextMenuStrip1 */
    /*  */
    @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
    DEFINE VARIABLE arrayvar2 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
    arrayvar2[1] = THIS-OBJECT:submenu1ToolStripMenuItem.
    arrayvar2[2] = THIS-OBJECT:menu2ToolStripMenuItem.
    THIS-OBJECT:contextMenuStrip1:Items:AddRange(arrayvar2).
    THIS-OBJECT:contextMenuStrip1:Name = "contextMenuStrip1".
    THIS-OBJECT:contextMenuStrip1:Size = NEW System.Drawing.Size(131, 48).
    /*  */
    /* submenu1ToolStripMenuItem */
    /*  */
    @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
    DEFINE VARIABLE arrayvar3 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
    arrayvar3[1] = THIS-OBJECT:menuUnderSubmenuToolStripMenuItem.
    THIS-OBJECT:submenu1ToolStripMenuItem:DropDownItems:AddRange(arrayvar3).
    THIS-OBJECT:submenu1ToolStripMenuItem:Name = "submenu1ToolStripMenuItem".
    THIS-OBJECT:submenu1ToolStripMenuItem:Size = NEW System.Drawing.Size(130, 22).
    THIS-OBJECT:submenu1ToolStripMenuItem:Text = "submenu1".
    THIS-OBJECT:submenu1ToolStripMenuItem:Click:Subscribe(THIS-OBJECT:MenuItemClick).
    /*  */
    /* menuUnderSubmenuToolStripMenuItem */
    /*  */
    THIS-OBJECT:menuUnderSubmenuToolStripMenuItem:Name = "menuUnderSubmenuToolStripMenuItem".
    THIS-OBJECT:menuUnderSubmenuToolStripMenuItem:Size = NEW System.Drawing.Size(192, 22).
    THIS-OBJECT:menuUnderSubmenuToolStripMenuItem:Text = "Menu under submenu".
    /*  */
    /* menu2ToolStripMenuItem */
    /*  */
    THIS-OBJECT:menu2ToolStripMenuItem:Image = CAST(resources:GetObject("menu2ToolStripMenuItem.Image"), System.Drawing.Image).
    THIS-OBJECT:menu2ToolStripMenuItem:Name = "menu2ToolStripMenuItem".
    THIS-OBJECT:menu2ToolStripMenuItem:Size = NEW System.Drawing.Size(130, 22).
    THIS-OBJECT:menu2ToolStripMenuItem:Text = "menu2".
    /*  */
    /* treeView1 */
    /*  */
    THIS-OBJECT:treeView1:AllowDrop = TRUE.
    THIS-OBJECT:treeView1:ImageIndex = 0.
    THIS-OBJECT:treeView1:ImageList = THIS-OBJECT:imageList1.
    THIS-OBJECT:treeView1:Location = NEW System.Drawing.Point(30, 53).
    THIS-OBJECT:treeView1:Name = "treeView1".
    treeNode1:Name = "Node0".
    treeNode1:Text = "Node0".
    treeNode2:Name = "Node1".
    treeNode2:Text = "Node1".
    treeNode3:ContextMenuStrip = THIS-OBJECT:contextMenuStrip1.
    treeNode3:ImageIndex = 0.
    treeNode3:Name = "Node0".
    treeNode3:Text = "Rot".
    treeNode4:Name = "Node3".
    treeNode4:Text = "Node3".
    treeNode5:Name = "Node2".
    treeNode5:Text = "Node2".
    @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
    DEFINE VARIABLE arrayvar4 AS System.Windows.Forms.TreeNode EXTENT 2 NO-UNDO.
    arrayvar4[1] = treeNode3.
    arrayvar4[2] = treeNode5.
    THIS-OBJECT:treeView1:Nodes:AddRange(arrayvar4).
    THIS-OBJECT:treeView1:SelectedImageIndex = 0.
    THIS-OBJECT:treeView1:Size = NEW System.Drawing.Size(286, 219).
    THIS-OBJECT:treeView1:TabIndex = 0.
    THIS-OBJECT:treeView1:ItemDrag:Subscribe(THIS-OBJECT:DragNode).
    THIS-OBJECT:treeView1:AfterSelect:Subscribe(THIS-OBJECT:treeView1_AfterSelect).
    THIS-OBJECT:treeView1:DragDrop:Subscribe(THIS-OBJECT:DropNode).
    THIS-OBJECT:treeView1:DragEnter:Subscribe(THIS-OBJECT:DragEnter).
    THIS-OBJECT:treeView1:DragOver:Subscribe(THIS-OBJECT:DragOver).
    /*  */
    /* imageList1 */
    /*  */
    THIS-OBJECT:imageList1:ImageStream = CAST(resources:GetObject("imageList1.ImageStream"), System.Windows.Forms.ImageListStreamer).
    THIS-OBJECT:imageList1:TransparentColor = System.Drawing.Color:Transparent.
    THIS-OBJECT:imageList1:Images:SetKeyName(0, "Fold.bmp").
    THIS-OBJECT:imageList1:Images:SetKeyName(1, "FoldOpen.bmp").
    /*  */
    /* MsTVForm */
    /*  */
    THIS-OBJECT:ClientSize = NEW System.Drawing.Size(357, 326).
    THIS-OBJECT:Controls:Add(THIS-OBJECT:treeView1).
    THIS-OBJECT:Name = "MsTVForm".
    THIS-OBJECT:Text = "MsTVForm".
    THIS-OBJECT:contextMenuStrip1:ResumeLayout(FALSE).
    THIS-OBJECT:ResumeLayout(FALSE).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID MenuItemClick( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID treeView1_AfterSelect( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.TreeViewEventArgs ):
		
		MESSAGE sender:ToString()
    VIEW-AS ALERT-BOX.
		RETURN.

	END METHOD.

	DESTRUCTOR PUBLIC MsTVForm ( ):

	END DESTRUCTOR.

END CLASS.