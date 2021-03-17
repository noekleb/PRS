 /*------------------------------------------------------------------------
    File        : test2
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tomn
    Created     : Thu Nov 21 13:54:58 CET 2019
    Notes       : 
  ----------------------------------------------------------------------*/
USING Progress.Windows.Form.
    
CLASS fix.test2 INHERITS Form : 
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE childFormNumber AS INTEGER INITIAL 0 NO-UNDO.
    
    DEFINE PRIVATE VARIABLE openFileDialog AS System.Windows.Forms.OpenFileDialog NO-UNDO. 
    DEFINE PRIVATE VARIABLE saveFileDialog AS System.Windows.Forms.SaveFileDialog NO-UNDO. 
    DEFINE PRIVATE VARIABLE menuStrip AS System.Windows.Forms.MenuStrip NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
    DEFINE PRIVATE VARIABLE statusStrip AS System.Windows.Forms.StatusStrip NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator1 AS System.Windows.Forms.ToolStripSeparator NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator2 AS System.Windows.Forms.ToolStripSeparator NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator3 AS System.Windows.Forms.ToolStripSeparator NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator4 AS System.Windows.Forms.ToolStripSeparator NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator5 AS System.Windows.Forms.ToolStripSeparator NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator6 AS System.Windows.Forms.ToolStripSeparator NO-UNDO.
    
    DEFINE PRIVATE VARIABLE printSetupToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator7 AS System.Windows.Forms.ToolStripSeparator NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripSeparator8 AS System.Windows.Forms.ToolStripSeparator NO-UNDO.
    DEFINE PRIVATE VARIABLE toolStripStatusLabel AS System.Windows.Forms.ToolStripStatusLabel NO-UNDO.
    DEFINE PRIVATE VARIABLE aboutToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE tileHorizontalToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE fileMenu AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE newToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE openToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE saveToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE saveAsToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE printToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE printPreviewToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE exitToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE editMenu AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    
    DEFINE PRIVATE VARIABLE undoToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE redoToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE cutToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE copyToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE pasteToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE selectAllToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE viewMenu AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE toolBarToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE statusBarToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE toolsMenu AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE optionsToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE windowsMenu AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    
    DEFINE PRIVATE VARIABLE newWindowToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE cascadeToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE tileVerticalToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE closeAllToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE arrangeIconsToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE helpMenu AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE contentsToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE indexToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    DEFINE PRIVATE VARIABLE searchToolStripMenuItem AS System.Windows.Forms.ToolStripMenuItem NO-UNDO.
    
    DEFINE PRIVATE VARIABLE newToolStripButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
    DEFINE PRIVATE VARIABLE openToolStripButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
    DEFINE PRIVATE VARIABLE saveToolStripButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
    DEFINE PRIVATE VARIABLE printToolStripButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
    DEFINE PRIVATE VARIABLE printPreviewToolStripButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
    DEFINE PRIVATE VARIABLE helpToolStripButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
    DEFINE PRIVATE VARIABLE toolTip AS System.Windows.Forms.ToolTip NO-UNDO.

  
  CONSTRUCTOR PUBLIC test2( ):
  
    SUPER().
    InitializeComponent ( ).
    THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.
  END CONSTRUCTOR.  

 
    @VisualDesigner.
    METHOD PRIVATE VOID ShowNewForm(sender AS System.Object, e AS System.EventArgs):
    
        /* Create a NEW instance of the child form. */
        DEFINE VARIABLE childForm AS Progress.Windows.Form NO-UNDO. 
        childForm = NEW Progress.Windows.Form( ).
        /* Make it a child of this MDI form before showing it. */
        childForm:MdiParent = THIS-OBJECT.
        childFormNumber = childFormNumber + 1.
        childForm:Text = "Window " + STRING(childFormNumber).
        childForm:FormClosed:SUBSCRIBE(Form_Closed).
        childForm:Show( ).
    END.
    
    
  @VisualDesigner.
    METHOD PRIVATE VOID OpenFile_Ok(sender AS System.Object, e AS System.ComponentModel.CancelEventArgs):
        DEFINE VARIABLE fileName AS CHARACTER NO-UNDO.
        fileName = openFileDialog:FileName.
        /* TODO: Add code here to open the file. */
    END.    

  @VisualDesigner.
    METHOD PRIVATE VOID OpenFile(sender AS System.Object, e AS System.EventArgs):
    
        openFileDialog = NEW System.Windows.Forms.OpenFileDialog( ).
        DEFINE VARIABLE x AS CHARACTER NO-UNDO.
        x = System.Environment:GetFolderPath(System.Environment+SpecialFolder:Personal).
        openFileDialog:InitialDirectory = System.Environment:GetFolderPath(System.Environment+SpecialFolder:Personal).
    openFileDialog:RestoreDirectory = TRUE.
        openFileDialog:Filter = "Text Files (*.txt)|*.txt|All Files (*.*)|*.*".
        openFileDialog:FileOk:Subscribe(OpenFile_Ok).
        WAIT-FOR openFileDialog:ShowDialog().
    END.

    @VisualDesigner.
    METHOD PRIVATE VOID SaveAsToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        saveFileDialog = NEW System.Windows.Forms.SaveFileDialog( ).
        saveFileDialog:InitialDirectory = System.Environment:GetFolderPath(System.Environment+SpecialFolder:Personal).
    saveFileDialog:RestoreDirectory = TRUE.
        saveFileDialog:Filter = "Text Files (*.txt)|*.txt|All Files (*.*)|*.*".
        saveFileDialog:FileOk:Subscribe(SaveAs_Ok).
        WAIT-FOR saveFileDialog:ShowDialog(THIS-OBJECT).

     END.

  @VisualDesigner.
    METHOD PRIVATE VOID SaveAs_Ok(o AS System.Object, e AS System.ComponentModel.CancelEventArgs):
        DEFINE VARIABLE fileName AS CHARACTER NO-UNDO.
        fileName = saveFileDialog:FileName.
        /* TODO: Add code here to save the current contents of the form to a file. */
    END.

  @VisualDesigner.
    METHOD PRIVATE VOID ExitToolsStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        THIS-OBJECT:Close().
    END.

  @VisualDesigner.
    METHOD PRIVATE VOID CutToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        /* TODO: Use System.Windows.Forms.Clipboard to insert the selected text or images into the clipboard */
    END.

  @VisualDesigner.
    METHOD PRIVATE VOID CopyToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        /* TODO: Use System.Windows.Forms.Clipboard to insert the selected text or images into the clipboard */
    END.

  @VisualDesigner.
    METHOD PRIVATE VOID PasteToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        /* TODO: Use System.Windows.Forms.Clipboard:GetText( ) or System.Windows.Forms:GetData to retrieve information from the clipboard. */
    END.

  @VisualDesigner.
    METHOD PRIVATE VOID ToolBarToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        toolStrip:Visible = toolBarToolStripMenuItem:Checked.
    END.

  @VisualDesigner.
    METHOD PRIVATE VOID StatusBarToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        statusStrip:Visible = statusBarToolStripMenuItem:Checked.
    END.

  @VisualDesigner.
    METHOD PRIVATE VOID CascadeToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        THIS-OBJECT:LayoutMdi(System.Windows.Forms.MdiLayout:Cascade).
    END.

  @VisualDesigner.
    METHOD PRIVATE VOID TileVerticleToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        THIS-OBJECT:LayoutMdi(System.Windows.Forms.MdiLayout:TileVertical).
    END.

  @VisualDesigner.
    METHOD PRIVATE VOID TileHorizontalToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        THIS-OBJECT:LayoutMdi(System.Windows.Forms.MdiLayout:TileHorizontal).
    END.

  @VisualDesigner.
    METHOD PRIVATE VOID ArrangeIconsToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        THIS-OBJECT:LayoutMdi(System.Windows.Forms.MdiLayout:ArrangeIcons).
    END.

  @VisualDesigner.
    METHOD PRIVATE VOID CloseAllToolStripMenuItem_Click(sender AS System.Object, e AS System.EventArgs):
    
        DEFINE VARIABLE formArray AS System.Array NO-UNDO.
        DEFINE VARIABLE formx AS System.Windows.Forms.Form NO-UNDO.
        formArray = THIS-OBJECT:MdiChildren.
        DEFINE VARIABLE icnt AS INTEGER NO-UNDO.
        DO icnt = 0 TO formArray:LENGTH - 1:
            formx = CAST( formArray:GetValue(icnt), System.Windows.Forms.Form ).
            formx:Close( ).
        END.
    END.

    
  METHOD PRIVATE VOID InitializeComponent ( ):

    /* NOTE: The following method is automatically generated.
       
       We strongly suggest that the contents of this method only be modified using the 
       Visual Designer to avoid any incompatible modifications.
       
       Modifying the contents of this method using a code editor will invalidate any support for this file. */


    components = NEW System.ComponentModel.Container( ).
    @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources  = NEW Progress.Util.ResourceManager("fix.test2").
        menuStrip = NEW System.Windows.Forms.MenuStrip( ).
        fileMenu = NEW System.Windows.Forms.ToolStripMenuItem( ).
        newToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        openToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        toolStripSeparator3 = NEW System.Windows.Forms.ToolStripSeparator( ).
        saveToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        saveAsToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        toolStripSeparator4 = NEW System.Windows.Forms.ToolStripSeparator( ).
        printToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        
        printPreviewToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        printSetupToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        toolStripSeparator5 = NEW System.Windows.Forms.ToolStripSeparator( ).
        exitToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        editMenu = NEW System.Windows.Forms.ToolStripMenuItem( ).
        undoToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        redoToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        toolStripSeparator6 = NEW System.Windows.Forms.ToolStripSeparator( ).
        cutToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        copyToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        pasteToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        toolStripSeparator7 = NEW System.Windows.Forms.ToolStripSeparator( ).
        selectAllToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        viewMenu = NEW System.Windows.Forms.ToolStripMenuItem( ).
        toolBarToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        statusBarToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        toolsMenu = NEW System.Windows.Forms.ToolStripMenuItem( ).
        
        optionsToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        windowsMenu = NEW System.Windows.Forms.ToolStripMenuItem( ).
        newWindowToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        cascadeToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        tileVerticalToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        tileHorizontalToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        closeAllToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        arrangeIconsToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        helpMenu = NEW System.Windows.Forms.ToolStripMenuItem( ).
        contentsToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        indexToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        searchToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        toolStripSeparator8 = NEW System.Windows.Forms.ToolStripSeparator( ).
        aboutToolStripMenuItem = NEW System.Windows.Forms.ToolStripMenuItem( ).
        toolStrip = NEW System.Windows.Forms.ToolStrip( ).
        newToolStripButton = NEW System.Windows.Forms.ToolStripButton( ).
        openToolStripButton = NEW System.Windows.Forms.ToolStripButton( ).
        saveToolStripButton = NEW System.Windows.Forms.ToolStripButton( ).
        toolStripSeparator1 = NEW System.Windows.Forms.ToolStripSeparator( ).
        printToolStripButton = NEW System.Windows.Forms.ToolStripButton( ).
        printPreviewToolStripButton = NEW System.Windows.Forms.ToolStripButton( ).
        toolStripSeparator2 = NEW System.Windows.Forms.ToolStripSeparator( ).
        helpToolStripButton = NEW System.Windows.Forms.ToolStripButton( ).
        statusStrip = NEW System.Windows.Forms.StatusStrip( ).
        toolStripStatusLabel = NEW System.Windows.Forms.ToolStripStatusLabel( ).
        toolTip = NEW System.Windows.Forms.ToolTip(components).
        menuStrip:SuspendLayout( ).
        toolStrip:SuspendLayout( ).
        statusStrip:SuspendLayout( ).
        THIS-OBJECT:SuspendLayout( ).

        /* menuStrip */
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar1 AS System.Array NO-UNDO.
        arrayvar1 = System.Array:CreateInstance(Progress.Util.TypeHelper:GetType("System.Windows.Forms.ToolStripItem"), 6).
        arrayvar1:SetValue(fileMenu, 0).
        arrayvar1:SetValue(editMenu, 1).
        arrayvar1:SetValue(viewMenu, 2).
        arrayvar1:SetValue(toolsMenu, 3).
        arrayvar1:SetValue(windowsMenu, 4).
        arrayvar1:SetValue(helpMenu, 5).
        menuStrip:Items:AddRange( CAST (arrayvar1, "System.Windows.Forms.ToolStripItem[]") ).
        menuStrip:Location = NEW System.Drawing.Point(0, 0).
        menuStrip:MdiWindowListItem = windowsMenu.
        menuStrip:Name = "menuStrip".
        menuStrip:Size = NEW System.Drawing.Size(632, 24).
        menuStrip:TabIndex = 0.
        menuStrip:Text = "MenuStrip".
        
        /* fileMenu */
         @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar2 AS System.Array NO-UNDO.
        arrayvar2 = System.Array:CreateInstance(Progress.Util.TypeHelper:GetType("System.Windows.Forms.ToolStripItem"), 11).
        arrayvar2:SetValue(newToolStripMenuItem, 0).
        arrayvar2:SetValue(openToolStripMenuItem, 1).
        arrayvar2:SetValue(toolStripSeparator3, 2).
        arrayvar2:SetValue(saveToolStripMenuItem, 3).
        arrayvar2:SetValue(saveAsToolStripMenuItem, 4).
        arrayvar2:SetValue(toolStripSeparator4, 5).
        arrayvar2:SetValue(printToolStripMenuItem, 6).
        arrayvar2:SetValue(printPreviewToolStripMenuItem, 7).
        arrayvar2:SetValue(printSetupToolStripMenuItem, 8).
        arrayvar2:SetValue(toolStripSeparator5, 9).
        arrayvar2:SetValue(exitToolStripMenuItem, 10).
        fileMenu:DropDownItems:AddRange( CAST (arrayvar2, "System.Windows.Forms.ToolStripItem[]") ). 
        fileMenu:ImageTransparentColor = System.Drawing.SystemColors:ActiveBorder.
        fileMenu:Name = "fileMenu".
        fileMenu:Size = NEW System.Drawing.Size(35, 20).
        fileMenu:Text = "&File".


        /* newToolStripMenuItem */
        newToolStripMenuItem:Image = CAST( resources:GetObject("newToolStripMenuItem.Image"), System.Drawing.Image ).
        newToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        newToolStripMenuItem:Name = "newToolStripMenuItem".
        newToolStripMenuItem:ShortcutKeys = CAST( Progress.Util.EnumHelper:Or(System.Windows.Forms.Keys:Control, System.Windows.Forms.Keys:N), System.Windows.Forms.Keys ).
        newToolStripMenuItem:Size = NEW System.Drawing.Size(151, 22).
        newToolStripMenuItem:Text = "&New".
        newToolStripMenuItem:Click:Subscribe(ShowNewForm).

        /* openToolStripMenuItem */
        openToolStripMenuItem:Image = CAST(resources:GetObject("openToolStripMenuItem.Image"), System.Drawing.Image).
        openToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        openToolStripMenuItem:Name = "openToolStripMenuItem".
        openToolStripMenuItem:ShortcutKeys = CAST( Progress.Util.EnumHelper:Or(System.Windows.Forms.Keys:Control, System.Windows.Forms.Keys:O), System.Windows.Forms.Keys).
        openToolStripMenuItem:Size = NEW System.Drawing.Size(151, 22).
        openToolStripMenuItem:Text = "&Open".
        openToolStripMenuItem:Click:Subscribe(OpenFile).

        /* toolStripSeparator3 */
        toolStripSeparator3:Name = "toolStripSeparator3".
        toolStripSeparator3:Size = NEW System.Drawing.Size(148, 6).

        /* saveToolStripMenuItem */
        saveToolStripMenuItem:Image = CAST( resources:GetObject("saveToolStripMenuItem.Image"), System.Drawing.Image ).
        saveToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        saveToolStripMenuItem:Name = "saveToolStripMenuItem".
        saveToolStripMenuItem:ShortcutKeys = CAST( Progress.Util.EnumHelper:Or(System.Windows.Forms.Keys:Control, System.Windows.Forms.Keys:S), System.Windows.Forms.Keys).
        saveToolStripMenuItem:Size = NEW System.Drawing.Size(151, 22).
        saveToolStripMenuItem:Text = "&Save".

        /* saveAsToolStripMenuItem  */
        saveAsToolStripMenuItem:Name = "saveAsToolStripMenuItem".
        saveAsToolStripMenuItem:Size = NEW System.Drawing.Size(151, 22).
        saveAsToolStripMenuItem:Text = "Save &As".
        saveAsToolStripMenuItem:Click:Subscribe(SaveAsToolStripMenuItem_Click).

        /* toolStripSeparator4 */
        toolStripSeparator4:Name = "toolStripSeparator4".
        toolStripSeparator4:Size = NEW System.Drawing.Size(148, 6).

        /* printToolStripMenuItem */
        printToolStripMenuItem:Image = CAST (resources:GetObject("printToolStripMenuItem.Image"), System.Drawing.Image ).
        printToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        printToolStripMenuItem:Name = "printToolStripMenuItem".
        printToolStripMenuItem:ShortcutKeys = CAST( Progress.Util.EnumHelper:Or(System.Windows.Forms.Keys:Control, System.Windows.Forms.Keys:P), System.Windows.Forms.Keys).
        printToolStripMenuItem:Size = NEW System.Drawing.Size(151, 22).
        printToolStripMenuItem:Text = "&Print".

        /* printPreviewToolStripMenuItem */
        printPreviewToolStripMenuItem:Image = CAST (resources:GetObject("printPreviewToolStripMenuItem.Image"), System.Drawing.Image ).
        printPreviewToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        printPreviewToolStripMenuItem:Name = "printPreviewToolStripMenuItem".
        printPreviewToolStripMenuItem:Size = NEW System.Drawing.Size(151, 22).
        printPreviewToolStripMenuItem:Text = "Print Pre&view".

        /* printSetupToolStripMenuItem */
        printSetupToolStripMenuItem:Name = "printSetupToolStripMenuItem".
        printSetupToolStripMenuItem:Size = NEW System.Drawing.Size(151, 22).
        printSetupToolStripMenuItem:Text = "Print Setup".

        /* toolStripSeparator5 */
        toolStripSeparator5:Name = "toolStripSeparator5".
        toolStripSeparator5:Size = NEW System.Drawing.Size(148, 6).

        /* exitToolStripMenuItem */
        exitToolStripMenuItem:Name = "exitToolStripMenuItem".
        exitToolStripMenuItem:Size = NEW System.Drawing.Size(151, 22).
        exitToolStripMenuItem:Text = "E&xit".
        exitToolStripMenuItem:Click:Subscribe(ExitToolsStripMenuItem_Click).

        /* editMenu */
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar3 AS System.Array NO-UNDO.
        arrayvar3 = System.Array:CreateInstance(Progress.Util.TypeHelper:GetType("System.Windows.Forms.ToolStripItem"), 8).
        arrayvar3:SetValue( undoToolStripMenuItem, 0).
        arrayvar3:SetValue( redoToolStripMenuItem, 1).
        arrayvar3:SetValue( toolStripSeparator6, 2).
        arrayvar3:SetValue( cutToolStripMenuItem, 3).
        arrayvar3:SetValue( copyToolStripMenuItem, 4).
        arrayvar3:SetValue( pasteToolStripMenuItem, 5).
        arrayvar3:SetValue( toolStripSeparator7, 6).
        arrayvar3:SetValue( selectAllToolStripMenuItem, 7).
        editMenu:DropDownItems:AddRange( CAST (arrayvar3, "System.Windows.Forms.ToolStripItem[]") ).
        editMenu:Name = "editMenu".
        editMenu:Size = NEW System.Drawing.Size(37, 20).
        editMenu:Text = "&Edit".

/*                                                                                                                                                                                   */
        /* undoToolStripMenuItem */
        undoToolStripMenuItem:Image = CAST (resources:GetObject("undoToolStripMenuItem.Image"), System.Drawing.Image ).
        undoToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        undoToolStripMenuItem:Name = "undoToolStripMenuItem".
        undoToolStripMenuItem:ShortcutKeys = CAST( Progress.Util.EnumHelper:Or(System.Windows.Forms.Keys:Control, System.Windows.Forms.Keys:Z), System.Windows.Forms.Keys).
        undoToolStripMenuItem:Size = NEW System.Drawing.Size(167, 22).
        undoToolStripMenuItem:Text = "&Undo".

        /* redoToolStripMenuItem */
        redoToolStripMenuItem:Image = CAST (resources:GetObject("redoToolStripMenuItem.Image"), System.Drawing.Image ).
        redoToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        redoToolStripMenuItem:Name = "redoToolStripMenuItem".
        redoToolStripMenuItem:ShortcutKeys = CAST( Progress.Util.EnumHelper:Or(System.Windows.Forms.Keys:Control, System.Windows.Forms.Keys:Y), System.Windows.Forms.Keys).
        redoToolStripMenuItem:Size = NEW System.Drawing.Size(167, 22).
        redoToolStripMenuItem:Text = "&Redo".

        /* toolStripSeparator6 */
        toolStripSeparator6:Name = "toolStripSeparator6".
        toolStripSeparator6:Size = NEW System.Drawing.Size(164, 6).

        /* cutToolStripMenuItem */
        cutToolStripMenuItem:Image = CAST( resources:GetObject("cutToolStripMenuItem.Image"), System.Drawing.Image ).
        cutToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        cutToolStripMenuItem:Name = "cutToolStripMenuItem".
        cutToolStripMenuItem:ShortcutKeys = CAST( Progress.Util.EnumHelper:Or(System.Windows.Forms.Keys:Control, System.Windows.Forms.Keys:X), System.Windows.Forms.Keys).
        cutToolStripMenuItem:Size = NEW System.Drawing.Size(167, 22).
        cutToolStripMenuItem:Text = "Cu&t".
        cutToolStripMenuItem:Click:Subscribe(CutToolStripMenuItem_Click).

        /* copyToolStripMenuItem */
        copyToolStripMenuItem:Image = CAST (resources:GetObject("copyToolStripMenuItem.Image"), System.Drawing.Image ).
        copyToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        copyToolStripMenuItem:Name = "copyToolStripMenuItem".
        copyToolStripMenuItem:ShortcutKeys = CAST( Progress.Util.EnumHelper:Or( System.Windows.Forms.Keys:Control, System.Windows.Forms.Keys:C), System.Windows.Forms.Keys ).
        copyToolStripMenuItem:Size = NEW System.Drawing.Size(167, 22).
        copyToolStripMenuItem:Text = "&Copy".
        copyToolStripMenuItem:Click:Subscribe(CopyToolStripMenuItem_Click).

        /* pasteToolStripMenuItem */
        pasteToolStripMenuItem:Image = CAST( resources:GetObject("pasteToolStripMenuItem.Image"), System.Drawing.Image ).
        pasteToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        pasteToolStripMenuItem:Name = "pasteToolStripMenuItem".
        pasteToolStripMenuItem:ShortcutKeys = CAST( Progress.Util.EnumHelper:Or( System.Windows.Forms.Keys:Control, System.Windows.Forms.Keys:V ), System.Windows.Forms.Keys ).
        pasteToolStripMenuItem:Size = NEW System.Drawing.Size(167, 22).
        pasteToolStripMenuItem:Text = "&Paste".
        pasteToolStripMenuItem:Click:Subscribe(PasteToolStripMenuItem_Click).

        /* toolStripSeparator7 */
        toolStripSeparator7:Name = "toolStripSeparator7".
        toolStripSeparator7:Size = NEW System.Drawing.Size(164, 6).

        /* selectAllToolStripMenuItem */
        selectAllToolStripMenuItem:Name = "selectAllToolStripMenuItem".
        selectAllToolStripMenuItem:ShortcutKeys = CAST( Progress.Util.EnumHelper:Or( System.Windows.Forms.Keys:Control, System.Windows.Forms.Keys:A ), System.Windows.Forms.Keys ).
        selectAllToolStripMenuItem:Size = NEW System.Drawing.Size(167, 22).
        selectAllToolStripMenuItem:Text = "Select &All".

        /* viewMenu */
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar3a AS System.Array NO-UNDO.
        arrayvar3a = System.Array:CreateInstance(Progress.Util.TypeHelper:GetType("System.Windows.Forms.ToolStripItem"), 2).
        arrayvar3a:SetValue( toolBarToolStripMenuItem, 0).
        arrayvar3a:SetValue( statusBarToolStripMenuItem, 1).
        viewMenu:DropDownItems:AddRange(CAST (arrayvar3a, "System.Windows.Forms.ToolStripItem[]")).
        viewMenu:Size = NEW System.Drawing.Size(41, 20).
        viewMenu:Text = "&View".


        /* toolBarToolStripMenuItem */
        toolBarToolStripMenuItem:Checked = TRUE.
        toolBarToolStripMenuItem:CheckOnClick = TRUE.
        toolBarToolStripMenuItem:CheckState = System.Windows.Forms.CheckState:Checked.
        toolBarToolStripMenuItem:Name = "toolBarToolStripMenuItem".
        toolBarToolStripMenuItem:Size = NEW System.Drawing.Size(135, 22).
        toolBarToolStripMenuItem:Text = "&Toolbar".
        toolBarToolStripMenuItem:Click:Subscribe(ToolBarToolStripMenuItem_Click).

        /* statusBarToolStripMenuItem */
        statusBarToolStripMenuItem:Checked = TRUE.
        statusBarToolStripMenuItem:CheckOnClick = TRUE.
        statusBarToolStripMenuItem:CheckState = System.Windows.Forms.CheckState:Checked.
        statusBarToolStripMenuItem:Name = "statusBarToolStripMenuItem".
        statusBarToolStripMenuItem:Size = NEW System.Drawing.Size(135, 22).
        statusBarToolStripMenuItem:Text = "&Status Bar".
        statusBarToolStripMenuItem:Click:Subscribe(StatusBarToolStripMenuItem_Click).

        /* toolsMenu */
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar4 AS System.Array NO-UNDO.
        arrayvar4 = System.Array:CreateInstance(Progress.Util.TypeHelper:GetType("System.Windows.Forms.ToolStripItem"), 1).
        arrayvar4:SetValue( optionsToolStripMenuItem, 0).
        toolsMenu:DropDownItems:AddRange( CAST (arrayvar4, "System.Windows.Forms.ToolStripItem[]") ).
        toolsMenu:Name = "toolsMenu".
        toolsMenu:Size = NEW System.Drawing.Size(44, 20).
        toolsMenu:Text = "&Tools".

        /* optionsToolStripMenuItem */
        optionsToolStripMenuItem:Name = "optionsToolStripMenuItem".
        optionsToolStripMenuItem:Size = NEW System.Drawing.Size(122, 22).
        optionsToolStripMenuItem:Text = "&Options".

        /* windowsMenu */
         @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar5 AS System.Array NO-UNDO.
        arrayvar5 = System.Array:CreateInstance(Progress.Util.TypeHelper:GetType("System.Windows.Forms.ToolStripItem"), 6).
        arrayvar5:SetValue( newWindowToolStripMenuItem, 0).
        arrayvar5:SetValue( cascadeToolStripMenuItem, 1).
        arrayvar5:SetValue( tileVerticalToolStripMenuItem, 2).
        arrayvar5:SetValue( tileHorizontalToolStripMenuItem, 3).
        arrayvar5:SetValue( closeAllToolStripMenuItem, 4).
        arrayvar5:SetValue( arrangeIconsToolStripMenuItem, 5).
        windowsMenu:DropDownItems:AddRange( CAST (arrayvar5, "System.Windows.Forms.ToolStripItem[]") ).
        windowsMenu:Name = "windowsMenu".
        windowsMenu:Size = NEW System.Drawing.Size(62, 20).
        windowsMenu:Text = "&Windows".

        /* newWindowToolStripMenuItem */
        newWindowToolStripMenuItem:Name = "newWindowToolStripMenuItem".
        newWindowToolStripMenuItem:Size = NEW System.Drawing.Size(153, 22).
        newWindowToolStripMenuItem:Text = "&New Window".
        newWindowToolStripMenuItem:Click:Subscribe(ShowNewForm).

        /* cascadeToolStripMenuItem */
        cascadeToolStripMenuItem:Name = "cascadeToolStripMenuItem".
        cascadeToolStripMenuItem:Size = NEW System.Drawing.Size(153, 22).
        cascadeToolStripMenuItem:Text = "&Cascade".
        cascadeToolStripMenuItem:Click:Subscribe(CascadeToolStripMenuItem_Click).

        /* tileVerticalToolStripMenuItem */
        tileVerticalToolStripMenuItem:Name = "tileVerticalToolStripMenuItem".
        tileVerticalToolStripMenuItem:Size = NEW System.Drawing.Size(153, 22).
        tileVerticalToolStripMenuItem:Text = "Tile &Vertical".
        tileVerticalToolStripMenuItem:Click:Subscribe(TileVerticleToolStripMenuItem_Click).

        /* tileHorizontalToolStripMenuItem */
        tileHorizontalToolStripMenuItem:Name = "tileHorizontalToolStripMenuItem".
        tileHorizontalToolStripMenuItem:Size = NEW System.Drawing.Size(153, 22).
        tileHorizontalToolStripMenuItem:Text = "Tile &Horizontal".
        tileHorizontalToolStripMenuItem:Click:Subscribe(TileHorizontalToolStripMenuItem_Click).

        /* closeAllToolStripMenuItem */
        closeAllToolStripMenuItem:Name = "closeAllToolStripMenuItem".
        closeAllToolStripMenuItem:Size = NEW System.Drawing.Size(153, 22).
        closeAllToolStripMenuItem:Text = "C&lose All".
        closeAllToolStripMenuItem:Click:Subscribe(CloseAllToolStripMenuItem_Click).

        /* arrangeIconsToolStripMenuItem */
        arrangeIconsToolStripMenuItem:Name = "arrangeIconsToolStripMenuItem".
        arrangeIconsToolStripMenuItem:Size = NEW System.Drawing.Size(153, 22).
        arrangeIconsToolStripMenuItem:Text = "&Arrange Icons".
        arrangeIconsToolStripMenuItem:Click:Subscribe(ArrangeIconsToolStripMenuItem_Click).

        /* helpMenu */
         @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar6 AS System.Array NO-UNDO.
        arrayvar6 = System.Array:CreateInstance(Progress.Util.TypeHelper:GetType("System.Windows.Forms.ToolStripItem"), 5).
        arrayvar6:SetValue( contentsToolStripMenuItem, 0).
        arrayvar6:SetValue( indexToolStripMenuItem, 1).
        arrayvar6:SetValue( searchToolStripMenuItem, 2).
        arrayvar6:SetValue( toolStripSeparator8, 3).
        arrayvar6:SetValue( aboutToolStripMenuItem, 4).
        helpMenu:DropDownItems:AddRange( CAST (arrayvar6, "System.Windows.Forms.ToolStripItem[]") ).
        helpMenu:Name = "helpMenu".
        helpMenu:Size = NEW System.Drawing.Size(40, 20).
        helpMenu:Text = "&Help".

        /* contentsToolStripMenuItem */
        contentsToolStripMenuItem:Name = "contentsToolStripMenuItem".
        contentsToolStripMenuItem:ShortcutKeys = CAST( Progress.Util.EnumHelper:Or( System.Windows.Forms.Keys:Control, System.Windows.Forms.Keys:F1 ), System.Windows.Forms.Keys ).
        contentsToolStripMenuItem:Size = NEW System.Drawing.Size(173, 22).
        contentsToolStripMenuItem:Text = "&Contents".

        /* indexToolStripMenuItem */
        indexToolStripMenuItem:Image = CAST (resources:GetObject("indexToolStripMenuItem.Image"), System.Drawing.Image ).
        indexToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        indexToolStripMenuItem:Name = "indexToolStripMenuItem".
        indexToolStripMenuItem:Size = NEW System.Drawing.Size(173, 22).
        indexToolStripMenuItem:Text = "&Index".

        /* searchToolStripMenuItem */
        searchToolStripMenuItem:Image = CAST (resources:GetObject("searchToolStripMenuItem.Image"), System.Drawing.Image ).
        searchToolStripMenuItem:ImageTransparentColor = System.Drawing.Color:Black.
        searchToolStripMenuItem:Name = "searchToolStripMenuItem".
        searchToolStripMenuItem:Size = NEW System.Drawing.Size(173, 22).
        searchToolStripMenuItem:Text = "&Search".

        /* toolStripSeparator8 */
        toolStripSeparator8:Name = "toolStripSeparator8".
        toolStripSeparator8:Size = NEW System.Drawing.Size(170, 6).

        /* aboutToolStripMenuItem */
        aboutToolStripMenuItem:Name = "aboutToolStripMenuItem".
        aboutToolStripMenuItem:Size = NEW System.Drawing.Size(173, 22).
        aboutToolStripMenuItem:Text = "&About...".

        /* toolStrip */
         @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar7 AS System.Array NO-UNDO.
        arrayvar7 = System.Array:CreateInstance(Progress.Util.TypeHelper:GetType("System.Windows.Forms.ToolStripItem"), 8).
        arrayvar7:SetValue( newToolStripButton, 0).
        arrayvar7:SetValue( openToolStripButton, 1).
        arrayvar7:SetValue( saveToolStripButton, 2).
        arrayvar7:SetValue( toolStripSeparator1, 3).
        arrayvar7:SetValue( printToolStripButton, 4).
        arrayvar7:SetValue( printPreviewToolStripButton, 5).
        arrayvar7:SetValue( toolStripSeparator2, 6).
        arrayvar7:SetValue( helpToolStripButton, 7).
        toolStrip:Items:AddRange( CAST (arrayvar7, "System.Windows.Forms.ToolStripItem[]") ).
        toolStrip:Location = NEW System.Drawing.Point(0, 24).
        toolStrip:Name = "toolStrip".
        toolStrip:Size = NEW System.Drawing.Size(632, 25).
        toolStrip:TabIndex = 1.
        toolStrip:Text = "ToolStrip".

        /* newToolStripButton */
        newToolStripButton:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
        newToolStripButton:Image = CAST (resources:GetObject("newToolStripButton.Image"), System.Drawing.Image ).
        newToolStripButton:ImageTransparentColor = System.Drawing.Color:Black.
        newToolStripButton:Name = "newToolStripButton".
        newToolStripButton:Size = NEW System.Drawing.Size(23, 22).
        newToolStripButton:Text = "New".
        newToolStripButton:Click:Subscribe(ShowNewForm).

        /* openToolStripButton */
        openToolStripButton:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
        openToolStripButton:Image = CAST (resources:GetObject("openToolStripButton.Image"), System.Drawing.Image ).
        openToolStripButton:ImageTransparentColor = System.Drawing.Color:Black.
        openToolStripButton:Name = "openToolStripButton".
        openToolStripButton:Size = NEW System.Drawing.Size(23, 22).
        openToolStripButton:Text = "Open".
        openToolStripButton:Click:Subscribe(OpenFile).

        /* saveToolStripButton */
        saveToolStripButton:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
        saveToolStripButton:Image = CAST (resources:GetObject("saveToolStripButton.Image"), System.Drawing.Image ).
        saveToolStripButton:ImageTransparentColor = System.Drawing.Color:Black.
        saveToolStripButton:Name = "saveToolStripButton".
        saveToolStripButton:Size = NEW System.Drawing.Size(23, 22).
        saveToolStripButton:Text = "Save".

        /* toolStripSeparator1 */
        toolStripSeparator1:Name = "toolStripSeparator1".
        toolStripSeparator1:Size = NEW System.Drawing.Size(6, 25).

        /* printToolStripButton */
        printToolStripButton:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
        printToolStripButton:Image = CAST (resources:GetObject("printToolStripButton.Image"), System.Drawing.Image ).
        printToolStripButton:ImageTransparentColor = System.Drawing.Color:Black.
        printToolStripButton:Name = "printToolStripButton".
        printToolStripButton:Size = NEW System.Drawing.Size(23, 22).
        printToolStripButton:Text = "Print".

        /* printPreviewToolStripButton */
        printPreviewToolStripButton:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
        printPreviewToolStripButton:Image = CAST (resources:GetObject("printPreviewToolStripButton.Image"), System.Drawing.Image ).
        printPreviewToolStripButton:ImageTransparentColor = System.Drawing.Color:Black.
        printPreviewToolStripButton:Name = "printPreviewToolStripButton".
        printPreviewToolStripButton:Size = NEW System.Drawing.Size(23, 22).
        printPreviewToolStripButton:Text = "Print Preview".

        /* toolStripSeparator2 */
        toolStripSeparator2:Name = "toolStripSeparator2".
        toolStripSeparator2:Size = NEW System.Drawing.Size(6, 25).

        /* helpToolStripButton */
        helpToolStripButton:DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle:Image.
        helpToolStripButton:Image = CAST (resources:GetObject("helpToolStripButton.Image"), System.Drawing.Image ).
        helpToolStripButton:ImageTransparentColor = System.Drawing.Color:Black.
        helpToolStripButton:Name = "helpToolStripButton".
        helpToolStripButton:Size = NEW System.Drawing.Size(23, 22).
        helpToolStripButton:Text = "Help".

        /* statusStrip */
         @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar8 AS System.Array NO-UNDO.
        arrayvar8 = System.Array:CreateInstance(Progress.Util.TypeHelper:GetType("System.Windows.Forms.ToolStripItem"), 1).
        arrayvar8:SetValue( toolStripStatusLabel, 0).
        statusStrip:Items:AddRange( CAST (arrayvar8, "System.Windows.Forms.ToolStripItem[]") ).
        statusStrip:Location = NEW System.Drawing.Point(0, 431).
        statusStrip:Name = "statusStrip".
        statusStrip:Size = NEW System.Drawing.Size(632, 22).
        statusStrip:TabIndex = 2.
        statusStrip:Text = "StatusStrip".

        /* toolStripStatusLabel */
        toolStripStatusLabel:Name = "toolStripStatusLabel".
        toolStripStatusLabel:Size = NEW System.Drawing.Size(38, 17).
        toolStripStatusLabel:Text = "Status".

        /* MDIForm */
        THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(6.0, 13.0).
        THIS-OBJECT:AutoScaleMode = System.Windows.Forms.AutoScaleMode:Font.
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(632, 453).
        THIS-OBJECT:Controls:Add(statusStrip).
        THIS-OBJECT:Controls:Add(toolStrip).
        THIS-OBJECT:Controls:Add(menuStrip).
        THIS-OBJECT:IsMdiContainer = TRUE.
        THIS-OBJECT:MainMenuStrip = menuStrip.
        THIS-OBJECT:Name = "test2".
        THIS-OBJECT:Text =  "test2".
        THIS-OBJECT:FormClosed:SUBSCRIBE(Form_Closed).
        
        menuStrip:ResumeLayout(FALSE).
        menuStrip:PerformLayout( ).
        toolStrip:ResumeLayout(FALSE).
        toolStrip:PerformLayout( ).
        statusStrip:ResumeLayout(FALSE).
        statusStrip:PerformLayout( ).
        THIS-OBJECT:ResumeLayout(FALSE).
        THIS-OBJECT:PerformLayout( ).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.        
    END METHOD.

  DESTRUCTOR PUBLIC test2( ):
  
  END DESTRUCTOR.
  
  
  @VisualDesigner.
    METHOD PRIVATE VOID Form_Closed(sender AS System.Object, e AS System.EventArgs):
       
    END.   

END CLASS.
