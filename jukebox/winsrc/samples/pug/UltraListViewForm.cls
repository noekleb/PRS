 
 /*------------------------------------------------------------------------
    File        : UltraListViewForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Tue Aug 12 13:40:13 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.



CLASS UltraListViewForm INHERITS Form: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraListView1 AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.

		
	CONSTRUCTOR PUBLIC UltraListViewForm (  ):
		
		
        SUPER().
        InitializeComponent().
        THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraListViewSubItem1 AS Infragistics.Win.UltraWinListView.UltraListViewSubItem NO-UNDO.
        ultraListViewSubItem1 = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItem("01.12.2013", ?).
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("UltraListViewForm").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraListViewSubItem2 AS Infragistics.Win.UltraWinListView.UltraListViewSubItem NO-UNDO.
        ultraListViewSubItem2 = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItem("Ver 1.1.2", ?).
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinListView.UltraListViewSubItem EXTENT 3 NO-UNDO.
        arrayvar0[1] = ultraListViewSubItem1.
        arrayvar0[2] = ultraListViewSubItem2.
        arrayvar0[3] = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItem(?, ?).
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraListViewItem1 AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO.
        ultraListViewItem1 = NEW Infragistics.Win.UltraWinListView.UltraListViewItem("Årsoppgave", arrayvar0, ?).
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance("1").
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinListView.UltraListViewSubItem EXTENT 3 NO-UNDO.
        arrayvar1[1] = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItem(?, ?).
        arrayvar1[2] = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItem(?, ?).
        arrayvar1[3] = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItem(?, ?).
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraListViewItem2 AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO.
        ultraListViewItem2 = NEW Infragistics.Win.UltraWinListView.UltraListViewItem(?, arrayvar1, ?).
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinListView.UltraListViewSubItem EXTENT 3 NO-UNDO.
        arrayvar2[1] = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItem(?, ?).
        arrayvar2[2] = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItem(?, ?).
        arrayvar2[3] = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItem(?, ?).
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraListViewItem3 AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO.
        ultraListViewItem3 = NEW Infragistics.Win.UltraWinListView.UltraListViewItem(?, arrayvar2, ?).
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinListView.UltraListViewSubItem EXTENT 3 NO-UNDO.
        arrayvar3[1] = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItem(?, ?).
        arrayvar3[2] = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItem(?, ?).
        arrayvar3[3] = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItem(?, ?).
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraListViewItem4 AS Infragistics.Win.UltraWinListView.UltraListViewItem NO-UNDO.
        ultraListViewItem4 = NEW Infragistics.Win.UltraWinListView.UltraListViewItem(?, arrayvar3, ?).
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraListViewSubItemColumn1 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn NO-UNDO.
        ultraListViewSubItemColumn1 = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn("SubItemColumn 0").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance4 AS Infragistics.Win.Appearance NO-UNDO.
        appearance4 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraListViewSubItemColumn2 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn NO-UNDO.
        ultraListViewSubItemColumn2 = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn("SubItemColumn 1").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE ultraListViewSubItemColumn3 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn NO-UNDO.
        ultraListViewSubItemColumn3 = NEW Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn("SubItemColumn 2").
        THIS-OBJECT:ultraListView1 = NEW Infragistics.Win.UltraWinListView.UltraListView().
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraListView1 */
        /*  */
        appearance1:Image = CAST(resources:GetObject("appearance1.Image"), System.Object).
        ultraListViewSubItem1:Appearance = appearance1.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image"), System.Object).
        ultraListViewSubItem2:Appearance = appearance2.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image"), System.Object).
        appearance3:TextHAlignAsString = "Left".
        ultraListViewItem1:Appearance = appearance3.
        ultraListViewItem1:Key = "1".
        ultraListViewItem2:Key = "2".
        ultraListViewItem3:Key = "3".
        ultraListViewItem4:Key = "4".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinListView.UltraListViewItem EXTENT 4 NO-UNDO.
        arrayvar4[1] = ultraListViewItem1.
        arrayvar4[2] = ultraListViewItem2.
        arrayvar4[3] = ultraListViewItem3.
        arrayvar4[4] = ultraListViewItem4.
        THIS-OBJECT:ultraListView1:Items:AddRange(arrayvar4).
        THIS-OBJECT:ultraListView1:ItemSettings:SubItemsVisibleInToolTipByDefault = TRUE.
        THIS-OBJECT:ultraListView1:Location = NEW System.Drawing.Point(28, 33).
        THIS-OBJECT:ultraListView1:Name = "ultraListView1".
        THIS-OBJECT:ultraListView1:Size = NEW System.Drawing.Size(708, 158).
        ultraListViewSubItemColumn1:Key = "SubItemColumn 0".
        appearance4:Image = CAST(resources:GetObject("appearance4.Image"), System.Object).
        ultraListViewSubItemColumn1:SubItemAppearance = appearance4.
        ultraListViewSubItemColumn1:Tag = "1".
        ultraListViewSubItemColumn1:Text = "Telephone contact".
        ultraListViewSubItemColumn1:VisibleInDetailsView = Infragistics.Win.DefaultableBoolean:True.
        ultraListViewSubItemColumn1:VisibleInTilesView = Infragistics.Win.DefaultableBoolean:True.
        ultraListViewSubItemColumn2:Key = "SubItemColumn 1".
        ultraListViewSubItemColumn3:Key = "SubItemColumn 2".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinListView.UltraListViewSubItemColumn EXTENT 3 NO-UNDO.
        arrayvar5[1] = ultraListViewSubItemColumn1.
        arrayvar5[2] = ultraListViewSubItemColumn2.
        arrayvar5[3] = ultraListViewSubItemColumn3.
        THIS-OBJECT:ultraListView1:SubItemColumns:AddRange(arrayvar5).
        THIS-OBJECT:ultraListView1:TabIndex = 0.
        THIS-OBJECT:ultraListView1:Text = "ultraListView1".
        THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:Tiles.
        THIS-OBJECT:ultraListView1:ItemActivated:Subscribe(THIS-OBJECT:ultraListView1_ItemActivated).
        /*  */
        /* UltraListViewForm */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(823, 343).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ultraListView1).
        THIS-OBJECT:Name = "UltraListViewForm".
        THIS-OBJECT:Text = "UltraListViewForm".
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):EndInit().
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
	METHOD PRIVATE VOID ultraListView1_ItemActivated( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinListView.ItemActivatedEventArgs ):
		
		RETURN.

	END METHOD.

	DESTRUCTOR PUBLIC UltraListViewForm ( ):

	END DESTRUCTOR.

END CLASS.