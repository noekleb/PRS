 
 /*------------------------------------------------------------------------
    File        : MsTabForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Tue Oct 27 22:49:57 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.


CLASS samples.oo.MsTabForm INHERITS Form: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE imageList1 AS System.Windows.Forms.ImageList NO-UNDO.
  DEFINE PRIVATE VARIABLE tabPage3 AS System.Windows.Forms.TabPage NO-UNDO.
  DEFINE PRIVATE VARIABLE tabPage2 AS System.Windows.Forms.TabPage NO-UNDO.
  DEFINE PRIVATE VARIABLE tabPage1 AS System.Windows.Forms.TabPage NO-UNDO.
  DEFINE PRIVATE VARIABLE tabControl1 AS System.Windows.Forms.TabControl NO-UNDO.
		
	CONSTRUCTOR PUBLIC MsTabForm (  ):
		
		
    SUPER().
        
    InitializeComponent().
    THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
    this-object:addPage("Prog3.w","TabFire",0).
    this-object:setImage("gif\i-code.gif",3).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.

	END CONSTRUCTOR.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
    /* NOTE: The following method is automatically generated.
    
    We strongly suggest that the contents of this method only be modified using the
    Visual Designer to avoid any incompatible modifications.
    
    Modifying the contents of this method using a code editor will invalidate any support for this file. */
    THIS-OBJECT:components = NEW System.ComponentModel.Container().
    @VisualDesigner.FormMember (NeedsInitialize="true").
    DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
    resources = NEW Progress.Util.ResourceManager("samples.oo.MsTabForm").
    THIS-OBJECT:tabControl1 = NEW System.Windows.Forms.TabControl().
    THIS-OBJECT:tabPage1 = NEW System.Windows.Forms.TabPage().
    THIS-OBJECT:tabPage2 = NEW System.Windows.Forms.TabPage().
    THIS-OBJECT:tabPage3 = NEW System.Windows.Forms.TabPage().
    THIS-OBJECT:imageList1 = NEW System.Windows.Forms.ImageList(THIS-OBJECT:components).
    THIS-OBJECT:tabControl1:SuspendLayout().
    THIS-OBJECT:SuspendLayout().
    /*  */
    /* tabControl1 */
    /*  */
    THIS-OBJECT:tabControl1:Controls:Add(THIS-OBJECT:tabPage1).
    THIS-OBJECT:tabControl1:Controls:Add(THIS-OBJECT:tabPage2).
    THIS-OBJECT:tabControl1:Controls:Add(THIS-OBJECT:tabPage3).
    THIS-OBJECT:tabControl1:ImageList = THIS-OBJECT:imageList1.
    THIS-OBJECT:tabControl1:Location = NEW System.Drawing.Point(29, 52).
    THIS-OBJECT:tabControl1:Name = "tabControl1".
    THIS-OBJECT:tabControl1:SelectedIndex = 0.
    THIS-OBJECT:tabControl1:Size = NEW System.Drawing.Size(338, 230).
    THIS-OBJECT:tabControl1:TabIndex = 0.
    THIS-OBJECT:tabControl1:SelectedIndexChanged:Subscribe(THIS-OBJECT:testClick).
    THIS-OBJECT:tabControl1:TabIndexChanged:Subscribe(THIS-OBJECT:testClick).
    /*  */
    /* tabPage1 */
    /*  */
    THIS-OBJECT:tabPage1:Location = NEW System.Drawing.Point(4, 22).
    THIS-OBJECT:tabPage1:Name = "tabPage1".
    THIS-OBJECT:tabPage1:Padding = NEW System.Windows.Forms.Padding(3).
    THIS-OBJECT:tabPage1:Size = NEW System.Drawing.Size(192, 74).
    THIS-OBJECT:tabPage1:TabIndex = 0.
    THIS-OBJECT:tabPage1:Text = "tabPage1".
    THIS-OBJECT:tabPage1:UseVisualStyleBackColor = TRUE.
    THIS-OBJECT:tabPage1:Click:Subscribe(THIS-OBJECT:testClick).
    /*  */
    /* tabPage2 */
    /*  */
    THIS-OBJECT:tabPage2:Location = NEW System.Drawing.Point(4, 22).
    THIS-OBJECT:tabPage2:Name = "tabPage2".
    THIS-OBJECT:tabPage2:Padding = NEW System.Windows.Forms.Padding(3).
    THIS-OBJECT:tabPage2:Size = NEW System.Drawing.Size(192, 74).
    THIS-OBJECT:tabPage2:TabIndex = 1.
    THIS-OBJECT:tabPage2:Text = "tabPage2".
    THIS-OBJECT:tabPage2:UseVisualStyleBackColor = TRUE.
    /*  */
    /* tabPage3 */
    /*  */
    THIS-OBJECT:tabPage3:ImageIndex = 1.
    THIS-OBJECT:tabPage3:Location = NEW System.Drawing.Point(4, 23).
    THIS-OBJECT:tabPage3:Name = "tabPage3".
    THIS-OBJECT:tabPage3:Padding = NEW System.Windows.Forms.Padding(3).
    THIS-OBJECT:tabPage3:Size = NEW System.Drawing.Size(330, 203).
    THIS-OBJECT:tabPage3:TabIndex = 2.
    THIS-OBJECT:tabPage3:Text = "tabPage3".
    THIS-OBJECT:tabPage3:UseVisualStyleBackColor = TRUE.
    THIS-OBJECT:tabPage3:Click:Subscribe(THIS-OBJECT:tabPage3_Click).
    /*  */
    /* imageList1 */
    /*  */
    THIS-OBJECT:imageList1:ImageStream = CAST(resources:GetObject("imageList1.ImageStream"), System.Windows.Forms.ImageListStreamer).
    THIS-OBJECT:imageList1:TransparentColor = System.Drawing.Color:Transparent.
    THIS-OBJECT:imageList1:Images:SetKeyName(0, "active.gif").
    THIS-OBJECT:imageList1:Images:SetKeyName(1, "afaudtick.gif").
    THIS-OBJECT:imageList1:Images:SetKeyName(2, "afbinos.gif").
    THIS-OBJECT:imageList1:Images:SetKeyName(3, "afcalc.gif").
    /*  */
    /* MsTabForm */
    /*  */
    THIS-OBJECT:ClientSize = NEW System.Drawing.Size(395, 307).
    THIS-OBJECT:Controls:Add(THIS-OBJECT:tabControl1).
    THIS-OBJECT:Name = "MsTabForm".
    THIS-OBJECT:Text = "MsTabForm".
    THIS-OBJECT:tabControl1:ResumeLayout(FALSE).
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
	METHOD PRIVATE VOID tabPage3_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID testClick( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		MESSAGE tabControl1:SelectedIndex skip 
    VIEW-AS ALERT-BOX.
		RETURN.

	END METHOD.
	
	METHOD PUBLIC LOGICAL setImage (INPUT icImage AS CHAR
	                               ,INPUT iiIdx   AS INT):

    IF imageList1:Images:IndexOfKey(icImage) = -1 THEN
      imageList1:Images:Add(icImage,System.Drawing.Image:FromFile(SEARCH(icImage))).
	                                 
    IF imageList1:Images:IndexOfKey(icImage) GE 0 THEN
      THIS-OBJECT:tabControl1:TabPages[iiIdx]:ImageKey = icImage.
  
  END METHOD.	                                 

  METHOD PUBLIC LOGICAL addPage (INPUT icProgram AS CHAR
                                ,INPUT icLabel   AS CHAR
                                ,INPUT iiIdx     AS INT):

    DEF VAR tabPage1 AS System.Windows.Forms.TabPage NO-UNDO. 

    tabPage1 = NEW System.Windows.Forms.TabPage().
    
/*    THIS-OBJECT:tabPage2 = NEW System.Windows.Forms.TabPage().                          */
/*    THIS-OBJECT:tabPage3 = NEW System.Windows.Forms.TabPage().                          */
/*    THIS-OBJECT:imageList1 = NEW System.Windows.Forms.ImageList(THIS-OBJECT:components).*/
/*    THIS-OBJECT:tabControl1:SuspendLayout().                                            */
/*    THIS-OBJECT:SuspendLayout().                                                        */

    /*  */
    /* tabControl1 */
    /*  */
    THIS-OBJECT:tabControl1:Controls:Add(tabPage1).

/*    THIS-OBJECT:tabPage3:ImageIndex = 1.                            */
/*    THIS-OBJECT:tabPage3:Location = NEW System.Drawing.Point(4, 23).*/
    tabPage1:Name = icLabel.
    tabPage1:Padding = NEW System.Windows.Forms.Padding(3).
/*    THIS-OBJECT:tabPage3:Size = NEW System.Drawing.Size(330, 203).     */
/*    THIS-OBJECT:tabPage3:TabIndex = 2.                                 */
    tabPage1:Text = icLabel.
    tabPage1:UseVisualStyleBackColor = TRUE.
/*    THIS-OBJECT:tabPage3:Click:Subscribe(THIS-OBJECT:tabPage3_Click).*/

/*    IF imageList1:Images:IndexOfKey(icImage) = -1 THEN                              */
/*      imageList1:Images:Add(icImage,System.Drawing.Image:FromFile(SEARCH(icImage))).*/
/*                                                                                    */
/*    IF imageList1:Images:IndexOfKey(icImage) GE 0 THEN                              */
/*      THIS-OBJECT:tabControl1:TabPages[0]:ImageKey = icImage.                       */
  
  END METHOD.                                  

	DESTRUCTOR PUBLIC MsTabForm ( ):

	END DESTRUCTOR.

END CLASS.