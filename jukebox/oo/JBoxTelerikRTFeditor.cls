USING Progress.Lang.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.
USING uc.* FROM PROPATH.


ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS uc.JBoxTelerikRTFeditor INHERITS uc.JBoxUCbase: 
  DEFINE PRIVATE VARIABLE components      AS System.ComponentModel.IContainer           NO-UNDO.
  DEFINE PRIVATE VARIABLE radRichTextEditor1 AS Telerik.WinControls.UI.RadRichTextEditor NO-UNDO.
  
  CONSTRUCTOR PUBLIC JBoxTelerikRTFeditor (INPUT ihWinSuper    AS HANDLE, /* Window super proc - unknown if object created from window */
                                     INPUT ihWinProc     AS HANDLE,
                                     INPUT ihPlaceHolder AS HANDLE, /* Design widget (rectangle, field, etc) */
                                     INPUT iiDeltaWidth  AS INTEGER):
     SUPER(ihWinSuper,ihWinProc,ihPlaceHolder,iiDeltaWidth).
   
  END CONSTRUCTOR.
  
  
  CONSTRUCTOR PUBLIC JBoxTelerikRTFeditor (INPUT ihWinProc     AS HANDLE,
                                     INPUT ihPlaceHolder AS HANDLE ): /* Design widget (rectangle, field, etc) */
                                   
     SUPER(?,ihWinProc,ihPlaceHolder,0).
   
  END CONSTRUCTOR.


		METHOD PUBLIC OVERRIDE VOID InitializeComponent():

    @VisualDesigner.FormMember (NeedsInitialize="true").
    DEFINE VARIABLE inputBinding1 AS Telerik.WinControls.Keyboard.InputBinding NO-UNDO.
    inputBinding1 = NEW Telerik.WinControls.Keyboard.InputBinding().
    THIS-OBJECT:radRichTextEditor1 = NEW Telerik.WinControls.UI.RadRichTextEditor().
    CAST(THIS-OBJECT:radRichTextEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
    THIS-OBJECT:SuspendLayout().
    /*  */
    /* radRichTextEditor1 */
    /*  */
    THIS-OBJECT:radRichTextEditor1:BorderColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(156)), System.Convert:ToInt32(System.Convert:ToByte(189)), System.Convert:ToInt32(System.Convert:ToByte(232))).
    THIS-OBJECT:radRichTextEditor1:CaretWidth = System.Single:NaN.
    inputBinding1:Chord = ?.
    @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
    DEFINE VARIABLE arrayvar0 AS Telerik.WinControls.Keyboard.InputBinding EXTENT 1 NO-UNDO.
    arrayvar0[1] = inputBinding1.
    THIS-OBJECT:radRichTextEditor1:CommandBindings:AddRange(arrayvar0).
    THIS-OBJECT:radRichTextEditor1:IsSelectionEnabled = FALSE.
    THIS-OBJECT:radRichTextEditor1:Location = NEW System.Drawing.Point(12, 12).
    THIS-OBJECT:radRichTextEditor1:Name = "radRichTextEditor1".
    THIS-OBJECT:radRichTextEditor1:SelectionFill = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(128)), System.Convert:ToInt32(System.Convert:ToByte(78)), System.Convert:ToInt32(System.Convert:ToByte(158)), System.Convert:ToInt32(System.Convert:ToByte(255))).
    THIS-OBJECT:radRichTextEditor1:ShowItemToolTips = FALSE.
    THIS-OBJECT:radRichTextEditor1:Size = NEW System.Drawing.Size(772, 444).
    THIS-OBJECT:radRichTextEditor1:TabIndex = 0.
    THIS-OBJECT:radRichTextEditor1:UseCompatibleTextRendering = FALSE.
    THIS-OBJECT:radRichTextEditor1:Click:Subscribe(THIS-OBJECT:radRichTextEditor1_Click).
    /*  */
    /* slettme */
    /*  */
    THIS-OBJECT:ClientSize = NEW System.Drawing.Size(782, 459).
    THIS-OBJECT:Controls:Add(THIS-OBJECT:radRichTextEditor1).
    THIS-OBJECT:Name = "slettme".
    THIS-OBJECT:Text = "slettme".
    CAST(THIS-OBJECT:radRichTextEditor1, System.ComponentModel.ISupportInitialize):EndInit().
    THIS-OBJECT:ResumeLayout(FALSE).

    SUPER:InitializeComponent().

    THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.

  END METHOD.

/* Mandatory methods: */
  METHOD PRIVATE VOID radRichTextEditor1_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
    
    RETURN.

  END METHOD.

  METHOD PUBLIC CHARACTER DEFAULT():
    RETURN DYNAMIC-INVOKE(THIS-OBJECT,ENTRY(1,PROGRAM-NAME(2)," ")).
  END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID loadDocument():
		
		RETURN.

	END METHOD.

  DESTRUCTOR PUBLIC JBoxTelerikRTFeditor ( ):
    
    IF VALID-OBJECT(components) THEN 
    DO:
      CAST(components, System.IDisposable):Dispose().
    END.

  END DESTRUCTOR.

END CLASS.
