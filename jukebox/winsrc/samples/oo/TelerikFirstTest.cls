 
 /*------------------------------------------------------------------------
    File        : TelerikFirstTest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Thu Mar 10 10:03:03 CET 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.


CLASS TelerikFirstTest INHERITS Form: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE radScheduler1 AS Telerik.WinControls.UI.RadScheduler NO-UNDO.

		
	CONSTRUCTOR PUBLIC TelerikFirstTest (  ):
		
		
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
    DEFINE VARIABLE schedulerDailyPrintStyle1 AS Telerik.WinControls.UI.SchedulerDailyPrintStyle NO-UNDO.
    schedulerDailyPrintStyle1 = NEW Telerik.WinControls.UI.SchedulerDailyPrintStyle().
    THIS-OBJECT:radScheduler1 = NEW Telerik.WinControls.UI.RadScheduler().
    CAST(THIS-OBJECT:radScheduler1, System.ComponentModel.ISupportInitialize):BeginInit().
    THIS-OBJECT:SuspendLayout().
    /*  */
    /* radScheduler1 */
    /*  */
    THIS-OBJECT:radScheduler1:Culture = NEW System.Globalization.CultureInfo("nb-NO").
    THIS-OBJECT:radScheduler1:Location = NEW System.Drawing.Point(32, 58).
    THIS-OBJECT:radScheduler1:Name = "radScheduler1".
    schedulerDailyPrintStyle1:AppointmentFont = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
    schedulerDailyPrintStyle1:DateEndRange = Progress.Util.DateTimeHelper:Create(2016, 3, 15, 0, 0, 0, 0).
    schedulerDailyPrintStyle1:DateHeadingFont = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Bold).
    schedulerDailyPrintStyle1:DateStartRange = Progress.Util.DateTimeHelper:Create(2016, 3, 10, 0, 0, 0, 0).
    schedulerDailyPrintStyle1:PageHeadingFont = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(22), System.Drawing.FontStyle:Bold).
    THIS-OBJECT:radScheduler1:PrintStyle = schedulerDailyPrintStyle1.
    THIS-OBJECT:radScheduler1:Size = NEW System.Drawing.Size(500, 500).
    THIS-OBJECT:radScheduler1:TabIndex = 0.
    THIS-OBJECT:radScheduler1:Text = "radScheduler1".
    /*  */
    /* TelerikFirstTest */
    /*  */
    THIS-OBJECT:ClientSize = NEW System.Drawing.Size(583, 608).
    THIS-OBJECT:Controls:Add(THIS-OBJECT:radScheduler1).
    THIS-OBJECT:Name = "TelerikFirstTest".
    THIS-OBJECT:Text = "TelerikFirstTest".
    CAST(THIS-OBJECT:radScheduler1, System.ComponentModel.ISupportInitialize):EndInit().
    THIS-OBJECT:ResumeLayout(FALSE).
    CATCH e AS Progress.Lang.Error:
      UNDO, THROW e.
    END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC TelerikFirstTest ( ):

	END DESTRUCTOR.

END CLASS.