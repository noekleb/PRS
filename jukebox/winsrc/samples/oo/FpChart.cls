 
 /*------------------------------------------------------------------------
    File        : FpChart
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : brynjar
    Created     : Tue Apr 01 00:11:46 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.



CLASS calita.jukebox.winsrc.samples.oo.FpChart INHERITS Form: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE fpChart1 AS FarPoint.Win.Chart.FpChart NO-UNDO.

		
	CONSTRUCTOR PUBLIC FpChart (  ):
		
		
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
        DEFINE VARIABLE labelArea1 AS FarPoint.Win.Chart.LabelArea NO-UNDO.
        labelArea1 = NEW FarPoint.Win.Chart.LabelArea().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE legendArea1 AS FarPoint.Win.Chart.LegendArea NO-UNDO.
        legendArea1 = NEW FarPoint.Win.Chart.LegendArea().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE yPlotArea1 AS FarPoint.Win.Chart.YPlotArea NO-UNDO.
        yPlotArea1 = NEW FarPoint.Win.Chart.YPlotArea().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE wall1 AS FarPoint.Win.Chart.Wall NO-UNDO.
        wall1 = NEW FarPoint.Win.Chart.Wall().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE wall2 AS FarPoint.Win.Chart.Wall NO-UNDO.
        wall2 = NEW FarPoint.Win.Chart.Wall().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE directionalLight1 AS FarPoint.Win.Chart.DirectionalLight NO-UNDO.
        directionalLight1 = NEW FarPoint.Win.Chart.DirectionalLight().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE barSeries1 AS FarPoint.Win.Chart.BarSeries NO-UNDO.
        barSeries1 = NEW FarPoint.Win.Chart.BarSeries().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE wall3 AS FarPoint.Win.Chart.Wall NO-UNDO.
        wall3 = NEW FarPoint.Win.Chart.Wall().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE indexAxis1 AS FarPoint.Win.Chart.IndexAxis NO-UNDO.
        indexAxis1 = NEW FarPoint.Win.Chart.IndexAxis().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE valueAxis1 AS FarPoint.Win.Chart.ValueAxis NO-UNDO.
        valueAxis1 = NEW FarPoint.Win.Chart.ValueAxis().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE indexAxis2 AS FarPoint.Win.Chart.IndexAxis NO-UNDO.
        indexAxis2 = NEW FarPoint.Win.Chart.IndexAxis().
        THIS-OBJECT:fpChart1 = NEW FarPoint.Win.Chart.FpChart().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* fpChart1 */
        /*  */
        THIS-OBJECT:fpChart1:Location = NEW System.Drawing.Point(12, 45).
        labelArea1:AlignmentX = Progress.Util.CastUtil:ToSingle(0.5).
        labelArea1:Location = NEW System.Drawing.PointF(Progress.Util.CastUtil:ToSingle(0.5), Progress.Util.CastUtil:ToSingle(0.1)).
        labelArea1:Padding = NEW FarPoint.Win.Chart.PaddingF(Progress.Util.CastUtil:ToSingle(1.5), Progress.Util.CastUtil:ToSingle(1.5), Progress.Util.CastUtil:ToSingle(1.5), Progress.Util.CastUtil:ToSingle(1.5)).
        labelArea1:Text = "Bar Chart".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar0 AS FarPoint.Win.Chart.LabelArea EXTENT 1 NO-UNDO.
        arrayvar0[1] = labelArea1.
        THIS-OBJECT:fpChart1:Model:LabelAreas:AddRange(arrayvar0).
        legendArea1:AlignmentX = Progress.Util.CastUtil:ToSingle(1).
        legendArea1:AlignmentY = Progress.Util.CastUtil:ToSingle(0.5).
        legendArea1:Location = NEW System.Drawing.PointF(Progress.Util.CastUtil:ToSingle(0.98), Progress.Util.CastUtil:ToSingle(0.5)).
        legendArea1:Padding = NEW FarPoint.Win.Chart.PaddingF(Progress.Util.CastUtil:ToSingle(3), Progress.Util.CastUtil:ToSingle(3), Progress.Util.CastUtil:ToSingle(3), Progress.Util.CastUtil:ToSingle(3)).
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar1 AS FarPoint.Win.Chart.LegendArea EXTENT 1 NO-UNDO.
        arrayvar1[1] = legendArea1.
        THIS-OBJECT:fpChart1:Model:LegendAreas:AddRange(arrayvar1).
        wall1:Visible = TRUE.
        yPlotArea1:BackWall = wall1.
        wall2:Visible = TRUE.
        yPlotArea1:BottomWall = wall2.
        yPlotArea1:Elevation = Progress.Util.CastUtil:ToSingle(15).
        yPlotArea1:GlobalAmbientLight = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(50)), System.Convert:ToInt32(System.Convert:ToByte(50)), System.Convert:ToInt32(System.Convert:ToByte(50))).
        directionalLight1:AmbientColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(128)), System.Convert:ToInt32(System.Convert:ToByte(128)), System.Convert:ToInt32(System.Convert:ToByte(128))).
        directionalLight1:DiffuseColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(128)), System.Convert:ToInt32(System.Convert:ToByte(128)), System.Convert:ToInt32(System.Convert:ToByte(128))).
        directionalLight1:DirectionX = Progress.Util.CastUtil:ToSingle(10).
        directionalLight1:DirectionY = Progress.Util.CastUtil:ToSingle(20).
        directionalLight1:DirectionZ = Progress.Util.CastUtil:ToSingle(30).
        directionalLight1:SpecularColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(0)), System.Convert:ToInt32(System.Convert:ToByte(0)), System.Convert:ToInt32(System.Convert:ToByte(0))).
        yPlotArea1:Lights:Clear().
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar2 AS FarPoint.Win.Chart.Light EXTENT 1 NO-UNDO.
        arrayvar2[1] = directionalLight1.
        yPlotArea1:Lights:AddRange(arrayvar2).
        yPlotArea1:Location = NEW System.Drawing.PointF(Progress.Util.CastUtil:ToSingle(0.1932433), Progress.Util.CastUtil:ToSingle(0.2017794)).
        yPlotArea1:Projection = NEW FarPoint.Win.Chart.OrthogonalProjection().
        yPlotArea1:Rotation = Progress.Util.CastUtil:ToSingle(-10).
        barSeries1:SeriesName = "series".
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar3 AS FarPoint.Win.Chart.Series EXTENT 1 NO-UNDO.
        arrayvar3[1] = barSeries1.
        yPlotArea1:Series:AddRange(arrayvar3).
        wall3:Visible = TRUE.
        yPlotArea1:SideWall = wall3.
        yPlotArea1:Size = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(0.6), Progress.Util.CastUtil:ToSingle(0.6)).
        yPlotArea1:XAxis = indexAxis1.
        yPlotArea1:YAxes:Clear().
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar4 AS FarPoint.Win.Chart.ValueAxis EXTENT 1 NO-UNDO.
        arrayvar4[1] = valueAxis1.
        yPlotArea1:YAxes:AddRange(arrayvar4).
        yPlotArea1:ZAxis = indexAxis2.
        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
        DEFINE VARIABLE arrayvar5 AS FarPoint.Win.Chart.PlotArea EXTENT 1 NO-UNDO.
        arrayvar5[1] = yPlotArea1.
        THIS-OBJECT:fpChart1:Model:PlotAreas:AddRange(arrayvar5).
        THIS-OBJECT:fpChart1:Name = "fpChart1".
        THIS-OBJECT:fpChart1:Size = NEW System.Drawing.Size(268, 190).
        THIS-OBJECT:fpChart1:TabIndex = 0.
        THIS-OBJECT:fpChart1:Text = "fpChart1".
        /*  */
        /* FpChart */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(292, 266).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:fpChart1).
        THIS-OBJECT:Name = "FpChart".
        THIS-OBJECT:Text = "FpChart".
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC FpChart ( ):

	END DESTRUCTOR.

END CLASS.