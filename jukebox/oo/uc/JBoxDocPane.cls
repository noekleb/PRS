 
 /*------------------------------------------------------------------------
    File        : JBoxDocPane
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Fri Sep 26 22:02:43 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.
USING uc.* FROM PROPATH.

CLASS uc.JBoxDocPane INHERITS uc.JBoxUCbase: 
        
/*    DEFINE PRIVATE VARIABLE dockableWindow1 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.*/
/*    DEFINE PRIVATE VARIABLE dockableWindow2 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.*/
/*    DEFINE PRIVATE VARIABLE dockableWindow4 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.*/
/*    DEFINE PRIVATE VARIABLE dockableWindow3 AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.*/
    DEFINE PRIVATE VARIABLE m_JBoxDocPaneUnpinnedTabAreaTop AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_JBoxDocPaneUnpinnedTabAreaRight AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_JBoxDocPaneUnpinnedTabAreaLeft AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_JBoxDocPaneUnpinnedTabAreaBottom AS Infragistics.Win.UltraWinDock.UnpinnedTabArea NO-UNDO.
    DEFINE PRIVATE VARIABLE m_JBoxDocPaneAutoHideControl AS Infragistics.Win.UltraWinDock.AutoHideControl NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraDockManager1 AS Infragistics.Win.UltraWinDock.UltraDockManager NO-UNDO.
/*    DEFINE PRIVATE VARIABLE windowContainer2 AS Progress.Windows.WindowContainer NO-UNDO.                 */
/*    DEFINE PRIVATE VARIABLE windowContainer1 AS Progress.Windows.WindowContainer NO-UNDO.                 */
/*    DEFINE PRIVATE VARIABLE windowContainer4 AS Progress.Windows.WindowContainer NO-UNDO.                 */
/*    DEFINE PRIVATE VARIABLE windowContainer3 AS Progress.Windows.WindowContainer NO-UNDO.                 */
/*    DEFINE PRIVATE VARIABLE windowDockingArea1 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.*/
/*    DEFINE PRIVATE VARIABLE windowDockingArea2 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.*/
/*    DEFINE PRIVATE VARIABLE windowDockingArea3 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.*/

        
    CONSTRUCTOR PUBLIC JBoxDocPane (INPUT ihWinSuper    AS HANDLE, /* Window super proc - unknown if object created from window */
                                     INPUT ihWinProc     AS HANDLE,
                                     INPUT ihPlaceHolder AS HANDLE, /* Design widget (rectangle, field, etc) */
                                     INPUT iiDeltaWidth  AS INTEGER):
      
      SUPER(ihWinSuper,ihWinProc,ihPlaceHolder,iiDeltaWidth).
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
    END CONSTRUCTOR.
  
    CONSTRUCTOR PUBLIC JBoxDocPane ( INPUT ihWinProc     AS HANDLE,
                                     INPUT ihPlaceHolder AS HANDLE ): /* Design widget (rectangle, field, etc) */
                                   
      SUPER(?,ihWinProc,ihPlaceHolder,0).
    END CONSTRUCTOR.

    METHOD PUBLIC OVERRIDE VOID InitializeComponent(  ):
        
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
        appearance1 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("uc.JBoxDocPane").
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance2 AS Infragistics.Win.Appearance NO-UNDO.
        appearance2 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
        DEFINE VARIABLE appearance3 AS Infragistics.Win.Appearance NO-UNDO.
        appearance3 = NEW Infragistics.Win.Appearance().
        @VisualDesigner.FormMember (NeedsInitialize="true").
/*        DEFINE VARIABLE dockAreaPane1 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.                                                                                            */
/*        dockAreaPane1 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedTop, NEW System.Guid("31627eaa-6cdc-47fd-b1bf-d80bfa921c7a")).*/
/*        @VisualDesigner.FormMember (NeedsInitialize="true").                                                                                                                            */
/*        DEFINE VARIABLE dockAreaPane2 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.                                                                                            */
/*        dockAreaPane2 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:Floating, NEW System.Guid("4d153ed0-7171-4d50-acb5-1bd654112b58")). */
/*        @VisualDesigner.FormMember (NeedsInitialize="true").                                                                                                                                                                                                            */
/*        DEFINE VARIABLE dockableControlPane1 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.                                                                                                                                                              */
/*        dockableControlPane1 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("3838982a-49f6-4b4b-abb7-4c468190283b"), NEW System.Guid("4d153ed0-7171-4d50-acb5-1bd654112b58"), -1, NEW System.Guid("31627eaa-6cdc-47fd-b1bf-d80bfa921c7a"), 0). */
/*        @VisualDesigner.FormMember (NeedsInitialize="true").                                                                                                                                                                                                            */
/*        DEFINE VARIABLE dockableControlPane2 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.                                                                                                                                                              */
/*        dockableControlPane2 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("1c312268-362f-4627-a5ff-df8c02b06d71"), NEW System.Guid("4d153ed0-7171-4d50-acb5-1bd654112b58"), -1, NEW System.Guid("31627eaa-6cdc-47fd-b1bf-d80bfa921c7a"), 1). */
/*        @VisualDesigner.FormMember (NeedsInitialize="true").                                                                                                                                                                                                            */
/*        DEFINE VARIABLE dockAreaPane3 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.                                                                                                                                                                            */
/*        dockAreaPane3 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedTop, NEW System.Guid("32b6ab4a-4707-4aa4-bb6f-3e0898c73825")).                                                                                */
/*        @VisualDesigner.FormMember (NeedsInitialize="true").                                                                                                                                                                                                            */
/*        DEFINE VARIABLE dockableControlPane3 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.                                                                                                                                                              */
/*        dockableControlPane3 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("5015122f-e527-4c05-91be-a0cfe934794d"), NEW System.Guid("00000000-0000-0000-0000-000000000000"), -1, NEW System.Guid("32b6ab4a-4707-4aa4-bb6f-3e0898c73825"), -1).*/
/*        @VisualDesigner.FormMember (NeedsInitialize="true").                                                                                                                                                                                                            */
/*        DEFINE VARIABLE dockableControlPane4 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.                                                                                                                                                              */
/*        dockableControlPane4 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("768172e4-bc81-4525-8f33-1bf6df1a9f5c"), NEW System.Guid("00000000-0000-0000-0000-000000000000"), -1, NEW System.Guid("32b6ab4a-4707-4aa4-bb6f-3e0898c73825"), -1).*/
        THIS-OBJECT:ultraDockManager1 = NEW Infragistics.Win.UltraWinDock.UltraDockManager(THIS-OBJECT:components).
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaLeft = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaRight = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaTop = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaBottom = NEW Infragistics.Win.UltraWinDock.UnpinnedTabArea().
        THIS-OBJECT:m_JBoxDocPaneAutoHideControl = NEW Infragistics.Win.UltraWinDock.AutoHideControl().
/*        THIS-OBJECT:windowContainer1 = NEW Progress.Windows.WindowContainer().*/
/*        THIS-OBJECT:windowContainer2 = NEW Progress.Windows.WindowContainer().*/
/*        THIS-OBJECT:windowDockingArea1 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().*/
/*        THIS-OBJECT:dockableWindow1 = NEW Infragistics.Win.UltraWinDock.DockableWindow().*/
/*        THIS-OBJECT:dockableWindow2 = NEW Infragistics.Win.UltraWinDock.DockableWindow().*/
/*        THIS-OBJECT:windowDockingArea2 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().*/
/*        THIS-OBJECT:windowContainer3 = NEW Progress.Windows.WindowContainer().*/
/*        THIS-OBJECT:windowContainer4 = NEW Progress.Windows.WindowContainer().*/
/*        THIS-OBJECT:windowDockingArea3 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().*/
/*        THIS-OBJECT:dockableWindow3 = NEW Infragistics.Win.UltraWinDock.DockableWindow().*/
/*        THIS-OBJECT:dockableWindow4 = NEW Infragistics.Win.UltraWinDock.DockableWindow().*/
        CAST(THIS-OBJECT:ultraDockManager1, System.ComponentModel.ISupportInitialize):BeginInit().
/*        THIS-OBJECT:dockableWindow1:SuspendLayout().*/
/*        THIS-OBJECT:dockableWindow2:SuspendLayout().*/
/*        THIS-OBJECT:windowDockingArea2:SuspendLayout().*/
/*        THIS-OBJECT:windowDockingArea3:SuspendLayout().*/
/*        THIS-OBJECT:dockableWindow3:SuspendLayout().*/
/*        THIS-OBJECT:dockableWindow4:SuspendLayout().*/
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraDockManager1 */
        /*  */
        THIS-OBJECT:ultraDockManager1:CaptionStyle = Infragistics.Win.UltraWinDock.CaptionStyle:VisualStudio2008.
        THIS-OBJECT:ultraDockManager1:CompressUnpinnedTabs = FALSE.
        THIS-OBJECT:ultraDockManager1:DefaultGroupSettings:TabLocation = Infragistics.Win.UltraWinDock.Location:Top.
        THIS-OBJECT:ultraDockManager1:DefaultGroupSettings:TabStyle = Infragistics.Win.UltraWinTabs.TabStyle:Office2010Ribbon.
        appearance1:Image = CAST(resources:GetObject("appearance1.Image"), System.Object).
        THIS-OBJECT:ultraDockManager1:DefaultPaneSettings:ActiveCaptionAppearance = appearance1.
        appearance2:Image = CAST(resources:GetObject("appearance2.Image"), System.Object).
        THIS-OBJECT:ultraDockManager1:DefaultPaneSettings:ActivePaneAppearance = appearance2.
        appearance3:Image = CAST(resources:GetObject("appearance3.Image"), System.Object).
        THIS-OBJECT:ultraDockManager1:DefaultPaneSettings:ActiveTabAppearance = appearance3.
        THIS-OBJECT:ultraDockManager1:DefaultPaneSettings:AllowDockAsTab = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraDockManager1:DefaultPaneSettings:AllowDragging = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraDockManager1:DefaultPaneSettings:AllowFloating = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraDockManager1:DefaultPaneSettings:AllowPin = Infragistics.Win.DefaultableBoolean:False.
        THIS-OBJECT:ultraDockManager1:DefaultPaneSettings:CanDisplayAsMdiChild = Infragistics.Win.DefaultableBoolean:True.
        THIS-OBJECT:ultraDockManager1:DefaultPaneSettings:ShowCaption = Infragistics.Win.DefaultableBoolean:False.
/*        dockAreaPane1:ChildPaneStyle = Infragistics.Win.UltraWinDock.ChildPaneStyle:TabGroup.*/
/*        dockAreaPane1:DockedBefore = NEW System.Guid("4d153ed0-7171-4d50-acb5-1bd654112b58").*/
/*        dockAreaPane1:Size = NEW System.Drawing.Size(450, 95).                               */
/*        dockAreaPane2:ChildPaneStyle = Infragistics.Win.UltraWinDock.ChildPaneStyle:TabGroup.*/
/*        dockAreaPane2:FloatingLocation = NEW System.Drawing.Point(445, 194).                 */
/*        dockableControlPane1:Closed = TRUE.                                                  */
/*        dockableControlPane1:Control = THIS-OBJECT:windowContainer1.*/
/*        dockableControlPane1:OriginalControlBounds = NEW System.Drawing.Rectangle(47, 0, 185, 173).*/
/*        dockableControlPane1:Size = NEW System.Drawing.Size(100, 100).                             */
/*        dockableControlPane1:Text = "windowContainer1".                                            */
/*        dockableControlPane2:Closed = TRUE.                                                        */
/*        dockableControlPane2:Control = THIS-OBJECT:windowContainer2.*/
/*        dockableControlPane2:OriginalControlBounds = NEW System.Drawing.Rectangle(131, 0, 185, 173). */
/*        dockableControlPane2:Size = NEW System.Drawing.Size(100, 100).                               */
/*        dockableControlPane2:Text = "windowContainer2".                                              */
/*        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").                */
/*        DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 2 NO-UNDO.*/
/*        arrayvar0[1] = dockableControlPane1.                                                         */
/*        arrayvar0[2] = dockableControlPane2.                                                         */
/*        dockAreaPane2:Panes:AddRange(arrayvar0).               */
/*        dockAreaPane2:Size = NEW System.Drawing.Size(360, 244).*/
/*        dockAreaPane3:ChildPaneStyle = Infragistics.Win.UltraWinDock.ChildPaneStyle:TabGroup.*/
/*        dockableControlPane3:Control = THIS-OBJECT:windowContainer3.*/
/*        dockableControlPane3:OriginalControlBounds = NEW System.Drawing.Rectangle(53, 39, 100, 100).*/
/*        dockableControlPane3:Size = NEW System.Drawing.Size(100, 100).                              */
/*        dockableControlPane3:Text = "windowContainer3".                                             */
/*        dockableControlPane4:Control = THIS-OBJECT:windowContainer4.*/
/*        dockableControlPane4:OriginalControlBounds = NEW System.Drawing.Rectangle(196, 39, 100, 100).*/
/*        dockableControlPane4:Size = NEW System.Drawing.Size(100, 100).                               */
/*        dockableControlPane4:Text = "windowContainer4".                                              */
/*        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").                */
/*        DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 2 NO-UNDO.*/
/*        arrayvar1[1] = dockableControlPane3.                                                         */
/*        arrayvar1[2] = dockableControlPane4.                                                         */
/*        dockAreaPane3:Panes:AddRange(arrayvar1).                                     */
/*        dockAreaPane3:Size = NEW System.Drawing.Size(450, 330).                      */
/*        dockAreaPane3:UnfilledSize = NEW System.Drawing.Size(450, 100).              */
/*        @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").*/
/*        DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinDock.DockAreaPane EXTENT 3 NO-UNDO.*/
/*        arrayvar2[1] = dockAreaPane1.                                                            */
/*        arrayvar2[2] = dockAreaPane2.                                                            */
/*        arrayvar2[3] = dockAreaPane3.                                                            */
/*        THIS-OBJECT:ultraDockManager1:DockAreas:AddRange(arrayvar2).                             */
        THIS-OBJECT:ultraDockManager1:HostControl = THIS-OBJECT.
        THIS-OBJECT:ultraDockManager1:HotTracking = FALSE.
        THIS-OBJECT:ultraDockManager1:LayoutStyle = Infragistics.Win.UltraWinDock.DockAreaLayoutStyle:FillContainer.
        THIS-OBJECT:ultraDockManager1:ShowPinButton = FALSE.
        /*  */
        /* _JBoxDocPaneUnpinnedTabAreaLeft */
        /*  */
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaLeft:Dock = System.Windows.Forms.DockStyle:Left.
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaLeft:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaLeft:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaLeft:Name = "_JBoxDocPaneUnpinnedTabAreaLeft".
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaLeft:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaLeft:Size = NEW System.Drawing.Size(0, 330).
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaLeft:TabIndex = 0.
        /*  */
        /* _JBoxDocPaneUnpinnedTabAreaRight */
        /*  */
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaRight:Dock = System.Windows.Forms.DockStyle:Right.
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaRight:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaRight:Location = NEW System.Drawing.Point(450, 0).
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaRight:Name = "_JBoxDocPaneUnpinnedTabAreaRight".
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaRight:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaRight:Size = NEW System.Drawing.Size(0, 330).
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaRight:TabIndex = 1.
        /*  */
        /* _JBoxDocPaneUnpinnedTabAreaTop */
        /*  */
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaTop:Dock = System.Windows.Forms.DockStyle:Top.
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaTop:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaTop:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaTop:Name = "_JBoxDocPaneUnpinnedTabAreaTop".
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaTop:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaTop:Size = NEW System.Drawing.Size(450, 0).
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaTop:TabIndex = 2.
        /*  */
        /* _JBoxDocPaneUnpinnedTabAreaBottom */
        /*  */
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaBottom:Dock = System.Windows.Forms.DockStyle:Bottom.
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaBottom:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaBottom:Location = NEW System.Drawing.Point(0, 330).
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaBottom:Name = "_JBoxDocPaneUnpinnedTabAreaBottom".
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaBottom:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaBottom:Size = NEW System.Drawing.Size(450, 0).
        THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaBottom:TabIndex = 3.
        /*  */
        /* _JBoxDocPaneAutoHideControl */
        /*  */
        THIS-OBJECT:m_JBoxDocPaneAutoHideControl:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
        THIS-OBJECT:m_JBoxDocPaneAutoHideControl:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:m_JBoxDocPaneAutoHideControl:Name = "_JBoxDocPaneAutoHideControl".
        THIS-OBJECT:m_JBoxDocPaneAutoHideControl:Owner = THIS-OBJECT:ultraDockManager1.
        THIS-OBJECT:m_JBoxDocPaneAutoHideControl:Size = NEW System.Drawing.Size(0, 330).
        THIS-OBJECT:m_JBoxDocPaneAutoHideControl:TabIndex = 4.
        /*  */
        /* windowContainer1 */
        /*  */
/*        THIS-OBJECT:windowContainer1:Location = NEW System.Drawing.Point(0, 0).*/
/*        THIS-OBJECT:windowContainer1:Name = "windowContainer1".                */
/*        THIS-OBJECT:windowContainer1:Size = NEW System.Drawing.Size(360, 244). */
/*        THIS-OBJECT:windowContainer1:TabIndex = 8.                             */
        /*  */
        /* windowContainer2 */
        /*  */
/*        THIS-OBJECT:windowContainer2:Location = NEW System.Drawing.Point(0, 0).*/
/*        THIS-OBJECT:windowContainer2:Name = "windowContainer2".                */
/*        THIS-OBJECT:windowContainer2:Size = NEW System.Drawing.Size(360, 224). */
/*        THIS-OBJECT:windowContainer2:TabIndex = 10.                            */
        /*  */
        /* windowDockingArea1 */
        /*  */
/*        THIS-OBJECT:windowDockingArea1:Dock = System.Windows.Forms.DockStyle:Top.                                                                                                                                                   */
/*        THIS-OBJECT:windowDockingArea1:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).*/
/*        THIS-OBJECT:windowDockingArea1:Location = NEW System.Drawing.Point(0, 0).                                                                                                                                                   */
/*        THIS-OBJECT:windowDockingArea1:Name = "windowDockingArea1".                                                                                                                                                                 */
/*        THIS-OBJECT:windowDockingArea1:Owner = THIS-OBJECT:ultraDockManager1.                                                                                                                                                       */
/*        THIS-OBJECT:windowDockingArea1:Size = NEW System.Drawing.Size(450, 100).                                                                                                                                                    */
/*        THIS-OBJECT:windowDockingArea1:TabIndex = 11.                                                                                                                                                                               */
        /*  */
        /* dockableWindow1 */
        /*  */
/*        THIS-OBJECT:dockableWindow1:Controls:Add(THIS-OBJECT:windowContainer1).    */
/*        THIS-OBJECT:dockableWindow1:Location = NEW System.Drawing.Point(-10000, 0).*/
/*        THIS-OBJECT:dockableWindow1:Name = "dockableWindow1".                      */
/*        THIS-OBJECT:dockableWindow1:Owner = THIS-OBJECT:ultraDockManager1.         */
/*        THIS-OBJECT:dockableWindow1:Size = NEW System.Drawing.Size(360, 244).      */
/*        THIS-OBJECT:dockableWindow1:TabIndex = 15.                                 */
        /*  */
        /* dockableWindow2 */
        /*  */
/*        THIS-OBJECT:dockableWindow2:Controls:Add(THIS-OBJECT:windowContainer2).    */
/*        THIS-OBJECT:dockableWindow2:Location = NEW System.Drawing.Point(-10000, 0).*/
/*        THIS-OBJECT:dockableWindow2:Name = "dockableWindow2".                      */
/*        THIS-OBJECT:dockableWindow2:Owner = THIS-OBJECT:ultraDockManager1.         */
/*        THIS-OBJECT:dockableWindow2:Size = NEW System.Drawing.Size(360, 224).      */
/*        THIS-OBJECT:dockableWindow2:TabIndex = 16.                                 */
        /*  */
        /* windowDockingArea2 */
        /*  */
/*        THIS-OBJECT:windowDockingArea2:Controls:Add(THIS-OBJECT:dockableWindow2).                                                                                                                                                   */
/*        THIS-OBJECT:windowDockingArea2:Controls:Add(THIS-OBJECT:dockableWindow1).                                                                                                                                                   */
/*        THIS-OBJECT:windowDockingArea2:Dock = System.Windows.Forms.DockStyle:Fill.                                                                                                                                                  */
/*        THIS-OBJECT:windowDockingArea2:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).*/
/*        THIS-OBJECT:windowDockingArea2:Location = NEW System.Drawing.Point(8, 8).                                                                                                                                                   */
/*        THIS-OBJECT:windowDockingArea2:Name = "windowDockingArea2".                                                                                                                                                                 */
/*        THIS-OBJECT:windowDockingArea2:Owner = THIS-OBJECT:ultraDockManager1.                                                                                                                                                       */
/*        THIS-OBJECT:windowDockingArea2:Size = NEW System.Drawing.Size(360, 244).                                                                                                                                                    */
/*        THIS-OBJECT:windowDockingArea2:TabIndex = 0.                                                                                                                                                                                */
        /*  */
        /* windowContainer3 */
        /*  */
/*        THIS-OBJECT:windowContainer3:Location = NEW System.Drawing.Point(0, 2).*/
/*        THIS-OBJECT:windowContainer3:Name = "windowContainer3".                */
/*        THIS-OBJECT:windowContainer3:Size = NEW System.Drawing.Size(450, 306). */
/*        THIS-OBJECT:windowContainer3:TabIndex = 12.                            */
        /*  */
        /* windowContainer4 */
        /*  */
/*        THIS-OBJECT:windowContainer4:Location = NEW System.Drawing.Point(0, 2).*/
/*        THIS-OBJECT:windowContainer4:Name = "windowContainer4".                */
/*        THIS-OBJECT:windowContainer4:Size = NEW System.Drawing.Size(450, 306). */
/*        THIS-OBJECT:windowContainer4:TabIndex = 13.                            */
        /*  */
        /* windowDockingArea3 */
        /*  */
/*        THIS-OBJECT:windowDockingArea3:Controls:Add(THIS-OBJECT:dockableWindow3).                                                                                                                                                   */
/*        THIS-OBJECT:windowDockingArea3:Controls:Add(THIS-OBJECT:dockableWindow4).                                                                                                                                                   */
/*        THIS-OBJECT:windowDockingArea3:Dock = System.Windows.Forms.DockStyle:Fill.                                                                                                                                                  */
/*        THIS-OBJECT:windowDockingArea3:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(8.25), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).*/
/*        THIS-OBJECT:windowDockingArea3:Location = NEW System.Drawing.Point(0, 0).                                                                                                                                                   */
/*        THIS-OBJECT:windowDockingArea3:Name = "windowDockingArea3".                                                                                                                                                                 */
/*        THIS-OBJECT:windowDockingArea3:Owner = THIS-OBJECT:ultraDockManager1.                                                                                                                                                       */
/*        THIS-OBJECT:windowDockingArea3:Size = NEW System.Drawing.Size(450, 330).                                                                                                                                                    */
/*        THIS-OBJECT:windowDockingArea3:TabIndex = 14.                                                                                                                                                                               */
        /*  */
        /* dockableWindow3 */
        /*  */
/*        THIS-OBJECT:dockableWindow3:Controls:Add(THIS-OBJECT:windowContainer3).*/
/*        THIS-OBJECT:dockableWindow3:Location = NEW System.Drawing.Point(0, 22).*/
/*        THIS-OBJECT:dockableWindow3:Name = "dockableWindow3".                  */
/*        THIS-OBJECT:dockableWindow3:Owner = THIS-OBJECT:ultraDockManager1.     */
/*        THIS-OBJECT:dockableWindow3:Size = NEW System.Drawing.Size(450, 308).  */
/*        THIS-OBJECT:dockableWindow3:TabIndex = 17.                             */
        /*  */
        /* dockableWindow4 */
        /*  */
/*        THIS-OBJECT:dockableWindow4:Controls:Add(THIS-OBJECT:windowContainer4).     */
/*        THIS-OBJECT:dockableWindow4:Location = NEW System.Drawing.Point(-10000, 22).*/
/*        THIS-OBJECT:dockableWindow4:Name = "dockableWindow4".                       */
/*        THIS-OBJECT:dockableWindow4:Owner = THIS-OBJECT:ultraDockManager1.          */
/*        THIS-OBJECT:dockableWindow4:Size = NEW System.Drawing.Size(450, 308).       */
/*        THIS-OBJECT:dockableWindow4:TabIndex = 18.                                  */
        /*  */
        /* JBoxDocPane */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(450, 330).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_JBoxDocPaneAutoHideControl).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaLeft).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaTop).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaBottom).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:m_JBoxDocPaneUnpinnedTabAreaRight).
/*        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea1).*/
/*        THIS-OBJECT:Controls:Add(THIS-OBJECT:windowDockingArea3).*/
        THIS-OBJECT:Name = "JBoxDocPane".
        THIS-OBJECT:Text = "JBoxDocPane".
        CAST(THIS-OBJECT:ultraDockManager1, System.ComponentModel.ISupportInitialize):EndInit().
/*        THIS-OBJECT:dockableWindow1:ResumeLayout(FALSE).*/
/*        THIS-OBJECT:dockableWindow2:ResumeLayout(FALSE).*/
/*        THIS-OBJECT:windowDockingArea2:ResumeLayout(FALSE).*/
/*        THIS-OBJECT:windowDockingArea3:ResumeLayout(FALSE).*/
/*        THIS-OBJECT:dockableWindow3:ResumeLayout(FALSE).   */
/*        THIS-OBJECT:dockableWindow4:ResumeLayout(FALSE).   */
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    METHOD PUBLIC Infragistics.Win.UltraWinDock.DockableWindow getDotNetWidget():
/*      RETURN dockableWindow1.*/
    END METHOD.
    
    METHOD PUBLIC CHARACTER getDotNetWidgetType():
/*      RETURN STRING(dockableWindow1:GetType()).*/
    END METHOD.

    METHOD PUBLIC HANDLE setTabProc (INPUT icTabProc AS CHAR):
      DEF VAR hProc   AS HANDLE NO-UNDO.
      DEF VAR windowContainer1 AS Progress.Windows.WindowContainer NO-UNDO.
      DEF VAR dockableWindow1  AS Infragistics.Win.UltraWinDock.DockableWindow NO-UNDO.
      DEF VAR windowDockingArea1 AS Infragistics.Win.UltraWinDock.WindowDockingArea NO-UNDO.
      DEF VAR dockAreaPane1 AS Infragistics.Win.UltraWinDock.DockAreaPane NO-UNDO.
      DEF VAR arrayvar2 AS Infragistics.Win.UltraWinDock.DockAreaPane EXTENT 1 NO-UNDO.
      DEF VAR arrayvar1 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 1 NO-UNDO.
      DEF VAR dockableControlPane1 AS Infragistics.Win.UltraWinDock.DockableControlPane NO-UNDO.
      DEF VAR arrayvar0 AS Infragistics.Win.UltraWinDock.DockablePaneBase EXTENT 1 NO-UNDO.
 
 
      dockAreaPane1 = NEW Infragistics.Win.UltraWinDock.DockAreaPane(Infragistics.Win.UltraWinDock.DockedLocation:DockedTop, NEW System.Guid("31627eaa-6cdc-47fd-b1bf-d80bfa921c7a")).
      dockAreaPane1:ChildPaneStyle = Infragistics.Win.UltraWinDock.ChildPaneStyle:TabGroup.
      dockAreaPane1:DockedBefore = NEW System.Guid("4d153ed0-7171-4d50-acb5-1bd654112b58").
      dockAreaPane1:Size = NEW System.Drawing.Size(450, 95).
      dockAreaPane1:UnfilledSize = NEW System.Drawing.Size(450, 100).
      arrayvar2[1] = dockAreaPane1.
      THIS-OBJECT:ultraDockManager1:DockAreas:AddRange(arrayvar2).
      
      
      windowContainer1 = NEW Progress.Windows.WindowContainer().
      windowDockingArea1 = NEW Infragistics.Win.UltraWinDock.WindowDockingArea().
      
      dockableControlPane1:OriginalControlBounds = NEW System.Drawing.Rectangle(47, 0, 185, 173).
      dockableControlPane1:Size = NEW System.Drawing.Size(100, 100).
      dockableControlPane1:Text = "windowContainer1".
      arrayvar1[1] = dockableControlPane1.

      dockableWindow1 = NEW Infragistics.Win.UltraWinDock.DockableWindow().
      dockableWindow1:Controls:Add(windowContainer1).
      dockableWindow1:Location = NEW System.Drawing.Point(-10000, 0).
      dockableWindow1:Name = "dockableWindow1".
      dockableWindow1:Owner = THIS-OBJECT:ultraDockManager1.
      dockableWindow1:Size = NEW System.Drawing.Size(360, 244).
      dockableWindow1:TabIndex = 15.

      dockableControlPane1 = NEW Infragistics.Win.UltraWinDock.DockableControlPane(NEW System.Guid("3838982a-49f6-4b4b-abb7-4c468190283b"), NEW System.Guid("4d153ed0-7171-4d50-acb5-1bd654112b58"), -1, NEW System.Guid("31627eaa-6cdc-47fd-b1bf-d80bfa921c7a"), 0).
      dockableControlPane1:OriginalControlBounds = NEW System.Drawing.Rectangle(131, 0, 185, 173).
      dockableControlPane1:Size = NEW System.Drawing.Size(100, 100).
      dockableControlPane1:Text = "Tab 1".
      arrayvar0[1] = dockableControlPane1.

 
      windowDockingArea1:Controls:Add(dockableWindow1).

      THIS-OBJECT:Controls:Add(windowDockingArea1).
  
/*      RUN samples\custview.w PERSIST SET hProc.*/
/*      run samples\oo\c-win11.w persist set hProc.*/
/*      windowContainer3:EmbeddedWindow = hProc:CURRENT-WINDOW NO-ERROR.*/
      IF ERROR-STATUS:GET-MESSAGE(1) NE "" THEN DO:
        
/*        ASSIGN hProc:CURRENT-WINDOW:FIRST-CHILD:PARENT-IDE = ?*/
/*               .                                              */
      END.   
     
/*      RUN InitializeObject IN hProc.*/
      RETURN hProc.  
        
    END METHOD.    

    DESTRUCTOR PUBLIC JBoxDocPane ( ):

    END DESTRUCTOR.

END CLASS.