 
 /*------------------------------------------------------------------------
    File        : WebBrowser
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : co
    Created     : Mon Oct 01 21:06:33 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.
USING uc.* FROM PROPATH.


ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS uc.JboxInternetExplorer INHERITS uc.JBoxUCbase: 


	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE webBrowser1 AS System.Windows.Forms.WebBrowser NO-UNDO.

  DEFINE PUBLIC PROPERTY ScriptErrorsSuppressed AS LOG
    GET ():
        RETURN THIS-OBJECT:webBrowser1:ScriptErrorsSuppressed.
    END GET.
    SET (input bValue AS LOGICAL):
        THIS-OBJECT:webBrowser1:ScriptErrorsSuppressed = bValue.
    END SET.         
    		
  CONSTRUCTOR PUBLIC JboxInternetExplorer (  INPUT ihWinSuper    AS HANDLE, /* Window super proc - unknown if object created from window */
                                    INPUT ihWinProc     AS HANDLE,
                                    INPUT ihPlaceHolder AS HANDLE, /* Design widget (rectangle, field, etc) */
                                    INPUT iiDeltaWidth  AS INTEGER):
     SUPER(ihWinSuper,ihWinProc,ihPlaceHolder,iiDeltaWidth).
   
  END CONSTRUCTOR.
  
  
  CONSTRUCTOR PUBLIC JboxInternetExplorer ( INPUT ihWinProc     AS HANDLE,
                                   INPUT ihPlaceHolder AS HANDLE ): /* Design widget (rectangle, field, etc) */
                                   
     SUPER(?,ihWinProc,ihPlaceHolder,0).
   
  END CONSTRUCTOR.


METHOD PUBLIC OVERRIDE VOID InitializeComponent(  ):
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:webBrowser1 = NEW System.Windows.Forms.WebBrowser().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* webBrowser1 */
        /*  */
        THIS-OBJECT:webBrowser1:Dock = System.Windows.Forms.DockStyle:Fill.
        THIS-OBJECT:webBrowser1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:webBrowser1:MinimumSize = NEW System.Drawing.Size(20, 20).
        THIS-OBJECT:webBrowser1:Name = "webBrowser1".
        THIS-OBJECT:webBrowser1:Size = NEW System.Drawing.Size(667, 329).
        THIS-OBJECT:webBrowser1:TabIndex = 0.
/*        THIS-OBJECT:webBrowser1:Url = NEW System.Uri("http:\\www.chemistry.no", System.UriKind:Absolute).*/
        /*
        THIS-OBJECT:webBrowser1:Url = NEW System.Uri("file:\\C:\tmp\test.swf", System.UriKind:Absolute).
        */
        THIS-OBJECT:webBrowser1:ScriptErrorsSuppressed     = TRUE.
        THIS-OBJECT:webBrowser1:WebBrowserShortcutsEnabled = FALSE.
        THIS-OBJECT:webBrowser1:DocumentCompleted:Subscribe(THIS-OBJECT:webBrowser1_DocumentCompleted).
     
     
        /*  */
        /* WebBrowser */
        /*  */
        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(667, 329).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:webBrowser1).
        THIS-OBJECT:Name = "WebBrowser".
        THIS-OBJECT:Text = "WebBrowser".
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
	END METHOD.


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	METHOD PRIVATE VOID webBrowser1_DocumentCompleted( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.WebBrowserDocumentCompletedEventArgs ):
/*        MESSAGE sender e:URL VIEW-AS ALERT-BOX.*/
        DYNAMIC-FUNCTION('WebDocumentLoaded') NO-ERROR. 
		RETURN.
	END METHOD.


	METHOD PUBLIC VOID setURI(INPUT ipcURI AS CHAR ):
        THIS-OBJECT:webBrowser1:Url = NEW System.Uri(ipcURI, System.UriKind:Absolute).
  END. 

/* Mandatory methods: */
  METHOD PUBLIC System.Windows.Forms.WebBrowser getDotNetWidget():
    RETURN webBrowser1.
  END METHOD.

  METHOD PUBLIC CHARACTER getDotNetWidgetType():
    RETURN STRING(webBrowser1:GetType()).
  END METHOD.

  METHOD PUBLIC CHARACTER DEFAULT():
    RETURN DYNAMIC-INVOKE(THIS-OBJECT,ENTRY(1,PROGRAM-NAME(2)," ")).
  END METHOD.


	DESTRUCTOR PUBLIC JboxInternetExplorer ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.

END CLASS.
