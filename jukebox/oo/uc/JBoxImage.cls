USING Progress.Lang.*.
USING Progress.Util.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.
USING System.TimeSpan.*.
USING Infragistics.Win.*. 

USING uc.* FROM PROPATH.
USING System.IO.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.


CLASS uc.JBoxImage INHERITS uc.JBoxUCbase: 
	  			
  DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PRIVATE VARIABLE pictureBox1 AS System.Windows.Forms.PictureBox NO-UNDO.
  DEFINE VARIABLE ParentProcedure AS HANDLE NO-UNDO. 
   

  CONSTRUCTOR PUBLIC JBoxImage (INPUT ihWinSuper    AS HANDLE, /* Window super proc - unknown if object created from window */
                                INPUT ihWinProc     AS HANDLE,
                                INPUT ihPlaceHolder AS HANDLE, /* Design widget (rectangle, field, etc) */
                                INPUT iiDeltaWidth  AS INTEGER):

     SUPER(ihWinSuper,ihWinProc,ihPlaceHolder,iiDeltaWidth).
     
     THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
  END CONSTRUCTOR.
  

  CONSTRUCTOR PUBLIC JBoxImage ( INPUT ihWinProc     AS HANDLE,
                                  INPUT ihPlaceHolder AS HANDLE,
                                  hQuery AS HANDLE
                                  ): /* Design widget (rectangle, field, etc) */
 
         SUPER(?,ihWinProc,ihPlaceHolder,0,hQuery,?).
 
         CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
  END CONSTRUCTOR.
  
  CONSTRUCTOR PUBLIC JBoxImage ( INPUT ihWinProc     AS HANDLE,
                                  INPUT ihPlaceHolder AS HANDLE,
                                  hQuery AS HANDLE,
                                  hPropertyTable AS HANDLE ): /* Design widget (rectangle, field, etc) */
 
         SUPER(?,ihWinProc,ihPlaceHolder,0,hQuery,hPropertyTable).
 
         CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
  END CONSTRUCTOR.
  
  
  CONSTRUCTOR PUBLIC JBoxImage ( INPUT ihWinProc     AS HANDLE,
                                      INPUT ihPlaceHolder AS HANDLE ): /* Design widget (rectangle, field, etc) */
                                   
    SUPER(?,ihWinProc,ihPlaceHolder,0).
  END CONSTRUCTOR.



  METHOD PUBLIC OVERRIDE VOID InitializeComponent(  ):
	    
        ParentProcedure = SOURCE-PROCEDURE:INSTANTIATING-PROCEDURE . 
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.

        resources = NEW Progress.Util.ResourceManager("src.test").
        THIS-OBJECT:pictureBox1 = NEW System.Windows.Forms.PictureBox().
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().

        /* default image 
        THIS-OBJECT:pictureBox1:Image = CAST(resources:GetObject("pictureBox1.Image"), System.Drawing.Image).
        THIS-OBJECT:pictureBox1:InitialImage = CAST(resources:GetObject("pictureBox1.InitialImage"), System.Drawing.Image).
        */
        THIS-OBJECT:AllowDrop = TRUE.
        THIS-OBJECT:pictureBox1:AllowDrop = TRUE.

        THIS-OBJECT:pictureBox1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:pictureBox1:Name = "pictureBox1".
        THIS-OBJECT:pictureBox1:Size = NEW System.Drawing.Size(hPlaceHolder:WIDTH-PIXELS, hPlaceHolder:HEIGHT-PIXELS).
        THIS-OBJECT:pictureBox1:SizeMode = System.Windows.Forms.PictureBoxSizeMode:StretchImage.
        THIS-OBJECT:pictureBox1:TabIndex = 0.
        THIS-OBJECT:pictureBox1:TabStop = FALSE.
        THIS-OBJECT:pictureBox1:Click:Subscribe(THIS-OBJECT:pictureBox1_Click).
        THIS-OBJECT:pictureBox1:DragDrop:Subscribe(THIS-OBJECT:pictureBox1_DragDrop).

        THIS-OBJECT:ClientSize = NEW System.Drawing.Size(342, 309).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:pictureBox1).
        THIS-OBJECT:Name = "Image".
        THIS-OBJECT:Text = "Image".
        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).

        SUPER:InitializeComponent().

        CAST(THIS-OBJECT:pictureBox1, System.ComponentModel.ISupportInitialize):EndInit().
           THIS-OBJECT:ResumeLayout(FALSE).
        
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
    END CATCH.
    

  END METHOD.
  
/*
using( FileStream stream = new FileStream( path, FileMode.Open, FileAccess.Read ) )

{
         image = Image.FromStream( stream );}

USING Progress.Lang.*.
using System.IO.*.
CLASS Test:
    DEFINE VARIABLE sourceFile AS CHARACTER.
    DEFINE VARIABLE sourceStream AS FileStream NO-UNDO.
    DEFINE VARIABLE byteBuffer AS "System.Byte[]" NO-UNDO. 
   
    constructor Test():           
        ASSIGN sourceFile = "C:\temp\fileOnDisk.txt".         
        ASSIGN byteBuffer = NEW "System.Byte[]"(2000).   
        sourceStream = NEW FileStream(sourceFile, FileMode:Open).         
        sourceStream:Read(byteBuffer, 1, byteBuffer:Length - 1). //fails here with ArgumentNullException: Buffer cannot be null       
        MESSAGE "ok" VIEW-AS ALERT-BOX.
    end constructor.
END CLASS.
 */


  METHOD PUBLIC VOID LoadFile( INPUT cFileName AS CHAR ):
            
        DEFINE VARIABLE sourceStream AS FileStream NO-UNDO.
         
        IF cFileName = ?  THEN
            THIS-OBJECT:pictureBox1:Image = ?.
        ELSE 
        DO:
            sourceStream = NEW FileStream(cFileName, FileMode:Open).  
            THIS-OBJECT:pictureBox1:Image = System.Drawing.Image:FromStream(sourceStream).
            /* get ridd of any file locks */ 
            sourceStream:CLOSE().
            sourceStream:dispose(). 
        END. 
            /*
            THIS-OBJECT:pictureBox1:IMAGE = ?.
            THIS-OBJECT:pictureBox1:Image = System.Drawing.Image:FromFile(cFileName) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
            DO:
                MESSAGE "loadfile Error" VIEW-AS ALERT-BOX. 
            END.
            */
        RETURN.

  END METHOD.


  METHOD PUBLIC VOID ShowPicture( INPUT cFileName AS CHAR ):
            
            THIS-OBJECT:pictureBox1:Image = System.Drawing.Image:FromFile(cFileName) no-error.   
        
        RETURN.

  END METHOD.


 METHOD PUBLIC VOID clear (  ):
            
        THIS-OBJECT:pictureBox1:IMAGE = ? . 
        RETURN.

  END METHOD.

 
  /* Mandatory methods: */

  METHOD PUBLIC System.Windows.Forms.PictureBox getDotNetWidget():
    RETURN PictureBox1.
  END METHOD.

  METHOD PUBLIC CHARACTER getDotNetWidgetType():
    RETURN STRING(PictureBox1:GetType()).
  END METHOD.


  /* Methods invoked from jukebox (via window super): */
  /* (Semi-)class specific methods - event handlers: */
  /* Class specific methods: */



	METHOD PRIVATE VOID pictureBox1_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
        RUN ImageClickEvent IN parentProcedure NO-ERROR.

		RETURN.
	END METHOD.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
  
	METHOD PRIVATE VOID pictureBox1_DragDrop( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):
		
		message "drag-drop" view-as alert-box. 
		RETURN.

	END METHOD.


     
END CLASS.
