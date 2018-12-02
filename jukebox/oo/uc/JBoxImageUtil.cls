
USING Progress.Lang.*.
USING Progress.Util.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.
USING System.TimeSpan.*.
USING uc.* FROM PROPATH.
USING System.IO.*.


ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS uc.JBoxImageUtil : 
	  			
  DEFINE PRIVATE VARIABLE components  AS System.ComponentModel.IContainer NO-UNDO.
  DEFINE PUBLIC  VARIABLE  ImageArray  AS "System.Drawing.Image" EXTENT 10 NO-UNDO. 
  DEFINE PUBLIC  VARIABLE FileNameIndex AS CHARACTER EXTENT 10 NO-UNDO. 
  DEFINE VARIABLE iImageCounter AS INTEGER INIT 1 NO-UNDO. 
  

  DEFINE PUBLIC VARIABLE Autoscale AS LOGICAL NO-UNDO. 
  DEFINE PUBLIC VARIABLE LOAD AS LOGICAL NO-UNDO. 
  DEFINE PUBLIC VARIABLE assignmode AS INTEGER NO-UNDO. 
  DEFINE PUBLIC VARIABLE ResizeMode AS INTEGER NO-UNDO. 
  DEFINE PUBLIC VARIABLE errornumber AS INTEGER NO-UNDO. 
  DEFINE PUBLIC VARIABLE OafFormat  AS CHAR NO-UNDO. 
  DEFINE PUBLIC VARIABLE YResolution AS INTEGER NO-UNDO. 
  DEFINE PUBLIC VARIABLE XResolution AS INTEGER NO-UNDO. 
  DEFINE VARIABLE ParentProcedure AS HANDLE NO-UNDO. 


  CONSTRUCTOR PUBLIC JBoxImageUtil ():

     
/*      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).  */

  END CONSTRUCTOR.
  

 


METHOD PUBLIC  VOID InitializeComponent(  ):
	    
        ParentProcedure = SOURCE-PROCEDURE:INSTANTIATING-PROCEDURE . 
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.

       resources = NEW Progress.Util.ResourceManager("src.test").
      
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
    END CATCH.
END METHOD.
  


METHOD PUBLIC VOID CLEAR( INPUT ipcList AS INTEGER ):
    
    RETURN.
END METHOD.



METHOD PUBLIC STATIC MEMPTR ConvertToMemptr( byteArray AS "System.Byte[]" ):

     DEFINE VARIABLE aMEMPTR AS MEMPTR NO-UNDO.
     DEFINE VARIABLE counter AS INTEGER NO-UNDO.
     DEFINE VARIABLE memStream AS System.IO.MemoryStream NO-UNDO.

     memStream = NEW System.IO.MemoryStream(byteArray).
     SET-SIZE(aMEMPTR) = memStream:Length.
     DO counter = 1 TO memStream:Length:
         put-byte(aMEMPTR,counter)= memStream:ReadByte().
     END.

     memStream:CLOSE().
     memStream:Dispose().
     DELETE OBJECT memStream.

     RETURN aMEMPTR.
END METHOD.


METHOD PUBLIC VOID  LoadImageFromFile(INPUT icFilename AS CHAR,INPUT iIndex AS INT ):

    DEFINE VARIABLE sourceStream     AS FileStream             NO-UNDO.
    DEFINE VARIABLE memStream        AS System.IO.MemoryStream NO-UNDO.
    DEFINE VARIABLE cTempFile        AS CHAR NO-UNDO.
    
    cTempFile = SESSION:TEMP-DIR + ENTRY(NUM-ENTRIES(icFileName,"\"),icFileName,"\").
    
    IF cTempFile NE icFilename THEN
      OS-COPY VALUE(SEARCH(icFileName)) VALUE(SESSION:TEMP-DIR + ENTRY(NUM-ENTRIES(icFileName,"\"),icFileName,"\")).
    
    sourceStream = NEW FileStream(SEARCH(cTempFile), FileMode:Open).  
    ImageArray[iIndex] = System.Drawing.Image:FromStream(sourceStream).
    filenameIndex[iIndex] = cTempFile.
     
    XResolution = imageArray[iIndex]:WIDTH.
    YResolution = imageArray[iIndex]:HEIGHT.

    sourceStream:CLOSE().
    sourceStream:dispose(). 
    DELETE OBJECT sourceStream.

END METHOD.


METHOD PUBLIC VOID  copyArrayImage(INPUT iiFromArrayIndex AS INT,
                                   INPUT iiToArrayIndex   AS INT ):

    ImageArray[iiToArrayIndex] = ImageArray[iiFromArrayIndex].
END METHOD.



METHOD PUBLIC VOID CopyAndResizeImage(
            INPUT iiFromArrayIndex AS INT,
            INPUT iiToArrayIndex   AS INT,       
            INPUT iSizeWidth AS INT, 
            INPUT iSizeHeight AS INT, 
            INPUT icFileType AS CHAR ):

       ImageArray[iiToArrayIndex] =  ImageArray[iiFromArrayIndex]:GetThumbnailImage(iSizeWidth,iSizeHeight,?,?).

     RETURN.
END METHOD.


METHOD PUBLIC VOID SaveImageToFile(INPUT iArray AS INTEGER ):
   
     XResolution = imageArray[iArray]:WIDTH .
     YResolution = imageArray[iArray]:HEIGHT .
     ImageArray[iArray]:SAVE(filenameIndex[iArray]). 
     
     RETURN.
END METHOD.


METHOD PUBLIC VOID PasteClipBoard(INPUT iArray AS INTEGER ):
     
     ImageArray[iArray] = "System.Windows.Forms.Clipboard":getImage().
     XResolution = imageArray[iArray]:WIDTH .
     YResolution = imageArray[iArray]:HEIGHT .
     errornumber = 0 . /* for ok */
     OafFormat = "10". /* added new type for png */ 
     RETURN.
END METHOD.



METHOD PUBLIC MEMPTR ConvertImageToNewType (INPUT iArray AS INT, 
                                            INPUT icFileType AS CHAR ):

   DEFINE VARIABLE memStream AS System.IO.MemoryStream NO-UNDO.

     IF icFileType = "bmp" OR  icFiletype = ""  THEN
         ImageArray[iArray]:SAVE(memStream,System.Drawing.Imaging.ImageFormat:Bmp).
     ELSE IF icFiletype = "jpeg" OR icFiletype = "jpg" THEN
         ImageArray[iArray]:SAVE(memStream,System.Drawing.Imaging.ImageFormat:Jpeg).
     ELSE IF icFiletype = "png" THEN
         ImageArray[iArray]:SAVE(memStream,System.Drawing.Imaging.ImageFormat:png).
     ELSE IF icFiletype = "tif" OR icFileType = "tiff" THEN
        ImageArray[iArray]:SAVE(memStream,System.Drawing.Imaging.ImageFormat:tiff).
     ELSE IF icFiletype = "gif" THEN
        ImageArray[iArray]:SAVE(memStream,System.Drawing.Imaging.ImageFormat:gif).
     ELSE IF icFiletype = "ico" THEN
        ImageArray[iArray]:SAVE(memStream,System.Drawing.Imaging.ImageFormat:ICON).


     DEFINE VARIABLE aMEMPTR AS MEMPTR NO-UNDO.
     DEFINE VARIABLE counter AS INTEGER NO-UNDO.
     
     SET-SIZE(aMEMPTR) = memStream:Length.
     DO counter = 1 TO memStream:Length:
         PUT-BYTE(aMEMPTR,counter)= memStream:ReadByte().
     END.

     memStream:CLOSE().
     memStream:Dispose().
     DELETE OBJECT memStream.
     
     RETURN aMEMPTR.

END METHOD.



METHOD PUBLIC VOID ResizeImageFromFile( INPUT icFilename AS CHARACTER,
                                        INPUT icSaveToFilename AS CHARACTER,
                                        INPUT iSizeWidth AS INTEGER,
                                        INPUT iSizeHeight AS INTEGER):

    DEFINE VARIABLE LoadImage        AS System.Drawing.Image.
    DEFINE VARIABLE NewResizedImage  AS System.Drawing.Image.
    DEFINE VARIABLE sourceStream     AS FileStream NO-UNDO.
         
    sourceStream = NEW FileStream(SEARCH( icFilename), FileMode:Open).  
    loadImage    = System.Drawing.Image:FromStream(sourceStream).
    sourceStream:CLOSE().
    sourceStream:dispose(). 

    NewResizedImage = loadImage:GetThumbnailImage(iSizeWidth,iSizeHeight,?,?).
    NewResizedImage:SAVE(icSaveToFilename).
    
    RETURN.

 END METHOD.

 
     
END CLASS.

