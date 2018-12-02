 
 /*------------------------------------------------------------------------
    File        : JBoxDynLookup
    Purpose     : Perform dynamic lookup
    Syntax      : 
    Description : 
    Author(s)   : Brynjar
    Created     : Tue May 19 12:43:22 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS JBoxDynLookup:
  DEF VAR hSourceProc AS HANDLE NO-UNDO. 
  DEF VAR hParent     AS HANDLE NO-UNDO. 
  DEF PUBLIC PROPERTY BROWSE-OBJECT  AS JBoxBrowse GET. PROTECTED SET. 
  DEF PUBLIC PROPERTY TOOLBAR-OBJECT AS JBoxToolbar GET. PROTECTED SET.
  DEF PUBLIC PROPERTY NAME           AS CHARACTER GET. SET.
  DEF VAR bOk         AS LOG    NO-UNDO.
  
                   
  CONSTRUCTOR PUBLIC JBoxDynLookup (INPUT ihParent  AS HANDLE
                                   ,INPUT ihLookup  AS HANDLE
                                   ,INPUT ihBrowse  AS HANDLE
                                   ,INPUT ihToolbar AS HANDLE 
                                    ):

    ASSIGN hSourceProc = SOURCE-PROCEDURE
           hParent = ihParent
           BROWSE-OBJECT = NEW JBoxBrowse(ihBrowse,ihParent)
           NAME = BROWSE-OBJECT:BUFFER-HANDLE:NAME
           .
    IF VALID-HANDLE(ihToolbar) THEN DO:      
      TOOLBAR-OBJECT = NEW JBoxToolbar(ihToolbar,ihParent).
      BROWSE-OBJECT:TOOLBAR-OBJECT = TOOLBAR-OBJECT.
    END.  
        
  END CONSTRUCTOR.

  CONSTRUCTOR PUBLIC JBoxDynLookup():
  END CONSTRUCTOR.
                               
  METHOD PUBLIC VOID ExpandSearchDialogX (iiPixels AS INT):
    RUN ExpandSearchDialog IN hSourceProc (BROWSE-OBJECT:BROWSE-HANDLE,"X",iiPixels).  
  END METHOD.

  METHOD PUBLIC VOID ExpandSearchDialogY (iiPixels AS INT):
    RUN ExpandSearchDialog IN hSourceProc (BROWSE-OBJECT:BROWSE-HANDLE,"Y",iiPixels).  
  END METHOD.
      
  DESTRUCTOR PUBLIC JBoxDynLookup ( ):
  END DESTRUCTOR.
END CLASS.
  