 
 /*------------------------------------------------------------------------
    File        : FileWatcher
    Purpose     : Demonstrate handling events from the .NET System.IO.Filewatcher
                  This is a non-UI class; note that subscribing to events is the same as for UI Controls. 
    Syntax      : 
    Description : 
    Author(s)   :
    Created     :
    Notes       : 2 parameters used: a pathname to monitor and a file filter.
                  Messages are raised when a file matching the filter is either deleted or created in the path
                  specified.         
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING System.*. 



CLASS FileWatcher inherits System.IO.FileSystemWatcher: 


	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC FileWatcher ( ):
        THIS-OBJECT( ".","*.*").
    END CONSTRUCTOR.	
    
    CONSTRUCTOR PUBLIC FileWatcher ( INPUT cPath AS CHARACTER, INPUT cFilter AS CHARACTER ):
		
		super().
        assign Path = cPath 
               Filter = cFilter.
        
        /* here we set the handlers for the Created and Deleted events */
        THIS-OBJECT:Created:Subscribe(THIS-OBJECT:OnCreated).
        THIS-OBJECT:Deleted:Subscribe(THIS-OBJECT:OnDeleted).
        
        EnableRaisingEvents = TRUE.
    END CONSTRUCTOR.
    
    
	
    METHOD PUBLIC VOID OnCreated ( INPUT sender AS System.Object, INPUT e AS System.IO.FileSystemEventArgs ):
         MESSAGE e:FullPath " created"    VIEW-AS ALERT-BOX. 
    END METHOD.
    
    METHOD PUBLIC VOID OnDeleted ( INPUT sender AS System.Object, INPUT e AS System.IO.FileSystemEventArgs ):
         MESSAGE e:FullPath " deleted"    VIEW-AS ALERT-BOX. 
    END METHOD.

END CLASS.
