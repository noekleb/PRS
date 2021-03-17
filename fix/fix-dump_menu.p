OUTPUT TO VALUE('fixjboxmenu.d').
FOR EACH JboxMenu NO-LOCK:
    EXPORT DELIMITER ';'
    iJBoxMenuId     
    cMenuType       
    cMenuLabel      
    iJBoxProgramId  
    cLaunch         
    cLaunchType     
    dCreated        
    cCreatedBy      
    dModified       
    cModifiedBy     
    cAccelerator    
    bConfigurable   
    cImage          
    cSelImage       
    cStateImage     
    cTextColor      
    cFontStyle      
    cMenuTooltip    
    cMenuNumber     
    iTvNavBarStyle  
    iImageSize      
    iSelImageSize   
    iStateImageSize 
    .
END.
OUTPUT CLOSE.

OUTPUT TO VALUE('fixjbmetome.d').
FOR EACH JboxMenuToMenu NO-LOCK:
    EXPORT DELIMITER ';'
    iToMenuId        
    iFromMenuId      
    iSeq             
    dCreated         
    cCreatedBy       
    dModified        
    cModifiedBy      
    cJBoxUserId      
    iJboxUserGroupId .
END.
OUTPUT CLOSE.


