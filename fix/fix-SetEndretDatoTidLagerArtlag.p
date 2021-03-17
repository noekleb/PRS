
/*------------------------------------------------------------------------
    File        : fix-SetLEndretDatoTidLagerArtlag.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tny
    Created     : Wed Aug 08 15:31:32 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FOR EACH Lager EXCLUSIVE-LOCK WHERE 
    Lager.EndretDateTime = ?:
    
    ASSIGN 
        Lager.EndretDateTime = NOW
        .
END.
FOR EACH ArtLag EXCLUSIVE-LOCK WHERE 
    ArtLag.EndretDatoTid = ?:
    
    ASSIGN 
        ArtLag.EndretDatoTid = NOW
        .    
END.
