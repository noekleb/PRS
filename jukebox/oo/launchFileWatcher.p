
/*------------------------------------------------------------------------
    File        : launchFileWatcher.p
    Purpose     : 

    Syntax      :

    Description : Launches the FileWatcher class

    Author(s)   : fmeulblo
    Created     : Thu Mar 24 12:57:50 CET 2011
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE myWatcher AS FileWatcher.


myWatcher = NEW FileWatcher("c:\temp","*.*").


/* WAIT-FOR System.Windows.Forms.Application:Run().  */
WAIT-FOR CLOSE OF THIS-PROCEDURE.

DELETE OBJECT myWatcher. 
