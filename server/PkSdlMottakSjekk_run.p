
/*------------------------------------------------------------------------
    File        : PkSdlMottakSjekk_run.p 
    Purpose     : 

    Syntax      :

    Description : Starter eksport

    Author(s)   : tomn
    Created     : 25/7-18
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*ROUTINE-LEVEL ON ERROR UNDO, THROW.*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE ocReturn  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDatoTid  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lTid      AS INTEGER   NO-UNDO.

ASSIGN lTid = TIME.
RUN PkSdlMottakSjekk.p.

ASSIGN lTid = TIME - lTid.

QUIT.
