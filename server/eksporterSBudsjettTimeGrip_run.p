
/*------------------------------------------------------------------------
    File        : eksporterSBudsjettTimeGrip_run.p 
    Purpose     : 

    Syntax      :

    Description : Starter eksport

    Author(s)   : tomn
    Created     : 18/1-15
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
RUN eksportSBudsjettTimeGrip.p (OUTPUT ocReturn).

ASSIGN lTid = TIME - lTid.

QUIT.
