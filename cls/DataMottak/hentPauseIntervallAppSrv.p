
/*------------------------------------------------------------------------
    File        : hentPauseIntervallAppSrv.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tomn
    Created     : Tue Feb 25 17:00:32 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE OUTPUT PARAMETER iPauseIntervall AS INTEGER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
{syspar2.i 200 1 2 iPauseIntervall INT}