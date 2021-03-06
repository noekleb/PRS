
/*------------------------------------------------------------------------
    File        : eksporterhitrate_run.p 
    Purpose     : 

    Syntax      :

    Description : Starter eksport

    Author(s)   : tomn
    Created     : Mon Apr  17:32:28 CEST 2009
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
RUN bibl_logg.p ('eksporter_maze', 'eksporterMaze_run.p: WinCheduler Starter eksport av Maze. ' + string(TIME,"HH:MM:SS")).

RUN fix-eksportmaze.p (01/01/2011,12/31/2012,OUTPUT ocReturn).

ASSIGN lTid = TIME - lTid.
RUN bibl_logg.p ('eksporter_maze', 'eksporterMaze_run.p: WinCheduler Stoppet eksport av Maze. ' + string(TIME,"HH:MM:SS") + ' Tidsbruk: ' + string(lTid,"HH:MM:SS")).

QUIT.
