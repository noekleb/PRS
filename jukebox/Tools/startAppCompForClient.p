
/*------------------------------------------------------------------------
    File        : startAppCompForClient.p
    Purpose     : Spawned by clientComp.p

    Syntax      :

    Description : 

    Author(s)   : brynj
    Created     : Mon Nov 27 11:28:09 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEF VAR hAppComp AS HANDLE NO-UNDO. 

RUN JBoxLoadLib.p ("ResizeLib.p,JBoxUIlib.p,JBoxAsLib.p,JBoxFuLib.p").

RUN AppComp.w PERSIST SET hAppComp.

hAppComp:CURRENT-WINDOW:TITLE = SESSION:PARAMETER.

WAIT-FOR CLOSE OF hAppComp.

QUIT.