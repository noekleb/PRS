
/*------------------------------------------------------------------------
    File        : clientComp.p
    Purpose     : Run AppComp.w without db connection

    Syntax      :

    Description : 

    Author(s)   : brynj
    Created     : Mon Nov 27 11:12:32 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
JBoxSession:Instance:PromptForValue("Working directory", "CHARACTER", "x(50)", System.Environment:CurrentDirectory).
IF JBoxSession:Instance:PromptValueOk THEN DO:
  DYNAMIC-FUNCTION("setSubProcessWorkDir",JBoxSession:Instance:PromptValue).
  JBoxSession:Instance:PromptForValue("Ini-file", "CHARACTER", "x(50)", DYNAMIC-FUNCTION("getMyIniFile")).
  IF JBoxSession:Instance:PromptValueOk THEN 
    DYNAMIC-FUNCTION("setMyIniFile",JBoxSession:Instance:PromptValue).
  ELSE RETURN.
END.
ELSE RETURN.  
JBoxFunctions:Instance:spawnProwin("startAppCompForClient.p",LDBNAME(1)).