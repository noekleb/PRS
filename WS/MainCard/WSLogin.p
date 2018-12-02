/* ---------------------------------------- (c) 2013 CHSO --------------- */
/* ---- Program Name : WSLogin.p                                     ---- */
/* ---- Description  :                                               ---- */
/* ----                                                              ---- */
/* ---- Author       : Curt H. Oldenborg                             ---- */
/* ---- Date Started : 2013-04-2            
------------------------------------------------------------------------- */

DEFINE INPUT  PARAMETER LoginUserId AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER LoginPassword AS CHAR NO-UNDO. 
DEFINE OUTPUT PARAMETER LoginSessionId AS CHAR NO-UNDO. 
DEFINE OUTPUT PARAMETER RequestStatus AS LOGICAL NO-UNDO. 

RUN getClientSessionId.p 
    (LoginUserId, 
     LoginPassword,
     OUTPUT LoginSessionId,
     OUTPUT RequestStatus).

LOG-MANAGER:WRITE-MESSAGE("Login:" + QUOTER(LoginUserid) + " Status:" + QUOTER(requestStatus),"WSLogin").

RETURN. 
