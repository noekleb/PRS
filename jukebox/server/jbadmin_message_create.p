/* Create Message
   Parameters:  New buffer
                Fields (comma-separated)
                Values (pipe-separated)
   
   Created: 28.03.07 by brynjar@chemistry.no                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM hBuffer     AS HANDLE NO-UNDO.
DEF INPUT  PARAM icFields    AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues    AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO. 

DEF VAR iMsgNum AS INT NO-UNDO INIT 1.

DO ON ERROR UNDO, RETURN:
  FIND LAST JBoxTranslation NO-LOCK 
       WHERE JBoxTranslation.iMsgNo > 0
         AND JBoxTranslation.cObjectType = "message"
       USE-INDEX idxMsgNo
       NO-ERROR.
  IF AVAIL JBoxTranslation THEN
    iMsgNum = JBoxTranslation.iMsgNo + 1.

  hBuffer:BUFFER-FIELD("iMsgNo"):BUFFER-VALUE = iMsgNum.
END.

