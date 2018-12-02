/* Export JukeBox menu structure
   Parameters:  none
   
   Created: 15.05.08 by brynjar@chemistry.no                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

FOR EACH JBoxMenu NO-LOCK:

END.

FOR EACH JBoxMenuToMenu NO-LOCK:
END.

FOR EACH JBoxUserMenu NO-LOCK:
END.

obOk = ocReturn = "".

