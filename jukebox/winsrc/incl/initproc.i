/* Any custom initialization. Included in wintrigg.i*/

/* IF SEARCH(LC("help\" + TRIM(THIS-PROCEDURE:FILE-NAME,".w")) + ".html") NE ? THEN                                                   */
/*   DYNAMIC-FUNCTION("setWebHelpFile",THIS-PROCEDURE:CURRENT-WINDOW,                                                                 */
/*                    DYNAMIC-FUNCTION("getAttribute",SESSION,"defaulthelpdir") + LC(TRIM(THIS-PROCEDURE:FILE-NAME,".w")) + ".html"). */

DYNAMIC-FUNCTION("LoadWinIcon",THIS-PROCEDURE:CURRENT-WINDOW,DYNAMIC-FUNCTION("getAttribute",SESSION,"WinIcon_" + THIS-PROCEDURE:FILE-NAME),YES) NO-ERROR.                                
   

DEF VAR cDefaultFrameFont  AS CHAR   NO-UNDO.

cDefaultFrameFont = DYNAMIC-FUNCTION("getAttribute",SESSION,"defaultFrameFont") NO-ERROR.
IF NOT ERROR-STATUS:ERROR AND cDefaultFrameFont NE "" THEN
  FRAME {&FRAME-NAME}:FONT = INTEGER(cDefaultFrameFont) NO-ERROR.
