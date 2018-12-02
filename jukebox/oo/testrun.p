
/*------------------------------------------------------------------------
    File        : testrun.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Brynjar
    Created     : Thu May 06 18:12:49 CEST 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
/* USING "c:\progress\jukebox\oo.*" FROM PROPATH.  */
USING System.Windows.Forms.Application FROM ASSEMBLY.
DEFINE VARIABLE rTemp AS CLASS JBoxMainForm NO-UNDO.
DO ON ERROR  UNDO, LEAVE
    ON ENDKEY UNDO, LEAVE
    ON STOP   UNDO, LEAVE
    ON QUIT   UNDO, LEAVE:
    rTemp = NEW JBoxMainForm ( ) .
    WAIT-FOR System.Windows.Forms.Application:Run ( rTemp ).
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    CATCH e1 AS Progress.Lang.AppError:
    DO i = 1 TO e1:NumMessages:
        MESSAGE e1:GetMessage(i) VIEW-AS ALERT-BOX BUTTONS OK TITLE "Error".
    END.
    IF e1:ReturnValue > "" THEN
        MESSAGE e1:ReturnValue VIEW-AS ALERT-BOX BUTTONS OK TITLE "Return Value".
    END CATCH.
    CATCH e2 AS Progress.Lang.Error:
        DO i = 1 TO e2:NumMessages:
            MESSAGE e2:GetMessage(i) VIEW-AS ALERT-BOX BUTTONS OK TITLE "Error".
        END.
    END CATCH.
END.
FINALLY.
IF VALID-OBJECT(rTemp) THEN DELETE OBJECT rTemp NO-ERROR.
END FINALLY.
