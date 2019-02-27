/********************************************************************************
Created....24/02/2014 by Brynjar
Purpose....
Used by....  <list of_ client procedures>
Modified...
********************************************************************************/

/* DYNAMIC-FUNCTION('startASlib' IN SOURCE-PROCEDURE). /* Optionally enable appserver library functions on server */ */

PROCEDURE CleanUp:
  /* Delete any dynamic objects here (avoid creating them inside the internal procedure(s)) */
END PROCEDURE.


PROCEDURE BigData:
  DEF INPUT  PARAM irOrder AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam AS CHAR NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn AS CHAR NO-UNDO.

  find order where rowid(order) = irOrder no-lock no-error.
  if avail order 
     and can-find(first orderline of order where qty * price > dec(icParam)) then
    ocreturn = "true".
  else
    ocReturn = "false".
END PROCEDURE.

PROCEDURE test:
  DEF INPUT  PARAM irOrder AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam AS CHAR NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn AS CHAR NO-UNDO.

  ocReturn = "". /* Always string value. Use SKIPROW to exclude row from result set */
END PROCEDURE.


