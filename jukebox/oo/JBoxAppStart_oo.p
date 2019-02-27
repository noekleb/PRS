/* Use the .Net event blocking loop if the CLR is loaded. */
/* Need some dummy window for this */
DEF VAR oForm AS Progress.Windows.Form.
oForm = NEW Progress.Windows.Form().


DEF VAR hCurrObject AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hButton     AS HANDLE NO-UNDO.
DEF VAR hMenuItem   AS HANDLE NO-UNDO.

/* ON "page-down" ANYWHERE DO:               */
/*   MESSAGE "yess"                          */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.  */
/* END.                                      */


ON 'alt-n':U ANYWHERE DO:
  hCurrObject = DYNAMIC-FUNCTION("getCurrentObject").
  IF VALID-HANDLE(hCurrObject) THEN DO:
    hToolbar = DYNAMIC-FUNCTION("getLinkedObject",hCurrObject,"toolbar","from").
    IF VALID-HANDLE(hToolbar) THEN DO:
      hMenuItem = DYNAMIC-FUNCTION("getEventWidget",hToolbar,"new","menu-item").
      IF VALID-HANDLE(hMenuItem) AND hMenuItem:ACCELERATOR = "Alt-n" THEN
        APPLY "choose" TO hMenuItem.
    END.
  END.
END.

ON 'alt-k','alt-c' ANYWHERE DO:
  hCurrObject = DYNAMIC-FUNCTION("getCurrentObject").
  IF VALID-HANDLE(hCurrObject) THEN DO:
    hToolbar = DYNAMIC-FUNCTION("getLinkedObject",hCurrObject,"toolbar","from").
    IF VALID-HANDLE(hToolbar) THEN DO:
      hMenuItem = DYNAMIC-FUNCTION("getEventWidget",hToolbar,"copy","menu-item").
      IF VALID-HANDLE(hMenuItem) AND CAN-DO("Alt-n,Alt-c",hMenuItem:ACCELERATOR) THEN
        APPLY "choose" TO hMenuItem.
    END.
  END.
END.

DYNAMIC-FUNCTION("setAttribute",SESSION,"latestWaitFor",STRING(ETIME)).
WAIT-FOR System.Windows.Forms.Application:Run().

