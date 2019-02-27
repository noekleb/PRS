DEFINE VARIABLE oServer AS COM-HANDLE NO-UNDO.

CREATE 'Shell.Application' oServer.

/* Invoke the Windows Explorer on the C:\WINNT folder               */

NO-RETURN-VALUE oServer:Explore(SESSION:TEMP-DIR).

/* Release the object references                                    */

RELEASE OBJECT oServer.
