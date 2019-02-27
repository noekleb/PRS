/*------------------------------------------------------------------------
    File        : getwinhostname.p
    Purpose     : Return hostname of local machine
    Author(s)   : Steve Southwell - BravePoint, Inc.
    Created     : 8/22/02
    Copyright   : 2002 - The FreeFrameWork Project, Inc. - Use according to FFW license
    Notes       : Thanks to Bill Prew (billprew@cox.net) for posting a 
                   fine example of the Windows getHostName API at
                   http://www.global-shared.com/api/ip.html
                  Thanks to David Craven, Scott Ziola, and Hugh Cruickshank 
                   for *nix input.  
                   
   Created from gethostname.p cause the win api references makes crashes on linux
  ----------------------------------------------------------------------*/

DEFINE OUTPUT PARAMETER p-TcpName AS CHARACTER NO-UNDO.

&SCOPED-DEFINE WSADATA_LENGTH           403
DEFINE VARIABLE w-TcpName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE w-Length       AS INTEGER   NO-UNDO.
DEFINE VARIABLE w-Return       AS INTEGER   NO-UNDO.
DEFINE VARIABLE ptr-WsaData    AS MEMPTR    NO-UNDO.

 /* Initialize return values */
ASSIGN p-TcpName = ?.

  /* Allocate work structure for WSADATA */
SET-SIZE(ptr-WsaData) = {&WSADATA_LENGTH}.

  /* Ask Win32 for winsock usage */
RUN WSAStartup (INPUT 257,        /* requested version 1.1 */
                INPUT GET-POINTER-VALUE(ptr-WsaData),
                OUTPUT w-Return).

  /* Release allocated memory */
SET-SIZE(ptr-WsaData) = 0.

  /* Check for errors */
IF w-Return NE 0 THEN DO:
    ASSIGN p-TcpName = "".
    RETURN.
END.

  /* Set up variables */
ASSIGN w-Length  = 100
w-TcpName = FILL(" ", w-Length)
         .

  /* Call Win32 routine to get host name */
RUN gethostname (OUTPUT w-TcpName,
                   INPUT w-Length,
                   OUTPUT w-Return).

  /* Check for errors */
IF w-Return NE 0 THEN DO:
    ASSIGN p-TcpName = "".
    RUN WSACleanup (OUTPUT w-Return).
    RETURN.
END.

  /* Pass back gathered info */
  /* remember: the string is null-terminated so there is a CHR(0)
               inside w-TcpName. We have to trim it:  */
p-TcpName = ENTRY(1,w-TcpName,CHR(0)).

     /* Terminate winsock usage */
RUN WSACleanup (OUTPUT w-Return).



/* External Procedure Prototypes */

PROCEDURE gethostname EXTERNAL "wsock32.dll" :
  DEFINE OUTPUT       PARAMETER p-Hostname      AS CHARACTER.
  DEFINE INPUT        PARAMETER p-Length        AS LONG.
  DEFINE RETURN       PARAMETER p-Return        AS LONG.
END PROCEDURE.

PROCEDURE WSAStartup EXTERNAL "wsock32.dll" :
  DEFINE INPUT        PARAMETER p-VersionReq    AS SHORT.
  DEFINE INPUT        PARAMETER ptr-WsaData     AS LONG.
  DEFINE RETURN       PARAMETER p-Return        AS LONG.
END PROCEDURE.

PROCEDURE WSACleanup EXTERNAL "wsock32":
  DEFINE RETURN       PARAMETER p-Return        AS LONG.
END PROCEDURE.

