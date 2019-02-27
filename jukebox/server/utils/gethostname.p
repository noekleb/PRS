/*------------------------------------------------------------------------
    File        : gethostname.p
    Purpose     : Return hostname of local machine
    Author(s)   : Steve Southwell - BravePoint, Inc.
    Created     : 8/22/02
    Copyright   : 2002 - The FreeFrameWork Project, Inc. - Use according to FFW license
    Notes       : Thanks to Bill Prew (billprew@cox.net) for posting a 
                   fine example of the Windows getHostName API at
                   http://www.global-shared.com/api/ip.html
                  Thanks to David Craven, Scott Ziola, and Hugh Cruickshank 
                   for *nix input.  
                   
04/11/05 Brynjar Hasle
        - Removed preprosessor test on OPSYS (doesn't work on my linux)
  ----------------------------------------------------------------------*/

DEFINE OUTPUT PARAMETER p-TcpName AS CHARACTER NO-UNDO.

IF OPSYS = "UNIX" THEN DO:   
    &SCOPED-DEFINE SC STREAM cmdstream
    DEFINE {&SC}.
    INPUT {&SC} THROUGH VALUE("hostname -f"). /* if this doesn't work on your platform, try uname -n */
    IMPORT {&SC} UNFORMATTED p-TcpName.
    INPUT {&SC} CLOSE.
END.
ELSE RUN utils/getwinhostname.p (OUTPUT p-TcpName).
