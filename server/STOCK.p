
/* &GLOBAL-DEFINE WEBSTREAM STREAM WebStream */
&GLOBAL-DEFINE OUT-STREAM PUT {&WEBSTREAM} UNFORMATTED
&GLOBAL-DEFINE OUT-FILE {&OUT-STREAM}
&GLOBAL-DEFINE OUT {&OUT-STREAM}
&GLOBAL-DEFINE OUT-LONG EXPORT {&WEBSTREAM}
&GLOBAL-DEFINE OUT-FMT PUT {&WEBSTREAM}
&GLOBAL-DEFINE DISPLAY DISPLAY {&WEBSTREAM}
/*
{src/web/method/cgidefs.i}
*/

{src/web2/wrap-cgi.i}


DEFINE VARIABLE hServer            AS HANDLE NO-UNDO. 
DEFINE VARIABLE lConnected         AS LOG NO-UNDO. 
DEFINE VARIABLE cConnectionString  AS CHAR NO-UNDO.
DEFINE VARIABLE lcLager            AS LONGCHAR   NO-UNDO.
DEFINE VARIABLE cinput             AS CHARACTER   NO-UNDO.
cConnectionString = "-H 192.168.100.68 -AppService asbroker1". 
CREATE SERVER hServer. 
lConnected = hServer:CONNECT(cConnectionString) NO-ERROR.

IF lConnected THEN DO:
    cInput = get-value(?).
    RUN asGetPRSEanLager.p ON hServer (entry(1,cInput),OUTPUT lcLager) .
    hServer:DISCONNECT().
END.
DELETE OBJECT hServer.
RUN OutputContentType IN web-utilities-hdl ("application/json":U).
/* application/json text/html */
{&OUT-LONG} lcLager.


