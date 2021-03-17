DEF VAR hServer AS HANDLE NO-UNDO.

/* Mot asPRS appserveren. */
CREATE SERVER hServer.
hServer:CONNECT("-H LAPTOP-CENEE05L -S 3190 -AppService asPRS -DirectConnect").

MESSAGE hServer:connected() |
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
