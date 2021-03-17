DEFINE VARIABLE hServer AS HANDLE NO-UNDO.
DEF VAR lConnected AS LOG NO-UNDO.

DEFINE VAR iButikkNr     AS INTEGER     NO-UNDO.
DEFINE VAR iKasseNr      AS INTEGER     NO-UNDO.
DEFINE VAR iBongNr       AS INTEGER     NO-UNDO.
DEFINE VAR cTilgodeNr    AS CHARACTER   NO-UNDO.
DEFINE VAR cType         AS CHARACTER   NO-UNDO.
DEFINE VAR dBelopp       AS DECIMAL     NO-UNDO.
DEFINE VAR lOK           AS LOGICAL     NO-UNDO.
DEFINE VAR lBrukt        AS LOGICAL     NO-UNDO.
DEFINE VAR cMelding      AS CHARACTER   NO-UNDO.


CREATE SERVER hServer.
lConnected = hServer:CONNECT("-H INFOPOS02 -S 3090 -AppService asbroker1 -DirectConnect").

/* Ubrukt 
ASSIGN
    iButikkNr  = 14
    iKasseNr   = 1
    iBongNr    = 1057
    cTilgodeNr = '01/06/12-1-1057-2'
    cType      = '1'
    dBelopp    = 1150
    .
*/    

/* Brukt */
ASSIGN
    iButikkNr  = 10
    iKasseNr   = 1   /* 1-Finnes, 2-Beløp */
    iBongNr    = 253093
    cTilgodeNr = '1911070000100100253093'
    cType      = '2'
    dBelopp    = 0
    .
  

IF lConnected THEN                                                                                 
DO:                                                                                                
    RUN asTilgode.p ON hServer (
                              iButikkNr,
                              iKasseNr,
                              ibongNr,
                              cTilgodeNr,
                              cType,
                              INPUT-OUTPUT dBelopp,
                              OUTPUT lOk,
                              OUTPUT lBrukt,
                              OUTPUT cMelding
                             ).

    MESSAGE 'Beløp:' dBelopp SKIP
            'Brukt:' lBrukt SKIP
            'Melding:' cMelding
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

END.
ELSE DO:

    MESSAGE 'asbroker1 oppkobling feilet!'
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

END.

IF lConnected THEN 
    hServer:DISCONNECT().
IF VALID-HANDLE(hServer) THEN
    DELETE OBJECT hServer.    

