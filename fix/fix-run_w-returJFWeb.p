DEFINE VAR ipcKOrdreID AS CHARACTER   NO-UNDO.
DEFINE VAR iButikkNr AS INTEGER     NO-UNDO.

ASSIGN
    ipcKOrdreID =  
    iButikkNr = 15
    .

RUN w-returJFWeb.w (ipcKOrdreID, iButikkNr).
 
