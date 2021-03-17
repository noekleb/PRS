DEFINE VAR lPkSdlId AS DEC       NO-UNDO.
DEFINE VAR iButNr   AS INTEGER   NO-UNDO. /* Opprinnelig overført fra.               */
DEFINE VAR iTilBut  AS INTEGER   NO-UNDO. /* Butikk som skal ha overføringen.        */
DEFINE VAR bOk      AS LOG INITIAL TRUE NO-UNDO.
DEFINE VAR      cReturn           AS CHAR      NO-UNDO.

DEF VAR hDataset AS HANDLE NO-UNDO. 

ASSIGN
    lPkSdlId  = 2700001
    iButNr    = 3
    iTilbut   = 4
    .

/* Kjører uten AppServer. */
RUN asPkSdl_AngreOverforing.p 
    (lPkSdlId,
     iButNr,
     iTilbut,
     OUTPUT bOk,
     OUTPUT cReturn
     ) 
    .

MESSAGE
    'Fra AppServer:' SKIP
    '   Okmsgs....:' bOk SKIP
    '   ReturnMsgs:' cReturn
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

