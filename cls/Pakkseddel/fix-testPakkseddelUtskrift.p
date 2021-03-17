DEF VAR bOk AS LOG NO-UNDO.
DEF VAR lPkSdlId AS DEC NO-UNDO.
DEF VAR iButNr AS INT NO-UNDO.

DEFINE VARIABLE rPakkseddel AS cls.Pakkseddel.Pakkseddel NO-UNDO. 

rPakkseddel  = NEW cls.Pakkseddel.Pakkseddel() NO-ERROR.

ASSIGN 
    iButNr = 10100
    lPkSdlId = 100001
    .

bOk = rPakkseddel:EtikettUtskrift( INPUT lPkSdlId, iButNr  ).

MESSAGE bOk
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
