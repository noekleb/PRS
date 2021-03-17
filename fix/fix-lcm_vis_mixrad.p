
CURRENT-WINDOW:WIDTH = 300.

FOR EACH MixRad NO-LOCK /*WHERE
    CAN-DO("1,2,17",STRING(mixgruppe.ButNr))*/:
    DISPLAY
        MixRad.MixNr
        MixRad.Ean
        MixRad.Antall
        MixRad.Utpris
        MixRad.ProfNr
        MixRad.ButNr
        MixRad.ModellMix
        MixRad.GrpNr
    WITH WIDTH 300.
END.
