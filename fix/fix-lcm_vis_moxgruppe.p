
CURRENT-WINDOW:WIDTH = 300.

FOR EACH MixGruppe NO-LOCK WHERE
    CAN-DO("1,2,17",STRING(mix.ButNr)):
    DISPLAY
        MixGruppe.MixNr
        MixGruppe.ProfNr
        MixGruppe.ButNr
        MixGruppe.GrpNr
        MixGruppe.Antall
        MixGruppe.Utpris
        WITH WIDTH 300.
END.
