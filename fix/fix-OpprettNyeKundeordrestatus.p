DEF VAR piLoop AS INT NO-UNDO.
DEF VAR pcText AS CHAR NO-UNDO.
DEFINE VARIABLE pcNrLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcMailLst AS CHARACTER NO-UNDO.

/* TN 30/12-18 Setter ny kundeordrestatus */
IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 19 AND
    SysGruppe.SysGr  = 14) THEN
DO TRANSACTION:
    CREATE SysGruppe.
    ASSIGN
        SysGruppe.SysHId      = 19
        SysGruppe.SysGr       = 14
        SysGruppe.Beskrivelse = "Kundeordrestatus"
        .
    RELEASE SysGruppe.
END. /* bSysGruppe TRANSACTION */

ASSIGN
    pcText    = "Opprettet,Tilbud sendt,Bekreftet,PkSdl skrevet,PPEtikett skrevet,Levert speditør,Bekreftet speditør,Levert,Ikke levert,Makulert"
    pcNrLst   = "10,20,30,35,40,45,47,50,55,60"
    pcMailLst = "0,0,1,0,0,1,1,1,0,0"
    .

LOOP:
DO piLoop = 1 TO NUM-ENTRIES(pcText) TRANSACTION:
    IF NOT CAN-FIND(SysPara WHERE
        SysPara.SysHId = 19 AND
        SysPara.SysGr  = 14 AND
        SysPara.ParaNr = INT(ENTRY(piLoop,pcNrLst))) THEN
    DO:
        CREATE SysPara.
        ASSIGN
            SysPara.SysHId       = 19
            SysPara.SysGr        = 14
            SysPara.ParaNr       = INT(ENTRY(piLoop,pcNrLst))
            SysPara.Parameter1   = ENTRY(piLoop,pcText)
            SysPara.Parameter2   = ENTRY(piLoop,pcMailLst)
            SysPara.Beskrivelse  = ENTRY(piLoop,pcText)
            SysPara.Hjelpetekst1 = 'Statustekst'
            SysPara.Hjelpetekst2 = 'Skal det sendes eMail ved denne status 0-Nei, 1-Ja.'
            .
        RELEASE SysPara.
    END.
END. /* LOOP */
