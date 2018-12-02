DEF VAR cLayoutLst AS CHAR NO-UNDO.
DEF VAR iLoop      AS INT  NO-UNDO.

ASSIGN
    cLayoutLst = "250,253,301,401,501,601,701,801,901,1001"
    .

DO iLoop = 1 TO NUM-ENTRIES(cLayoutLst):
  FOR EACH SysGruppe WHERE
      SysGruppe.SysHId = 6 AND
      SysGruppe.SysGr  = INT(ENTRY(iLoop,cLayoutLst)):
      FOR EACH SysPara OF sysGruppe:
          DELETE SysPara.
      END.
      DELETE SysGruppe.
  END.
  FOR EACH SysGruppe WHERE
      SysGruppe.SysHId = 9 AND
      SysGruppe.SysGr  = INT(ENTRY(iLoop,cLayoutLst)):
      FOR EACH SysPara OF sysGruppe:
          DELETE SysPara.
      END.
      DELETE SysGruppe.
  END.
END.

