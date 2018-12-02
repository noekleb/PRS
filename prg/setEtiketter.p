
DEF VAR piLoop AS INT NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 5 AND
    SysGruppe.SysGr  = 20) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 5
        bSysGruppe.SysGr  = 20
        Beskrivelse      = "Filnavn etikettfiler".
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 5 AND
    SysGruppe.SysGr  = 21) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 5
        bSysGruppe.SysGr  = 21
        Beskrivelse      = "Etikettskrivere".
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

DO FOR bSysPara TRANSACTION:
    LOOP:
    DO piLoop = 90 TO 93:
        IF NOT CAN-FIND(FIRST bSysPara WHERE
            bSysPara.SysHId = 5 AND
            bSysPara.SysGr  = 20 AND
            bSysPara.ParaNr = piLoop) THEN
        DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 5
                bSysPara.SysGr       = 20
                bSysPara.ParaNr      = piLoop
                bSysPara.Parameter1  = "dummy".
                CASE piLoop:
                  WHEN 90 THEN
                    ASSIGN bSysPara.Beskrivelse = "TIME_2_11"
                           bSysPara.Parameter2  = "TIME_2_11".
                  WHEN 91 THEN
                    ASSIGN bSysPara.Beskrivelse = "TIME_3_5"
                           bSysPara.Parameter2  = "TIME_3_5".
                  WHEN 92 THEN
                    ASSIGN bSysPara.Beskrivelse = "TIME_Plakat"
                           bSysPara.Parameter2  = "TIME_Plakat".
                  WHEN 93 THEN
                    ASSIGN bSysPara.Beskrivelse = "TIME_11_2"
                           bSysPara.Parameter2  = "TIME_11_2".
                END CASE.
            RELEASE bSysPara.
        END.
    END. /* LOOP */
    LOOP2:
    DO piLoop = 90 TO 93:
        IF NOT CAN-FIND(FIRST bSysPara WHERE
            bSysPara.SysHId = 5 AND
            bSysPara.SysGr  = 21 AND
            bSysPara.ParaNr = piLoop) THEN
        DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId       = 5
                bSysPara.SysGr        = 21
                bSysPara.ParaNr       = piLoop
                bSysPara.Parameter1 = "startetikett"
                bSysPara.Parameter2  = "AVAIL".
                CASE piLoop:
                    WHEN 90 THEN
                      ASSIGN bSysPara.Beskrivelse = "Utskrift TIME_2_11".
                    WHEN 91 THEN
                      ASSIGN bSysPara.Beskrivelse = "Utskrift TIME_3_5".
                    WHEN 92 THEN
                      ASSIGN bSysPara.Beskrivelse = "Utskrift TIME_Plakat".
                    WHEN 93 THEN
                      ASSIGN bSysPara.Beskrivelse = "Utskrift TIME_11_2".
                END CASE.
            RELEASE bSysPara.
        END.
    END. /* LOOP2 */
END. /* bSysPara TRANSACTION*/


