DEFINE BUFFER bSysPara FOR SysPara.

IF NOT CAN-FIND(FIRST bSysPara WHERE 
   bSysPara.SysHId = 19 AND
   bSysPara.SysGr = 12 AND
   bSysPara.ParaNr = 50) THEN
DO:
    CREATE bSysPara.
    ASSIGN bSysPara.SysHId = 19
           bSysPara.SysGr = 12
           bSysPara.ParaNr = 50
           bSysPara.Parameter1 = "1"
           bSysPara.Beskrivelse = "Detaljnivå av kvitto/order på faktura"
           bSysPara.Hjelpetekst1 = "1-Full,2-Kvitto/Order,3-Total".
    RELEASE bSysPara.
END.
