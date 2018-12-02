DEF BUFFER bSyspara FOR Syspara.    
    
IF NOT CAN-FIND(syspara WHERE
    syspara.syshid = 50 AND
    syspara.sysgr  = 50 AND
    syspara.paranr = 29) THEN DO:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId       = 50 
        bSysPara.SysGr        = 50 
        bSysPara.ParaNr       = 29
        bSysPara.Parameter1   = "prssport1@polygonsoftware.no"
        bSysPara.Beskrivelse  = "Default avsender suppleringsordre".
        bSysPara.Hjelpetekst1 = "Er denne blank, benyttes eMAil fra butikkregister"
        .
    RELEASE bSysPara.
END.

