DEF BUFFER bSysPara FOR SysPara.

    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 34) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 34
            bSysPara.Parameter1   = "tomn@nsoft.no"
            bSysPara.Parameter2   = "0"
            bSysPara.Beskrivelse  = "Mottaker fakturaEMail"
            bSysPara.Hjelpetekst1 = "eMail med faktura fra overføring ved varemottak."
            bSysPara.Hjelpetekst2 = "Ikke aktiv=0, Aktiv =1."
            .
        RELEASE bSysPara.
    END.

    IF NOT CAN-FIND(syspara WHERE
    syspara.syshid = 50 AND
    syspara.sysgr  = 50 AND
    syspara.paranr = 35) THEN DO:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId       = 50 
        bSysPara.SysGr        = 50 
        bSysPara.ParaNr       = 35
        bSysPara.Parameter1   = "10,16,20,40"
        bSysPara.Parameter2   = "16,20"
        bSysPara.Beskrivelse  = "Gyldige sender/mottagere"
        bSysPara.Hjelpetekst1 = "Mottagere det skal varsles for."
        bSysPara.Hjelpetekst2 = "Sendere det skal varsles for."
        .
    RELEASE bSysPara.
END.
