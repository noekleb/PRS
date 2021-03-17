DEFINE VARIABLE dVarebehnr AS DECIMAL NO-UNDO.

RUN setEkstVPILev.
RUN setSysParaSmtpmail.

/* Hämtar aktiv varebok */
dVarebehnr = DYNAMIC-FUNCTION('getAktivSupplering':U).

IF dVarebehnr = 0 THEN 
  RUN opprettSuppleringsbok.
  
PROCEDURE opprettSuppleringsbok:
  
  FIND FIRST Messe NO-LOCK WHERE
    Messe.MesseType = 2 AND
    Messe.MesseFraDato <= TODAY AND
    Messe.MesseTilDato >= TODAY NO-ERROR.
  IF NOT AVAILABLE Messe THEN 
  DO:
    CREATE Messe.
    ASSIGN
      Messe.MesseType         = 2
      Messe.MesseBeskrivelse  = 'Suppleringsordre'
      Messe.MesseFraDato      = 01/01/2009
      Messe.MesseTilDato      = 12/31/2012
      Messe.FraDato           = 01/01/2009
      Messe.TilDato           = 12/31/2012
      Messe.PubliserStartDato = 01/01/2009
      Messe.PubliserStoppDato = 12/31/2012
      .
  END.
  
  FIND FIRST VareBehHode NO-LOCK WHERE
    VareBehHode.MesseNr = Messe.MesseNr NO-ERROR.
  IF NOT AVAILABLE VareBehHode THEN 
  DO:
    FIND FIRST prisprofil NO-LOCK WHERE PrisProfil.Profilnr > 0. 
    CREATE VareBehHode.
    ASSIGN
      VareBehHode.ProfilNr           = PrisProfil.ProfilNr
      VareBehHode.VareBehType        = 1
      VareBehHode.VareBehBeskrivelse = "Suppleringsordre"
      VareBehHode.MesseNr            = Messe.MesseNr
      .
  END.  
  
END PROCEDURE.  

FUNCTION getAktivSupplering RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dVarebehnr LIKE varebehhode.varebehnr    NO-UNDO.
  GETVAREBEHNR:
  FOR EACH Messe NO-LOCK WHERE Messe.MesseType = 2 AND
                       Messe.PubliserStartDato <= TODAY AND
                       Messe.PubliserStoppDato >= TODAY BY Messe.PubliserStoppDato DESCENDING:
          FIND LAST VareBehHode WHERE VarebeHhode.messenr = messe.messenr NO-LOCK NO-ERROR.
          IF AVAIL VarebehHode THEN DO:
               dVarebehnr = varebehhode.varebehnr.
               LEAVE GETVAREBEHNR.
          END.
  END.
  RETURN dVarebehnr.   /* Function return value. */

END FUNCTION.

PROCEDURE setEkstVPILev:

    /* Websystem */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 899) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 899
            EkstVPILev.KortNavn     = "VISMAG"
            EkstVPILev.Navn         = "VISMA Ordre/ordrebekr."
            EkstVPILev.AktivLev     = FALSE
            .
    END.
    
    FIND EkstVPILev WHERE
      EkstVPILEv.EkstVPILevNr = 899 EXCLUSIVE-LOCK.
    IF AVAILABLE EkstVPILev THEN 
      EkstVPILev.AktivLev = TRUE.

END PROCEDURE.

PROCEDURE setSysParaSmtpmail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bSysPara   FOR SysPara.
  DEF BUFFER bSysGruppe FOR SysGruppe.

  DO TRANSACTION:
    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 50 AND
        SysGruppe.SysGr  = 50) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId      = 50
            bSysGruppe.SysGr       = 50
            bSysGruppe.Beskrivelse = "smtpmail"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */

    /*  */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 1) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 1
            bSysPara.Parameter1   = "mail.polygonsoftware.no:587"
            bSysPara.Beskrivelse  = "Mailhub".
            bSysPara.Hjelpetekst1 = "0-Ja,1-Nei"
            .
        RELEASE bSysPara.
    END.
    ELSE DO:
      FIND bsyspara EXCLUSIVE-LOCK WHERE
        bsyspara.syshid = 50 AND
        bsyspara.sysgr  = 50 AND
        bsyspara.paranr = 1.
        ASSIGN  
            bSysPara.Parameter1   = "mail.polygonsoftware.no:587"
            bSysPara.Beskrivelse  = "Mailhub".
            bSysPara.Hjelpetekst1 = "0-Ja,1-Nei"
            .
        RELEASE bSysPara.
    END.

    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 2) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 2
            bSysPara.Parameter1   = "1"
            bSysPara.Beskrivelse  = "DoAuth".
            bSysPara.Hjelpetekst1 = "0-FALSE,1-TRUE"
            .
        RELEASE bSysPara.
    END.
    ELSE DO:
      FIND bsyspara EXCLUSIVE-LOCK WHERE
        bsyspara.syshid = 50 AND
        bsyspara.sysgr  = 50 AND
        bsyspara.paranr = 2.
        ASSIGN  
            bSysPara.Parameter1   = "1"
            bSysPara.Beskrivelse  = "DoAuth".
            bSysPara.Hjelpetekst1 = "0-FALSE,1-TRUE"
            .
        RELEASE bSysPara.
    END.
    
    
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 3) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 3
            bSysPara.Parameter1   = "base64"
            bSysPara.Beskrivelse  = "AuthType".
            bSysPara.Hjelpetekst1 = "Type autentisering"
            .
        RELEASE bSysPara.
    END.
    ELSE DO:
      FIND bsyspara EXCLUSIVE-LOCK WHERE
        bsyspara.syshid = 50 AND
        bsyspara.sysgr  = 50 AND
        bsyspara.paranr = 3.
        ASSIGN  
            bSysPara.Parameter1   = "base64"
            bSysPara.Beskrivelse  = "AuthType".
            bSysPara.Hjelpetekst1 = "Type autentisering"
            .
        RELEASE bSysPara.
    END.

    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 4) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 4
            bSysPara.Parameter1   = "prssport1@polygonsoftware.no"
            bSysPara.Beskrivelse  = "User".
            bSysPara.Hjelpetekst1 = "smtpbruker - blank om para 2 = 0"
            .
        RELEASE bSysPara.
    END.
    ELSE DO:
      FIND bsyspara EXCLUSIVE-LOCK WHERE
        bsyspara.syshid = 50 AND
        bsyspara.sysgr  = 50 AND
        bsyspara.paranr = 4.
        ASSIGN  
            bSysPara.Parameter1   = "prssport1@polygonsoftware.no"
            bSysPara.Beskrivelse  = "User".
            bSysPara.Hjelpetekst1 = "smtpbruker - blank om para 2 = 0"
            .
        RELEASE bSysPara.
    END.

    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 5) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 5
            bSysPara.Parameter1   = "2009Sport1"
            bSysPara.Beskrivelse  = "Password".
            bSysPara.Hjelpetekst1 = "blank om para 4 = blank"
            .
        RELEASE bSysPara.
    END.
    ELSE DO:
      FIND bsyspara EXCLUSIVE-LOCK WHERE
        bsyspara.syshid = 50 AND
        bsyspara.sysgr  = 50 AND
        bsyspara.paranr = 5.
        ASSIGN  
            bSysPara.Parameter1   = "2009Sport1"
            bSysPara.Beskrivelse  = "Password".
            bSysPara.Hjelpetekst1 = "blank om para 4 = blank"
            .
        RELEASE bSysPara.
    END.

    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 10) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 10
            bSysPara.Parameter1   = "tomn@polygonsoftware.no"
            bSysPara.Parameter2   = "ken1@polygonsoftware.no"
            bSysPara.Beskrivelse  = "Mottaker nettbutikkordre".
            bSysPara.Hjelpetekst1 = "eMail til mottaker av overføringsordre fra nettbutikk"
            .
        RELEASE bSysPara.
    END.
    ELSE DO:
      FIND bsyspara EXCLUSIVE-LOCK WHERE
        bsyspara.syshid = 50 AND
        bsyspara.sysgr  = 50 AND
        bsyspara.paranr = 10.
        ASSIGN  
            bSysPara.Parameter1   = "tomn@polygonsoftware.no"
            bSysPara.Parameter2   = "ken1@polygonsoftware.no"
            bSysPara.Beskrivelse  = "Mottaker nettbutikkordre".
            bSysPara.Hjelpetekst1 = "eMail til mottaker av overføringsordre fra nettbutikk"
            .
        RELEASE bSysPara.
    END.

    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 20) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 20
            bSysPara.Parameter1   = "supplbestilling@sport1.no"
            bSysPara.Parameter2   = "david@sport1.no"
            bSysPara.Beskrivelse  = "Mottaker suppleringsordre".
            bSysPara.Hjelpetekst1 = "eMail til mottaker av suppleringsordre"
            .
        RELEASE bSysPara.
    END.
    ELSE DO:
      FIND bsyspara EXCLUSIVE-LOCK WHERE
        bsyspara.syshid = 50 AND
        bsyspara.sysgr  = 50 AND
        bsyspara.paranr = 20.
        ASSIGN  
            bSysPara.Parameter1   = "supplbestilling@sport1.no"
            bSysPara.Parameter2   = "david@sport1.no"
            bSysPara.Beskrivelse  = "Mottaker suppleringsordre".
            bSysPara.Hjelpetekst1 = "eMail til mottaker av suppleringsordre"
            .
        RELEASE bSysPara.
    END.

    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 30) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 30
            bSysPara.Parameter1   = "ordreforslag@sport1.no"
            bSysPara.Parameter2   = "david@sport1.no"
            bSysPara.Beskrivelse  = "Mottaker forslag suppleringsordre".
            bSysPara.Hjelpetekst1 = "eMail til mottaker av forslag suppleringsordre"
            .
        RELEASE bSysPara.
    END.
    ELSE DO:
      FIND bsyspara EXCLUSIVE-LOCK WHERE
        bsyspara.syshid = 50 AND
        bsyspara.sysgr  = 50 AND
        bsyspara.paranr = 30.
        ASSIGN  
            bSysPara.Parameter1   = "ordreforslag@sport1.no"
            bSysPara.Parameter2   = "david@sport1.no"
            bSysPara.Beskrivelse  = "Mottaker forslag suppleringsordre".
            bSysPara.Hjelpetekst1 = "eMail til mottaker av forslag suppleringsordre"
            .
        RELEASE bSysPara.
    END.


  END. /* TRANSACTION */
END PROCEDURE.
