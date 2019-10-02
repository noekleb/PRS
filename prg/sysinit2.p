
/*------------------------------------------------------------------------
    File        : sysinit2.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tny
    Created     : Mon Jan 07 09:56:14 CET 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN setGoogleMerchantParameters.
RUN setNavisonIntegrasjon.


/* **********************  Internal Procedures  *********************** */

PROCEDURE setGoogleMerchantParameters:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
EKSTRAFEED:
DO TRANSACTION:
  IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 50 AND
    SysGruppe.SysGr  = 65) THEN
  DO:
    CREATE SysGruppe.
    ASSIGN
        SysGruppe.SysHId      = 50
        SysGruppe.SysGr       = 65
        SysGruppe.Beskrivelse = "Oppsett Google Merchant Center (EkstraFeed)"
        .
    RELEASE SysGruppe.
  END. /* SysGruppe */
  
  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  65 AND
    SysPara.ParaNr = 1 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  65 
          SysPara.ParaNr = 1.
      ASSIGN  
          SysPara.Parameter1   = "0"
          Syspara.Beskrivelse  = "Aktiv?"
          SysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  65 AND
    SysPara.ParaNr = 2 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  65 
          SysPara.ParaNr = 2.
      ASSIGN  
          SysPara.Parameter1   = "GoogleMerchantEkstra.log"
          SysPara.Parameter2   = 'konv\'
          Syspara.Beskrivelse  = "Loggfil"
          SysPara.Hjelpetekst1 = "Navn på loggfil"
          SysPara.Hjelpetekst2 = "Katalog for loggfil"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  65 AND
    SysPara.ParaNr = 3 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  65 
          SysPara.ParaNr = 3.
      ASSIGN  
          SysPara.Parameter1   = "GoogleMerchantEkstraDDMMYY_HHMMSS.xml"
          SysPara.Parameter2   = 'konv\'
          Syspara.Beskrivelse  = "Eksportfil"
          SysPara.Hjelpetekst1 = "Navn på eksportfil"
          SysPara.Hjelpetekst2 = "Katalog for eksportfil"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  65 AND
    SysPara.ParaNr = 4 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  65 
          SysPara.ParaNr = 4.
      ASSIGN  
          SysPara.Parameter1   = ""
          Syspara.Beskrivelse  = "Butikkliste (Komma separert)"
          SysPara.Hjelpetekst1 = "Liste med butikker hvor lagerendringer skal logges."
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  65 AND
    SysPara.ParaNr = 5 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  65 
          SysPara.ParaNr = 5.
      ASSIGN  
          Syspara.Beskrivelse  = "EkstraFeeder filnavn"
          SysPara.Parameter1   = ""
          SysPara.Hjelpetekst1 = "Filnavn som er angitt i feeder definisjonen."
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  65 AND
    SysPara.ParaNr = 6 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  65 
          SysPara.ParaNr = 6.
      ASSIGN  
          Syspara.Beskrivelse  = "FTP Fil prefix og suffix"
          SysPara.Parameter1   = "GoogleMerchant"
          SysPara.Parameter2   = ".xml"
          SysPara.Hjelpetekst1 = "Prefix på eksportfil angitt i parameter 3."
          SysPara.Hjelpetekst1 = "Fil suffix. Normalt '.xml' - Husk '.' skal med i suffixet."
          .
      RELEASE SysPara.
    END.
  
END. /* EKSTRAFEED TRANSACTION */

HOVEDFEED:
DO TRANSACTION:
  IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 50 AND
    SysGruppe.SysGr  = 66) THEN
  DO:
    CREATE SysGruppe.
    ASSIGN
        SysGruppe.SysHId      = 50
        SysGruppe.SysGr       = 66
        SysGruppe.Beskrivelse = "Oppsett Google Merchant Center (HovedFeed)"
        .
    RELEASE SysGruppe.
  END. /* SysGruppe */
  
  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  66 AND
    SysPara.ParaNr = 1 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  66 
          SysPara.ParaNr = 1.
      ASSIGN  
          SysPara.Parameter1   = "0"
          Syspara.Beskrivelse  = "Aktiv?"
          SysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  66 AND
    SysPara.ParaNr = 2 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  66 
          SysPara.ParaNr = 2.
      ASSIGN  
          SysPara.Parameter1   = "GoogleMerchantHoved.log"
          SysPara.Parameter2   = 'konv\'
          Syspara.Beskrivelse  = "Loggfil"
          SysPara.Hjelpetekst1 = "Navn på loggfil"
          SysPara.Hjelpetekst2 = "Katalog for loggfil"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  66 AND
    SysPara.ParaNr = 3 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  66 
          SysPara.ParaNr = 3.
      ASSIGN  
          SysPara.Parameter1   = "GoogleMerchantHovedDDMMYY_HHMMSS.xml"
          SysPara.Parameter2   = 'konv\'
          Syspara.Beskrivelse  = "Eksportfil"
          SysPara.Hjelpetekst1 = "Navn på eksportfil"
          SysPara.Hjelpetekst2 = "Katalog for eksportfil"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  66 AND
    SysPara.ParaNr = 4 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  66 
          SysPara.ParaNr = 4.
      ASSIGN  
          SysPara.Parameter1   = ""
          SysPara.Parameter2   = ""
          Syspara.Beskrivelse  = "Liste med prisprofiler (Komma separert)"
          SysPara.Hjelpetekst1 = "Liste med prisprofiler hvor vareendringer skal logges."
          SysPara.Hjelpetekst1 = "Butikk som skal settes i xml filen.."
          .
      RELEASE SysPara.
    END.
  
  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  66 AND
    SysPara.ParaNr = 5 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  66 
          SysPara.ParaNr = 5.
      ASSIGN  
          Syspara.Beskrivelse  = "HovedFeeder filnavn"
          SysPara.Parameter1   = ""
          SysPara.Hjelpetekst1 = "Filnavn som er angitt i feeder definisjonen."
          .
      RELEASE SysPara.
    END.
END. /* HOVEDFEED TRANSACTION */

FTPFEED:
DO TRANSACTION:
  IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 50 AND
    SysGruppe.SysGr  = 67) THEN
  DO:
    CREATE SysGruppe.
    ASSIGN
        SysGruppe.SysHId      = 50
        SysGruppe.SysGr       = 67
        SysGruppe.Beskrivelse = "Oppsett Google Merchant Center (FTP Feeder)"
        .
    RELEASE SysGruppe.
  END. /* SysGruppe */

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  67 AND
    SysPara.ParaNr = 1 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  67 
          SysPara.ParaNr = 1.
      ASSIGN  
          SysPara.Parameter1   = "0"
          Syspara.Beskrivelse  = "Aktiv?"
          SysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
          .
      RELEASE SysPara.
    END.
  
  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  67 AND
    SysPara.ParaNr = 2 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  67 
          SysPara.ParaNr = 2.
      ASSIGN  
          Syspara.Beskrivelse  = "Tjener"
          SysPara.Parameter1   = ""
          SysPara.Hjelpetekst1 = "Ip adresse eller hostname på tjener"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  67 AND
    SysPara.ParaNr = 3 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  67 
          SysPara.ParaNr = 3.
      ASSIGN  
          Syspara.Beskrivelse  = "Brukernavn"
          SysPara.Parameter1   = ""
          SysPara.Hjelpetekst1 = "Navn på FTP bruker"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  67 AND
    SysPara.ParaNr = 4 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  67 
          SysPara.ParaNr = 4.
      ASSIGN  
          Syspara.Beskrivelse  = "Passord"
          SysPara.Parameter1   = ""
          SysPara.Hjelpetekst1 = "Passord til FTP kontoen"
          .
      RELEASE SysPara.
    END.
END. /* FTPFEED TRANSACTION */

SFTPFEED:
DO TRANSACTION:
  IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 50 AND
    SysGruppe.SysGr  = 68) THEN
  DO:
    CREATE SysGruppe.
    ASSIGN
        SysGruppe.SysHId      = 50
        SysGruppe.SysGr       = 68
        SysGruppe.Beskrivelse = "Oppsett Google Merchant Center (SFTP Feeder)"
        .
    RELEASE SysGruppe.
  END. /* SysGruppe */

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  68 AND
    SysPara.ParaNr = 1 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  68 
          SysPara.ParaNr = 1.
      ASSIGN  
          SysPara.Parameter1   = "0"
          Syspara.Beskrivelse  = "Aktiv?"
          SysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
          .
      RELEASE SysPara.
    END.
  
  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  68 AND
    SysPara.ParaNr = 2 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  68 
          SysPara.ParaNr = 2.
      ASSIGN  
          Syspara.Beskrivelse  = "Tjener"
          SysPara.Parameter1   = ""
          SysPara.Hjelpetekst1 = "Ip adresse eller hostname på tjener"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  68 AND
    SysPara.ParaNr = 3 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  68 
          SysPara.ParaNr = 3.
      ASSIGN  
          Syspara.Beskrivelse  = "Brukernavn"
          SysPara.Parameter1   = ""
          SysPara.Hjelpetekst1 = "Navn på FTP bruker"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  68 AND
    SysPara.ParaNr = 4 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  68 
          SysPara.ParaNr = 4.
      ASSIGN  
          Syspara.Beskrivelse  = "Passord"
          SysPara.Parameter1   = ""
          SysPara.Hjelpetekst1 = "Passord til FTP kontoen"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  68 AND
    SysPara.ParaNr = 5 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  68 
          SysPara.ParaNr = 5.
      ASSIGN  
          Syspara.Beskrivelse  = "Port"
          SysPara.Parameter1   = ""
          SysPara.Hjelpetekst1 = "Portnummer"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  68 AND
    SysPara.ParaNr = 6 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  68 
          SysPara.ParaNr = 6.
      ASSIGN  
          Syspara.Beskrivelse  = "Fingeravtrykk"
          SysPara.Parameter1   = ""
          SysPara.Hjelpetekst1 = "Fingeravtrykk"
          .
      RELEASE SysPara.
    END.
END. /* SFTPFEED TRANSACTION */

END PROCEDURE.

PROCEDURE setNavisonIntegrasjon:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
NAVISION:
DO TRANSACTION:
  IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 50 AND
    SysGruppe.SysGr  = 55) THEN
  DO:
    CREATE SysGruppe.
    ASSIGN
        SysGruppe.SysHId      = 50
        SysGruppe.SysGr       = 55
        SysGruppe.Beskrivelse = "Oppsett Navision eksport"
        .
    RELEASE SysGruppe.
  END. /* SysGruppe */
  
  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  55 AND
    SysPara.ParaNr = 1 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  55 
          SysPara.ParaNr = 1.
      ASSIGN  
          SysPara.Parameter1   = "0"
          Syspara.Beskrivelse  = "Aktiv?"
          SysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  55 AND
    SysPara.ParaNr = 2 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  55 
          SysPara.ParaNr = 2.
      ASSIGN  
          Syspara.Beskrivelse  = "Dagsoppgjørsfiler"
          SysPara.Parameter1   = "DagsoppgjDDMMYYYY_HHMMSS.csv"
          SysPara.Parameter2   = 'konv\Nav'
          SysPara.Hjelpetekst1 = "Navn på loggfil"
          SysPara.Hjelpetekst2 = "Katalog for loggfil"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  55 AND
    SysPara.ParaNr = 3 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  55 
          SysPara.ParaNr = 3.
      ASSIGN  
          Syspara.Beskrivelse  = "Fakturafiler"
          SysPara.Parameter1   = "FakturalisteDDMMYYYY_HHMMSS.csv"
          SysPara.Parameter2   = 'konv\Navn'
          SysPara.Hjelpetekst1 = "Navn på loggfil"
          SysPara.Hjelpetekst2 = "Katalog for loggfil"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  55 AND
    SysPara.ParaNr = 4 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  55 
          SysPara.ParaNr = 4.
      ASSIGN  
          Syspara.Beskrivelse  = "EksportType"
          SysPara.Parameter1   = "0"
          SysPara.Hjelpetekst1 = "Eksporttype identifiserer hvilket record format utleggene skal ha."
          SysPara.Hjelpetekst1 = "1-Type1,2-type2 osv."
          .
      RELEASE SysPara.
    END.
END. /* NAVISION */


END PROCEDURE.

