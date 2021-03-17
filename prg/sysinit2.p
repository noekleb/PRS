
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
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE cIdLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTxtLst AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN setGoogleMerchantParameters.
RUN setNavisonIntegrasjon.
RUN setPakkseddelInfo.
RUN setKomInfo.
RUN setBatchJobber.
RUN setEMailAvsender.
RUN setBokforingsbilag.
RUN setVarselDbSjekk.
RUN setVarselEODSjekk.
RUN setVarselDagsoppgjor.
RUN setKundespesifikkeParam.
RUN setPosBongOverfort.
RUN setReservasjonsStatus.
RUN setKOrdreTransportStatus.
RUN setCRMSystem.
RUN setRabattKontrollRetur.
RUN setDinteroParametre.
RUN setSMSParametre.
RUN setEDIParam.
RUN PkSdlImport.
RUN setFTP.

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
          Syspara.Beskrivelse  = "Aktivere filutlegg av dagsoppgjør?"
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
          SysPara.Parameter1   = "DagsoppgjDDMMYYYY_HHMMSS.json"
          SysPara.Parameter2   = 'konv\Nav'
          SysPara.Hjelpetekst1 = "Navn på loggfil"
          SysPara.Hjelpetekst2 = "Katalog for loggfil"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  55 AND
    SysPara.ParaNr = 5 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  55 
          SysPara.ParaNr = 5.
      ASSIGN  
          Syspara.Beskrivelse  = "Aktivere utlegg til Eyescan?"
          SysPara.Parameter1   = "0"
          SysPara.Parameter2   = ''
          SysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
          SysPara.Hjelpetekst2 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  55 AND
    SysPara.ParaNr = 6 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  55 
          SysPara.ParaNr = 6.
      ASSIGN  
          Syspara.Beskrivelse  = "eMail mottagere faktura"
          SysPara.Parameter1   = "0"
          SysPara.Parameter2   = 'tomn@nsoft.no'
          SysPara.Hjelpetekst1 = "0-Ikke aktiv, 1-Aktiv"
          SysPara.Hjelpetekst2 = "eMail liste med mottagere av faktura."
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  55 AND
    SysPara.ParaNr = 10 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  55 
          SysPara.ParaNr = 10.
      ASSIGN  
          SysPara.Parameter1   = "0"
          Syspara.Beskrivelse  = "Aktivere filutlegg av fakturadata?"
          SysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  55 AND
    SysPara.ParaNr = 11 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  55 
          SysPara.ParaNr = 11.
      ASSIGN  
          Syspara.Beskrivelse  = "Fakturafil"
          SysPara.Parameter1   = "CREDIT&SeqNr.txt"
          SysPara.Parameter2   = '\\gant0047\Felles\GantGlobal\Faktura'
          SysPara.Hjelpetekst1 = "Navn på faktura eksportfil"
          SysPara.Hjelpetekst2 = "Katalog for faktura eksportfil"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  55 AND
    SysPara.ParaNr = 12 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  55 
          SysPara.ParaNr = 12.
      ASSIGN  
          Syspara.Beskrivelse  = "Sekvensnr fakturafil"
          SysPara.Parameter1   = "0000"
          SysPara.Parameter2   = ''
          SysPara.Hjelpetekst1 = "Sekvensnummer - 4 siffer."
          SysPara.Hjelpetekst2 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  55 AND
    SysPara.ParaNr = 14 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  55 
          SysPara.ParaNr = 14.
      ASSIGN  
          Syspara.Beskrivelse  = "Liste butikkmottagere (for Kunde)"
          SysPara.Parameter1   = "1,20"
          SysPara.Parameter2   = '0'
          SysPara.Hjelpetekst1 = "Er denne <> '', skal faktura bare eksporteres for disse mottagende butikkene."
          SysPara.Hjelpetekst2 = "0=Nei, 1=Ja Eksporter faktura for kommisjonsbutikker"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  55 AND
    SysPara.ParaNr = 15 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  55 
          SysPara.ParaNr = 15.
      ASSIGN  
          Syspara.Beskrivelse  = "Liste eksterne leverandører"
          SysPara.Parameter1   = "" /* 99,501,502,504 for GANT */
          SysPara.Parameter2   = ''
          SysPara.Hjelpetekst1 = "Faktura med varer fra disse leverandørene skal ha positiv verdi i eksport."
          SysPara.Hjelpetekst2 = ""
          .
      RELEASE SysPara.
    END.

END. /* NAVISION */


END PROCEDURE.

PROCEDURE setPakkseddelInfo:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
   
------------------------------------------------------------------------------*/

  IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 22 AND
    SysGruppe.SysGr  = 30) THEN
  DO:
    CREATE SysGruppe.
    ASSIGN
        SysGruppe.SysHId      = 22
        SysGruppe.SysGr       = 30
        SysGruppe.Beskrivelse = "Oppsett SendtOutlet koder"
        .
    RELEASE SysGruppe.
  END. /* SysGruppe */

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 22 AND
    SysPara.SysGr  = 30 AND
    SysPara.ParaNr = 1 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 22 
          SysPara.SysGr  = 30 
          SysPara.ParaNr = 1.
      ASSIGN  
          Syspara.Beskrivelse  = "Ikke tildelt"
          SysPara.Parameter1   = "0"
          SysPara.Hjelpetekst1 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 22 AND
    SysPara.SysGr  = 30 AND
    SysPara.ParaNr = 2 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 22 
          SysPara.SysGr  = 30 
          SysPara.ParaNr = 2.
      ASSIGN  
          Syspara.Beskrivelse  = "Tilgjengelig"
          SysPara.Parameter1   = "1"
          SysPara.Hjelpetekst1 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 22 AND
    SysPara.SysGr  = 30 AND
    SysPara.ParaNr = 3 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 22 
          SysPara.SysGr  = 30 
          SysPara.ParaNr = 3.
      ASSIGN  
          Syspara.Beskrivelse  = "Skal ikke sendes"
          SysPara.Parameter1   = "2"
          SysPara.Hjelpetekst1 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 22 AND
    SysPara.SysGr  = 30 AND
    SysPara.ParaNr = 4 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 22 
          SysPara.SysGr  = 30 
          SysPara.ParaNr = 4.
      ASSIGN  
          Syspara.Beskrivelse  = "Sendt Vestby"
          SysPara.Parameter1   = "3"
          SysPara.Hjelpetekst1 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 22 AND
    SysPara.SysGr  = 30 AND
    SysPara.ParaNr = 5 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 22 
          SysPara.SysGr  = 30 
          SysPara.ParaNr = 5.
      ASSIGN  
          Syspara.Beskrivelse  = "Sendt Algard"
          SysPara.Parameter1   = "4"
          SysPara.Hjelpetekst1 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 22 AND
    SysPara.SysGr  = 30 AND
    SysPara.ParaNr = 6 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 22 
          SysPara.SysGr  = 30 
          SysPara.ParaNr = 6.
      ASSIGN  
          Syspara.Beskrivelse  = "Sendt eCom"
          SysPara.Parameter1   = "5"
          SysPara.Hjelpetekst1 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 22 AND
    SysPara.SysGr  = 30 AND
    SysPara.ParaNr = 7 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 22 
          SysPara.SysGr  = 30 
          SysPara.ParaNr = 7.
      ASSIGN  
          Syspara.Beskrivelse  = "Avvent"
          SysPara.Parameter1   = "6"
          SysPara.Hjelpetekst1 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 22 AND
    SysPara.SysGr  = 30 AND
    SysPara.ParaNr = 8 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 22 
          SysPara.SysGr  = 30 
          SysPara.ParaNr = 8.
      ASSIGN  
          Syspara.Beskrivelse  = "Bestillt Vestby"
          SysPara.Parameter1   = "10"
          SysPara.Hjelpetekst1 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 22 AND
    SysPara.SysGr  = 30 AND
    SysPara.ParaNr = 9 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 22 
          SysPara.SysGr  = 30 
          SysPara.ParaNr = 9.
      ASSIGN  
          Syspara.Beskrivelse  = "Bestillt Algard"
          SysPara.Parameter1   = "11"
          SysPara.Hjelpetekst1 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 22 AND
    SysPara.SysGr  = 5 AND
    SysPara.ParaNr = 4 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 22 
          SysPara.SysGr  = 5 
          SysPara.ParaNr = 4.
      ASSIGN  
          SysPara.Parameter1   = "0"
          SysPara.Parameter2   = ""
          Syspara.Beskrivelse  = "Sist benyttede palleNr"
          SysPara.Hjelpetekst1 = "Siste brukte pallenr. Brukt ved automatisk tildeling av pallenr."
          SysPara.Hjelpetekst2 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  22 AND
    SysPara.SysGr  =  1 AND
    SysPara.ParaNr = 10 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  22 
          SysPara.SysGr  =  1 
          SysPara.ParaNr = 10.
      ASSIGN  
          SysPara.Parameter1   = "0"
          SysPara.Parameter2   = ""
          Syspara.Beskrivelse  = "Varsle mottak av suppleringsordre"
          SysPara.Hjelpetekst1 = "0-Ikke aktiv, 1-Aktiver varsling via email"
          SysPara.Hjelpetekst2 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  22 AND
    SysPara.SysGr  =  1 AND
    SysPara.ParaNr = 11 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  22 
          SysPara.SysGr  =  1 
          SysPara.ParaNr = 11.
      ASSIGN  
          SysPara.Parameter1   = "2,3,4,5,6,7,8,9,11,12,13,17,18"
          SysPara.Parameter2   = "nath@gant.no;tomn@nsoft.no"
          Syspara.Beskrivelse  = "Varsling for butikker"
          SysPara.Hjelpetekst1 = "Butikkliste"
          SysPara.Hjelpetekst2 = "eMail til mottager"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  22 AND
    SysPara.SysGr  =  1 AND
    SysPara.ParaNr = 12 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  22 
          SysPara.SysGr  =  1 
          SysPara.ParaNr = 12.
      ASSIGN  
          SysPara.Parameter1   = "16"
          SysPara.Parameter2   = "svein-inge@gantretail.no;tomn@nsoft.no"
          Syspara.Beskrivelse  = "Varsling for nettbutikker"
          SysPara.Hjelpetekst1 = "Butikkliste"
          SysPara.Hjelpetekst2 = "eMail til mottager"
          .
      RELEASE SysPara.
    END.

END PROCEDURE.

PROCEDURE setKomInfo:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  50 AND
    SysPara.ParaNr = 50 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  50 
          SysPara.ParaNr = 50.
      ASSIGN  
          SysPara.Parameter1   = "henrikh@gant.no;gant.outlet@gantretail.no;freestock@gant.no;are@gant.no;tomn@nsoft.no"
          SysPara.Parameter2   = "gant.stavanger@gantretail.no;gant.outlet@gantretail.no;freestock@gant.no;are@gant.no;tomn@nsoft.no"
          Syspara.Beskrivelse  = "Varebestilling 0utlet"
          SysPara.Hjelpetekst1 = "eMail til mottagere Outlet 10"
          SysPara.Hjelpetekst2 = "eMail til mottagere Oultet 40"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  50 AND
    SysPara.ParaNr = 51 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  50 
          SysPara.ParaNr = 51.
      ASSIGN  
          SysPara.Parameter1   = "tomn@nsoft.no"
          Syspara.Beskrivelse  = "Mailmottager DataMottak varsel"
          SysPara.Hjelpetekst1 = "eMail til mottagere."
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  50 AND
    SysPara.ParaNr = 52 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  50 
          SysPara.ParaNr = 52.
      ASSIGN  
          SysPara.Parameter1   = "tomn@nsoft.no;are@gant.no"
          Syspara.Beskrivelse  = "Mailmottager - varsel EDIWeb fil import"
          SysPara.Hjelpetekst1 = "eMail til mottagere."
          .
      RELEASE SysPara.
    END.

END PROCEDURE.

PROCEDURE setBatchJobber:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 200 AND
    SysPara.SysGr  = 1 AND
    SysPara.ParaNr = 8 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 200 
          SysPara.SysGr  = 1 
          SysPara.ParaNr = 8.
      ASSIGN  
          SysPara.Parameter1   = "1"
          Syspara.Beskrivelse  = "1 Eller <Blank> kjør DatAMottakGUI, 2 Kjør via AppServer"
          SysPara.Hjelpetekst1 = "eMail til mottagere."
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 200 AND
    SysPara.SysGr  = 1 AND
    SysPara.ParaNr = 9 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 200 
          SysPara.SysGr  = 1 
          SysPara.ParaNr = 9.
      ASSIGN  
          SysPara.Parameter1   = "C:\home\lindbak\ankommet"
          Syspara.Beskrivelse  = "Filkatalog for innlesning av bonger via AppServer"
          SysPara.Hjelpetekst1 = "Filkatalog for bonger fra kassene"
          .
      RELEASE SysPara.
    END.

END PROCEDURE.

PROCEDURE setEMailAvsender:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
   
------------------------------------------------------------------------------*/

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 50 AND
    SysPara.SysGr  = 50 AND
    SysPara.ParaNr = 60 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 50 
          SysPara.SysGr  = 50 
          SysPara.ParaNr = 60.
      ASSIGN  
          SysPara.Parameter1   = "0"
          Syspara.Beskrivelse  = "Aktiver alternativ eMail avsender"
          SysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 50 AND
    SysPara.SysGr  = 50 AND
    SysPara.ParaNr = 61 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 50 
          SysPara.SysGr  = 50 
          SysPara.ParaNr = 61.
      ASSIGN  
          SysPara.Parameter1   = "support@GANT.NO"
          Syspara.Beskrivelse  = "eMail: FromADDRESS"
          SysPara.Hjelpetekst1 = "Avsender eMail adresse"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 50 AND
    SysPara.SysGr  = 50 AND
    SysPara.ParaNr = 62 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 50 
          SysPara.SysGr  = 50 
          SysPara.ParaNr = 62.
      ASSIGN  
          SysPara.Parameter1   = "smtp.office365.com:587"
          Syspara.Beskrivelse  = "eMail: SERVER"
          SysPara.Hjelpetekst1 = "Avsender server."
          .
      RELEASE SysPara.
    END.
    
  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 50 AND
    SysPara.SysGr  = 50 AND
    SysPara.ParaNr = 63 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 50 
          SysPara.SysGr  = 50 
          SysPara.ParaNr = 63.
      ASSIGN  
          SysPara.Parameter1   = "support@gant.no"
          Syspara.Beskrivelse  = "eMail: USERNAME"
          SysPara.Hjelpetekst1 = "Avsender bruker."
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 50 AND
    SysPara.SysGr  = 50 AND
    SysPara.ParaNr = 64 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 50 
          SysPara.SysGr  = 50 
          SysPara.ParaNr = 64.
      ASSIGN  
          SysPara.Parameter1   = "6431Gino!!"
          Syspara.Beskrivelse  = "eMail: PASSWORD"
          SysPara.Hjelpetekst1 = "Avsender passord."
          .
      RELEASE SysPara.
    END.
    
END PROCEDURE.

PROCEDURE setBokforingsbilag:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
   6550 mindre anskaffelser
6940 porto
6810 kontorrekvisita
2750 ørediffer ( denne kan vel du fikse?)
4130 skredder
   
------------------------------------------------------------------------------*/

  IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 20 AND
    SysGruppe.SysGr  = 6) THEN
  DO:
    CREATE SysGruppe.
    ASSIGN
        SysGruppe.SysHId      = 20
        SysGruppe.SysGr       = 6
        SysGruppe.Beskrivelse = "Kontering av korreksjonstransaksjoner bokføringsbilag"
        .
    RELEASE SysGruppe.
  END. /* SysGruppe */

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 20 AND
    SysPara.SysGr  = 6 AND
    SysPara.ParaNr = 1 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 20 
          SysPara.SysGr  = 6 
          SysPara.ParaNr = 1.
      ASSIGN  
          SysPara.Parameter1   = "6550"
          Syspara.Beskrivelse  = "Mindre anskaffelser"
          SysPara.Hjelpetekst1 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 20 AND
    SysPara.SysGr  = 6 AND
    SysPara.ParaNr = 2 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 20 
          SysPara.SysGr  = 6 
          SysPara.ParaNr = 2.
      ASSIGN  
          SysPara.Parameter1   = "6940"
          Syspara.Beskrivelse  = "Porto"
          SysPara.Hjelpetekst1 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 20 AND
    SysPara.SysGr  = 6 AND
    SysPara.ParaNr = 3 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 20 
          SysPara.SysGr  = 6 
          SysPara.ParaNr = 3.
      ASSIGN  
          SysPara.Parameter1   = "6810"
          Syspara.Beskrivelse  = "Kontorrekvisita"
          SysPara.Hjelpetekst1 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 20 AND
    SysPara.SysGr  = 6 AND
    SysPara.ParaNr = 4 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 20 
          SysPara.SysGr  = 6 
          SysPara.ParaNr = 4.
      ASSIGN  
          SysPara.Parameter1   = "2750"
          Syspara.Beskrivelse  = "Øresavrunding"
          SysPara.Hjelpetekst1 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 20 AND
    SysPara.SysGr  = 6 AND
    SysPara.ParaNr = 5 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 20 
          SysPara.SysGr  = 6 
          SysPara.ParaNr = 5.
      ASSIGN  
          SysPara.Parameter1   = "4130"
          Syspara.Beskrivelse  = "Skredder"
          SysPara.Hjelpetekst1 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 20 AND
    SysPara.SysGr  = 6 AND
    SysPara.ParaNr = 6 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 20 
          SysPara.SysGr  = 6 
          SysPara.ParaNr = 6.
      ASSIGN  
          SysPara.Parameter1   = "8190"
          Syspara.Beskrivelse  = "Gebyr"
          SysPara.Hjelpetekst1 = ""
          .
      RELEASE SysPara.
    END.

END PROCEDURE.

PROCEDURE setVarselDbSjekk:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 50 AND
    SysPara.SysGr  = 50 AND
    SysPara.ParaNr = 53 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 50 
          SysPara.SysGr  = 50 
          SysPara.ParaNr = 53.
      ASSIGN  
          Syspara.Beskrivelse  = "Mailmottager: Varsel DBSjekk"
          SysPara.Parameter1   = "are@gant.no;tomn@nsoft.no;ken1@polygon.se"
          SysPara.Hjelpetekst1 = "Liste med mottagere av mail som varsler full db ekstent."
          .
      RELEASE SysPara.
    END.

END PROCEDURE.

PROCEDURE setVarselEODSjekk:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 50 AND
    SysPara.SysGr  = 50 AND
    SysPara.ParaNr = 54 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 50 
          SysPara.SysGr  = 50 
          SysPara.ParaNr = 54.
      ASSIGN  
          Syspara.Beskrivelse  = "Mailmottager: Varsel EODSjekk"
          SysPara.Parameter1   = "are@gant.no;tomn@nsoft.no;ken1@polygon.se"
          SysPara.Hjelpetekst1 = "Liste med mottagere av mail som manglende EOD for en eller flere kasser."
          .
      RELEASE SysPara.
    END.

END PROCEDURE.

PROCEDURE setVarselDagsoppgjor:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 50 AND
    SysPara.SysGr  = 50 AND
    SysPara.ParaNr = 55 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 50 
          SysPara.SysGr  = 50 
          SysPara.ParaNr = 55.
      ASSIGN  
          Syspara.Beskrivelse  = "Mailmottager: Varsel DagsoppgjørSjekk"
          SysPara.Parameter1   = "are@gant.no;tomn@nsoft.no"
          SysPara.Hjelpetekst1 = "Liste med mottagere av varselmail om manglende dagsoppgjor for en eller flere butikker."
          .
      RELEASE SysPara.
    END.

END PROCEDURE.

PROCEDURE setKundespesifikkeParam:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 210 AND
    SysPara.SysGr  = 260 AND
    SysPara.ParaNr = 20 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 210 
          SysPara.SysGr  = 260 
          SysPara.ParaNr = 20.
      ASSIGN  
          Syspara.Beskrivelse  = "GANT Aktiv"
          SysPara.Parameter1   = "0"
          SysPara.Hjelpetekst1 = "Aktiverer spesialtilpasning for GANT."
          .
      RELEASE SysPara.
    END.
END PROCEDURE.

PROCEDURE setPosBongOverfort:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  IF NOT CAN-FIND(SysHode WHERE
      SysHode.SysHId = 201) THEN
  DO:
      CREATE SysHode.
      ASSIGN
          SysHode.SysHId  = 201
          Beskrivelse      = "POSlogg send/hent bonger"
          .
      RELEASE SysHode.
  END. /* SysGruppe TRANSACTION */

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 50 AND
    SysPara.SysGr  = 50 AND
    SysPara.ParaNr = 56 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 50 
          SysPara.SysGr  = 50 
          SysPara.ParaNr = 56.
      ASSIGN  
          Syspara.Beskrivelse  = "Mailmottager: Varsel DagsoppgjørSjekk"
          SysPara.Parameter1   = "are@gant.no;tomn@nsoft.no"
          SysPara.Hjelpetekst1 = "Liste med mottagere av varselmail om stopp i kassekommunikasjonen."
          .
      RELEASE SysPara.
    END.

END PROCEDURE.

PROCEDURE setReservasjonsStatus:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  ASSIGN 
    cIdLst  = '10,20,30,40,50,60'
    cTxtLst = '10 Ny,' +
              '20 Sendt eCom,' +
              '30 Godkjent eCom,' +
              '40 Avvist eCom,' + 
              '50 Utlevert kunde,' + 
              '60 Ferdigbehandlet'
    .
  IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 11 AND
    SysGruppe.SysGr  = 10) THEN
  DO:
    CREATE SysGruppe.
    ASSIGN
        SysGruppe.SysHId      = 11
        SysGruppe.SysGr       = 10
        SysGruppe.Beskrivelse = "Behandlingsstatus reservasjonsordre"
        .
    RELEASE SysGruppe.
  END. /* SysGruppe */

  DO iLoop = 1 TO NUM-ENTRIES(cIdLst):
      FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId = 11 AND
        SysPara.SysGr  = 10 AND
        SysPara.ParaNr = INT(ENTRY(iLoop,cIdLst)) NO-ERROR.
      IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE SysPara.
          ASSIGN
              SysPara.SysHId = 11 
              SysPara.SysGr  = 10 
              SysPara.ParaNr = INT(ENTRY(iLoop,cIdLst)).
          ASSIGN  
              Syspara.Beskrivelse  = ENTRY(iLoop,cTxtLst)
              SysPara.Parameter1   = ENTRY(iLoop,cTxtLst)
              SysPara.Hjelpetekst1 = ""
              .
          RELEASE SysPara.
        END.
  END.

END PROCEDURE.

PROCEDURE setKOrdreTransportStatus:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
  - *10 At Wharehouse*
    o PRS Status < 50 - Er mottatt og behandles på eCom.
  - *20 In Transit*
    o PRS Status 50 - Utlevert fra eCom
  - *30 Awaiting Pickup*
    o PRS Status 50 - Kvittert mottatt i butikk
  - *40 Pickup Overdue*
    o PRS Status 50 - Lagt for lenge i butikk uten å bli hentet (14 dager?)
  - *50 Complete*
    o PRS Staatus 50 - Utlevert til kunde
   
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcTekst1 AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcTekst2 AS CHARACTER NO-UNDO.
  
  ASSIGN 
    pctekst1 = '10,20,30,40,50'
    pcTekst2 = 'At Wharehouse,In Transit,Awaiting Pickup,Pickup Overdue,Complete'
    .
  
  IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 150 AND
    SysGruppe.SysGr  = 20) THEN
  DO:
    CREATE SysGruppe.
    ASSIGN
        SysGruppe.SysHId      = 150
        SysGruppe.SysGr       = 20
        SysGruppe.Beskrivelse = "Transportstatus Click&Collect ordre"
        .
    RELEASE SysGruppe.
  END. /* SysGruppe */
  
  DO iLoop = 1 TO NUM-ENTRIES(pcTekst1):
    IF NOT CAN-FIND(FIRST SysPara WHERE
        SysPara.SysHId =  150 AND
        SysPara.SysGr  =  20 AND
        SysPara.ParaNr = INT(ENTRY(iLoop,pcTekst1))) THEN 
    DO:
      FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId =  150 AND
        SysPara.SysGr  =  20 AND
        SysPara.ParaNr = INT(ENTRY(iLoop,pcTekst1)) NO-ERROR.
      IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE SysPara.
          ASSIGN
              SysPara.SysHId =  150 
              SysPara.SysGr  =  20 
              SysPara.ParaNr = INT(ENTRY(iLoop,pcTekst1)).
          ASSIGN  
              SysPara.Parameter1   = ENTRY(iLoop,pcTekst1)
              Syspara.Beskrivelse  = ENTRY(iLoop,pcTekst2)
              SysPara.Hjelpetekst1 = ''
              .
          RELEASE SysPara.
        END.
    END.
    
  END.
  


END PROCEDURE.

PROCEDURE setCRMSystem:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE pcTekst1 AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcTekst2 AS CHARACTER NO-UNDO.
  
  ASSIGN 
    pctekst1 = '0,1,0' /* Mayflower er default. */
    pcTekst2 = 'Ikke tilkoblet,MayFlower,Dintero'
    .
  
  IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 14 AND
    SysGruppe.SysGr  = 200) THEN
  DO:
    CREATE SysGruppe.
    ASSIGN
        SysGruppe.SysHId      = 14
        SysGruppe.SysGr       = 200
        SysGruppe.Beskrivelse = "Eksternt CRM system"
        .
    RELEASE SysGruppe.
  END. /* SysGruppe */
  
  DO iLoop = 1 TO NUM-ENTRIES(pcTekst1):
    IF NOT CAN-FIND(FIRST SysPara WHERE
        SysPara.SysHId =  14 AND
        SysPara.SysGr  =  200 AND
        SysPara.ParaNr = iLoop) THEN 
    DO:
      FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId =  14 AND
        SysPara.SysGr  =  200 AND
        SysPara.ParaNr = iLoop NO-ERROR.
      IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE SysPara.
          ASSIGN
              SysPara.SysHId =  14 
              SysPara.SysGr  =  200 
              SysPara.ParaNr = iLoop.
          ASSIGN  
              SysPara.Parameter1   = ENTRY(iLoop,pcTekst1)
              Syspara.Beskrivelse  = ENTRY(iLoop,pcTekst2)
              SysPara.Hjelpetekst1 = '0-Ikke aktivt,1=Aktivt. Bare et system skal stå som aktivt.'
              .
          RELEASE SysPara.
        END.
    END.
    
  END.

END PROCEDURE.

PROCEDURE setRabattKontrollRetur:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 11 AND
    SysGruppe.SysGr  = 100) THEN
  DO:
    CREATE SysGruppe.
    ASSIGN
        SysGruppe.SysHId      = 11
        SysGruppe.SysGr       = 100
        SysGruppe.Beskrivelse = "Rabattkontroll returer"
        .
    RELEASE SysGruppe.
  END. /* SysGruppe */

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  11 AND
    SysPara.SysGr  =  100 AND
    SysPara.ParaNr = 1 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 11 
          SysPara.SysGr  = 100 
          SysPara.ParaNr = 1.
      ASSIGN  
          SysPara.Parameter1   = '0'
          Syspara.Beskrivelse  = 'Aktiver rabatt kontroll returer'
          SysPara.Hjelpetekst1 = '0-Ikke aktivt,1=Aktivt.'
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  11 AND
    SysPara.SysGr  =  100 AND
    SysPara.ParaNr = 2 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 11 
          SysPara.SysGr  = 100 
          SysPara.ParaNr = 2.
      ASSIGN  
          SysPara.Parameter1   = '180'
          Syspara.Beskrivelse  = 'Antall dager i innkjøpsperiode'
          SysPara.Hjelpetekst1 = 'Normalt 6 mnd'
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  11 AND
    SysPara.SysGr  =  100 AND
    SysPara.ParaNr = 3 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 11 
          SysPara.SysGr  = 100 
          SysPara.ParaNr = 3.
      ASSIGN  
          SysPara.Parameter1   = '28'
          Syspara.Beskrivelse  = 'Rabattlimit'
          SysPara.Hjelpetekst1 = 'Rabatt må overstige denne rabattsats.'
          .
      RELEASE SysPara.
    END.

END PROCEDURE.

PROCEDURE setDinteroParametre:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 14 AND
    SysGruppe.SysGr  = 303) THEN
  DO:
    CREATE SysGruppe.
    ASSIGN
        SysGruppe.SysHId      = 14
        SysGruppe.SysGr       = 303
        SysGruppe.Beskrivelse = "Dintero parametre"
        .
    RELEASE SysGruppe.
  END. /* SysGruppe */

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  14 AND
    SysPara.SysGr  =  303 AND
    SysPara.ParaNr = 1 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 14
          SysPara.SysGr  = 303 
          SysPara.ParaNr = 1.
      ASSIGN  
          SysPara.Parameter1   = ''
          Syspara.Beskrivelse  = 'UserDomain'
          SysPara.Hjelpetekst1 = ''
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  14 AND
    SysPara.SysGr  =  303 AND
    SysPara.ParaNr = 2 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 14
          SysPara.SysGr  = 303 
          SysPara.ParaNr = 2.
      ASSIGN  
          SysPara.Parameter1   = ''
          Syspara.Beskrivelse  = 'User'
          SysPara.Hjelpetekst1 = ''
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  14 AND
    SysPara.SysGr  =  303 AND
    SysPara.ParaNr = 3 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 14
          SysPara.SysGr  = 303 
          SysPara.ParaNr = 3.
      ASSIGN  
          SysPara.Parameter1   = ''
          Syspara.Beskrivelse  = 'Pwd'
          SysPara.Hjelpetekst1 = ''
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  14 AND
    SysPara.SysGr  =  303 AND
    SysPara.ParaNr = 4 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 14
          SysPara.SysGr  = 303 
          SysPara.ParaNr = 4.
      ASSIGN  
          SysPara.Parameter1   = ''
          Syspara.Beskrivelse  = 'Aid'
          SysPara.Hjelpetekst1 = ''
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  14 AND
    SysPara.SysGr  =  303 AND
    SysPara.ParaNr = 5 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 14
          SysPara.SysGr  = 303 
          SysPara.ParaNr = 5.
      ASSIGN  
          SysPara.Parameter1   = 'https://api.dintero.com/v1/accounts/&Aid/auth/token'
          Syspara.Beskrivelse  = 'Fullpath'
          SysPara.Hjelpetekst1 = 'URL til API for henting av Token (NB: La &Aid stå urørt).'
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  14 AND
    SysPara.SysGr  =  303 AND
    SysPara.ParaNr = 6 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 14
          SysPara.SysGr  = 303 
          SysPara.ParaNr = 6.
      ASSIGN  
          SysPara.Parameter1   = 'https://gant.mp.test.dintero.com'
          Syspara.Beskrivelse  = 'URL til MinSide'
          SysPara.Hjelpetekst1 = 'URL til API for henting av Token (NB: La &Aid stå urørt).'
          .
      RELEASE SysPara.
    END.


  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  14 AND
    SysPara.SysGr  =  303 AND
    SysPara.ParaNr = 10 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 14
          SysPara.SysGr  = 303 
          SysPara.ParaNr = 10.
      ASSIGN  
          SysPara.Parameter1   = ''
          Syspara.Beskrivelse  = 'Access_Token'
          SysPara.Hjelpetekst1 = ''
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  14 AND
    SysPara.SysGr  =  303 AND
    SysPara.ParaNr = 11 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 14
          SysPara.SysGr  = 303 
          SysPara.ParaNr = 11.
      ASSIGN  
          SysPara.Parameter1   = ''
          Syspara.Beskrivelse  = 'Token_Type'
          SysPara.Hjelpetekst1 = ''
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  14 AND
    SysPara.SysGr  =  303 AND
    SysPara.ParaNr = 12 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 14
          SysPara.SysGr  = 303 
          SysPara.ParaNr = 12.
      ASSIGN  
          SysPara.Parameter1   = ''
          Syspara.Beskrivelse  = 'dtRead'
          SysPara.Hjelpetekst1 = ''
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  14 AND
    SysPara.SysGr  =  303 AND
    SysPara.ParaNr = 13 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 14
          SysPara.SysGr  = 303 
          SysPara.ParaNr = 13.
      ASSIGN  
          SysPara.Parameter1   = ''
          Syspara.Beskrivelse  = 'dtExpires'
          SysPara.Hjelpetekst1 = ''
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  14 AND
    SysPara.SysGr  =  303 AND
    SysPara.ParaNr = 14 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 14
          SysPara.SysGr  = 303 
          SysPara.ParaNr = 14.
      ASSIGN  
          SysPara.Parameter1   = ''
          Syspara.Beskrivelse  = 'FullPath'
          SysPara.Hjelpetekst1 = ''
          .
      RELEASE SysPara.
    END.

END PROCEDURE.

PROCEDURE setSMSParametre:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  50 AND
    SysPara.ParaNr = 74 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 50
          SysPara.SysGr  = 50 
          SysPara.ParaNr = 74.
      ASSIGN  
          SysPara.Parameter1   = ''
          Syspara.Beskrivelse  = 'TLS'
          SysPara.Hjelpetekst1 = ''
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  50 AND
    SysPara.ParaNr = 73 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 50
          SysPara.SysGr  = 50 
          SysPara.ParaNr = 73.
      ASSIGN  
          SysPara.Parameter1   = ''
          Syspara.Beskrivelse  = 'PASSWORD'
          SysPara.Hjelpetekst1 = ''
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  50 AND
    SysPara.ParaNr = 72 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 50
          SysPara.SysGr  = 50 
          SysPara.ParaNr = 72.
      ASSIGN  
          SysPara.Parameter1   = ''
          Syspara.Beskrivelse  = 'USERNAME'
          SysPara.Hjelpetekst1 = ''
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  50 AND
    SysPara.ParaNr = 71 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 50
          SysPara.SysGr  = 50 
          SysPara.ParaNr = 71.
      ASSIGN  
          SysPara.Parameter1   = ''
          Syspara.Beskrivelse  = 'FromADDRESS'
          SysPara.Hjelpetekst1 = ''
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  50 AND
    SysPara.ParaNr = 70 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 50
          SysPara.SysGr  = 50 
          SysPara.ParaNr = 70.
      ASSIGN  
          SysPara.Parameter1   = ''
          Syspara.Beskrivelse  = 'SERVER'
          SysPara.Hjelpetekst1 = ''
          .
      RELEASE SysPara.
    END.


END PROCEDURE.

PROCEDURE setEDIParam:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 5 AND
    SysGruppe.SysGr  = 40) THEN
  DO:
    CREATE SysGruppe.
    ASSIGN
        SysGruppe.SysHId      = 5
        SysGruppe.SysGr       = 40
        SysGruppe.Beskrivelse = "EDI oppsett kommisjon"
        .
    RELEASE SysGruppe.
  END. /* SysGruppe */

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  5 AND
    SysPara.SysGr  =  40 AND
    SysPara.ParaNr = 1 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 5
          SysPara.SysGr  = 40 
          SysPara.ParaNr = 1.
      ASSIGN  
          SysPara.Parameter1   = '893635122'
          SysPara.Parameter2   = 'ZZZ'
          Syspara.Beskrivelse  = 'GLN og Qualifier for Sender'
          SysPara.Hjelpetekst1 = 'GLN nr. eller avtalt ID'
          SysPara.Hjelpetekst2 = 'Bruk 14 for GLN, eller ZZZ for avtalt ID'
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  5 AND
    SysPara.SysGr  =  40 AND
    SysPara.ParaNr = 2 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 5
          SysPara.SysGr  = 40 
          SysPara.ParaNr = 2.
      ASSIGN  
          SysPara.Parameter1   = '7080003883322'
          SysPara.Parameter2   = '14'
          Syspara.Beskrivelse  = 'GLN og Qualifier for Recipient'
          SysPara.Hjelpetekst1 = 'GLN nr. eller avtalt ID'
          SysPara.Hjelpetekst2 = 'Bruk 14 for GLN, eller ZZZ for avtalt ID'
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  5 AND
    SysPara.SysGr  =  40 AND
    SysPara.ParaNr = 3 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 5
          SysPara.SysGr  = 40 
          SysPara.ParaNr = 3.
      ASSIGN  
          SysPara.Parameter1   = '0'
          Syspara.Beskrivelse  = 'Sist brukte meldingsId'
          SysPara.Hjelpetekst1 = 'Oppdateres for hver sending. Sett startnr.'
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  5 AND
    SysPara.SysGr  =  40 AND
    SysPara.ParaNr = 4 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 5
          SysPara.SysGr  = 40 
          SysPara.ParaNr = 4.
      ASSIGN  
          SysPara.Parameter1   = '1'
          Syspara.Beskrivelse  = 'DriftsModus'
          SysPara.Hjelpetekst1 = '0-Produksjon, 1-Test.'
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  5 AND
    SysPara.SysGr  =  40 AND
    SysPara.ParaNr = 5 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 5
          SysPara.SysGr  = 40 
          SysPara.ParaNr = 5.
      ASSIGN  
          SysPara.Parameter1   = 'Isabella Næssan-Do'
          Syspara.Beskrivelse  = 'Kontaktperson Illums Bolighus'
          SysPara.Hjelpetekst1 = ''
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  5 AND
    SysPara.SysGr  =  40 AND
    SysPara.ParaNr = 10 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 5
          SysPara.SysGr  = 40 
          SysPara.ParaNr = 10.
      ASSIGN  
          SysPara.Parameter1   = 'kom\ut\EDI'
          Syspara.Beskrivelse  = 'Eksport katalog EDI filer'
          SysPara.Hjelpetekst1 = ''
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  5 AND
    SysPara.SysGr  =  40 AND
    SysPara.ParaNr = 11 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 5
          SysPara.SysGr  = 40 
          SysPara.ParaNr = 11.
      ASSIGN  
          SysPara.Parameter1   = 'kom\in\EDI'
          Syspara.Beskrivelse  = 'Import katalog EDI filer'
          SysPara.Hjelpetekst1 = ''
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  5 AND
    SysPara.SysGr  =  40 AND
    SysPara.ParaNr = 12 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 5
          SysPara.SysGr  = 40 
          SysPara.ParaNr = 12.
      ASSIGN  
          SysPara.Parameter1   = '100'
          Syspara.Beskrivelse  = 'Prisprofil for kommisjonsbutikker'
          SysPara.Hjelpetekst1 = ''
          .
      RELEASE SysPara.
    END.

END PROCEDURE.

PROCEDURE PkSdlImport:
  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 210 AND
    SysPara.SysGr  = 100 AND
    SysPara.ParaNr =   9 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 210
          SysPara.SysGr  = 100
          SysPara.ParaNr =   9.
      ASSIGN  
          SysPara.Parameter1   = '1'
          Syspara.Beskrivelse  = 'Outlet skal ha HK pris på etikett'
          SysPara.Hjelpetekst1 = '0=Nei, 1=Ja.'
          .
      RELEASE SysPara.
    END. 

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 210 AND
    SysPara.SysGr  = 100 AND
    SysPara.ParaNr =   10 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 210
          SysPara.SysGr  = 100
          SysPara.ParaNr =   10.
      ASSIGN  
          SysPara.Parameter1   = '45'
          Syspara.Beskrivelse  = '%Andel for beregnet LC pris'
          SysPara.Hjelpetekst1 = 'Benyttes på artikler hvor LC pris ikke er angitt.'
          .
      RELEASE SysPara.
    END. 
END PROCEDURE.

PROCEDURE setFTP:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(SysHode WHERE 
    SysHode.SysHId = 55) THEN 
  DO:
    CREATE SysHode.
    ASSIGN
      SysHode.SysHId      = 55
      SysHode.Beskrivelse = 'FTP oppsett' 
      .
  END.
  
  IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 55 AND
    SysGruppe.SysGr  = 10) THEN
  DO:
    CREATE SysGruppe.
    ASSIGN
        SysGruppe.SysHId      = 55
        SysGruppe.SysGr       = 10
        SysGruppe.Beskrivelse = "FTP Oppsett Illums Bolighus"
        .
    RELEASE SysGruppe.
  END. /* SysGruppe */

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 55 AND
    SysPara.SysGr  = 10 AND
    SysPara.ParaNr =  1 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 55
          SysPara.SysGr  = 10 
          SysPara.ParaNr =  1.
      ASSIGN  
          SysPara.Parameter1   = '0'
          SysPara.Parameter2   = ''
          Syspara.Beskrivelse  = 'Akitver'
          SysPara.Hjelpetekst1 = '0=Ikke aktiv, 1=Aktiv.'
          SysPara.Hjelpetekst2 = ''
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 55 AND
    SysPara.SysGr  = 10 AND
    SysPara.ParaNr =  2 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 55
          SysPara.SysGr  = 10 
          SysPara.ParaNr =  2.
      ASSIGN  
          SysPara.Parameter1   = 'ftp.nemedi.dk'
          SysPara.Parameter2   = ''
          Syspara.Beskrivelse  = 'Tjener'
          SysPara.Hjelpetekst1 = ''
          SysPara.Hjelpetekst2 = ''
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 55 AND
    SysPara.SysGr  = 10 AND
    SysPara.ParaNr =  3 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 55
          SysPara.SysGr  = 10 
          SysPara.ParaNr =  3.
      ASSIGN  
          SysPara.Parameter1   = 'gant'
          SysPara.Parameter2   = ''
          Syspara.Beskrivelse  = 'Bruker'
          SysPara.Hjelpetekst1 = ''
          SysPara.Hjelpetekst2 = ''
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 55 AND
    SysPara.SysGr  = 10 AND
    SysPara.ParaNr =  4 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 55
          SysPara.SysGr  = 10 
          SysPara.ParaNr =  4.
      ASSIGN  
          SysPara.Parameter1   = 'kUy43Gs90K'
          SysPara.Parameter2   = ''
          Syspara.Beskrivelse  = 'Passord'
          SysPara.Hjelpetekst1 = ''
          SysPara.Hjelpetekst2 = ''
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 55 AND
    SysPara.SysGr  = 10 AND
    SysPara.ParaNr =  5 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 55
          SysPara.SysGr  = 10 
          SysPara.ParaNr =  5.
      ASSIGN  
          SysPara.Parameter1   = '.\FromGant'
          SysPara.Parameter2   = ''
          Syspara.Beskrivelse  = 'Katalog for sending'
          SysPara.Hjelpetekst1 = ''
          SysPara.Hjelpetekst2 = ''
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 55 AND
    SysPara.SysGr  = 10 AND
    SysPara.ParaNr =  6 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 55
          SysPara.SysGr  = 10 
          SysPara.ParaNr =  6.
      ASSIGN  
          SysPara.Parameter1   = '.\ToGant'
          SysPara.Parameter2   = ''
          Syspara.Beskrivelse  = 'Katalog for mottak'
          SysPara.Hjelpetekst1 = ''
          SysPara.Hjelpetekst2 = ''
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 55 AND
    SysPara.SysGr  = 10 AND
    SysPara.ParaNr =  7 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 55
          SysPara.SysGr  = 10 
          SysPara.ParaNr =  7.
      ASSIGN  
          SysPara.Parameter1   = 'C:\NSoft\Polygon\PRS\kom\Ut\EDI'
          SysPara.Parameter2   = 'edi'
          Syspara.Beskrivelse  = 'Katalog for filer som skal sendes'
          SysPara.Hjelpetekst1 = 'Sti til katalog der edi filer som skal sendes ligger.'
          SysPara.Hjelpetekst2 = 'Fil ekstent på filer som skal sendes.'
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId = 55 AND
    SysPara.SysGr  = 10 AND
    SysPara.ParaNr =  8 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId = 55
          SysPara.SysGr  = 10 
          SysPara.ParaNr =  8.
      ASSIGN  
          SysPara.Parameter1   = 'C:\NSoft\Polygon\PRS\kom\in\EDI'
          SysPara.Parameter2   = 'edi'
          Syspara.Beskrivelse  = 'Katalog for mottatte filer som skal importeres'
          SysPara.Hjelpetekst1 = 'Sti til katalog der edi filer som skal importeres ligger.'
          SysPara.Hjelpetekst2 = 'Fil ekstent på filer som skal importeres.'
          .
      RELEASE SysPara.
    END.
END PROCEDURE.
