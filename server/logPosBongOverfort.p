
/*------------------------------------------------------------------------
    File        : logPosBongOverfort.p
    Purpose     : Logger sist kjørt pr. kasse slik at det blir mulig å følge med på dette fra server.

    Syntax      :

    Description : Logger siste send/hent av bonger og informasjon til kassene.  

    Author(s)   : tomn
    Created     : Tue May 05 11:29:37 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER iButNr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iKasseNr AS INTEGER NO-UNDO.

DEFINE VARIABLE cDmy AS CHARACTER NO-UNDO.

DEFINE BUFFER bSysGruppe FOR SysGruppe.
DEFINE BUFFER bSysPara   FOR sysPara.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

IF NOT CAN-FIND(SysGruppe WHERE
  SysGruppe.SysHId = 201 AND  
  SysGruppe.SysGr = iButNr) THEN 
DO FOR bsysGruppe TRANSACTION ON ERROR UNDO, LEAVE:
  FIND Butiker NO-LOCK WHERE 
    Butiker.Butik = iButNr NO-ERROR.
  CREATE bsysGruppe.
  ASSIGN
    bSysGruppe.SysHId      = 201
    bSysGruppe.SysGr       = iButNr
    bSysGruppe.Beskrivelse = (IF AVAILABLE Butiker THEN Butiker.butNamn ELSE STRING(iButNr)) 
    NO-ERROR.
  RELEASE bSysGruppe.
END.

DO FOR bSysPara TRANSACTION ON ERROR UNDO, LEAVE:
  FIND bSysPara EXCLUSIVE-LOCK WHERE
    bSysPara.SysHId = 201 AND
    bSysPara.SysGr  = iButNr AND
    bSysPara.ParaNr = iKasseNr NO-ERROR.
  IF NOT AVAILABLE bSysPara AND NOT LOCKED bSysPara THEN
    DO:
      CREATE bSysPara.
      ASSIGN
          bSysPara.SysHId = 201 
          bSysPara.SysGr  = iButNr 
          bSysPara.ParaNr = iKasseNr.
    END.  
  IF AVAILABLE bSysPara AND NOT LOCKED bSysPara THEN 
  DO:
    ASSIGN 
      cDmy = SESSION:DATE-FORMAT
      SESSION:DATE-FORMAT = 'dmy'
      .    
    ASSIGN  
        bSyspara.Beskrivelse  = "Dato/tid sis sendt/hentet bonger"
        bSysPara.Parameter1   = STRING(NOW,"99/99/9999 HH:MM:SS") 
        bSysPara.Hjelpetekst1 = "Stemples hver gang kassen sender/henter bongdata"
        .
    ASSIGN 
      SESSION:DATE-FORMAT = cDmy
      .    
    RELEASE bSysPara.
  END.
END.
