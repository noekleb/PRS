/*------------------------------------------------------------------------
    File        : xsport1vpiauto.p 
    Purpose     : Automatiserer innlesning av VPI

    Syntax      : run xsport1vpiauto.p (VPIFilHode.VPIFilId).

    Description : importere VPI inn i VPIMottakskontrollen. Derfra oppdateres den videre inn 
                  inn i varebok. Varebokens nr. skal fremgå av filnavnet.

    Author(s)   : Tom Nøkleby
    Created     : 19/9-10
    Notes       : Helgearbeid :)
  ----------------------------------------------------------------------*/

DEF INPUT PARAMETER lFilId AS DEC NO-UNDO.

DEFINE VARIABLE lVareBokNr    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iLevNr        AS INTEGER   NO-UNDO.
DEFINE VARIABLE bOk           AS LOG       NO-UNDO.
DEFINE VARIABLE cFieldList    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAnt          AS INTEGER   NO-UNDO. 
DEFINE VARIABLE iEkstVPILevNr AS INTEGER   NO-UNDO.
DEFINE VARIABLE iETid         AS INT       NO-UNDO.

/* VPI lomme som artikklene skal legges inn i. */
{syspara.i 50 19 1 iEkstVPILevNr INT}
IF iEkstVPILevNr = 0 THEN 
  iEkstVPILevNr = 903.
  
/* Leverandør skal være kjedens logistikkpartner. */
{syspara.i 1 1 53 iLevNr INT}
IF iLevNr = 0 THEN 
  iLevNr = 38.
  
ASSIGN
    iETid      = TIME
    cFieldList = {tbchooseAll.i}.

DEFINE TEMP-TABLE tmpVPIArtBas NO-UNDO LIKE VPIArtBas
  INDEX VPIArtBas EkstVPILevNr VareNr BehStatus.

/* Filhode. */
FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
/* Feil! */    
IF NOT AVAILABLE VPIFilHode THEN
DO:
  RUN bibl_logg.p ('VPIImport', 'xsport1vpiauto.p: Automatisk VPI import av fil: FEIL Finner ikke VPIFilhode post ' + VPIFilHode.FilNavn + ' FilId ' + STRING(lFilId) + ' ' + string(TIME,"HH:MM:SS")).
  RUN settFilStatus(9).
  RETURN.
END.
/* Kobler om filhode til HK's ERP VPI lomme */
DO TRANSACTION:
  FIND CURRENT VPIFilHode EXCLUSIVE-LOCK NO-ERROR.
  IF AVAILABLE VPIFilHode THEN 
  DO:
    VPIFilHode.EkstVPILevNr = iEkstVPILevNr.
    FIND CURRENT VPIFilHode NO-LOCK.
  END.
END. /* TRANSACTION */


/* Sesongbok */
IF VPIFilHode.FilNavn BEGINS 'SBA' AND NUM-ENTRIES(VPIFilHode.FilNavn,'-') = 4 THEN 
DO:
  ASSIGN
    lVareBokNr = DECIMAL(ENTRY(3,VPIFilHode.FilNavn,'-')) 
    /*iLevNr     = DECIMAL(ENTRY(2,VPIFilHode.FilNavn,'-'))*/ 
    NO-ERROR.
  IF lVareBokNr = 0 THEN 
    DO:
      RUN bibl_logg.p ('VPIImport', 'xsport1vpiauto.p: Automatisk VPI import av fil: FEIL Finner ikke varebok ' + VPIFilHode.FilNavn + ' ' + string(TIME,"HH:MM:SS")).
      RUN settFilStatus(9).
      RETURN.
    END.
  
  IF NOT CAN-FIND(VareBokHode WHERE VareBokHode.VareBokNr = lVareBokNr) THEN 
  DO TRANSACTION:
    FIND FIRST PrisProfil NO-LOCK NO-ERROR.
    CREATE VareBokHode.
    ASSIGN
      VareBokHode.VarebokBeskrivelse = "VPI fra ERP " + STRING(TODAY)
      VarebokHode.VareBokType        = 1
      VareBokHode.ProfilNr           = IF AVAILABLE PrisProfil THEN PrisProfil.ProfilNr ELSE 1      
      .
    RELEASE VareBokHode.
  END.
     
  IF iLevNr = 0 OR NOT CAN-FIND(LevBas WHERE 
                                LevBas.LevNr = iLevNr) THEN 
    DO:
      RUN bibl_logg.p ('VPIImport', 'xsport1vpiauto.p: Automatisk VPI import av fil: FEIL Finner ikke leverandør ' + VPIFilHode.FilNavn + ' ' + string(TIME,"HH:MM:SS")).
      RUN settFilStatus(9).
      RETURN.
    END.
  IF NOT CAN-FIND(FIRST EkstVPILev WHERE
                  EkstVPILev.LevNr = iLevNr USE-INDEX EkstVPILev) THEN 
    DO:
      RUN bibl_logg.p ('VPIImport', 'xsport1vpiauto.p: Automatisk VPI import av fil: FEIL Finner ikke VPIleverandør ' + VPIFilHode.FilNavn + ' ' + string(TIME,"HH:MM:SS")).
      RUN settFilStatus(9).
      RETURN.
    END.
      
  /*RUN settEkstVPILevNr (iLevNr). - Gjøres via systemparameter. */
  IF bOk THEN 
    RUN bibl_logg.p ('VPIImport', 'xsport1vpiauto.p: Automatisk VPI import av fil: ' + VPIFilHode.FilNavn + ' ' + string(TIME,"HH:MM:SS")).
  RUN xsport1vpiutpakk.p (VPIFilHode.FilId).
  RUN settFilStatus(5).
  RUN sjekkStrekkode(VPIFilHode.EkstVPILevNr).
  RUN oppdaterVarebok(VPIFilHode.EkstVPILevNr).
END. 

/* Endringer i sesong */
ELSE IF VPIFilHode.FilNavn  BEGINS 'AE' AND NUM-ENTRIES(VPIFilHode.FilNavn,'-') = 3 THEN 
DO:
  /*
  ASSIGN
    iLevNr = DECIMAL(ENTRY(2,VPIFilHode.FilNavn,'-')) NO-ERROR.
  */
  IF iLevNr = 0 OR NOT CAN-FIND(LevBas WHERE 
                                LevBas.LevNr = iLevNr) THEN 
    DO:
      RUN bibl_logg.p ('VPIImport', 'xsport1vpiauto.p: Automatisk VPI import av fil: FEIL Finner ikke leverandør ' + VPIFilHode.FilNavn + ' ' + string(TIME,"HH:MM:SS")).
      RUN settFilStatus(9).
      RETURN.
    END.
  IF NOT CAN-FIND(FIRST EkstVPILev WHERE
                  EkstVPILev.LevNr = iLevNr USE-INDEX EkstVPILev) THEN 
    DO:
      RUN bibl_logg.p ('VPIImport', 'xsport1vpiauto.p: Automatisk VPI import av fil: FEIL Finner ikke VPIleverandør ' + VPIFilHode.FilNavn + ' ' + string(TIME,"HH:MM:SS")).
      RUN settFilStatus(9).
      RETURN.
    END.

  /*RUN settEkstVPILevNr (iLevNr). - Gjøres via systemparameter. */
  IF bOk THEN 
    RUN bibl_logg.p ('VPIImport', 'xsport1vpiauto.p: Automatisk VPI import av fil: ' + VPIFilHode.FilNavn + ' ' + string(TIME,"HH:MM:SS")).
  RUN xsport1vpiutpakk.p (VPIFilHode.FilId).
  RUN settFilStatus(5).
  RUN sjekkStrekkode(VPIFilHode.EkstVPILevNr).
  RUN oppdaterArtBas (VPIFilHode.EkstVPILevNr).
  RUN sendTilButikk (VPIFilHode.EkstVPILevNr).
END.

/* Feil */
ELSE DO:
  RUN bibl_logg.p ('VPIImport', 'xsport1vpiauto.p: Automatisk VPI import av fil: FEIL Ugyldig filnavn ' + VPIFilHode.FilNavn + ' ' + string(TIME,"HH:MM:SS")).
  RUN settFilStatus(9).
  RETURN.
END.



/* **********************  Internal Procedures  *********************** */

PROCEDURE oppdaterArtBas:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iEkstVPILevNr AS INTEGER NO-UNDO.

DEFINE VARIABLE ocReturn    AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk        AS LOG       NO-UNDO.
DEFINE VARIABLE ihBuffer    AS HANDLE    NO-UNDO.
DEFINE VARIABLE icSessionid AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAnt        AS INTEGER   NO-UNDO. 
 
DEFINE BUFFER bVPIArtBas FOR VPIArtBas.
  
FOR EACH VPIArtBas NO-LOCK WHERE 
  VPIArtBas.EkstVPILevNr = iEkstVPILevNr AND 
  VPIArtBas.VPIDato      = TODAY AND 
  VPIArtBas.ETid         >= iETid:

  iAnt = iAnt + 1.
  
  cFieldList = {tbchooseAll.i}.  
  RUN artbas_new.p (STRING(VPIArtBas.EkstVPILevNr) + ';' + cFieldList + ';' + STRING(VPIArtBas.Varenr), 
                    ihBuffer, 
                    icSessionid, 
                    OUTPUT ocReturn, 
                    OUTPUT obOk).
  /*VPIArtBas.BehStatus = 90. Gjøres ved sending til butikk. */ /** Behandlet */ 
END.  

RUN bibl_logg.p ('VPIImport', 'xsport1vpiauto.p: Automatisk VPI import av fil: Overført til artikkelregister - ant.artikler ' + STRING(iAnt) + ' ' + string(TIME,"HH:MM:SS")).

END PROCEDURE.

PROCEDURE oppdaterVarebok:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iEkstVPILevNr AS INTEGER NO-UNDO.

DEFINE VARIABLE ocReturn    AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk        AS LOG       NO-UNDO.
DEFINE VARIABLE ihBuffer    AS HANDLE    NO-UNDO.
DEFINE VARIABLE icSessionid AS CHARACTER NO-UNDO.

/* Tømmer hvis det skulle være noe i den. */
FOR EACH tmpVPIArtBas:
  DELETE tmpVPIArtBas.
END.

ASSIGN
  iAnt     = 0
  .

/* Bygger opp tabellen*/
FOR EACH VPIArtBas EXCLUSIVE-LOCK WHERE 
  VPIArtBas.EkstVPILevNr = iEkstVPILevNr AND 
  VPIArtBas.VPIDato      = TODAY AND 
  VPIArtBAs.ETid         >= iETid:

  iAnt = iAnt + 1.  
  CREATE tmpVPIArtBas.
  BUFFER-COPY VPIArtBas TO tmpVPIArtBas.
  
  VPIArtBas.BehStatus = 90. /** Behandlet */ 
END.
IF AVAILABLE VPIArtBas THEN 
  RELEASE VPIArtBas.
  
IF iAnt > 0 THEN 
DO:
  ihBuffer = BUFFER tmpVPIArtBas:HANDLE.
  RUN vareboklinje_new_update.p (STRING(lVareBokNr) + ";" + STRING(iEkstVPILevNr) + ';' + cFieldList , 
                    ihBuffer, 
                    icSessionid, 
                    OUTPUT ocReturn, 
                    OUTPUT obOk).
  RUN bibl_logg.p ('VPIImport', 'xsport1vpiauto.p: Automatisk VPI import av fil: Overført til varebok - ant.artikler ' + STRING(iAnt) + ' ' + string(TIME,"HH:MM:SS")).
END.
ELSE DO:
  RUN bibl_logg.p ('VPIImport', 'xsport1vpiauto.p: Automatisk VPI import av fil: Ingen artikler overført til varebok.' + ' ' + string(TIME,"HH:MM:SS")).
END.
END PROCEDURE.

PROCEDURE sendTilButikk:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes: Benytter temp-table som ble bygget i oppdaterVarebok rutinen. 																	  
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iEkstVPILevNr AS INTEGER NO-UNDO.

DEFINE VARIABLE ocReturn    AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk        AS LOG       NO-UNDO.
DEFINE VARIABLE ihBuffer    AS HANDLE    NO-UNDO.
DEFINE VARIABLE icSessionid AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldList  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cButList    AS CHARACTER NO-UNDO.

FOR EACH tmpVPIArtBas:
  DELETE tmpVPIArtBas.
END.

ASSIGN
  ihBuffer = BUFFER tmpVPIArtBas:HANDLE
  cButList = '|yes|no|yes' /* <Blank butikklst>|<Alle Butikker>|<Send Bilder>|<Send via HK's VPI register> */
  iAnt     = 0
  .

/* Bygger opp tabellen*/
FOR EACH VPIArtBas EXCLUSIVE-LOCK WHERE 
  VPIArtBas.EkstVPILevNr = iEkstVPILevNr AND 
  VPIArtBas.VPIDato      = TODAY AND 
  VPIArtBas.ETid         >= iETid:

  iAnt = iAnt + 1.  
  CREATE tmpVPIArtBas.
  BUFFER-COPY VPIArtBas TO tmpVPIArtBas.
  
  VPIArtBas.BehStatus = 90. /** Behandlet */ 
END.
  
IF CAN-FIND(FIRST tmpVPIArtBas) THEN DO:
  RUN vpiartbas_send_til_butikk.p (STRING(iEkstVPILevNr) + ';' + cButList, 
                    ihBuffer, 
                    icSessionid, 
                    OUTPUT ocReturn, 
                    OUTPUT obOk).
  RUN bibl_logg.p ('VPIImport', 'xsport1vpiauto.p: Automatisk VPI import av fil: Sendt til butikk - ant.artikler ' + STRING(iAnt) + ' ' + string(TIME,"HH:MM:SS")).
END.
ELSE DO:
  RUN bibl_logg.p ('VPIImport', 'xsport1vpiauto.p: Automatisk VPI import av fil: Ingen artikler sendt til butikk.' + ' ' + string(TIME,"HH:MM:SS")).
END.


END PROCEDURE.

PROCEDURE settEkstVPILevNr:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER piLevNr AS INTEGER NO-UNDO.

bOk = FALSE.

FIND FIRST EkstVPILev NO-LOCK WHERE
  EkstVPILev.LevNr = iLevNr USE-INDEX EkstVPILev NO-ERROR.
IF NOT AVAILABLE EkstVPILev OR EkstVPILev.LevNr = 0 THEN 
  DO:
    RUN bibl_logg.p ('VPIImport', 'xsport1vpiauto.p: Automatisk VPI import av fil: FEIL Ukjent VPI leverandør ' + VPIFilHode.FilNavn + ' ' + string(TIME,"HH:MM:SS")).
    RETURN.
  END.

DO TRANSACTION:
  FIND CURRENT VPIFilHode EXCLUSIVE-LOCK NO-ERROR.
  IF AVAILABLE VPIFilHode THEN 
  DO:
    ASSIGN
      VPIFilHode.EkstVPILevNr = EkstVPILev.EkstVPILevNr
      bOk = TRUE.
    FIND CURRENT VPIFilHode NO-LOCK NO-ERROR.   
    IF NOT CAN-FIND(VPIDatasett WHERE
                    VPIDatasett.EkstVPILevNr = EkstVPILev.EkstVPILevNr) THEN 
    DO:
      CREATE VPIDatasett.
      ASSIGN
        VPIDatasett.EkstVPILevNr = EkstVPILev.EkstVPILevNr.
      RELEASE VPIDatasett.
    END. 
  END.
END.

END PROCEDURE.

PROCEDURE settFilStatus:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iVPIFilStatus AS INTEGER NO-UNDO.

DO TRANSACTION:
  FIND CURRENT VPIFilHode EXCLUSIVE-LOCK NO-ERROR.
  IF AVAILABLE VPIFilHode THEN 
  DO:
    ASSIGN
      VPIFilHode.VPIFilStatus = iVPIFilStatus.
    FIND CURRENT VPIFilHode NO-LOCK NO-ERROR.    
    RUN bibl_logg.p ('VPIImport', 'xsport1vpiauto.p: Automatisk VPI import av fil: Filstatus satt til  ' + STRING(iVPIFilStatus) + ' ' + STRING(TIME,"HH:MM:SS")).
  END.
END.


END PROCEDURE.

PROCEDURE sjekkStrekkode:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iEkstVPILevNr AS INTEGER NO-UNDO.

DEFINE VARIABLE bSjekk      AS LOG       NO-UNDO.

DEFINE BUFFER bVPIArtBas FOR VPIArtBas.
  
FOR EACH VPIArtBas NO-LOCK WHERE 
  VPIArtBas.EkstVPILevNr = iEkstVPILevNr AND 
  VPIArtBas.VPIDato      = TODAY AND 
  VPIArtBas.ETid         >= iETid AND 
  CAN-FIND(ArtBas WHERE
           ArtBAs.ArtikkelNr = dec(VPIArtBas.VareNr)):
  
   SJEKK:  
   FOR EACH VPIStrekkode OF VPIArtBas NO-LOCK:
     FIND Strekkode NO-LOCK WHERE
       Strekkode.Kode = VPIStrekkode.Kode NO-ERROR.
     IF AVAILABLE Strekkode THEN 
       DO:
         IF Strekkode.ArtikkelNr <> dec(VPIArtBas.VareNr) THEN 
           DO:
             bSjekk = TRUE.
             LEAVE SJEKK.
           END. 
       END.
  END. /* SJEKK */
  
  /* Denne skal ikke videre. */
  IF bSjekk THEN DO TRANSACTION: 
    FIND bVPIArtBas EXCLUSIVE-LOCK WHERE 
      RECID(bVPIArtBas) = RECID(VPIArtBas) NO-ERROR.
    IF AVAILABLE bVPIArtBas THEN 
    DO:
      bVPIArtBas.BehStatus = 3.
      FIND CURRENT bVPIArtBas NO-LOCK NO-ERROR.
    END.
  END.
  
  bSjekk = FALSE. 
END.  

END PROCEDURE.
