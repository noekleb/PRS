/*
   Opprettet: 30.03.2009 Geir Otto Olsen
 -----------------------------------------------------------------------------------*/
  DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
  DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
  DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
  DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
  DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.

  DEF VAR qh    AS HANDLE  NO-UNDO.
  DEF VAR bh    AS HANDLE  NO-UNDO.
  DEF VAR iCnt  AS INTEGER NO-UNDO.

  DEF VAR isysHid AS INT  NO-UNDO.
  DEF VAR isysGr  AS INT  NO-UNDO.
  DEF VAR cAction AS CHAR NO-UNDO.

  ASSIGN 
    isysHid = INT(ENTRY(1,icParam,'|'))
    isysGr  = INT(ENTRY(2,icParam,'|'))
    cAction = ENTRY(3,icParam,'|')
  .

  /*Dette gjøres i SaveRecord på kallende program... */
/* /* Nytt rapportnummer */                   */
/* IF cAction = 'New' THEN                    */
/* DO:                                        */
/*     FIND LAST SysGruppe NO-LOCK WHERE      */
/*       SysGruppe.SysHId = iSysHid NO-ERROR. */
/*     IF AVAILABLE SysGruppe THEN            */
/*       iSysGr = SysGruppe.SysGr + 1.        */
/*     ELSE                                   */
/*       iSysGr = 1.                          */
/* END.                                       */


DO TRANSACTION:
  FIND sysGruppe WHERE sysGruppe.syshid = iSysHid AND sysGruppe.sysgr = iSysGr EXCLUSIVE-LOCK NO-ERROR.

  IF NOT VALID-HANDLE(ihBuffer) THEN
  DO:
    ASSIGN 
      ocReturn = 'ingen ihBuffer'      
      obOK     = FALSE
    .
    RETURN.
  END.
  IF cAction = '' THEN /*Update*/
  DO:
    /*Delete all records in the syspara for SYSGRUPPE and add the new fields,*/
    FOR EACH sysPara EXCLUSIVE-LOCK WHERE sysPara.sysHid = isysHid AND SysPara.SysGr = isysGr:
      DELETE sysPara.
    END.
  END.
  
  ASSIGN
    bh   = BUFFER SysPara:HANDLE.
  .
  CREATE QUERY qh.
  qh:SET-BUFFERS(ihBuffer).
  qh:QUERY-PREPARE('FOR EACH ' + ihBuffer:NAME).
  qh:QUERY-OPEN().
  qh:GET-FIRST().
  
  DO WHILE ihbuffer:AVAIL:
    bh:BUFFER-CREATE().
    bh:BUFFER-COPY(ihBuffer,'sysGr').
    bh:BUFFER-FIELD('sysGr'):BUFFER-VALUE = iSysGr.

    /*Spesialbehanding av sysGr parameter*/
    IF bh:BUFFER-FIELD('parameter1'):BUFFER-VALUE = 'sysGr' THEN
      bh:BUFFER-FIELD('parameter2'):BUFFER-VALUE  = iSysGr.
    
    /*Spesialbehanding av RapportNavn parameter*/
    IF bh:BUFFER-FIELD('parameter1'):BUFFER-VALUE = 'RapportNavn' THEN
      bh:BUFFER-FIELD('parameter2'):BUFFER-VALUE  = sysGruppe.beskrivelse.
    qh:GET-NEXT().
  END.

END. /*Transaction*/
IF VALID-HANDLE(qh) THEN DELETE OBJECT qh.
