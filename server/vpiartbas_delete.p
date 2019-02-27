/* Registrer Slett VPIArtBas record
   Parameter:  
   Opprettet: 25.11.2007             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE ihHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE bOppdInnpris AS LOG NO-UNDO.

DEF VAR hQuery          AS HANDLE NO-UNDO.

IF ENTRY(1,icParam,'|') = 'Yes'
  THEN bOppdInnpris = TRUE.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE
  .

  DO TRANSACTION:
    FIND FIRST VPIArtBas WHERE VPIArtBas.EkstVPILevNr = INT(ihBuffer:BUFFER-FIELD('EkstVPILevNr'):BUFFER-VALUE)
                           AND VPIArtBas.VareNr       = STRING(ihBuffer:BUFFER-FIELD('VareNr'):BUFFER-VALUE)
                         EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAIL VPIArtBas THEN
    DO:
      FIND FIRST VPIArtPris OF VPIArtBas NO-LOCK NO-ERROR.
      /* Avviker innpris, skal denne legges over i artikkelregisteret før postene slettes. */
      IF bOppdInnpris AND AVAILABLE VPIArtPris THEN 
      OPPDATINNPRIS:
      DO:      
        FIND ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = DEC(VPIArtPris.VareNr) AND 
            ArtPris.ProfilNr   = VPIArtPris.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
          FIND FIRST ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = DEC(VPIArtPris.VareNr) NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
          LEAVE OPPDATINNPRIS.
        IF VPIArtPris.Varekost[1] = ArtPris.VareKost[1] THEN    
          LEAVE OPPDATINNPRIS.
        ihHandle = BUFFER VPIArtPris:HANDLE.
        RUN vpiartbas_pris.p (STRING(TODAY) + '|yes|yes|no',ihHandle,icSessionId,OUTPUT ocReturn,OUTPUT obOk).          
      END. /* OPPDATINNPRIS */
             
      FOR EACH VPIStrekkode OF VPIArtBas NO-LOCK:
          RUN bibl_logg.p ('VPIMottakskontroll', 'vpiartbas_delete.p: Slettet artikkel: ' 
              + STRING(VPIArtBas.ArtikkelNr) + ' '
              + VPIArtBas.LevKod + ' ' 
              + VPIArtBas.Beskr + ' ' 
              + VPIArtBas.LevFargKod + ' ' 
              + VPIStrekkode.Kode + ' '  
              + ' EkstVPILev: ' + STRING(VPIArtBas.EkstVPILevNr)).
      END.
      DELETE VPIArtBas NO-ERROR.
      obOk = NOT ERROR-STATUS:ERROR.
      IF NOT obOk THEN
      DO:
        ocReturn = ERROR-STATUS:GET-MESSAGE(1).
        LEAVE.
      END.
    END.
  END.
  IF AVAIL VPIArtBas THEN RELEASE VPIArtBas.
  hQuery:GET-NEXT().
END.

