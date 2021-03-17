/* pksdlhode_skrivpakkseddel.p
   Parameter:  
   Opprettet:             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.

DEFINE VARIABLE cPrinter        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iX AS INTEGER NO-UNDO.
DEFINE VARIABLE iFormat AS INTEGER NO-UNDO.
DEFINE VARIABLE bSkipJBoxInit AS LOG NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEF VAR hQuery       AS HANDLE NO-UNDO.

DEFINE BUFFER bufPkSdlHode FOR PkSdlHode.

/* sjekker om init av Jukebox skal kjøres. Er programmet starter fra kassen, skal det initieres, ellers ikke. */
PUBLISH 'skipInitJukeBox' (OUTPUT bSkipJBoxInit).
IF bSkipJBoxInit = FALSE THEN
    {initjukebox.i}

ASSIGN 
    bTest   = TRUE
    iFormat = 1
    cLogg   = 'pksdlhode_skrivpakkseddel' + REPLACE(STRING(TODAY),'/','') 
    .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Start' 
      ).    

FIND Bruker NO-LOCK WHERE 
  Bruker.BrukerId = USERID("SkoTex") NO-ERROR.
IF AVAILABLE Bruker AND Bruker.Butik > 0 THEN 
DO:
    FIND Butiker NO-LOCK WHERE
      Butiker.Butik = Bruker.Butik NO-ERROR.
    IF AVAILABLE Butiker THEN 
      cPrinter = Butiker.RAPPrinter.    
END.
ELSE cPrinter = ''.

IF bTest THEN
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Bruker: ' + USERID("SkoTex") 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Skriver og butikk fra bruker: ' + cPrinter + ' / ' + (IF AVAILABLE Bruker THEN STRING(Bruker.Butik) ELSE '**Ukjent bruker' + USERID("SkoTex"))  
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Buffer-Name: ' + ihBuffer:NAME 
        ).    
  END. 
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, LEAVE: 
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE.

    FIND FIRST PkSdlHode WHERE 
        PkSdlHode.PkSdlId = DEC(ihBuffer:BUFFER-FIELD('PksdlId'):BUFFER-VALUE)
        NO-LOCK NO-ERROR.
    
    IF AVAIL PkSdlHode  THEN
    BEHANDLE:
    DO:
        IF bTest THEN
          DO: 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  PakkseddelId: ' + STRING(PkSdlHode.PkSdlId) 
                ).    
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  PakkseddelNr: ' + STRING(PkSdlHode.PkSdlNr) 
                ).    
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  Starter program skrivpakkseddel.p. ' 
                ).    
          END.    
        RUN skrivpakkseddel.p (STRING(PkSdlHode.PkSdlId) + "|",TRUE,cPrinter,1,"",1).
        ASSIGN 
            obOk     = TRUE
            ocReturn = ''
            .
    END. /* BEHANDLE */
    ELSE DO:
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            ' ** Fant ikke pakkseddel - utskrift avbrutt. ' 
            ).
    END.
  IF AVAIL PkSdlHode THEN RELEASE PkSdlHode.
  hQuery:GET-NEXT().
END.

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Slutt' 
      ).    
