/* pksdlhode_skrivfaktura.p
   Parameter:  
   Opprettet:             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cPrinter        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iX AS INTEGER NO-UNDO.
DEFINE VARIABLE iFormat AS INTEGER NO-UNDO.
DEFINE VARIABLE bSkipJBoxInit AS LOG NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.

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
    cLogg   = 'pksdlhode_skrivfaktura' + REPLACE(STRING(TODAY),'/','') 
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
/*      cPrinter = Butiker.RAPPrinter.*/
      cPrinter = Butiker.Fakturaskriver.  
END.
ELSE cPrinter = ''.

IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      ' Skriver og butikk fra bruker: ' + cPrinter + ' / ' + (IF AVAILABLE Bruker THEN STRING(Bruker.Butik) ELSE '**Ukjent bruker' + USERID("SkoTex"))  
      ).    

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
    
    IF AVAIL PkSdlHode AND CAN-FIND(FIRST FakturaHode WHERE 
                                     FakturaHode.FakturaNr = PkSdlHode.FakturaNr) THEN
    BEHANDLE:
    DO:
        IF bTest THEN
          DO: 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                ' PakkseddelId ' + STRING(PkSdlHode.PkSdlId) 
                ).    
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                ' PakkseddelNr ' + STRING(PkSdlHode.PkSdlNr) 
                ).    
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                ' Faktura ' + STRING(PkSdlHode.FakturaNr) 
                ).
          END.    
        FIND FIRST FakturaHode NO-LOCK WHERE
            FakturaHode.FakturaNr = PkSdlHode.FakturaNr NO-ERROR.
        IF AVAILABLE FakturaHode THEN 
          DO:
            RUN skrivfaktura.p (STRING(FakturaHode.Faktura_Id) + "|",TRUE,cPrinter,1,"",iFormat). 
            ASSIGN 
                obOk     = TRUE
                ocReturn = ''
                .
            DO TRANSACTION:
                FIND CURRENT PkSdlHode EXCLUSIVE-LOCK.
                ASSIGN 
                    PkSdlHode.LeveringsDato = TODAY.
                FIND CURRENT PkSdlHode NO-LOCK.
            END.
            IF bTest THEN 
              rStandardFunksjoner:SkrivTilLogg(cLogg,
                  ' Faktura funnet og skrevet ut med rutine skrivfaktura.p. ' 
                  ).
          END.
        ELSE DO:
          IF bTest THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                ' ** Fant ikke faktura - utskrift avbrutt. ' 
                ).
        END. 
    END. /* BEHANDLE */
  IF AVAIL PkSdlHode THEN RELEASE PkSdlHode.
  hQuery:GET-NEXT().
END.
IF bTest THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Slutt' 
      ).    

