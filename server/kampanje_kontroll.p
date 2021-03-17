/* artpris_kampanje.p

    cType1 = "Varetekst;Lev.artikkelnr;Lev.farkgekode;TilbudFraDato;TilbudTilDato;Pris;%Rabatt 1;Pris;Solgt%;AntSolgt;VerdiSolgt;Lager;Lager verdi;Varemerke;Produsent;Sesong;Varegruppe;Hovedgruppe;Prisprofil;Artikkelnummer"
    cType2 = ''
    cType3 = ''

-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iKampanjeId AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE lKamp% AS DECIMAL NO-UNDO.
DEFINE VARIABLE lKampanjePris AS DECIMAL NO-UNDO.
DEFINE VARIABLE hQuery    AS HANDLE NO-UNDO.
DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.
DEFINE VARIABLE lMinstePris AS DECIMAL NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

ASSIGN
  bTest       = TRUE
  iKampanjeId = INTEGER(ENTRY(1,icParam,'|'))
  cLogg       = ENTRY(2,icParam,'|')
  .
IF cLogg = '' THEN
  cLogg = 'kampanje_kontroll' + REPLACE(STRING(TODAY),'/','').

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

FIND KampanjeHode NO-LOCK WHERE
  KampanjeHode.KampanjeId = iKampanjeId NO-ERROR.

IF bTest THEN
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg, '  Start kampanje_kontroll.p').
  rStandardFunksjoner:SkrivTilLogg(cLogg, '    Parametre:').
  rStandardFunksjoner:SkrivTilLogg(cLogg, '      iKampanjeId: ' + STRING(iKampanjeId)).
  rStandardFunksjoner:SkrivTilLogg(cLogg, '      icParam: ' + icParam).
END.

IF NOT AVAILABLE KampanjeHode THEN
  DO:
    ocReturn = '** Ukjent kampanjeid ' + STRING(iKampanjeId) + '.'.
    ASSIGN
      obOK = YES
      obOk = ocReturn = ""
      .
    RETURN.
  END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

IF bTest THEN
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg, '    available kampanjelinje: ' + STRING(ihBuffer:AVAILABLE)).
  END.

rStandardFunksjoner:SkrivTilLogg(cLogg, '    Linjer:').
BUFFER_LOOP:
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    
  ASSIGN
    iant = iant + 1
    lKamp% = ihBuffer:BUFFER-FIELD("KampanjeLinje_Rab%"):BUFFER-VALUE
    lKampanjePris = ihBuffer:BUFFER-FIELD("Pris_2"):BUFFER-VALUE
    NO-ERROR.

    rStandardFunksjoner:SkrivTilLogg(cLogg, '      Linje/Rab%/Kampanjepris: ' + STRING(ihBuffer:BUFFER-FIELD("KampanjeLinje"):BUFFER-VALUE) + '/' + 
                                     STRING(ihBuffer:BUFFER-FIELD("KampanjeLinje_Rab%"):BUFFER-VALUE) + '/' +
                                     STRING(ihBuffer:BUFFER-FIELD("Pris_2"):BUFFER-VALUE)
                                     ).

  IF KampanjeHode.AvslagType = 2 THEN
  SJEKKKAMPANJEPRIS: 
  DO:
    FIND FIRST ArtPris NO-LOCK WHERE 
      ArtPris.ArtikkelNr = ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE NO-ERROR.
    IF AVAILABLE ArtPris THEN 
    DO:
      IF ArtPris.Pris[1] < KampanjeHode.KampanjePris THEN 
      DO:
        ocReturn = 'Kampanjen inneholder en eller flere linjer hvor artikkelens normalpris er mindre enn kampanjeprisen.'.
        LEAVE BUFFER_LOOP.
      END.
    END.
  END. /* SJEKKKAMPANJEPRIS */

  IF KampanjeHode.AvslagType = 3 THEN
  SJEKKKRONERABATT: 
  DO:
    FIND FIRST ArtPris NO-LOCK WHERE 
      ArtPris.ArtikkelNr = ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE NO-ERROR.
    IF AVAILABLE ArtPris THEN 
    DO:
      IF ArtPris.Pris[1] < KampanjeHode.MistePris THEN 
      DO:
        ocReturn = 'Kampanjen inneholder en eller flere linjer hvor artikkelens normalpris er mindre enn minstepris.'.
        LEAVE BUFFER_LOOP.
      END.
    END.
  END. /* SJEKKKRONERABATT */

  IF lKamp% < 0 THEN 
    ASSIGN 
      ocReturn = 'kampanjen inneholder en eller flere linjer med neg. rabatt.'
      .
  ELSE IF lKampanjePris <= 0 THEN 
    ASSIGN 
      ocReturn = 'kampanjen inneholder en eller flere linjer med neg. eller 0 i kampanjepris.'
      .
  IF ocReturn <> '' THEN 
    LEAVE BUFFER_LOOP.
     
  hQuery:GET-NEXT().
END. /* BUFFER_LOOP */

DELETE OBJECT hQuery.
IF iAnt > 0 THEN 
  obOk = ocReturn = ''.
ELSE 
  ASSIGN 
    obOk = FALSE
    ocReturn =  'det er ingen varelinjer lagt inn på kampanjen.'
    .
  
IF bTest THEN
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg, '    iAnt: ' + STRING(iant)).
    rStandardFunksjoner:SkrivTilLogg(cLogg, '    ocReturn: ' + ocReturn).
    rStandardFunksjoner:SkrivTilLogg(cLogg, '    obOk: ' + STRING(obOk)).
    rStandardFunksjoner:SkrivTilLogg(cLogg, '  Slutt kampanje_kontroll.p').
  END.
  