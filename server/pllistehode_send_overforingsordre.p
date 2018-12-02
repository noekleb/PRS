/* Skaper overføringsordre av plukklisten.
   Parametere:  buffersandfields
                query 
   
   Opprettet: 21.04.08 av BHa 
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR hBuffer      AS HANDLE NO-UNDO.
DEF VAR piBuntNr     AS INT    NO-UNDO.
DEF VAR iAntLinjer   AS INT    NO-UNDO.
DEF VAR iAntFeil     AS INT    NO-UNDO.
DEF VAR piLinjeNr    AS INT    NO-UNDO.
DEF VAR wEDB-System  AS CHAR   NO-UNDO.
DEF VAR wTabell      AS CHAR   NO-UNDO.
DEF VAR iOppdDirekte AS INT    NO-UNDO.
DEF VAR lAntallPlukket AS DEC NO-UNDO.

DEF VAR dPlListeId  LIKE PlListeHode.PlListeId NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE TT_OvBuffer NO-UNDO LIKE OvBuffer.

{syspara.i 11 5 3 iOppdDirekte INT}
{syspara.i 1 2 3 wEDB-System}
if wEDB-System = "" then
  wEDB-System = "OVERFOR-LOCK".

/* Tømmer */
FOR EACH TT_OvBuffer:
    DELETE TT_OvBuffer. /* ev metod empty-temp-table */
END.

ASSIGN 
    dPlListeId = DECI(ENTRY(1,icParam,"¤"))
    piLinjeNr  = 1
    .
    
FIND PlListeHode NO-LOCK WHERE
    PlListeHode.PlListeId = dPlListeId NO-ERROR.

CREATE QUERY hQuery.
CREATE BUFFER hBuffer FOR TABLE "PlListeLinje".
hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE("For each PlListeLinje no-lock where PlListeLinje.PlListeId = " + 
                      STRING(dPlListeId) + " and PlListeLinje.AntallPlukket > 0").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
IF NOT hBuffer:AVAIL THEN
    RETURN.
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:

  FIND FIRST ArtBas WHERE ArtBas.ArtikkelNr = DEC(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) NO-LOCK NO-ERROR.
  IF NOT AVAIL ArtBas THEN DO:
    hQuery:GET-NEXT().
    iAntFeil = iAntFeil + 1.
    NEXT.
  END.

  FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
  IF NOT AVAIL VarGr THEN DO:
    hQuery:GET-NEXT().
    iAntFeil = iAntFeil + 1.
    NEXT.
  END.

  FIND StrKonv NO-LOCK WHERE
      StrKonv.StrKode = INT(hBuffer:BUFFER-FIELD("StrKode"):BUFFER-VALUE) NO-ERROR.
  IF NOT AVAIL StrKonv THEN DO:
    hQuery:GET-NEXT().
    iAntFeil = iAntFeil + 1.
    NEXT.
  END.

  /* Hvis det er gjort overføring på en post, må den kogges og posteres. Konsekvensen er at */
  /* lagerposter må opprettes hvis de ikke finnes fra før.                                  */
  FIND Lager NO-LOCK WHERE
      Lager.Butik = PlListeHode.FraButikkNr AND
      Lager.ArtikkelNr = DEC(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) NO-ERROR.
  IF NOT AVAIL Lager THEN DO:
      BUTIKKLOOP:
      for each Butiker no-lock break by Butiker.ProfilNr:
          create Lager.
          ASSIGN Lager.ArtikkelNr = ArtBas.ArtikkelNr
                 Lager.Butik      = Butiker.Butik.                    
          RELEASE Lager.
      end. /* BUTIKKLOOP */
      FIND Lager NO-LOCK WHERE
          Lager.Butik = PlListeHode.FraButikkNr AND
          Lager.ArtikkelNr = DEC(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) NO-ERROR.
  END.

  /* Antall linjer lagt ut i filen */
  iAntLinjer = iAntLinjer + 1.

  /* Her posterer vi */
  IF DEC(hBuffer:BUFFER-FIELD("AntallPlukket"):BUFFER-VALUE) > 0 THEN
  OVERFORINGSPOST:
  DO:
    /* Logger overføringstransaksjonen */
    CREATE TT_OvBuffer.
    ASSIGN TT_OvBuffer.BuntNr      = 998 /* dummy, kan vara vad som helst */
           TT_OvBuffer.LinjeNr     = piLinjeNr
           TT_OvBuffer.ButikkNrFra = PlListeHode.FraButikkNr
           TT_OvBuffer.ButikkNrTil = PlListeHode.TilButikkNr        
           TT_OvBuffer.ArtikkelNr  = DEC(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
           TT_OvBuffer.Vg          = ArtBas.Vg   
           TT_OvBuffer.LopNr       = ArtBas.LopNr
           TT_OvBuffer.Antall      = DEC(hBuffer:BUFFER-FIELD("AntallPlukket"):BUFFER-VALUE)
           TT_OvBuffer.Merknad     = "Plukkliste PDA"
           TT_OvBuffer.Storl       = StrKonv.Storl
           TT_OvBuffer.TilStorl    = StrKonv.Storl
           TT_OvBuffer.Varekost    = Lager.VVarekost
           piLinjeNr               = piLinjeNr + 1
           /* Setter datoinfo i registrert dato og tid. */
           TT_OvBuffer.RegistrertDato = PlListeHode.DatoPlukket
           TT_OvBuffer.RegistrertTid  = PlListeHode.TidPlukket
           TT_OvBuffer.RegistrertAv   = USERID("SkoTex")
           lAntallPlukket          = lAntallPlukket + DEC(hBuffer:BUFFER-FIELD("AntallPlukket"):BUFFER-VALUE)
           .
  END. /* OVERFORINGSPOST */

  hQuery:GET-NEXT().
END.

OUTPUT CLOSE.

DELETE OBJECT hBuffer NO-ERROR.
DELETE OBJECT hQuery  NO-ERROR.

IF CAN-FIND(FIRST TT_OvBuffer) THEN DO:
    ASSIGN piBuntNr = ?.
    RUN LagraOvBuffer.p (INPUT-OUTPUT piBuntNr,
                         0,
                         (IF iOppdDirekte = 0 THEN "J" ELSE "N") + CHR(1) +
                         "Plukkliste PDA " + STRING(PlListeHode.PlListeId) + " " + 
                           STRING(TODAY) + " " + 
                           STRING(TIME,"HH:MM"), 
                         wEDB-System,
                         wTabell,
                         8).
    EMPTY TEMP-TABLE TT_OvBuffer NO-ERROR.
END.

IF iAntLinjer > 0 THEN
DO TRANSACTION:
    FIND CURRENT PlListeHode EXCLUSIVE-LOCK.
    ASSIGN
        PlListeHode.OverfortDato = TODAY
        PlListeHode.BuntNr       = piBuntNr
        .
    RELEASE PlListeHode.
END.

IF ocReturn = "" THEN obOk = TRUE.

