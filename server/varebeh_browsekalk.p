/* Samling av alle prosedyrer som kalkulerer verdier for vareh.bok messeregistrering 
   for browser på artikkelnivå (Registrering)
   Opprettet: 11.01.06 av BHa
-------------------------------------------------------------------*/   
PROCEDURE varebehlinje_best_verdi:
  DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
  DEF INPUT PARAM  icParam       AS CHAR  NO-UNDO.
  DEF INPUT PARAM  icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO.

  DEF VAR fSumVerdi    AS DEC NO-UNDO.
  DEF VAR iAntFord     AS INT NO-UNDO.

  IF icParam NE "" THEN DO:

    FIND VareBehLinje WHERE ROWID(VareBehLinje) = irBuffer NO-LOCK NO-ERROR.
    IF AVAIL VarebehLinje THEN DO:
      FOR EACH VareBehLinjeTrans OF VarebehLinje NO-LOCK
          WHERE ButikkNr = INT(ENTRY(1,icParam,"¤")) AND
          (Bestilt1 > 0 OR
           Bestilt2 > 0 OR
           Bestilt3 > 0 OR
           Bestilt4 > 0):

        FOR EACH ArtSort NO-LOCK
            WHERE ArtSort.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
              AND ArtSort.SortId     = VarebehLinjeTrans.Kode
           ,FIRST LevSort OF ArtSort NO-LOCK:

          FOR EACH LevSAnt OF LevSort NO-LOCK:
            iAntFord      = iAntFord + LevSAnt.SoAnt.
          END.
        END.
        fSumVerdi = (Bestilt1 + Bestilt2 + Bestilt3 + Bestilt4) * VarebehLinje.Varekost * MAX(1,iAntFord).
      END.

      IF NUM-ENTRIES(icParam,"¤") > 1 AND INT(ENTRY(2,icParam,"¤")) > fSumVerdi THEN ocReturn = "skiprow".
      ELSE ocReturn = STRING(fSumVerdi).
    END.
  END.
  IF ocReturn = ? THEN ocReturn = "".
END PROCEDURE.

PROCEDURE varebehlinje_bestilt:
  DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
  DEF INPUT PARAM  icParam       AS CHAR  NO-UNDO.
  DEF INPUT PARAM  icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO INIT "no".

  DEF VAR iButikkNr AS INT NO-UNDO.

  IF icParam = "" THEN RETURN.
  ELSE iButikkNr = INT(ENTRY(1,icParam,"¤")).

  FIND VareBehLinje WHERE ROWID(VareBehLinje) = irBuffer NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinje THEN DO:
    ocReturn = STRING(CAN-FIND(FIRST VareBehLinjeTrans OF VarebehLinje WHERE
                            VarebehLinjeTrans.ButikkNr = iButikkNr AND
                            (Bestilt1 > 0 OR
                             Bestilt2 > 0 OR
                             Bestilt3 > 0 OR
                             Bestilt4 > 0)
                      )).
    IF NUM-ENTRIES(icParam,"¤") > 1 THEN DO:
      IF ENTRY(2,icParam,"¤") = "1" AND ocReturn = "no" THEN ocReturn = "skiprow".
      ELSE IF ENTRY(2,icParam,"¤") = "2" AND ocReturn = "yes" THEN ocReturn = "skiprow".
    END.  
  END.
END PROCEDURE.

PROCEDURE varebehlinje_match_merknad:
  DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
  DEF INPUT PARAM  icParam       AS CHAR  NO-UNDO.
  DEF INPUT PARAM  icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO INIT "no".

  DEF VAR ix     AS INT NO-UNDO.
  DEF VAR bMatch AS LOG NO-UNDO.

  IF icParam = "" THEN RETURN.

  FIND VareBehLinje WHERE ROWID(VareBehLinje) = irBuffer NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinje THEN DO ix = 1 TO NUM-ENTRIES(icParam,"¤"):
    IF CAN-DO(VarebehLinje.LinjeMerknad,ENTRY(ix,icParam,"¤")) THEN DO:
      bMatch = TRUE.
      LEAVE.
    END.
  END.
  IF NOT bMatch THEN ocReturn = "skiprow".
END PROCEDURE.

PROCEDURE varebehlinje_bestforslag:
  DEF INPUT PARAM  irBuffer      AS ROWID NO-UNDO.
  DEF INPUT PARAM  icParam       AS CHAR  NO-UNDO.
  DEF INPUT PARAM  icSessionId   AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocReturn      AS CHAR  NO-UNDO INIT "no".

  DEF VAR iButikkNr AS INT NO-UNDO.
  DEF VAR bGodkjent AS LOG NO-UNDO.

  IF icParam = "" THEN RETURN.
  
  iButikkNr = INT(ENTRY(1,icParam,"¤")).

  IF NUM-ENTRIES(icParam,"¤") > 1 THEN 
    bGodkjent = INT(ENTRY(2,icParam,"¤")) = 1.
  ELSE bGodkjent = ?.

  FIND VareBehLinje WHERE ROWID(VareBehLinje) = irBuffer NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinje THEN DO:
    FIND FIRST VarebehLinjeTrans OF VareBehLinje NO-LOCK
         WHERE ButikkNr = iButikkNr
           AND (Bestilt1 > 0 OR Bestilt2 > 0 OR Bestilt3 > 0 OR Bestilt4 > 0) NO-ERROR.
    IF AVAIL VarebehLinjeTrans THEN DO:
      IF bGodkjent NE ? AND VareBehLinjeTrans.GodkjentBestilling NE bGodkjent THEN 
        ocReturn = "skiprow".
      ELSE
        ocReturn = STRING(VareBehLinjeTrans.GodkjentBestilling).
    END.
  END.

END PROCEDURE.

PROCEDURE varebehlinje_rgb:
  DEF INPUT  PARAM irVarebehLinje  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId     AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn        AS CHAR NO-UNDO.

  DEF VAR iColorEntry AS INT NO-UNDO.

  FIND VarebehLinje WHERE ROWID(VarebehLinje) = irVarebehLinje NO-LOCK NO-ERROR.

  IF AVAIL VarebehLinje THEN DO:
    FIND FIRST VarebehHode OF VarebehLinje NO-LOCK.
    FIND FIRST Messe OF VarebehHode NO-LOCK NO-ERROR.
    IF AVAIL Messe THEN DO:
      iColorEntry = LOOKUP(VarebehLinje.LinjeMerknad,Messe.Oppmerking,"¤").
      IF iColorEntry NE 0 AND NUM-ENTRIES(Messe.Fargekoder) GE iColorEntry THEN
        ocReturn = ENTRY(iColorEntry,Fargekoder).
    END.
  END.

END PROCEDURE.

PROCEDURE varebehlinje_tema:
  DEF INPUT  PARAM irVarebehLinje  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam         AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId     AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn        AS CHAR NO-UNDO.

  DEF VAR iVbTemeNr AS INT NO-UNDO.
  
  IF icParam = "" THEN RETURN.
  ELSE iVbTemeNr = INT(icParam).

  FIND VarebehLinje WHERE ROWID(VarebehLinje) = irVarebehLinje NO-LOCK NO-ERROR.

  IF AVAIL VarebehLinje THEN DO:
    FIND FIRST VarebokTemaHode NO-LOCK
         WHERE VarebokTemaHode.VbTemeNr  = iVbTemeNr
           AND VarebokTemaHode.LevNr     GE 0
           AND VarebokTemaHode.VarebehNr = VarebehLinje.VarebehNr 
         NO-ERROR.
          
    IF AVAIL VarebokTemaHode THEN DO:
      FIND FIRST VareBokTemaLinje NO-LOCK
           WHERE VareBokTemaLinje.ArtikkelNr = VarebehLinje.ArtikkelNr
             AND VareBokTemaLinje.LevNr      = VarebokTemaHode.LevNr
             AND VareBokTemaLinje.VarebehNr  = VarebehLinje.VarebehNr
             AND VareBokTemaLinje.VbTemaNr   = iVbTemeNr
           NO-ERROR.
      IF NOT AVAIL VareBokTemaLinje THEN ocReturn = "skiprow".
      ELSE ocReturn = STRING(VareBokTemaLinje.SeqNr).
    END.
  END.
END PROCEDURE.

PROCEDURE artbas_kjedevare:
  DEF INPUT  PARAM irArtBas     AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

  IF icParam = "" THEN RETURN.

  FIND ArtBas WHERE ROWID(ArtBas) = irArtBas NO-LOCK NO-ERROR.

  IF AVAIL ArtBas AND NOT ArtBas.KjedeVare THEN ocReturn = "skiprow".
END PROCEDURE.
