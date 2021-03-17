/* Vedlikehold kobling til tema fra artikler i varehåndteringsbok  
   Opprettet: 16.01.06 av BHa 
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO INIT YES.

DEF VAR fVarebehNr AS DEC  NO-UNDO.
DEF VAR cRowIdList AS CHAR NO-UNDO.
DEF VAR iVbTemeNr  AS INT  NO-UNDO.
DEF VAR bLeggTil   AS LOG  NO-UNDO.
DEF VAR ix         AS INT  NO-UNDO.
DEF VAR iSeq       AS INT  NO-UNDO.

ASSIGN fVarebehNr = DEC(ENTRY(1,icParam,";"))
       cRowIdList = ENTRY(2,icParam,";")
       iVbTemeNr  = INT(ENTRY(3,icParam,";"))
       bLeggTil   = LOGICAL(ENTRY(4,icParam,";")).

FIND FIRST VarebokTemaHode NO-LOCK
     WHERE VarebokTemaHode.VarebehNr = fVarebehNr
       AND VarebokTemaHode.VbTemeNr  = iVbTemeNr
     NO-ERROR.

IF AVAIL VarebokTemaHode THEN DO ix = 1 TO NUM-ENTRIES(cRowIdList):
  FIND VarebehLinje WHERE ROWID(VarebehLinje) = TO-ROWID(ENTRY(ix,cRowIdList)) NO-LOCK NO-ERROR.
  IF AVAIL VarebehLinje THEN DO:
    FIND FIRST VareBokTemaLinje EXCLUSIVE-LOCK
         WHERE VareBokTemaLinje.ArtikkelNr = VarebehLinje.ArtikkelNr
           AND VareBokTemaLinje.LevNr      = VarebokTemaHode.LevNr
           AND VareBokTemaLinje.VarebehNr  = VarebehLinje.VarebehNr
           AND VareBokTemaLinje.VbTemaNr   = iVbTemeNr
         NO-ERROR.
    IF bLeggTil AND NOT AVAIL VareBokTemaLinje THEN DO:
      iSeq = 0.
      FOR EACH VareBokTemaLinje NO-LOCK
           WHERE VareBokTemaLinje.LevNr      = VarebokTemaHode.LevNr
             AND VareBokTemaLinje.VarebehNr  = VarebehLinje.VarebehNr
             AND VareBokTemaLinje.VbTemaNr   = iVbTemeNr
           BY SeqNr DESC:
        iSeq = VareBokTemaLinje.SeqNr.
        LEAVE.
      END.
      CREATE VareBokTemaLinje.
      ASSIGN VareBokTemaLinje.ArtikkelNr = VarebehLinje.ArtikkelNr
             VareBokTemaLinje.LevNr      = VarebokTemaHode.LevNr     
             VareBokTemaLinje.VarebehNr  = VarebehLinje.VarebehNr 
             VareBokTemaLinje.VbTemaNr   = iVbTemeNr   
             VareBokTemaLinje.SeqNr      = iSeq + 10.
    END.
    ELSE IF NOT bLeggTil AND AVAIL VareBokTemaLinje THEN
      DELETE VareBokTemaLinje.
  END.
END.
