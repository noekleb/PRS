DEF INPUT  PARAM irVarebehLinjeThode AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocValue             AS CHAR NO-UNDO.

FIND VarebehLinjeThode WHERE ROWID(VarebehLinjeThode) = irVarebehLinjeThode NO-LOCK NO-ERROR.
IF AVAIL VarebehLinjeThode THEN DO:
  FIND FIRST VarebehLinjeTrans NO-LOCK
       WHERE VarebehLinjeTrans.VarebehNr = VarebehLinjeThode.VarebehNr
         AND VarebehLinjeTrans.ArtikkelNr > 0
         AND VarebehLinjeTrans.RegistrertBestilling
         AND VarebehLinjeTrans.ButikkNr = VarebehLinjeThode.ButikkNr
       NO-ERROR.
  IF NOT AVAIL VarebehLinjeTrans THEN
    ocValue = "skiprow".
END.
