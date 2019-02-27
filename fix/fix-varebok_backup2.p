

FIND VareBokHode NO-LOCK WHERE
    VareBokHode.VareBokNr = 9000026 NO-ERROR.

OUTPUT TO VALUE('varebok_9000026_bku105012010').
FOR EACH VareBokLinje OF VareBokHode NO-LOCK:
  EXPORT Vareboklinje.
END.
