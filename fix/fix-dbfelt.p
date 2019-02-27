
CURRENT-WINDOW:WIDTH = 200.  
FOR EACH _Field WHERE _Field._Field-Name = "BestNr":
  FIND _File OF _Field NO-LOCK.

  _Field._Format = ">>>>>>>9". 
  DISPLAY
    _File._File-Name
    _Field._Field-Name
    _Field._Format
    WITH WIDTH 198
    .

END.
