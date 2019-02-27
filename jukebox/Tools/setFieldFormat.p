CURRENT-WINDOW:WIDTH-CHARS = 120.
FOR EACH _File NO-LOCK WHERE _File._Tbl-Type = "T"
   ,EACH _Field OF _File
         WHERE _Field._Field-Name = "cModifiedBy"
   :
  _Field._Format = "x(20)".
  DISP _File._File-Name
       _Field._Field-Name
       _Field._Format
       WITH WIDTH 120.
END.
