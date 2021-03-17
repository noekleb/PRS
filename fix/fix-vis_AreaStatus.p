CURRENT-WINDOW:WIDTH = 350. 
FOR EACH _AreaStatus /* where
    (NOT _AreaStatus-Areaname MATCHES "*AfterImageArea*" ) */
    no-lock:
    display
        _AreaStatus-Areanum FORMAT ">>>" COLUMN-LABEL "Num"
        _AreaStatus-Areaname FORMAT "x(20)" COLUMN-LABEL "AreaName"
        _AreaStatus-Totblocks COLUMN-LABEL "Totblocks"
        _AreaStatus-Hiwater COLUMN-LABEL "Highwatermark"
        _AreaStatus-Hiwater / _AreaStatus-Totblocks * 100 column-label "%use"
        _AreaStatus-Extents FORMAT ">>>" COLUMN-LABEL "NumExtents"
        _AreaStatus-Freenum COLUMN-LABEL "Freenum"
        _AreaStatus-Rmnum COLUMN-LABEL "RMnum"
    WITH WIDTH 350.
end.
