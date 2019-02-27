
def var wRapLib as char no-undo.
def var wRapLst as char no-undo.
def var wRapAnt as int  no-undo.


run aderb/_getname.p (wRapLib,
                      output wRapLst,
                      output  wRapAnt).
   
message "Rapporter:" wRapLst skip
        "Rap.Antall:" wRapAnt skip
        view-as alert-box title "RapportLib. info".   
