/* Kontroll av varebok. */

current-window:width = 200.

def var lVareBokNr1 like VareBokHode.VareBokNr no-undo.
def var lVareBokNr2 like VareBokHode.VareBokNr no-undo.

def buffer bVareBokLinje for VareBokLinje.

def stream ut.

assign
  lVareBokNr1 = 90000079
  lVareBokNr2 = 90000075
  .
  
for each VareBokLinje exclusive-lock where
  VareBokLinje.VareBokNr = lVareBokNr1:
  
  find bVareBokLinje no-lock where
    bVareBokLinje.VareBokNr = lVareBokNr2 and
    bVareBokLinje.ArtikkelNr = VareBokLinje.ArtikkelNR no-error.
  
  if not available bVareBokLinje then
  do:
    /*
    pause 0.
    display
    VareBokLinje.VareBokNr
    bVareBokLinje.VareBokNr when available bVareBokLinje
    VareBokLinje.ArtikkelNr
    VareBokLinje.Beskr
    VareBokLinje.LevKod
    VareBokLinje.LevFarg
    with width 200.
   */ 
   create bVarebokLinje.
   buffer-copy vareboklinje except VareBokNr
               to bVareboklinje
   assign
          bVareBokLinje.VareBokNr = lVareBokNr2 
          .
  end.
end.  

  
