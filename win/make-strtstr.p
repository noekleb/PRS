def var wStr as char no-undo.
def var wLoop as int no-undo.

for each ArtLag no-lock /* where
  Artlag.Vg = 1 and
  ArtLag.LopNr = 807 */:
    
  if lookup(ArtLAg.Storl,wStr,";") <> 0 then next.
  else do:
    assign 
      wLoop = wLoop + 1
      wStr = wStr + 
             (if wStr <> ""
               then ";"
               else "") +           
             ArtLag.Storl.
    pause 0.
    display wLoop skip wstr format "x(70)" with frame g.
  end.

end.

def temp-table bStorl
  field Storl as char.

do wLoop = 1 to num-entries(wStr,";"):
  create bStorl.
  assign bStorl.Storl = entry(wLoop,wStr,";").
end.

wLoop = 0.
for each bStorl:
  assign wLoop = wLoop + 1.
  create
    StrTStr.
  assign
    StrTstr.StrTypeId = 1
    StrTSTr.SeqNr     = wLoop
    StrTStr.SoStorl   = bStorl.Storl.
end.
