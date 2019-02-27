
def var wAntPrStr     as int extent 48 no-undo.
def var wStorrelser   as char no-undo.
def var wFordeling    as char no-undo.
def var wLoop         as int no-undo.

assign
  current-window:width = 180.

find BestHode no-lock where
  BestHode.BestNr = 2772 no-error.

for each BestSort where 
  BestSort.BestNr = BestHode.BestNr /*and
  BestSort.Fri    = true*/:
  
  assign
    wStorrelser = BestSort.Storrelser
    wAntPrStr   = 0.
  
  display 
    BestSort.BestNr 
    wStorrelser format "x(30)"
    entry(1,wStorrelser," ")
    entry(2,wStorrelser," ")
    entry(3,wStorrelser," ")
    entry(4,wStorrelser," ")
    num-entries(wStorrelser," ")
  with width 178.
  
  
  for each BestStr no-lock where
    BestStr.BestNr   = BestHode.BestNr and
    BestSTr.BestStat = BestHode.BestStat
    break by BestSTr.Butik:
        
    display
      BestStr.BestStat
      BestStr.Butik
      BestStr.Storl
      lookup(trim(BestStr.Storl),wStorrelser," ")
      BestStr.Bestilt (total).
    assign
      wAntPrStr[lookup(trim(BestStr.Storl),wStorrelser," ")] = 
        wAntPrStr[lookup(trim(BestStr.Storl),wStorrelser," ")] + BestStr.Bestilt.
  
  end.
  
  do wLoop = 1 to num-entries(wStorrelser," "):
    assign
      wFordeling = wFordeling +
                      (if wFordeling = ""
                         then ""
                         else " ") +
                      string(
                             wAntPrStr[wLoop]
                            ).                              
                      
    
  end.

  display skip
    wfordeling format "x(40)" 
  .
  
  assign
    BestSort.Fordeling = wFordeling.
 
end.
