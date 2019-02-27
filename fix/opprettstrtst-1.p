def var wSeqnr as  int.

wseqnr = 0.

find last strtstr where
  strtstr.strtypeid = 1 use-index strtstr no-error.
if available strtstr 
  then wSeqNr = strTstr.seqnr + 1.
else wSeqNr = 1.

for each ArtLAg no-lock where
  ArtLag.Vg > 73:
  output to "gurre.dat" append. 
      
  export artlag.vg artlag.lopnr artlag.storl artlag.butik.
  
  find first StrTStr no-lock where
    StrTstr.StrTypeId = 1  and
    StrTStr.SoStorl   = ArtLag.Storl no-error.
  if not available strtstr then
    do:
      assign wSeqNr = wSeqNr + 1.
      create strtstr.
      assign
        strtstr.StrTypeId = 1
        strtstr.seqnr     = wseqnr
        strtstr.sostorl   = artlag.storl. 
    end.
        
  
  output close.
end.
