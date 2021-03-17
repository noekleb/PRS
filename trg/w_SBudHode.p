TRIGGER PROCEDURE FOR WRITE OF SBudHode OLD BUFFER oldSBudHode.

{trg\c_w_trg.i &Fil=SBudHode &TYPE=W}

if oldSBudHode.Aktiv = false and SBudHode.Aktiv = true then 
assign 
    SBudHode.AktivertDato = today
    SBudHode.AktivertTid    = time
    SBudHode.AktivertAv     = USERID('SkoTex')
    .
else if oldSBudHode.Aktiv = true and SBudHode.Aktiv = false then 
assign 
    SBudHode.AktivertDato = ?
    SBudHode.AktivertTid    = 0 
    SBudHode.AktivertAv     = USERID('SkoTex')
    .

