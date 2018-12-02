TRIGGER PROCEDURE FOR WRITE OF VPIStrekkode.

{trg\c_w_trg.i &Fil=VpiStrekkode &Type="W"}

/* Initierer ERPNr hvis det er blankt. */  
IF (VPIStrekkode.StrKode > 0 AND 
    VPIStrekkode.VareNr <> '' AND 
    VPIStrekkode.ERPNr = '') THEN
    ASSIGN VPIStrekkode.ERPNr = string(VPISTrekkode.VareNr) + trim(string(VPIStrekkode.StrKode,">>999")).
