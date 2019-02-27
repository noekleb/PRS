OUTPUT TO VALUE("exp-pfdaysales.txt").
EXPORT DELIMITER ";" 
    "Butikk"        
    "Butikknavn"
    "Strekkode"  
    "Varetekst"
    "Dato"          
    "Omsetning"     
    "NettoOms"
    "DbKr"
    "Db%"
    "Rabatt"        
    "Antall"        
    "Leverandør"    
    "MvaKr"         
    "Varekost"      
    "Avdeling"
    "AvdNavn"
    "HovedGr"
    "HgBeskr"
    "VarGr"
    "VgBeskr"
    .

for each pfDaySales NO-LOCK WHERE
    pfDaySales.Date >= 03/22/2004 AND
    pfDaySales.Date <= 03/28/2004:

    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = Store_No NO-ERROR.
    FIND Strekkode NO-LOCK WHERE
        Strekkode.Kode = pfDaySales.Plu_code NO-ERROR.
    IF AVAILABLE Strekkode THEN
        FIND ArtBas OF Strekkode NO-ERROR.
    IF AVAILABLE ArtBas THEN
        FIND HuvGr OF ArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE HuvGr THEN
        FIND Avdeling OF HuvGr NO-LOCK NO-ERROR.
    IF AVAILABLE ArtBas THEN
        FIND VarGr OF ArtBas NO-LOCK NO-ERROR.

    EXPORT DELIMITER ";" 
      pfDaySales.Store_No       FORMAT ">>>>>9"      
      (IF AVAILABLE Butiker 
        THEN Butiker.ButNamn
        ELSE "")
      pfDaySales.Plu_code       FORMAT "X(20)"          
      (IF AVAILABLE ArtBas
         THEN ArtBas.Besk
         ELSE "")
      pfDaySales.Date           FORMAT "99.99.9999"     
      pfDaySales.Sales          FORMAT "->>,>>>,>>9.99" 
      (pfDaySales.Sales - pfDaySales.Vat) FORMAT "->>,>>>,>>9.99" 
      (pfDaySales.Sales - pfDaySales.Vat - pfDaySales.SalesCost) FORMAT "->>,>>>,>>9.99" 
      ((pfDaySales.Sales - pfDaySales.Vat - pfDaySales.SalesCost) /
      (pfDaySales.Sales - pfDaySales.Vat) * 100)  FORMAT "->>,>>>,>>9.99" 
      pfDaySales.disc           FORMAT "->,>>>,>>9.99"  
      pfDaySales.Qty            FORMAT "->>,>>9"        
      pfDaySales.VendorId       FORMAT ">>>>>9"         
      pfDaySales.Vat            FORMAT "->,>>>,>>9.99"  
      pfDaySales.SalesCost      FORMAT "->>,>>>,>>9.99" 
      (IF AVAILABLE HuvGr
         THEN string(HuvGr.AvdelingNr)
         ELSE "")
      (IF AVAILABLE Avdeling
         THEN Avdeling.AvdelingNavn
         ELSE "")
      (IF AVAILABLE ArtBas
         THEN string(ArtBas.Hg)
         ELSE "")
      (IF AVAILABLE HuvGr
         THEN HuvGr.HgBesk
         ELSE "")
      (IF AVAILABLE ArtBas
         THEN string(ArtBas.Vg)
         ELSE "")
      (IF AVAILABLE VarGr
         THEN VarGr.VgBesk
         ELSE "")
      SKIP
      .
  
end.  
