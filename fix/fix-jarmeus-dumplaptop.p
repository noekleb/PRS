
def stream ut1 .
def stream Bilder1.
def stream Bilder2.

output stream ut1 to value("jarmeus.log").
output stream Bilder1 to value("bilderegister.d").
output stream Bilder2 to value("bildedata.d").

for each ArtBas where ArtBas.RegistrertDato <= 09/08/99
  break by ArtBas.RegistrertDato
        by ArtBas.ArtikkelNr:
  
  export stream ut1
    ArtBas.ArtikkelNr 
    ArtBas.BildNr.  
    
  for each BildeRegister of ArtBas:
    export stream Bilder1
      BildeRegister.
      
    for each BildeData of BildeRegister:
      export stream Bilder2
        BildeData.
    end.
  end.
      
end.        
