FOR EACH FakturaHode EXCLUSIVE-LOCK WHERE 
      FakturaHode.FakturertDato <= 09/11/2020:
    FakturaHode.EksportertDato = ?.
    FakturaHode.EksportertAv = ''.
END.
