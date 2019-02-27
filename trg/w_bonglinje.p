TRIGGER PROCEDURE FOR WRITE OF BongLinje.

assign
  BongLinje.AaaaMmDd = string(year(BongLinje.TransDato),"9999") + 
                       string(month(BongLinje.TransDato),"99") + 
                       string(day(BongLinje.TransDato),"99")
                       .


