TRIGGER PROCEDURE FOR CREATE OF ELogg.

{trg\c_w_trg.i &Fil=SkoTex.ELogg &Type=C}
assign
  Elogg.Opprettet = dec(
                        string(Year(today),"9999") +
                        string(month(today),"99") + 
                        string(day(today),"99") +
                        string(time)
                       ).


