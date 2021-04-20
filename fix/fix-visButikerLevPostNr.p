CURRENT-WINDOW:WIDTH = 350.
/* 
  Ukjent andvika storsenter 1337 
  
*/
FOR EACH Butiker NO-LOCK WHERE Butiker.LevPostNr = '1337':
    DISPLAY
    Butiker.butik
    butiker.BuPoNr
    Butiker.LevPostNr
    Butiker.butNamn FORMAT "x(40)"
    Butiker.KortNavn FORMAT "x(20)"
    WITH WIDTH 350.
END.
