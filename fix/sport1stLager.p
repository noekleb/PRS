
OUTPUT TO "CLIPBOARD".
PUT UNFORMATTED
"Butnr;butnamn;Lagant;Lagerverdi;Solgt;Solgt netto;DbKr;Rabatter;Rabattverdi;VVarekost;Kunderekl;Kunderekl kr;Levrekl;Levrekl kr;Retur lev;Svinn;Svinn kr;Returer kunde;Returer kr;Innkjopt;Innkjopt kr;Brekkasje;Brekkasje kr;Internt forbruk;Internt forbruk kr;Justert;Justert kr;Overført;Overførte kr" SKIP.
/*
DataobjektTXT butnamn  Pos.lager Lagerverdi Solgt Solgt netto /* Solgt% Utsolgt% */ DbKr /* DbAndel% Db% */ Rabatter Rabattverdi /* Rabandel% */ VVarekost Kunderekl Kunderekl kr Levrekl Levrekl kr Retur lev Svinn Svinn kr Returer kunde Returer kr Innkjopt Innkjopt kr /* Kjopandel% */ Brekkasje Brekkasje kr Internt forbruk Internt forbruk kr Justert Justert kr Overført Overførte kr
*/
FOR EACH stlager WHERE sttypeid = "BUTSTAT" NO-LOCK.
    FIND butiker WHERE butiker.butik = INT(TRIM(dataobjekt,"0")) NO-LOCK NO-ERROR.
    IF NOT AVAIL butiker THEN
        NEXT.
    EXPORT DELIMITER ";"
    ("'" + dataobjekt) butiker.butnamn  LagAnt (vvarekost) AntSolgt VerdiSolgt /* Solgt% UtSolgt% */ (stLager.vvarekost - stLager.Svk) /* DbAndel% Db% */ AntRab VerdiRabatt /* Rabandel% */ VVarekost ReklAnt ReklVerdi ReklLAnt ReklLVerdi RetLAnt SvinnAnt SvinnVerdi GjenkjopAnt GjenkjopVerdi KjopAnt KjopVerdi /* Kjopandel% */  BrekkAnt BrekkVerdi IntAnt IntVerdi JustAnt JustVerdi OvAnt OvVerdi.
.
END.
OUTPUT CLOSE.

