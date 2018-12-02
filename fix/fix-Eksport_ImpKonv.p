/* fix-Eksport_ImpKonv.p */

DEF STREAM Ut.

CURRENT-WINDOW:WIDTH = 350.

OUTPUT STREAM Ut TO VALUE('konv\PRSMapping_SportNorge01_' + REPLACE(STRING(TODAY),'/','') + '.csv').
FOR EACH ImpKonv WHERE 
    ImpKonv.EDB-System = 'Sport Norge' AND 
    ImpKonv.Tabell     = 'VarGr':
/*    DISPLAY        */
/*        ImpKonv    */
/*    WITH WIDTH 350.*/

    PUT STREAM Ut UNFORMATTED
        ImpKonv.EDB-System ';'
        ImpKonv.Tabell ';'
        ImpKonv.EksterntId ';'
        ImpKonv.Merknad ';'
        ImpKonv.InterntId
        SKIP.
END.

OUTPUT STREAM Ut CLOSE.
