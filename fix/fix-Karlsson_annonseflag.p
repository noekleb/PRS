/* Dessa leverantörer skall */
/* 1   adidas Norge as                */
/* 6   Trygve Alm A/S                 */
/* 10  Bergans Fritid AS              */
/* 11  Great Outdoors AS              */
/* 12  Bios Norge AS                  */
/* 18  Devold of Norway AS            */
/* 19  Elbe Normark AS                */
/* 38  Karlsson AS                    */
/* 39  EAGLE Products AS              */
/* 48  Ajungilak AS                   */
/* 49  Pure Fishing Norway AS         */
/* 52  Ramo A/S                       */
/* 54  Rottefella AS                  */
/* 68  Swix Sport AS                  */
/* 69  Sølvkroken AS                  */
/* 70  Torinor Sport AS               */
/* 72  Vertikal AS                    */
/* 76  Åsnes Ski AS                   */
/* 157 Madshus AS                     */
/* 159 O. Mustad & Søn AS             */
/* 163 Asics Norge AS                 */
/* 169 Tretorn Norge                  */
/* 173 Frisport AS                    */
/* 183 Nordic Fitness AS              */
/* 189 Speedo Northern Europe         */
/* 191 Select Sport Norge AS          */
/* 209 Shimano Nordic Cycle AS        */
/* 223 Nordic Outdoor AS              */
/* 441 Silva Norge AS                 */
/* 465 Brusletto & Co AS              */
/* 487 AssistSport (tidl Hydro Sport) */
/* 499 SportAgile AS                  */
/* 633 Midelfart AS                   */
/* 924 Beacon Products AS             */

DEFINE VARIABLE cLevnr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
cLevNr = "1,6,10,11,12,18,19,38,39,48,49,52,54,68,69,70,72,76,157,159,163,169,173,183,189,191,209,223,441,465,487,499,633,924".

DO iCount = 1 TO NUM-ENTRIES(cLevNr).
    FOR EACH Artbas WHERE artbas.levnr = INT(ENTRY(iCount,cLevnr)) AND ArtBas.AnonseArtikkel = TRUE:
        ArtBas.AnonseArtikkel = FALSE.
    END.
END.
