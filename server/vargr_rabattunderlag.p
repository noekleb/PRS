/* Setter rabattunderlag på artikler i varegruppen.

   Rapport fra varehåndteringsbok
   Parametere: Input: Varebehandlingsnr,butikknr,levnr (butikknr og levnr trenger ikke være med)
               Output (ocReturn): Filnavn
               Merk at antall parametere styrer layout, summeringer, etc
               ENTRY(1,icParam): VarebehNr
               ENTRY(2,icParam): (Liste over) butikknr 
               ENTRY(3,icParam): (Liste over) leverandørnr 
               ENTRY(4,icParam): Evt annen sortering enn default for butikkbekr 
               ENTRY(9,icParam) = "kjedepris": Benytt kjedepris i stedet for varekost
   Opprettet: 13.10.10 av BHa                          
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.


DEF VAR cVg            AS CHAR NO-UNDO.
DEF VAR bBonus_Givende AS LOG  NO-UNDO.

ASSIGN 
    cVg      = ENTRY(2,icParam)
    ocReturn = ''.

IF CAN-DO('yes,1,true,j,ja',ENTRY(1,icParam)) THEN
    bBonus_Givende = TRUE.
ELSE
    bBonus_givende = FALSE.

IF INT(cVg) = 0 OR cVg = ?  THEN
    ocReturn = 'Varegruppe ikke angitt.'.
ELSE IF NOT CAN-FIND(VarGr WHERE
                VarGr.Vg = INT(cVg)) THEN
    ocReturn = 'Ukjent varegruppe!'.
IF ocReturn = '' THEN
DO:
    FOR EACH ArtBas EXCLUSIVE-LOCK WHERE
        ArtBas.Vg = INT(cVg):
        ASSIGN
            ArtBas.Bonus_Givende = bBonus_Givende.
    END.
    obOK = TRUE.
END.
ELSE RETURN ocReturn.

