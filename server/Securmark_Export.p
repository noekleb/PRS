
DEFINE VARIABLE cRubr      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cExportStr AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFileName  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDatoFodt  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dD         AS DATE        NO-UNDO.
DEFINE VARIABLE cDatoSolgt AS CHARACTER   NO-UNDO.
IF NOT CAN-FIND(FIRST Falck_Sykkelregister WHERE Falck_Sykkelregister.KlarForSending = TRUE AND Falck_Sykkelregister.EksportertDato = ?) THEN
    RETURN.

FIND FIRST butiker WHERE TRIM(Butiker.FalckMedlNr) <> "" NO-LOCK NO-ERROR.
IF NOT AVAIL butiker THEN
    RETURN.
cFileName = "c:\home\Lindbak\Securmark\" + TRIM(Butiker.FalckMedlNr) + "-" + STRING(YEAR(TODAY),"9999") + 
                                                                             STRING(MONTH(TODAY),"99") + 
                                                                             STRING(DAY(TODAY),"99") + 
                                                                             REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt".

cRubr = "ForhandlerID;Fornavn;Etternavn;Adresse;Postnr;Poststed;Epost;Telefon;Mobiltelefon;Fødselsdato;Pris;Kjøpsdato;Fabrikat;Modell;SECnr;Rammenr;Årsmodell;Farge;AvtaleID".

OUTPUT TO VALUE(cFilename).
PUT UNFORMATTED cRubr SKIP.
FOR EACH Falck_Sykkelregister WHERE Falck_Sykkelregister.KlarForSending = TRUE AND Falck_Sykkelregister.EksportertDato = ?:
/*     dD = Falck_Sykkelregister.Eier_Fodt.                                                                                               */
/*     cDatoFodt  = IF dD = ? THEN "0000-00-00" ELSE STRING(YEAR(dD),"9999") + "-" + STRING(MONTH(dD),"99") + "-" + STRING(DAY(dD),"99"). */
    dD = Falck_Sykkelregister.Dato_Solgt.
    cDatoSolgt = IF dD = ? THEN "0000-00-00" ELSE STRING(YEAR(dD),"9999") + "-" + STRING(MONTH(dD),"99") + "-" + STRING(DAY(dD),"99").
    cExportStr = FILL(";",18).
    ASSIGN ENTRY( 1,cExportStr,";") = Falck_Sykkelregister.Kampanjekode
           ENTRY( 2,cExportStr,";") = Falck_Sykkelregister.Eier_Fornavn
           ENTRY( 3,cExportStr,";") = Falck_Sykkelregister.Eier_Etternavn
           ENTRY( 4,cExportStr,";") = Falck_Sykkelregister.Eier_Adresse
           ENTRY( 5,cExportStr,";") = Falck_Sykkelregister.Eier_PostNr
           ENTRY( 6,cExportStr,";") = Falck_Sykkelregister.Eier_Poststed
           ENTRY( 7,cExportStr,";") = Falck_Sykkelregister.Eier_epost
           ENTRY( 8,cExportStr,";") = Falck_Sykkelregister.Eier_Telefon
           ENTRY( 9,cExportStr,";") = Falck_Sykkelregister.Eier_Mobil
           ENTRY(10,cExportStr,";") = cDatoFodt
           ENTRY(11,cExportStr,";") = STRING(ROUND(Falck_Sykkelregister.Pris,0))
           ENTRY(12,cExportStr,";") = cDatoSolgt
           ENTRY(13,cExportStr,";") = Falck_Sykkelregister.Fabrikat
           ENTRY(14,cExportStr,";") = Falck_Sykkelregister.Sykkeltype
           ENTRY(15,cExportStr,";") = Falck_Sykkelregister.Sec_Nr
           ENTRY(16,cExportStr,";") = Falck_Sykkelregister.Rammenummer
           ENTRY(17,cExportStr,";") = Falck_Sykkelregister.Arsmodell
           ENTRY(18,cExportStr,";") = "-"
           ENTRY(19,cExportStr,";") = STRING(Falck_Sykkelregister.AvtaleID).

    PUT UNFORMATTED cExportStr SKIP.
    Falck_Sykkelregister.EksportertDato = TODAY.
END.
OUTPUT CLOSE.

QUIT.
/* 
FOR EACH Falck_Sykkelregister.
    ASSIGN Falck_Sykkelregister.EksportertDato = TODAY
           Falck_Sykkelregister.EksportertTid  = TIME.
END.
 */

