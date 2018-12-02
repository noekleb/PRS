/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       RUN vpiartbas_kopier_vpi_til_vpi.p (iEkstVPILevNr,1,VPIArtBas.ArtikkelNr).
  
  NB: Denne rutinen kopierer ikke pakkeartikkel eller Alternativ varekobling.
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER iFraVPI     AS INT  NO-UNDO.
DEF INPUT PARAMETER iTilVPI     AS INT  NO-UNDO.
DEF INPUT PARAMETER cVareNr     AS CHAR NO-UNDO.

DEF BUFFER bufVPIArtBas        FOR VPIArtBas.
DEF BUFFER bufVPIStrekKode     FOR VPIStrekkode.
DEF BUFFER bufVPIArtPris       FOR VPIArtPris.
DEF BUFFER bufVPIAltLevBas     FOR VPIAltLevBas.
DEF BUFFER bufVPIArtBestPk     FOR VPIArtBestPk.
DEF BUFFER bufVPIBildeRegister FOR VPIBildeRegister.
DEF BUFFER bufVPIBildeData     FOR VPIBildeData.
DEFINE BUFFER bufVPIArtBasKarakteristikk FOR VPIArtBasKarakteristikk.

MAINBLOKK:
DO TRANSACTION:
    /* Record det skal kopieres fra */
    FIND bufVPIArtBas WHERE bufVPIArtBas.EkstVPILevNr = iFraVPI AND
                            bufVPIArtBas.VareNr       = cVareNr NO-ERROR.
    IF NOT AVAILABLE bufVPIArtBas THEN
        LEAVE MAINBLOKK.
    IF bufVPIArtBas.Pakke = TRUE THEN
        LEAVE MAINBLOKK.
    /* Record det skal kopieres til */
    FIND VPIArtBas WHERE VPIArtBas.EkstVPILevNr = iTilVPI AND
                         VPIArtBas.VareNr       = cVareNr NO-ERROR.

    /* Renser bort VPI hvis den finnes fra før */
    IF AVAIL VPIArtBas THEN DO:
        FOR EACH VPIArtPris OF VPIArtBas:
            DELETE VPIArtPris.
        END.
        FIND VPIBilderegister WHERE VPIBilderegister.EkstVPILevNr = iTilVPI AND
                                    VPIBilderegister.VareNr       = VPIArtBas.VareNr AND
                                    VPIBilderegister.BildNr       = VPIArtBas.BildNr NO-ERROR.
        IF AVAIL VPIBilderegister THEN DO:
            FOR EACH VPIBildeData OF VPIBilderegister:
                DELETE VPIBildeData.
            END.
            DELETE VPIBilderegister.
        END.
        FOR EACH VPIStrekKode OF VPIArtBas:
            DELETE VPIStrekKode.
        END.
        FOR EACH VPIAltLevBas WHERE
            VPIAltLevBas.EkstVPILevNr = iTilVPI AND
            VPIAltLevBas.VareNr       = VPIArtBas.VareNr:
            DELETE VPIAltLevBas.
        END.
        FOR EACH VPIArtBestPkt OF VPIArtBas:
            DELETE VPIArtBestPkt.
        END.
        FOR EACH VPIArtBasKarakteristikk OF VPIArtBas:
            DELETE VPIArtBasKarakteristikk.
        END.
        DELETE VPIArtBas.
    END.

    /* Så kopierer vi postene - det er nå tomt på mottagersiden. */
    IF AVAIL bufVPIArtBas THEN DO:
        CREATE VPIArtBas.
        BUFFER-COPY bufVPIArtBas 
            TO VPIArtBas
            ASSIGN VPIArtBas.EkstVPILevNr = iTilVPI
                   .
        FOR EACH bufVPIStrekKode OF bufVPIArtBas NO-LOCK:
            CREATE VPIStrekKode.
            BUFFER-COPY bufVPIStrekKode TO VPIStrekKode
                ASSIGN VPIStrekKode.EkstVPILevNr = iTilVPI.
        END.
        FOR EACH bufVPIArtPris OF bufVPIArtBas NO-LOCK.
            CREATE VPIArtPris.
            BUFFER-COPY bufVPIArtPris TO VPIArtPris
                ASSIGN VPIArtPris.EkstVPILevNr = iTilVPI.
        END.
        FOR EACH bufVPIAltLevBas NO-LOCK WHERE
            bufVPIAltLevBas.EkstVPILevNr = iFraVPI AND
            bufVPIAltLevBas.VareNr       = bufVPIArtBas.VareNr AND
            bufVPIAltLevBas.LevNr       > 0:
            IF NOT CAN-FIND(VPIAltLevBas WHERE
                            VPIAltLevBas.EkstVPILevNr = iTilVPI AND
                            VPIAltLevBas.VareNr       = bufVPIAltLevBas.VareNr AND
                            VPIAltLevBas.LevNr        = bufVPIAltLevBas.LevNr) THEN
            DO:
                CREATE VPIAltLevBas.
                BUFFER-COPY bufVPIAltLevBas TO VPIAltLevBas
                    ASSIGN VPIAltLevBas.EkstVPILevNr = iTilVPI
                    NO-ERROR.
            END.
        END.
        FOR EACH bufVPIArtBestPk OF bufVPIArtBas NO-LOCK:
            CREATE VPIArtBestPk.
            BUFFER-COPY bufVPIArtBestPk TO VPIArtBestPk
                ASSIGN VPIArtBestPk.EkstVPILevNr = iTilVPI
                NO-ERROR.
        END.
        FOR EACH bufVPIArtBasKarakteristikk OF bufVPIArtBas NO-LOCK.
            CREATE VPIArtBasKarakteristikk.
            BUFFER-COPY bufVPIArtBasKarakteristikk TO VPIArtBasKarakteristikk
                ASSIGN VPIArtBasKarakteristikk.EkstVPILevNr = iTilVPI NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE VPIArtBasKarakteristikk.
        END.
        IF bufVPIArtBas.BildNr > 0 THEN
            FIND bufVPIBildeRegister NO-LOCK WHERE
                bufVPIBildeRegister.EkstVPILevNr = iTilVPI AND
                bufVPIBildeRegister.BildNr       = bufVPIArtBas.BildNr
                NO-ERROR.
        ELSE DO:
            IF AVAILABLE bufVPIBildeRegister THEN
                RELEASE bufVPIBildeRegister.
        END.

        IF AVAIL bufVPIBildeRegister THEN 
        DO:
            CREATE VPIBildeRegister.
            BUFFER-COPY bufVPIBildeRegister TO VPIBilderegister
                ASSIGN VPIBilderegister.EkstVPILevNr = iTilVPI.

            FOR EACH bufVPIBildeData OF bufVPIBildeRegister NO-LOCK:
                CREATE VPIBildeData.
                BUFFER-COPY bufVPIBildeData TO VPIBildeData
                ASSIGN VPIBildeData.EkstVPILevNr = iTilVPI.
            END.
        END.
    END.

END. /* MAINBLOKK */

