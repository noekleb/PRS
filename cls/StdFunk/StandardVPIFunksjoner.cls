 
 /*------------------------------------------------------------------------
    File        : standardfunksjoner
    Purpose     : 
    Syntax      : 
    Description : Generelt procedurebibliotek
    Author(s)   : tny
    Created     : Thu Apr 28 14:06:07 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

/* BLOCK-LEVEL ON ERROR UNDO, THROW. */

CLASS cls.StdFunk.StandardVPIFunksjoner: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLogg  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iX     AS INTEGER   NO-UNDO.       
    DEFINE VARIABLE cTxt   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE obOk              AS LOG       NO-UNDO.
    DEFINE VARIABLE cIpAdr            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStdStrTypeLst    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.

	CONSTRUCTOR PUBLIC StandardVPIFunksjoner (  ):
		SUPER ().

        ASSIGN 
            cLogg = 'StandardVPIFunksjoner' + REPLACE(STRING(TODAY),'/','') 
            .
        {syspara.i 50 15 51 cStdStrTypeLst}
        IF cStdStrTypeLst = '' THEN 
            cStdStrTypeLst = "900,999".
		
	END CONSTRUCTOR.

    CONSTRUCTOR PUBLIC StandardVPIFunksjoner ( INPUT pcLogg AS CHARACTER ):
        SUPER ().

        ASSIGN 
            cLogg = pcLogg 
            .
        
        {syspara.i 50 15 51 cStdStrTypeLst}
        IF cStdStrTypeLst = '' THEN 
            cStdStrTypeLst = "900,999".
    END CONSTRUCTOR.


    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID setVPIStdStrType( INPUT piEkstVPILevNr AS INTEGER,
                                         INPUT plArtikkelNr AS DECIMAL,
                                         INPUT-OUTPUT piStrTypeId AS INTEGER ):
        DEFINE VARIABLE piStrTypeId1 AS INTEGER NO-UNDO.
        DEFINE VARIABLE piStrTypeId2 AS INTEGER NO-UNDO.
        DEFINE VARIABLE piValgtStrTypeId AS INTEGER NO-UNDO.
        DEFINE VARIABLE pbFlagg AS LOG NO-UNDO.

        IF NUM-ENTRIES(cStdStrTypeLst) = 2 THEN 
            ASSIGN 
                piStrTypeId1 = INT(ENTRY(1,cStdStrTypeLst))
                piStrTypeId2 = INT(ENTRY(2,cStdStrTypeLst))
            .
        FIND VPIArtBas NO-LOCK WHERE
            VPIArtBas.EkstVPILevNr = piEkstVPILEvNr AND
            VPIArtBas.ArtikkelNr = plArtikkelNr NO-ERROR.
        IF AVAILABLE VPIArtBas THEN     
        STRTYPESJEKK:
        DO:
            FIND ArtBas NO-LOCK WHERE
                ArtBas.ArtikkelNr = plArtikkelNr NO-ERROR.
            IF AVAILABLE ArtBas THEN 
            DO:
                IF ArtBas.StrTypeID >= piStrTypeId1 AND 
                   ArtBas.StrTypeID <= piStrTypeId2 THEN
                DO: 
                   ASSIGN 
                       piStrtypeId         = ArtBas.StrTypeID
                       VPIArtBas.StrTypeID = ArtBas.StrTypeID
                   .
                   LEAVE STRTYPESJEKK.
                END. 
            END.    
                
            IF AVAILABLE VPIArtBas AND piStrTypeId1 > 0 THEN
            DO:
                STRTYPER:
                FOR EACH StrType NO-LOCK WHERE
                    StrType.StrTypeID >= piStrTypeId1 AND
                    StrType.StrTypeID <= piStrTypeId2:
    
                    pbFlagg = TRUE.
                    /* Tar ikke hensyn til PLU'er. */
                    FOR EACH VPIStrekKode OF VPIArtBas NO-LOCK WHERE 
                        LENGTH(VPIStrekKode.Kode) > 7:
                        IF NOT CAN-DO(StrType.Fordeling,STRING(VPIStrekKode.StrKode)) THEN
                        DO:
                            pbFlagg = FALSE.
                            NEXT STRTYPER.
                        END.
                    END.
    
                    IF pbFlagg THEN
                    DO:
                        FIND CURRENT VPIArtBas EXCLUSIVE-LOCK.
                        ASSIGN
                            VPIArtBas.StrTypeID = StrType.StrTypeID
                            piStrtypeId         = StrType.StrTypeID
                            .
                        FIND CURRENT VPIArtBas NO-LOCK.
                        LEAVE STRTYPER.
                    END.
                END. /*STRTYPER*/
            END.
        END. /* STRTYPESJEKK */
        RETURN.

    END METHOD.


	DESTRUCTOR PUBLIC StandardVPIFunksjoner ( ):

	END DESTRUCTOR.

END CLASS.