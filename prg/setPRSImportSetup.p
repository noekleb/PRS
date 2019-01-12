/* Setter opp VPI leverandør FOR PRS IMPORT */
DEFINE VARIABLE cImportKatalog AS CHARACTER NO-UNDO.

DEF BUFFER bEkstVPIFil FOR EkstVPIFil.

{syspara.i 1 1 52 cImportKatalog}
IF cImportKatalog = '' THEN 
  cImportKatalog = '.\kom\in'.

RUN setVPIFiltyper. /* Gml */
RUN OpprettVPIFilType.
RUN OpprettVPIleverandor.
RUN OpprettVPIFiler.

/* **********************  Internal Procedures  *********************** */

PROCEDURE OpprettVPIFilType:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
    IF NOT CAN-FIND(VPIFilType WHERE 
                    VPIFilType.VPIFilTypeNr = 22) THEN 
    DO TRANSACTION:
      CREATE VPIFilType.
      ASSIGN
          VPIFilType.VPIFilTypeNr = 22
          VPIFilType.VPIFilTypeBeskrivelse = 'Butikkregister'
          VPIFiltype.VPIFilTypeKNavn       = 'BUTREG'.
      RELEASE VPIFilType.
    END. /* TRANSACTION */

    IF NOT CAN-FIND(VPIFilType WHERE 
                    VPIFilType.VPIFilTypeNr = 23) THEN 
    DO TRANSACTION:
      CREATE VPIFilType.
      ASSIGN
          VPIFilType.VPIFilTypeNr = 23
          VPIFilType.VPIFilTypeBeskrivelse = 'Leverandørregister'
          VPIFiltype.VPIFilTypeKNavn       = 'LEVREG'.
      RELEASE VPIFilType.
    END. /* TRANSACTION */

    IF NOT CAN-FIND(VPIFilType WHERE 
                    VPIFilType.VPIFilTypeNr = 24) THEN 
    DO TRANSACTION:
      CREATE VPIFilType.
      ASSIGN
          VPIFilType.VPIFilTypeNr = 24
          VPIFilType.VPIFilTypeBeskrivelse = 'Størrelsestyperegister'
          VPIFiltype.VPIFilTypeKNavn       = 'STRTYP'.
      RELEASE VPIFilType.
    END. /* TRANSACTION */

    IF NOT CAN-FIND(VPIFilType WHERE 
                    VPIFilType.VPIFilTypeNr = 25) THEN 
    DO TRANSACTION:
      CREATE VPIFilType.
      ASSIGN
          VPIFilType.VPIFilTypeNr = 25
          VPIFilType.VPIFilTypeBeskrivelse = 'Varemerkeregister'
          VPIFiltype.VPIFilTypeKNavn       = 'VAREM'.
      RELEASE VPIFilType.
    END. /* TRANSACTION */

    IF NOT CAN-FIND(VPIFilType WHERE 
                    VPIFilType.VPIFilTypeNr = 26) THEN 
    DO TRANSACTION:
      CREATE VPIFilType.
      ASSIGN
          VPIFilType.VPIFilTypeNr = 26
          VPIFilType.VPIFilTypeBeskrivelse = 'Salgsenheter'
          VPIFiltype.VPIFilTypeKNavn       = 'SENH'.
      RELEASE VPIFilType.
    END. /* TRANSACTION */

    IF NOT CAN-FIND(VPIFilType WHERE 
                    VPIFilType.VPIFilTypeNr = 27) THEN 
    DO TRANSACTION:
      CREATE VPIFilType.
      ASSIGN
          VPIFilType.VPIFilTypeNr = 27
          VPIFilType.VPIFilTypeBeskrivelse = 'Varegrupper'
          VPIFiltype.VPIFilTypeKNavn       = 'VG'.
      RELEASE VPIFilType.
    END. /* TRANSACTION */
    IF NOT CAN-FIND(VPIFilType WHERE 
                    VPIFilType.VPIFilTypeNr = 28) THEN 
    DO TRANSACTION:
      CREATE VPIFilType.
      ASSIGN
          VPIFilType.VPIFilTypeNr = 28
          VPIFilType.VPIFilTypeBeskrivelse = 'VARETELL'
          VPIFiltype.VPIFilTypeKNavn       = 'Varetelling'.
      RELEASE VPIFilType.
    END. /* TRANSACTION */
    IF NOT CAN-FIND(VPIFilType WHERE 
                    VPIFilType.VPIFilTypeNr = 29) THEN 
    DO TRANSACTION:
      CREATE VPIFilType.
      ASSIGN
          VPIFilType.VPIFilTypeNr = 29
          VPIFilType.VPIFilTypeBeskrivelse = 'MATERIAL'
          VPIFiltype.VPIFilTypeKNavn       = 'Materialkoder'.
      RELEASE VPIFilType.
    END. /* TRANSACTION */

    IF NOT CAN-FIND(VPIFilType WHERE 
                    VPIFilType.VPIFilTypeNr = 30) THEN 
    DO TRANSACTION:
      CREATE VPIFilType.
      ASSIGN
          VPIFilType.VPIFilTypeNr = 30
          VPIFilType.VPIFilTypeBeskrivelse = 'TRANS'
          VPIFiltype.VPIFilTypeKNavn       = 'Translogg transaksjoner'.
      RELEASE VPIFilType.
    END. /* TRANSACTION */
    
    IF NOT CAN-FIND(VPIFilType WHERE 
                    VPIFilType.VPIFilTypeNr = 31) THEN 
    DO TRANSACTION:
      CREATE VPIFilType.
      ASSIGN
          VPIFilType.VPIFilTypeNr = 31
          VPIFilType.VPIFilTypeBeskrivelse = 'FARG'
          VPIFiltype.VPIFilTypeKNavn       = 'Fargekoder'.
      RELEASE VPIFilType.
    END. /* TRANSACTION */
    
    IF NOT CAN-FIND(VPIFilType WHERE 
                    VPIFilType.VPIFilTypeNr = 32) THEN 
    DO TRANSACTION:
      CREATE VPIFilType.
      ASSIGN
          VPIFilType.VPIFilTypeNr = 32
          VPIFilType.VPIFilTypeBeskrivelse = 'GAVTIL'
          VPIFiltype.VPIFilTypeKNavn       = 'GAvekort og tilgodelapper'.
      RELEASE VPIFilType.
    END. /* TRANSACTION */

    IF NOT CAN-FIND(VPIFilType WHERE 
                    VPIFilType.VPIFilTypeNr = 33) THEN 
    DO TRANSACTION:
      CREATE VPIFilType.
      ASSIGN
          VPIFilType.VPIFilTypeNr = 33
          VPIFilType.VPIFilTypeBeskrivelse = 'MAPPING'
          VPIFiltype.VPIFilTypeKNavn       = 'Mapping ved PRS import'.
      RELEASE VPIFilType.
    END. /* TRANSACTION */

    IF NOT CAN-FIND(VPIFilType WHERE 
        VPIFilType.VPIFilTypeNr = 34) THEN 
    DO TRANSACTION:
        CREATE VPIFilType.
        ASSIGN
            VPIFilType.VPIFilTypeNr          = 34
            VPIFilType.VPIFilTypeBeskrivelse = 'POST'
            VPIFiltype.VPIFilTypeKNavn       = 'Mapping ved PostNr. import'.
        RELEASE VPIFilType.
    END. /* TRANSACTION */

    IF NOT CAN-FIND(VPIFilType WHERE 
        VPIFilType.VPIFilTypeNr = 35) THEN 
    DO TRANSACTION:
        CREATE VPIFilType.
        ASSIGN
            VPIFilType.VPIFilTypeNr          = 35
            VPIFilType.VPIFilTypeBeskrivelse = 'KAMP'
            VPIFiltype.VPIFilTypeKNavn       = 'Kampanje'.
        RELEASE VPIFilType.
    END. /* TRANSACTION */

    IF NOT CAN-FIND(VPIFilType WHERE 
        VPIFilType.VPIFilTypeNr = 36) THEN 
    DO TRANSACTION:
        CREATE VPIFilType.
        ASSIGN
            VPIFilType.VPIFilTypeNr          = 36
            VPIFilType.VPIFilTypeBeskrivelse = 'SBUD'
            VPIFiltype.VPIFilTypeKNavn       = 'Salgsbudsjett'.
        RELEASE VPIFilType.
    END. /* TRANSACTION */

    IF NOT CAN-FIND(VPIFilType WHERE 
        VPIFilType.VPIFilTypeNr = 37) THEN 
    DO TRANSACTION:
        CREATE VPIFilType.
        ASSIGN
            VPIFilType.VPIFilTypeNr          = 37
            VPIFilType.VPIFilTypeBeskrivelse = 'FEDAS varegrupper'
            VPIFiltype.VPIFilTypeKNavn       = 'FEDASGrp'.
        RELEASE VPIFilType.
    END. /* TRANSACTION */

    IF NOT CAN-FIND(VPIFilType WHERE 
        VPIFilType.VPIFilTypeNr = 38) THEN 
    DO TRANSACTION:
        CREATE VPIFilType.
        ASSIGN
            VPIFilType.VPIFilTypeNr          = 38
            VPIFilType.VPIFilTypeBeskrivelse = 'Country Codes'
            VPIFiltype.VPIFilTypeKNavn       = 'CountryCodes'.
        RELEASE VPIFilType.
    END. /* TRANSACTION */
END PROCEDURE.

PROCEDURE OpprettVPIleverandor:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/

    /* Opprettelse av VPI leveranødør */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 915) THEN
    DO TRANSACTION:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 915
            EkstVPILev.KortNavn     = "PRS Import"
            EkstVPILev.Navn         = "PRS Importer"
            EkstVPILev.AktivLev     = FALSE
            .
        RELEASE EkstVPILev.
    END. /* TRANSACTION */

END PROCEDURE.

PROCEDURE OpprettVPIFiler:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/

    /* Opprettelse av import for PRS Pricat */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 915 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 915 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 1
            bEkstVPIFil.VPIFilBeskrivelse     = "PRS Pricat"
            bEkstVPIFil.VPIFilNavn            = "PRSPricat"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine  = "xPRSPricatInnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xPRSPricatUtpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */

    /* Opprettelse av import for PRS butikkregister */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 915 AND
                    bEkstVPIFil.VPIFilNr     = 2) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 915 
            bEkstVPIFil.VPIFilNr              = 2
            bEkstVPIFil.VPIFilType            = 22
            bEkstVPIFil.VPIFilBeskrivelse     = "PRS Butikkregister"
            bEkstVPIFil.VPIFilNavn            = "PRSBut"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine  = "xPRSButInnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */

    /* Opprettelse av import for PRS butikkregister */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 915 AND
                    bEkstVPIFil.VPIFilNr     = 3) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 915 
            bEkstVPIFil.VPIFilNr              = 3
            bEkstVPIFil.VPIFilType            = 23
            bEkstVPIFil.VPIFilBeskrivelse     = "PRS Leverandørregister"
            bEkstVPIFil.VPIFilNavn            = "PRSLev"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine  = "xPRSLevInnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */
    
    /* Opprettelse av import for PRS størrelsestyper */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 915 AND
                    bEkstVPIFil.VPIFilNr     = 4) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 915 
            bEkstVPIFil.VPIFilNr              = 4
            bEkstVPIFil.VPIFilType            = 24
            bEkstVPIFil.VPIFilBeskrivelse     = "PRS Størrelsestyper"
            bEkstVPIFil.VPIFilNavn            = "PRSStrTyp"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine  = "xPRSStrTypInnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */

    /* Opprettelse av import for PRS varemerke */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 915 AND
                    bEkstVPIFil.VPIFilNr     = 5) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 915 
            bEkstVPIFil.VPIFilNr              = 5
            bEkstVPIFil.VPIFilType            = 25
            bEkstVPIFil.VPIFilBeskrivelse     = "PRS Varemerker"
            bEkstVPIFil.VPIFilNavn            = "PRSVarem"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine  = "xPRSVaremInnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */

    /* Opprettelse av import for PRS salgsenheter */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 915 AND
                    bEkstVPIFil.VPIFilNr     = 6) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 915 
            bEkstVPIFil.VPIFilNr              = 6
            bEkstVPIFil.VPIFilType            = 26
            bEkstVPIFil.VPIFilBeskrivelse     = "PRS Salgsenhet"
            bEkstVPIFil.VPIFilNavn            = "PRSSEnh"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine  = "xPRSSEnhInnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */

    /* Opprettelse av import for PRS varegrupper */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 915 AND
                    bEkstVPIFil.VPIFilNr     = 7) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 915 
            bEkstVPIFil.VPIFilNr              = 7
            bEkstVPIFil.VPIFilType            = 27
            bEkstVPIFil.VPIFilBeskrivelse     = "PRS Varegrupper"
            bEkstVPIFil.VPIFilNavn            = "PRSVg"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine  = "xPRSVgInnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */
    
    /* Opprettelse av import for PRS varegrupper */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 915 AND
                    bEkstVPIFil.VPIFilNr     = 8) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 915 
            bEkstVPIFil.VPIFilNr              = 8
            bEkstVPIFil.VPIFilType            = 28
            bEkstVPIFil.VPIFilBeskrivelse     = "PRS Varetelling"
            bEkstVPIFil.VPIFilNavn            = "PRSInv"
            bEkstVPIFil.VPIEkst               = "*"
            bEkstVPIFil.VPIKatalog            = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine  = "xPRSInvInnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */
    
    
    /* Opprettelse av import for PRS varegrupper */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 915 AND
                    bEkstVPIFil.VPIFilNr     = 9) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 915 
            bEkstVPIFil.VPIFilNr              = 9
            bEkstVPIFil.VPIFilType            = 2
            bEkstVPIFil.VPIFilBeskrivelse     = "PRS Pakkseddel"
            bEkstVPIFil.VPIFilNavn            = "PRSPkSdl"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine  = "xPRSPkSdlInnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */
        
    /* Opprettelse av import for PRS varegrupper */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 915 AND
                    bEkstVPIFil.VPIFilNr     = 10) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 915 
            bEkstVPIFil.VPIFilNr              = 10
            bEkstVPIFil.VPIFilType            = 29
            bEkstVPIFil.VPIFilBeskrivelse     = "PRS Materialkoder"
            bEkstVPIFil.VPIFilNavn            = "PRSMaterial"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine  = "xPRSMaterialInnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */
        
    /* Opprettelse av import for PRS translogg transaksjoner */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 915 AND
                    bEkstVPIFil.VPIFilNr     = 11) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 915 
            bEkstVPIFil.VPIFilNr              = 11
            bEkstVPIFil.VPIFilType            = 30
            bEkstVPIFil.VPIFilBeskrivelse     = "PRS Translogg transaksjoner"
            bEkstVPIFil.VPIFilNavn            = "PRSTrans"
            bEkstVPIFil.VPIEkst               = "*"
            bEkstVPIFil.VPIKatalog            = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine  = "xPRSTransInnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */
        
    /* Opprettelse av import for PRS fargekoder */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 915 AND
                    bEkstVPIFil.VPIFilNr     = 12) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 915 
            bEkstVPIFil.VPIFilNr              = 12
            bEkstVPIFil.VPIFilType            = 31
            bEkstVPIFil.VPIFilBeskrivelse     = "PRS Fargekoder"
            bEkstVPIFil.VPIFilNavn            = "PRSFarg"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine  = "xPRSFargInnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */
                
    /* Opprettelse av import for PRS gavekort og tilgodelapper */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 915 AND
                    bEkstVPIFil.VPIFilNr     = 14) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 915 
            bEkstVPIFil.VPIFilNr              = 14
            bEkstVPIFil.VPIFilType            = 32
            bEkstVPIFil.VPIFilBeskrivelse     = "PRS Gavekort og Tilgodelapper"
            bEkstVPIFil.VPIFilNavn            = "PRSGAVTIL"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine  = "xPRSGavTilInnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */
        
    /* Opprettelse av import for PRS gavekort og tilgodelapper */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 915 AND
                    bEkstVPIFil.VPIFilNr     = 15) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 915 
            bEkstVPIFil.VPIFilNr              = 15
            bEkstVPIFil.VPIFilType            = 33
            bEkstVPIFil.VPIFilBeskrivelse     = "PRS mappingtabell"
            bEkstVPIFil.VPIFilNavn            = "PRSMapping"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine  = "xPRSMappingInnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */
        
    /* Opprettelse av import for postnr. register */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
        bEkstVPIFil.EkstVPILevNr = 915 AND
        bEkstVPIFil.VPIFilNr     = 16) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr         = 915 
            bEkstVPIFil.VPIFilNr             = 16
            bEkstVPIFil.VPIFilType           = 34
            bEkstVPIFil.VPIFilBeskrivelse    = "PRS postnr. import"
            bEkstVPIFil.VPIFilNavn           = "PRSPost"
            bEkstVPIFil.VPIEkst              = "csv"
            bEkstVPIFil.VPIKatalog           = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine = "xPRSPostInnles"
            bEkstVPIFil.VPIUtpakkingsrutine  = "xstdutpakk"
            bEkstVPIFil.VPIOperator          = 2
            bEkstVPIFil.VPIFilAktiv          = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */
    
    /* Opprettelse av import for postnr. register */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
        bEkstVPIFil.EkstVPILevNr = 915 AND
        bEkstVPIFil.VPIFilNr     = 17) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr         = 915 
            bEkstVPIFil.VPIFilNr             = 17
            bEkstVPIFil.VPIFilType           = 35
            bEkstVPIFil.VPIFilBeskrivelse    = "PRS kampanje import"
            bEkstVPIFil.VPIFilNavn           = "PRSKamp"
            bEkstVPIFil.VPIEkst              = "csv"
            bEkstVPIFil.VPIKatalog           = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine = "xPRSKampInnles"
            bEkstVPIFil.VPIUtpakkingsrutine  = "xstdutpakk"
            bEkstVPIFil.VPIOperator          = 2
            bEkstVPIFil.VPIFilAktiv          = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */
    
    /* Opprettelse av import for salgsbudsjett */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
        bEkstVPIFil.EkstVPILevNr = 915 AND
        bEkstVPIFil.VPIFilNr     = 18) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr         = 915 
            bEkstVPIFil.VPIFilNr             = 18
            bEkstVPIFil.VPIFilType           = 36
            bEkstVPIFil.VPIFilBeskrivelse    = "PRS salgsbudsjett import"
            bEkstVPIFil.VPIFilNavn           = "PRSSBud"
            bEkstVPIFil.VPIEkst              = "csv"
            bEkstVPIFil.VPIKatalog           = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine = "xPRSSBudInnles"
            bEkstVPIFil.VPIUtpakkingsrutine  = "xstdutpakk"
            bEkstVPIFil.VPIOperator          = 2
            bEkstVPIFil.VPIFilAktiv          = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */
    
    /* Opprettelse av import for salgsbudsjett */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
        bEkstVPIFil.EkstVPILevNr = 915 AND
        bEkstVPIFil.VPIFilNr     = 19) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr         = 915 
            bEkstVPIFil.VPIFilNr             = 19
            bEkstVPIFil.VPIFilType           = 24
            bEkstVPIFil.VPIFilBeskrivelse    = "PRS størrelsestype SGIDHO import"
            bEkstVPIFil.VPIFilNavn           = "PRSStrTypSGIDHO"
            bEkstVPIFil.VPIEkst              = "xlsx"
            bEkstVPIFil.VPIKatalog           = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine = "xPRSStrTypSGIDHOInnles"
            bEkstVPIFil.VPIUtpakkingsrutine  = "xstdutpakk"
            bEkstVPIFil.VPIOperator          = 2
            bEkstVPIFil.VPIFilAktiv          = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */

    /* Opprettelse av import for PRS størrelsestyper */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 915 AND
                    bEkstVPIFil.VPIFilNr     = 20) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 915 
            bEkstVPIFil.VPIFilNr              = 20
            bEkstVPIFil.VPIFilType            = 37
            bEkstVPIFil.VPIFilBeskrivelse     = "PRS Fedas grupper"
            bEkstVPIFil.VPIFilNavn            = "PRSFedasGrp"
            bEkstVPIFil.VPIEkst               = "xlsx"
            bEkstVPIFil.VPIKatalog            = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine  = "xPRSFedasGrpInnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */

    
    /* Opprettelse av import for salgsbudsjett */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
        bEkstVPIFil.EkstVPILevNr = 892 AND
        bEkstVPIFil.VPIFilNr     = 6) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr         = 892 
            bEkstVPIFil.VPIFilNr             = 6
            bEkstVPIFil.VPIFilType           = 12
            bEkstVPIFil.VPIFilBeskrivelse    = "Varemottak til RFID etikett"
            bEkstVPIFil.VPIFilNavn           = "VMOTRFID"
            bEkstVPIFil.VPIEkst              = "*"
            bEkstVPIFil.VPIKatalog           = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine = "xbxvmotRFIDinnles"
            bEkstVPIFil.VPIUtpakkingsrutine  = "xstdutpakk"
            bEkstVPIFil.VPIOperator          = 2
            bEkstVPIFil.VPIFilAktiv          = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */

    /* Opprettelse av import for PRS countrycodes */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 915 AND
                    bEkstVPIFil.VPIFilNr     = 21) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 915 
            bEkstVPIFil.VPIFilNr              = 21
            bEkstVPIFil.VPIFilType            = 38
            bEkstVPIFil.VPIFilBeskrivelse     = "Country Codes"
            bEkstVPIFil.VPIFilNavn            = "PRSCountryCodes"
            bEkstVPIFil.VPIEkst               = "xlsx"
            bEkstVPIFil.VPIKatalog            = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine  = "xPRSCountryCodesInnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */

    

END PROCEDURE.

&IF DEFINED(EXCLUDE-setVPIFiltyper) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setVPIFiltyper Procedure 
PROCEDURE setVPIFiltyper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR pcTypeLst  AS CHAR NO-UNDO.
DEF VAR pcKortLst  AS CHAR NO-UNDO.
DEF VAR pcBeskrLst AS CHAR NO-UNDO.
DEF VAR piType     AS INT  NO-UNDO.
DEF VAR pcBeskr    AS CHAR NO-UNDO.
DEF VAR piLoop     AS INT  NO-UNDO.
DEF VAR pcKort     AS CHAR NO-UNDO.

DEF BUFFER bVPIFiltype FOR VPIFilType.

ASSIGN        
    pcTypeLst  = "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21"
    pcKortLst  = "VPI,VPIKonv,LAG,ORD,POSVPI,KORRVPI,PLUKKLISTE,VarefilPDA,WLAG,SALG,Varemottak,PKSDL,PKSDLHK,MED,KORD,KASSDAG,BUDGET,STRIND,LAGERJUST,PRISKONTR,OVERFOR"
    pcBeskrLst = "Vare og prisinformasjon," +
                 "Konvertering til pricat," +
                 "Lagerstatus fra leverandør," +
                 "Ordre og ordrebekreftelse," + 
                 "Lokal VPI fra butikk," +
                 "Korr. VPI melding fra HK," +
                 "Plukkliste fra PDA," +
                 "Varefil fra PDA," +
                 "Lagerstatus fra webbutikk," +
                 "Salgsdata fra butikk," + 
                 "Varemottak," + 
                 "Pakkseddel fra VismaGlobal," +
                 "Pakkseddel fra HK," + 
                 "Medlemsinformasjon," + 
                 "Kundeordre," + 
                 "Kassereroppgjør InfoPOS 8.0," + 
                 "Budsjettunderlag," + 
                 "Str.ind," + 
                 "Lagerjustering," + 
                 "Priskontroll," + 
                 "Lageroverføring"
    .

STRONG_SCOOP:
DO FOR bVPIFilType piLoop = 1 TO NUM-ENTRIES(pcTypeLst) TRANSACTION:
    ASSIGN
        piType  = int(ENTRY(piLoop,pcTypeLst))
        pcKort  = ENTRY(piLoop,pcKortLst)
        pcBeskr = ENTRY(piLoop,pcBeskrLst)
        .
    IF NOT CAN-FIND(bVPIFilType WHERE
                    bVPIFilType.VPIFilTypeNr = piType) THEN
    DO:
        CREATE bVPIFilType.
        ASSIGN
            bVPIFilType.VPIFilTypeNr          = piType
            bVPIFilType.VPIFilTypeKNavn       = pcKort
            bVPIFilType.VPIFiltypeBeskrivelse = pcBeskr
            .
        RELEASE bVPIFilType.
    END.
END. /* STRONG_SCOOP */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
