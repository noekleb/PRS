TRIGGER PROCEDURE FOR DELETE OF VPIFilHode.

/* Sletter buffer for innlest fil. */

FOR EACH VPIFillinje OF VPIFilHode:
    DELETE VPIFilLinje.
END.

/* Nullstiller loggen for filen */
FOR EACH VPIFilLogg OF VPIFilHode:
    DELETE VPIFilLogg.
END.




