TRIGGER PROCEDURE FOR DELETE OF OvBunt.

DEF buffer trgOvBuffer for Ovbuffer.

for each trgOvBuffer of Ovbunt:
    delete Ovbuffer.
end.
