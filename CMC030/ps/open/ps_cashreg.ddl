DEFINE RECORD CDD$TOP.PS.PS_CASHREG

        DESCRIPTION IS /*Cash Register Description Table*/.

        PS_CASHREG_CDD STRUCTURE.

        /* Element = CASHREG
        Description = Cash Register Number */
        CASHREG                 DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 20.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = NOTES
        Description = Notes */
        NOTES                   ARRAY 0:1 DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Last Invoice Number */
        LAST_INVNUM             DATATYPE IS TEXT SIZE IS 8.

        /* Element = ACCOUNT
        Description = Petty Cash Account Number */
        PETTYCASH               DATATYPE IS TEXT SIZE IS 18.

        END PS_CASHREG_CDD STRUCTURE.

END PS_CASHREG.
