DEFINE RECORD CDD$TOP.PO.PO_ACKNOWLEDGE

        DESCRIPTION IS /*Acknowledgement Codes*/.

        PO_ACKNOWLEDGE_CDD STRUCTURE.

        /* Element =
        Description = Acknowledgement Code */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        END PO_ACKNOWLEDGE_CDD STRUCTURE.

END PO_ACKNOWLEDGE.
