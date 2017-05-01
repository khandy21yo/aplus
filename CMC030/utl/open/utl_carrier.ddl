DEFINE RECORD CDD$TOP.UTL.UTL_CARRIER

        DESCRIPTION IS /*Table of Carriers (Ship-via)*/.

        UTL_CARRIER_CDD STRUCTURE.

        /* Element =
        Description = Carrier Code */
        CODE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        END UTL_CARRIER_CDD STRUCTURE.

END UTL_CARRIER.
