DEFINE RECORD CDD$TOP.PO.PO_TYPE

        DESCRIPTION IS /*Purchase Order Type*/.

        PO_TYPE_CDD STRUCTURE.

        /* Element =
        Description = PO Type */
        POTYPE                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        END PO_TYPE_CDD STRUCTURE.

END PO_TYPE.
