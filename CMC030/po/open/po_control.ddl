DEFINE RECORD CDD$TOP.PO.PO_CONTROL

        DESCRIPTION IS /*Purchase Order Controling File*/.

        PO_CONTROL_CDD STRUCTURE.

        /* Element =
        Description = Last Purchase Order number */
        LAST_PO                 DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Formula for calculating re-order qty */
        LOAD_FORMULA            DATATYPE IS TEXT SIZE IS 10.

        END PO_CONTROL_CDD STRUCTURE.

END PO_CONTROL.
