DEFINE RECORD CDD$TOP.PO.PO_RECJOUR

        DESCRIPTION IS /*PO Receiver Journal Header*/.

        PO_RECJOUR_CDD STRUCTURE.

        /* Element = PO
        Description = Purchase order number */
        PO                      DATATYPE IS TEXT SIZE IS 10.

        /* Element = DATE
        Description = Receive Date (YYYYMMDD) */
        RECDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element = REFNO
        Description = Reference number */
        REFNO                   DATATYPE IS TEXT SIZE IS 16.

        /* Element = OPERATOR
        Description = Operator */
        OPERATOR                DATATYPE IS TEXT SIZE IS 10.

        END PO_RECJOUR_CDD STRUCTURE.

END PO_RECJOUR.
