DEFINE RECORD CDD$TOP.OE.OE_SHIPJOUR

        DESCRIPTION IS /*Shipping Journal Header File*/.

        OE_SHIPJOUR_CDD STRUCTURE.

        /* Element = ORDNUM
        Description = Order Number */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = DATE
        Description = Shipping Date (YYYYMMDD) */
        SHIPDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = CARRIER
        Description = Carrier Code (Ship Via) */
        SHIPVIA                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = OPERATOR
        Description = Operator */
        OPERATOR                DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Notes */
        NOTES                   ARRAY 0:3 DATATYPE IS TEXT SIZE IS 40.

        /* Element = SHIPNO
        Description = Packing List Release Number */
        SHIPNO                  DATATYPE IS TEXT SIZE IS 2.

        END OE_SHIPJOUR_CDD STRUCTURE.

END OE_SHIPJOUR.
