DEFINE RECORD CDD$TOP.BM.BM_MAXMIN

        DESCRIPTION IS /*File to define MAX/MIN build information for reports*/.

        BM_MAXMIN_CDD STRUCTURE.

        /* Element = PRODUCT
        Description = Built Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Grouping Indicator */
        MGROUP                  DATATYPE IS TEXT SIZE IS 4.

        /* Element = QUANTITY
        Description = Maximum Quantity to build */
        MAXQTY                  DATATYPE IS G_FLOATING.

        /* Element = QUANTITY
        Description = Minimum Quantity To Build */
        MINQTY                  DATATYPE IS G_FLOATING.

        END BM_MAXMIN_CDD STRUCTURE.

END BM_MAXMIN.
