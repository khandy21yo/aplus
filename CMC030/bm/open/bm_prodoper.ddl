DEFINE RECORD CDD$TOP.BM.BM_PRODOPER

        DESCRIPTION IS /*Product operation table*/.

        BM_PRODOPER_CDD STRUCTURE.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = ITEMNUM
        Description = Oparation seq. number */
        ITEMNUM                 DATATYPE IS TEXT SIZE IS 4.

        /* Element = OPERATION
        Description = Operation */
        OPERATION               DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Number of hours */
        HOURS                   DATATYPE IS G_FLOATING.

        /* Element = DATE
        Description = Effective Date (YYYYMMDD) */
        EFFDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Status (A,O..) */
        STAT                    DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Number of hours on this level */
        THISHOURS               DATATYPE IS G_FLOATING.

        END BM_PRODOPER_CDD STRUCTURE.

END BM_PRODOPER.
