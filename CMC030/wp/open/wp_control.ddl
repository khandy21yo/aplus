DEFINE RECORD CDD$TOP.WP.WP_CONTROL

        DESCRIPTION IS /*WIP Controlling File*/.

        WP_CONTROL_CDD STRUCTURE.

        /* Element = ORDNUM
        Description = Order Number */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = DATE
        Description = Last Purge Date (YYYYMMDD) */
        PURGDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Activity Flag */
        STATUS_FLAG             DATATYPE IS TEXT SIZE IS 1.

        /* Element = REQNUM
        Description = Requisition Number */
        REQNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = ACCOUNT
        Description = Inventory Material Price Variance */
        INVMATPVAR              DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Inventory Material Usage Variance */
        INVMATUVAR              DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Inventory Labor Rate Variance */
        INVLABRVAR              DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Inventory Labor Efficiency Variance */
        INVLABEVAR              DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Inventory Buÿrdn Variance */
        INVBURVAR               DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Equipment Material Price Variance */
        EQMATPVAR               DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Equipment Material Usage Variance */
        EQMATUVAR               DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Equipment Labor Rate Variance */
        EQLABRVAR               DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Equipment Labor Efficiency Variance */
        EQLABEVAR               DATATYPE IS TEXT SIZE IS 18.

        /* Element = ACCOUNT
        Description = Equipment Burden Variance */
        EQBURVAR                DATATYPE IS TEXT SIZE IS 18.

        END WP_CONTROL_CDD STRUCTURE.

END WP_CONTROL.
