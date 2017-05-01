DEFINE RECORD CDD$TOP.MO.MO_REGLINEOPT

        DESCRIPTION IS /*Manufacturing Order Register Line Option File*/.

        MO_REGLINEOPT_CDD STRUCTURE.

        /* Element =
        Description = Order Number */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Model record line number */
        LIN                     DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Option Record line number */
        OPTLIN                  DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Option Group */
        OPTGROUP                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Option Code */
        OPTN                    DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Order Quantity */
        QTY                     DATATYPE IS G_FLOATING.

        /* Element =
        Description = Cost Per Unit */
        COST                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = Price Per Unit */
        PRICE                   DATATYPE IS G_FLOATING.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element =
        Description = Transaction Code */
        TRANTYPE                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Batch Number */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Ship No */
        SHIPNO                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = DATE
        Description = Date (YYYYMMDD) */
        POSTDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element = TIME
        Description = Time (HHMMSS) */
        POSTTIME                DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Option Description */
        OPTDESCR                DATATYPE IS TEXT SIZE IS 40.

        /* Element = DATE
        Description = Transaction Date (YYYYMMDD) */
        TDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element = PERIOD
        Description = Fiscal year (YYYY) and Cycle (PP) */
        PERIOD                  DATATYPE IS TEXT SIZE IS 6.

        END MO_REGLINEOPT_CDD STRUCTURE.

END MO_REGLINEOPT.
