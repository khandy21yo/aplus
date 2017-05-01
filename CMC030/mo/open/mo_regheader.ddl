DEFINE RECORD CDD$TOP.MO.MO_REGHEADER

        DESCRIPTION IS /*MO Register Header*/.

        MO_REGHEADER_CDD STRUCTURE.

        /* Element = ORDNUM
        Description = Order Number */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = ORDTYPE
        Description = Sales Order Type */
        ORDTYPE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Order Category */
        ORDCAT                  DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Order Date */
        ORDDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element = ASTATUS
        Description = Activity status */
        ASTATUS                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Date */
        SDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Shipping Name */
        SHIPNAM                 DATATYPE IS TEXT SIZE IS 50.

        /* Element =
        Description = Address, line 1 */
        ADD1                    DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = Address, line 2 */
        ADD2                    DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = Address, line 3 */
        ADD3                    DATATYPE IS TEXT SIZE IS 25.

        /* Element =
        Description = City */
        CITY                    DATATYPE IS TEXT SIZE IS 15.

        /* Element =
        Description = State */
        STATE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Zip Code */
        ZIP                     DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Country */
        COUNTRY                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Customer PO. */
        CUSTPO                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Ship Via */
        SHIPVIA                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Terms */
        TERMS                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Discount Percentage */
        DISC                    DATATYPE IS G_FLOATING.

        /* Element = TAXCODE
        Description = Tax code */
        TAXCODE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element = TAXFLAG
        Description = Tax Flag */
        TAXFLAG                 DATATYPE IS TEXT SIZE IS 1.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Commission amount */
        COMMAMT                 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Salesmen */
        SALESMAN                ARRAY 0:1 DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Commission for salesmen */
        SALCOMM                 ARRAY 0:1 DATATYPE IS G_FLOATING.

        /* Element =
        Description = Packing List Release Number */
        SHIPNO                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = BATCH
        Description = Batch number used for process (post,clos */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        END MO_REGHEADER_CDD STRUCTURE.

END MO_REGHEADER.
