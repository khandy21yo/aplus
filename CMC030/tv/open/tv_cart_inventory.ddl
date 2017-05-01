DEFINE RECORD CDD$TOP.TV.TV_CART_INVENTORY

        DESCRIPTION IS /*TV Cart Inventory Master File*/.

        TV_CART_INVENTORY_CDD STRUCTURE.

        /* Element =
        Description = House Cart number */
        CARTNUM                 DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Production name */
        PRONAME                 DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Customer name */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Agency number */
        AGENCY_NUM              DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Title */
        TITLE                   DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Length */
        LENGTH                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Date in */
        DATE_IN                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Date out */
        DATE_OUT                DATATYPE IS TEXT SIZE IS 8.

        END TV_CART_INVENTORY_CDD STRUCTURE.

END TV_CART_INVENTORY.
