DEFINE RECORD CDD$TOP.EL.EL_EQUIPMENT

        DESCRIPTION IS /*Equipment Ledger Descriptons Master File*/.

        EL_EQUIPMENT_CDD STRUCTURE.

        /* Element =
        Description = Subject type for equipment "E" */
        SUBJECT                 DATATYPE IS TEXT SIZE IS 1.

        /* Element = SUBACCT
        Description = Equipment Number */
        EQNUM                   DATATYPE IS TEXT SIZE IS 10.

        /* Element = DESCRIPTION
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Equipment Type */
        TTYPE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element = CLASS
        Description = Equipment Class */
        CLASS                   DATATYPE IS TEXT SIZE IS 4.

        /* Element = DATE
        Description = Creation Date */
        BDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Equipment Status */
        SSTATUS                 DATATYPE IS TEXT SIZE IS 1.

        /* Element = DATE
        Description = Closed Date */
        EDATE                   DATATYPE IS TEXT SIZE IS 8.

        /* Element = LOCATION
        Description = Equipment Location */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = OPERATOR
        Description = Operator */
        OPERATOR                DATATYPE IS TEXT SIZE IS 10.

        /* Element = REFNO
        Description = Reference Number */
        REFNO                   DATATYPE IS TEXT SIZE IS 16.

        END EL_EQUIPMENT_CDD STRUCTURE.

END EL_EQUIPMENT.
