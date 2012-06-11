    // Basic node stored in AVL trees
    // Note that this class is not accessible outside
    // of package DataStructures

    class AvlNode
    {
            // Constructors
        AvlNode( int theElement )
        {
            this( theElement, null, null );
        }

        AvlNode( int theElement, AvlNode lt, AvlNode rt )
        {
            element  = theElement;
            left     = lt;
            right    = rt;
            height   = 0;
        }

            // Friendly data; accessible by other package routines
        int        element;      // The data in the node
        AvlNode    left;         // Left child
        AvlNode    right;        // Right child
        int        height;       // Height
    }
