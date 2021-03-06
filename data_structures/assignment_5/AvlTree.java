    // BinarySearchTree class
    //
    // CONSTRUCTION: with no initializer
    //
    // ******************PUBLIC OPERATIONS*********************
    // void insert( x )    --> Insert x
    // void remove( x )    --> Remove x (unimplemented)
    // int find( x )       --> Return item that matches x
    // int findMin( )      --> Return smallest item
    // int findMax( )      --> Return largest item
    // boolean isEmpty( )  --> Return true if empty; else false
    // void makeEmpty( )   --> Remove all items
    // void printTree( )   --> Print tree in sorted order

    /**
     * Implements an AVL tree.
     * Note that all "matching" is based on the compareTo method.  // Ory: This is no changed to primitive int so Comparable is no longer necessary.
     * @author Mark Allen Weiss
     */
    public class AvlTree
    {
        /**
         * Construct the tree.
         */
        public AvlTree( )
        {
            root = null;
            size = 0;
        }

        /**
         * Insert into the tree; duplicates are ignored.
         * @param x the item to insert.
         */
        public void insert( int x )
        {
            root = insert( x, root );
            size++;
        }

        /**
         * Remove from the tree. Nothing is done if x is not found.
         * @param x the item to remove.
         */
        public void remove( int x )
        {
            System.out.println( "Sorry, remove unimplemented" );
        }

        /**
         * Find the smallest item in the tree.
         * @return smallest item or null if empty.
         */
        public int findMin( )
        {
            return elementAt( findMin( root ) );
        }

        /**
         * Find the largest item in the tree.
         * @return the largest item of null if empty.
         */
        public int findMax( )
        {
            return elementAt( findMax( root ) );
        }

        /**
         * @param int AVL node to find.
         *
         * @return Integer array of [found, steps taken]. If found != 0 then node wasn't found.
         */
        public int[] steps(int x) {
            AvlNode t = root;
            int steps;

            if (t == null) {  // If tree is empty;
                int[] result = {0, 1};  // 1 step for polling root node.
                return result;
            } else {
                steps = 0;

                while(t != null) {
                    if (x < t.element) {
                        t = t.left;
                        steps++;  // Node entry equals 1 step.
                    } else if (x > t.element) {
                        t = t.right;
                        steps++;
                    }
                    else {
                        steps++;  // Node polling equals 1 step.
                        int[] result = {1, steps};
                        return result;    // Match
                    }
                }
            }

            int[] result = {0, steps};
            return result;   // No match
        }

        /**
         * Find an item in the tree.
         * @param x the item to search for.
         * @return the matching item or null if not found.
         */
        public int find( int x )
        {
            return elementAt( find( x, root ) );
        }

        /**
         * Make the tree logically empty.
         */
        public void makeEmpty( )
        {
            root = null;
        }

        /**
         * Test if the tree is logically empty.
         * @return true if empty, false otherwise.
         */
        public boolean isEmpty( )
        {
            return root == null;
        }

        /**
         * Print the tree contents in sorted order.
         */
        public void printTree( )
        {
            if( isEmpty( ) )
                System.out.println( "Empty tree" );
            else
                printTree( root );
        }

        /**
         * Internal method to get element field.
         * @param t the node.
         * @return the element field or null if t is null.
         */
        private int elementAt( AvlNode t )
        {
            return t == null ? null : t.element;
        }

        /**
         * Internal method to insert into a subtree.
         * @param x the item to insert.
         * @param t the node that roots the tree.
         * @return the new root.
         */
        private AvlNode insert( int x, AvlNode t )
        {
            if( t == null )
                t = new AvlNode( x, null, null );
            else if( x < t.element )
            {
                t.left = insert( x, t.left );
                if( height( t.left ) - height( t.right ) == 2 )
                    if( x < t.left.element )
                        t = rotateWithLeftChild( t );
                    else
                        t = doubleWithLeftChild( t );
            }
            else if( x > t.element )
            {
                t.right = insert( x, t.right );
                if( height( t.right ) - height( t.left ) == 2 )
                    if( x > t.right.element )
                        t = rotateWithRightChild( t );
                    else
                        t = doubleWithRightChild( t );
            }
            else
                ;  // Duplicate; do nothing
            t.height = max( height( t.left ), height( t.right ) ) + 1;
            return t;
        }

        /**
         * Internal method to find the smallest item in a subtree.
         * @param t the node that roots the tree.
         * @return node containing the smallest item.
         */
        private AvlNode findMin( AvlNode t )
        {
            if( t == null )
                return t;

            while( t.left != null )
                t = t.left;
            return t;
        }

        /**
         * Internal method to find the largest item in a subtree.
         * @param t the node that roots the tree.
         * @return node containing the largest item.
         */
        private AvlNode findMax( AvlNode t )
        {
            if( t == null )
                return t;

            while( t.right != null )
                t = t.right;
            return t;
        }

        /**
         * Internal method to find an item in a subtree.
         * @param x is item to search for.
         * @param t the node that roots the tree.
         * @return node containing the matched item.
         */
        private AvlNode find( int x, AvlNode t )
        {
            while( t != null )
                if( x < t.element )
                    t = t.left;
                else if( x > t.element )
                    t = t.right;
                else
                    return t;    // Match

            return null;   // No match
        }

        /**
         * Internal method to print a subtree in sorted order.
         * @param t the node that roots the tree.
         */
        private void printTree( AvlNode t )
        {
            if( t != null )
            {
                printTree( t.left );
                System.out.println( t.element );
                printTree( t.right );
            }
        }

        /**
         * @return Amount of nodes in tree.
         */
        public int size() {
            return this.size;
        }

        /**
         * @return Tree's height.
         */
        public int height() {
            return height(root);
        }

        /**
         * Return the height of node t, or -1, if null.
         */
        private static int height( AvlNode t )
        {
            return t == null ? 0 : t.height;
        }

        /**
         * Return maximum of lhs and rhs.
         */
        private static int max( int lhs, int rhs )
        {
            return lhs > rhs ? lhs : rhs;
        }

        /**
         * Rotate binary tree node with left child.
         * For AVL trees, this is a single rotation for case 1.
         * Update heights, then return new root.
         */
        private static AvlNode rotateWithLeftChild( AvlNode k2 )
        {
            AvlNode k1 = k2.left;
            k2.left = k1.right;
            k1.right = k2;
            k2.height = max( height( k2.left ), height( k2.right ) ) + 1;
            k1.height = max( height( k1.left ), k2.height ) + 1;
            return k1;
        }

        /**
         * Rotate binary tree node with right child.
         * For AVL trees, this is a single rotation for case 4.
         * Update heights, then return new root.
         */
        private static AvlNode rotateWithRightChild( AvlNode k1 )
        {
            AvlNode k2 = k1.right;
            k1.right = k2.left;
            k2.left = k1;
            k1.height = max( height( k1.left ), height( k1.right ) ) + 1;
            k2.height = max( height( k2.right ), k1.height ) + 1;
            return k2;
        }

        /**
         * Double rotate binary tree node: first left child
         * with its right child; then node k3 with new left child.
         * For AVL trees, this is a double rotation for case 2.
         * Update heights, then return new root.
         */
        private static AvlNode doubleWithLeftChild( AvlNode k3 )
        {
            k3.left = rotateWithRightChild( k3.left );
            return rotateWithLeftChild( k3 );
        }

        /**
         * Double rotate binary tree node: first right child
         * with its left child; then node k1 with new right child.
         * For AVL trees, this is a double rotation for case 3.
         * Update heights, then return new root.
         */
        private static AvlNode doubleWithRightChild( AvlNode k1 )
        {
            k1.right = rotateWithLeftChild( k1.right );
            return rotateWithRightChild( k1 );
        }

          /** The tree root. */
        private AvlNode root;
        private int size;


            // Test program
        public static void main( String [ ] args )
        {
            AvlTree t = new AvlTree( );
            final int NUMS = 4000;
            final int GAP  =   37;

            System.out.println( "Checking... (no more output means success)" );

            for( int i = GAP; i != 0; i = ( i + GAP ) % NUMS )
                t.insert( i );

            if( NUMS < 40 )
                t.printTree( );
            if( t.findMin( ) != 1 ||
                t.findMax( ) != NUMS - 1 )
                System.out.println( "FindMin or FindMax error!" );

            for( int i = 1; i < NUMS; i++ )
                 if( t.find( i ) != i )
                     System.out.println( "Find error1!" );
    }
}
