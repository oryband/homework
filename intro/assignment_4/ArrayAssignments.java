public class ArrayAssignments implements Assignments {
    private Assignment[] assignments;

    /** @return new ArrayAssignment object with no assignments. */
    public ArrayAssignments() {
        assignments = new Assignment[0];
    }

    /**
     * @param assignments Assignment array.
     *
     * @return new ArrayAssignment object with initial assignments given as argument.
     */
    public ArrayAssignments(Assignment[] as) {
        // TODO: Check if you can have duplicates..
        assignments = new Assignment[as.length];

        for (int i=0; i < as.length; i++) {
            assignments[i] = as[i];
        }
    }

    public double valueOf(Variable v) {
        // Validate argument.
        if (var == null) {
            // TODO: Throw exception.
        }

        // Search for var in the assignments list.
        for (int i=0; i < assignments.length; i++) {
            if (assignments[i].getVar().equals(v)) {
                return assignments[i].getVar().getValue();
            }
        }

        // If var wasn't found, throw exception.
        // TODO: Throw exception.
    }

    public void addAssignment(Assignment a) {
        // Creates a new list, and add a single new assignment to it.
        Assignment[] copy = new Assignment[assignments.length +1];

        // Copies the old assignments.
        for (int i=0; i < assignments.length; i++) {
            copy[i] = assignments[i];
        }

        // Add the new assignment to the end of the list.
        copy[copy.length -1] = a;

        // Replace the old assignments list with new one.
        this.assignments = copy;
    }
}

